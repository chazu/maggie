package vm

import (
	"database/sql"
	"fmt"
	"reflect"
	"strings"
	"sync"

	_ "modernc.org/sqlite"
)

// ---------------------------------------------------------------------------
// SQLite Primitives — Go-level types
// ---------------------------------------------------------------------------

// SqliteDatabaseObject wraps a Go *sql.DB for use in Maggie.
type SqliteDatabaseObject struct {
	db     *sql.DB
	path   string
	closed bool
	mu     sync.Mutex
}

// SqliteStatementObject wraps a Go *sql.Stmt for use in Maggie.
type SqliteStatementObject struct {
	stmt   *sql.Stmt
	query  string
	dbObj  *SqliteDatabaseObject
	closed bool
}

// SqliteRowsObject wraps *sql.Rows for iteration in Maggie.
type SqliteRowsObject struct {
	rows    *sql.Rows
	columns []string
	closed  bool
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerSqlitePrimitives() {
	// Register Go types for GoObject dispatch
	dbType := reflect.TypeOf((*SqliteDatabaseObject)(nil))
	stmtType := reflect.TypeOf((*SqliteStatementObject)(nil))
	rowsType := reflect.TypeOf((*SqliteRowsObject)(nil))

	dbClass := vm.RegisterGoType("SqliteDatabase", dbType)
	stmtClass := vm.RegisterGoType("SqliteStatement", stmtType)
	rowsClass := vm.RegisterGoType("SqliteRows", rowsType)

	vm.registerSqliteDatabasePrimitives(dbClass)
	vm.registerSqliteStatementPrimitives(stmtClass)
	vm.registerSqliteRowsPrimitives(rowsClass)
}

// ---------------------------------------------------------------------------
// SqliteDatabase primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSqliteDatabasePrimitives(dbClass *Class) {
	// open: path — Open a SQLite database at the given path
	dbClass.AddClassMethod1(vm.Selectors, "primOpen:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("SqliteDatabase open: requires a path string")
		}

		db, err := sql.Open("sqlite", path)
		if err != nil {
			return v.newFailureResult("Cannot open database: " + err.Error())
		}

		// Ping to verify connection
		if err := db.Ping(); err != nil {
			db.Close()
			return v.newFailureResult("Cannot connect to database: " + err.Error())
		}

		obj := &SqliteDatabaseObject{db: db, path: path}
		val, regErr := v.RegisterGoObject(obj)
		if regErr != nil {
			db.Close()
			return v.newFailureResult("Cannot register database: " + regErr.Error())
		}
		return val
	})

	// openMemory — Open an in-memory SQLite database
	dbClass.AddClassMethod0(vm.Selectors, "primOpenMemory", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)

		db, err := sql.Open("sqlite", ":memory:")
		if err != nil {
			return v.newFailureResult("Cannot open in-memory database: " + err.Error())
		}

		obj := &SqliteDatabaseObject{db: db, path: ":memory:"}
		val, regErr := v.RegisterGoObject(obj)
		if regErr != nil {
			db.Close()
			return v.newFailureResult("Cannot register database: " + regErr.Error())
		}
		return val
	})

	// close — Close the database
	dbClass.AddMethod0(vm.Selectors, "primClose", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		dbObj.mu.Lock()
		defer dbObj.mu.Unlock()
		if dbObj.closed {
			return v.newFailureResult("Database already closed")
		}
		err := dbObj.db.Close()
		dbObj.closed = true
		if err != nil {
			return v.newFailureResult("Error closing database: " + err.Error())
		}
		return True
	})

	// execute: sql — Execute SQL that returns no rows (CREATE, INSERT, UPDATE, DELETE)
	dbClass.AddMethod1(vm.Selectors, "primExecute:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("execute: requires a SQL string")
		}

		result, err := dbObj.db.Exec(sqlStr)
		if err != nil {
			return v.newFailureResult("SQL error: " + err.Error())
		}

		rowsAffected, _ := result.RowsAffected()
		return FromSmallInt(rowsAffected)
	})

	// execute:with: sql params — Execute SQL with positional parameters (array)
	dbClass.AddMethod2(vm.Selectors, "primExecuteWith:params:", func(vmPtr interface{}, recv Value, sqlVal, paramsVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("execute:with: requires a SQL string")
		}

		args := v.valueToGoArgs(paramsVal)
		result, err := dbObj.db.Exec(sqlStr, args...)
		if err != nil {
			return v.newFailureResult("SQL error: " + err.Error())
		}

		rowsAffected, _ := result.RowsAffected()
		return FromSmallInt(rowsAffected)
	})

	// query: sql — Execute SQL that returns rows (SELECT)
	dbClass.AddMethod1(vm.Selectors, "primQuery:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("query: requires a SQL string")
		}

		rows, err := dbObj.db.Query(sqlStr)
		if err != nil {
			return v.newFailureResult("SQL error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.newFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// query:with: sql params — Execute SQL query with positional parameters
	dbClass.AddMethod2(vm.Selectors, "primQueryWith:params:", func(vmPtr interface{}, recv Value, sqlVal, paramsVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("query:with: requires a SQL string")
		}

		args := v.valueToGoArgs(paramsVal)
		rows, err := dbObj.db.Query(sqlStr, args...)
		if err != nil {
			return v.newFailureResult("SQL error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.newFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// queryRow: sql — Execute SQL and return a single row as a Dictionary
	dbClass.AddMethod1(vm.Selectors, "primQueryRow:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("queryRow: requires a SQL string")
		}

		rows, err := dbObj.db.Query(sqlStr)
		if err != nil {
			return v.newFailureResult("SQL error: " + err.Error())
		}
		defer rows.Close()

		return v.rowToDict(rows)
	})

	// queryRow:with: sql params — Execute SQL with params and return a single row
	dbClass.AddMethod2(vm.Selectors, "primQueryRowWith:params:", func(vmPtr interface{}, recv Value, sqlVal, paramsVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("queryRow:with: requires a SQL string")
		}

		args := v.valueToGoArgs(paramsVal)
		rows, err := dbObj.db.Query(sqlStr, args...)
		if err != nil {
			return v.newFailureResult("SQL error: " + err.Error())
		}
		defer rows.Close()

		return v.rowToDict(rows)
	})

	// queryAll: sql — Execute SQL and return all rows as an Array of Dictionaries
	dbClass.AddMethod1(vm.Selectors, "primQueryAll:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		return v.queryAllRows(dbObj, sqlStr, nil)
	})

	// queryAll:with: sql params — Execute SQL with params and return all rows
	dbClass.AddMethod2(vm.Selectors, "primQueryAllWith:params:", func(vmPtr interface{}, recv Value, sqlVal, paramsVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		args := v.valueToGoArgs(paramsVal)
		return v.queryAllRows(dbObj, sqlStr, args)
	})

	// prepare: sql — Prepare a statement for repeated execution
	dbClass.AddMethod1(vm.Selectors, "primPrepare:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("prepare: requires a SQL string")
		}

		stmt, err := dbObj.db.Prepare(sqlStr)
		if err != nil {
			return v.newFailureResult("Cannot prepare statement: " + err.Error())
		}

		stmtObj := &SqliteStatementObject{stmt: stmt, query: sqlStr, dbObj: dbObj}
		val, regErr := v.RegisterGoObject(stmtObj)
		if regErr != nil {
			stmt.Close()
			return v.newFailureResult("Cannot register statement: " + regErr.Error())
		}
		return val
	})

	// pragma: name — Get a PRAGMA value
	dbClass.AddMethod1(vm.Selectors, "primPragma:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		name := v.valueToString(nameVal)
		var result string
		err := dbObj.db.QueryRow("PRAGMA " + name).Scan(&result)
		if err != nil {
			return v.newFailureResult("PRAGMA error: " + err.Error())
		}
		return v.registry.NewStringValue(result)
	})

	// pragma:set: name value — Set a PRAGMA value
	dbClass.AddMethod2(vm.Selectors, "primPragmaSet:value:", func(vmPtr interface{}, recv Value, nameVal, valVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		name := v.valueToString(nameVal)
		val := v.valueToString(valVal)
		_, err := dbObj.db.Exec(fmt.Sprintf("PRAGMA %s = %s", name, val))
		if err != nil {
			return v.newFailureResult("PRAGMA error: " + err.Error())
		}
		return True
	})

	// enableWAL — Enable WAL journal mode
	dbClass.AddMethod0(vm.Selectors, "primEnableWAL", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		var result string
		err := dbObj.db.QueryRow("PRAGMA journal_mode=WAL").Scan(&result)
		if err != nil {
			return v.newFailureResult("Cannot enable WAL: " + err.Error())
		}
		return v.registry.NewStringValue(result)
	})

	// beginTransaction — Start a new transaction, returns the db for chaining
	dbClass.AddMethod0(vm.Selectors, "primBeginTransaction", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		_, err := dbObj.db.Exec("BEGIN TRANSACTION")
		if err != nil {
			return v.newFailureResult("Cannot begin transaction: " + err.Error())
		}
		return recv
	})

	// commitTransaction — Commit the current transaction
	dbClass.AddMethod0(vm.Selectors, "primCommitTransaction", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		_, err := dbObj.db.Exec("COMMIT")
		if err != nil {
			return v.newFailureResult("Cannot commit transaction: " + err.Error())
		}
		return True
	})

	// rollbackTransaction — Rollback the current transaction
	dbClass.AddMethod0(vm.Selectors, "primRollbackTransaction", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		_, err := dbObj.db.Exec("ROLLBACK")
		if err != nil {
			return v.newFailureResult("Cannot rollback transaction: " + err.Error())
		}
		return True
	})

	// path — Get the database path
	dbClass.AddMethod0(vm.Selectors, "primPath", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		return v.registry.NewStringValue(dbObj.path)
	})

	// isClosed — Check if the database is closed
	dbClass.AddMethod0(vm.Selectors, "primIsClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return False
		}
		if dbObj.closed {
			return True
		}
		return False
	})

	// lastInsertId — Get the last insert row ID (via a query)
	dbClass.AddMethod0(vm.Selectors, "primLastInsertId", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		var id int64
		err := dbObj.db.QueryRow("SELECT last_insert_rowid()").Scan(&id)
		if err != nil {
			return v.newFailureResult("Cannot get last insert ID: " + err.Error())
		}
		return FromSmallInt(id)
	})

	// tableExists: name — Check if a table exists
	dbClass.AddMethod1(vm.Selectors, "primTableExists:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		name := v.valueToString(nameVal)
		var count int
		err := dbObj.db.QueryRow(
			"SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?", name,
		).Scan(&count)
		if err != nil {
			return v.newFailureResult("Error checking table: " + err.Error())
		}
		if count > 0 {
			return True
		}
		return False
	})

	// tables — List all table names
	dbClass.AddMethod0(vm.Selectors, "primTables", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		rows, err := dbObj.db.Query("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
		if err != nil {
			return v.newFailureResult("Error listing tables: " + err.Error())
		}
		defer rows.Close()

		var names []Value
		for rows.Next() {
			var name string
			if err := rows.Scan(&name); err != nil {
				return v.newFailureResult("Error scanning table name: " + err.Error())
			}
			names = append(names, v.registry.NewStringValue(name))
		}
		return v.NewArrayWithElements(names)
	})

	// version — Get SQLite version string
	dbClass.AddMethod0(vm.Selectors, "primVersion", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		var version string
		err := dbObj.db.QueryRow("SELECT sqlite_version()").Scan(&version)
		if err != nil {
			return v.newFailureResult("Cannot get version: " + err.Error())
		}
		return v.registry.NewStringValue(version)
	})

	// migrate:version: sql version — Run a migration if not already applied
	dbClass.AddMethod2(vm.Selectors, "primMigrate:version:", func(vmPtr interface{}, recv Value, sqlVal, versionVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		sqlStr := v.valueToString(sqlVal)
		var version int64
		if versionVal.IsSmallInt() {
			version = versionVal.SmallInt()
		} else {
			return v.newFailureResult("migrate:version: version must be an integer")
		}

		// Create migrations table if it doesn't exist
		_, err := dbObj.db.Exec(`CREATE TABLE IF NOT EXISTS _maggie_migrations (
			version INTEGER PRIMARY KEY,
			applied_at TEXT DEFAULT (datetime('now'))
		)`)
		if err != nil {
			return v.newFailureResult("Cannot create migrations table: " + err.Error())
		}

		// Check if migration already applied
		var count int
		err = dbObj.db.QueryRow("SELECT COUNT(*) FROM _maggie_migrations WHERE version = ?", version).Scan(&count)
		if err != nil {
			return v.newFailureResult("Cannot check migration status: " + err.Error())
		}
		if count > 0 {
			return False // Already applied
		}

		// Apply migration in a transaction
		tx, err := dbObj.db.Begin()
		if err != nil {
			return v.newFailureResult("Cannot begin migration transaction: " + err.Error())
		}

		_, err = tx.Exec(sqlStr)
		if err != nil {
			tx.Rollback()
			return v.newFailureResult("Migration failed: " + err.Error())
		}

		_, err = tx.Exec("INSERT INTO _maggie_migrations (version) VALUES (?)", version)
		if err != nil {
			tx.Rollback()
			return v.newFailureResult("Cannot record migration: " + err.Error())
		}

		if err := tx.Commit(); err != nil {
			return v.newFailureResult("Cannot commit migration: " + err.Error())
		}

		return True // Migration applied
	})

	// migrationVersion — Get the highest applied migration version
	dbClass.AddMethod0(vm.Selectors, "primMigrationVersion", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getSqliteDB(recv)
		if dbObj == nil {
			return v.newFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.newFailureResult("Database is closed")
		}

		// Check if migrations table exists
		var count int
		err := dbObj.db.QueryRow(
			"SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='_maggie_migrations'",
		).Scan(&count)
		if err != nil {
			return v.newFailureResult("Error checking migrations table: " + err.Error())
		}
		if count == 0 {
			return FromSmallInt(0)
		}

		var version int64
		err = dbObj.db.QueryRow("SELECT COALESCE(MAX(version), 0) FROM _maggie_migrations").Scan(&version)
		if err != nil {
			return v.newFailureResult("Cannot get migration version: " + err.Error())
		}
		return FromSmallInt(version)
	})
}

// ---------------------------------------------------------------------------
// SqliteStatement primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSqliteStatementPrimitives(stmtClass *Class) {
	// execute — Execute the prepared statement with no params
	stmtClass.AddMethod0(vm.Selectors, "primExecute", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stmtObj := v.getSqliteStmt(recv)
		if stmtObj == nil {
			return v.newFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.newFailureResult("Statement is closed")
		}

		result, err := stmtObj.stmt.Exec()
		if err != nil {
			return v.newFailureResult("Statement execute error: " + err.Error())
		}
		rowsAffected, _ := result.RowsAffected()
		return FromSmallInt(rowsAffected)
	})

	// executeWith: params — Execute the prepared statement with params (array)
	stmtClass.AddMethod1(vm.Selectors, "primExecuteWith:", func(vmPtr interface{}, recv Value, paramsVal Value) Value {
		v := vmPtr.(*VM)
		stmtObj := v.getSqliteStmt(recv)
		if stmtObj == nil {
			return v.newFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.newFailureResult("Statement is closed")
		}

		args := v.valueToGoArgs(paramsVal)
		result, err := stmtObj.stmt.Exec(args...)
		if err != nil {
			return v.newFailureResult("Statement execute error: " + err.Error())
		}
		rowsAffected, _ := result.RowsAffected()
		return FromSmallInt(rowsAffected)
	})

	// query — Query with the prepared statement, no params
	stmtClass.AddMethod0(vm.Selectors, "primQuery", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stmtObj := v.getSqliteStmt(recv)
		if stmtObj == nil {
			return v.newFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.newFailureResult("Statement is closed")
		}

		rows, err := stmtObj.stmt.Query()
		if err != nil {
			return v.newFailureResult("Statement query error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.newFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// queryWith: params — Query with the prepared statement and params
	stmtClass.AddMethod1(vm.Selectors, "primQueryWith:", func(vmPtr interface{}, recv Value, paramsVal Value) Value {
		v := vmPtr.(*VM)
		stmtObj := v.getSqliteStmt(recv)
		if stmtObj == nil {
			return v.newFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.newFailureResult("Statement is closed")
		}

		args := v.valueToGoArgs(paramsVal)
		rows, err := stmtObj.stmt.Query(args...)
		if err != nil {
			return v.newFailureResult("Statement query error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.newFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// close — Close the prepared statement
	stmtClass.AddMethod0(vm.Selectors, "primClose", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stmtObj := v.getSqliteStmt(recv)
		if stmtObj == nil {
			return v.newFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.newFailureResult("Statement already closed")
		}
		err := stmtObj.stmt.Close()
		stmtObj.closed = true
		if err != nil {
			return v.newFailureResult("Error closing statement: " + err.Error())
		}
		return True
	})

	// sql — Get the SQL string used to create this statement
	stmtClass.AddMethod0(vm.Selectors, "primSql", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		stmtObj := v.getSqliteStmt(recv)
		if stmtObj == nil {
			return v.newFailureResult("Not a SqliteStatement")
		}
		return v.registry.NewStringValue(stmtObj.query)
	})
}

// ---------------------------------------------------------------------------
// SqliteRows primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSqliteRowsPrimitives(rowsClass *Class) {
	// next — Advance to next row. Returns true if a row is available, false if done.
	rowsClass.AddMethod0(vm.Selectors, "primNext", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}
		if rowsObj.closed {
			return False
		}
		if rowsObj.rows.Next() {
			return True
		}
		return False
	})

	// columnAt: index — Get column value at zero-based index for current row
	rowsClass.AddMethod1(vm.Selectors, "primColumnAt:", func(vmPtr interface{}, recv Value, indexVal Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}

		if !indexVal.IsSmallInt() {
			return v.newFailureResult("columnAt: requires an integer index")
		}
		idx := int(indexVal.SmallInt())
		if idx < 0 || idx >= len(rowsObj.columns) {
			return v.newFailureResult(fmt.Sprintf("columnAt: index %d out of range (0..%d)", idx, len(rowsObj.columns)-1))
		}

		return v.scanCurrentRow(rowsObj, idx)
	})

	// columnNamed: name — Get column value by name for current row
	rowsClass.AddMethod1(vm.Selectors, "primColumnNamed:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}

		name := v.valueToString(nameVal)
		idx := -1
		for i, col := range rowsObj.columns {
			if strings.EqualFold(col, name) {
				idx = i
				break
			}
		}
		if idx < 0 {
			return v.newFailureResult("columnNamed: no column named '" + name + "'")
		}

		return v.scanCurrentRow(rowsObj, idx)
	})

	// asDict — Get current row as a Dictionary
	rowsClass.AddMethod0(vm.Selectors, "primAsDict", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}

		return v.currentRowToDict(rowsObj)
	})

	// columns — Get column names as an Array of Strings
	rowsClass.AddMethod0(vm.Selectors, "primColumns", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}

		vals := make([]Value, len(rowsObj.columns))
		for i, col := range rowsObj.columns {
			vals[i] = v.registry.NewStringValue(col)
		}
		return v.NewArrayWithElements(vals)
	})

	// columnCount — Get number of columns
	rowsClass.AddMethod0(vm.Selectors, "primColumnCount", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}
		return FromSmallInt(int64(len(rowsObj.columns)))
	})

	// close — Close the rows cursor
	rowsClass.AddMethod0(vm.Selectors, "primClose", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		rowsObj := v.getSqliteRows(recv)
		if rowsObj == nil {
			return v.newFailureResult("Not a SqliteRows")
		}
		if rowsObj.closed {
			return True
		}
		err := rowsObj.rows.Close()
		rowsObj.closed = true
		if err != nil {
			return v.newFailureResult("Error closing rows: " + err.Error())
		}
		return True
	})
}

// ---------------------------------------------------------------------------
// Helper methods
// ---------------------------------------------------------------------------

// getSqliteDB extracts a SqliteDatabaseObject from a GoObject Value.
func (vm *VM) getSqliteDB(v Value) *SqliteDatabaseObject {
	goVal, ok := vm.GetGoObject(v)
	if !ok {
		return nil
	}
	dbObj, ok := goVal.(*SqliteDatabaseObject)
	if !ok {
		return nil
	}
	return dbObj
}

// getSqliteStmt extracts a SqliteStatementObject from a GoObject Value.
func (vm *VM) getSqliteStmt(v Value) *SqliteStatementObject {
	goVal, ok := vm.GetGoObject(v)
	if !ok {
		return nil
	}
	stmtObj, ok := goVal.(*SqliteStatementObject)
	if !ok {
		return nil
	}
	return stmtObj
}

// getSqliteRows extracts a SqliteRowsObject from a GoObject Value.
func (vm *VM) getSqliteRows(v Value) *SqliteRowsObject {
	goVal, ok := vm.GetGoObject(v)
	if !ok {
		return nil
	}
	rowsObj, ok := goVal.(*SqliteRowsObject)
	if !ok {
		return nil
	}
	return rowsObj
}

// valueToGoArgs converts a Maggie Array value to a slice of Go interface{} for SQL parameters.
func (vm *VM) valueToGoArgs(v Value) []interface{} {
	if !v.IsObject() {
		// Single value
		return []interface{}{vm.ValueToGo(v)}
	}
	obj := ObjectFromValue(v)
	if obj == nil {
		return []interface{}{vm.ValueToGo(v)}
	}

	n := obj.NumSlots()
	args := make([]interface{}, n)
	for i := 0; i < n; i++ {
		args[i] = vm.ValueToGo(obj.GetSlot(i))
	}
	return args
}

// scanCurrentRow scans all columns of the current row and returns the value at idx.
func (vm *VM) scanCurrentRow(rowsObj *SqliteRowsObject, idx int) Value {
	colCount := len(rowsObj.columns)
	values := make([]interface{}, colCount)
	valuePtrs := make([]interface{}, colCount)
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rowsObj.rows.Scan(valuePtrs...); err != nil {
		return vm.newFailureResult("Error scanning row: " + err.Error())
	}

	return vm.sqlValueToMaggie(values[idx])
}

// currentRowToDict scans the current row and returns it as a Dictionary.
func (vm *VM) currentRowToDict(rowsObj *SqliteRowsObject) Value {
	colCount := len(rowsObj.columns)
	values := make([]interface{}, colCount)
	valuePtrs := make([]interface{}, colCount)
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rowsObj.rows.Scan(valuePtrs...); err != nil {
		return vm.newFailureResult("Error scanning row: " + err.Error())
	}

	dict := vm.registry.NewDictionaryValue()
	dictObj := vm.registry.GetDictionaryObject(dict)
	if dictObj == nil {
		return vm.newFailureResult("Cannot create dictionary")
	}

	for i, col := range rowsObj.columns {
		key := vm.registry.NewStringValue(col)
		val := vm.sqlValueToMaggie(values[i])
		h := hashValue(vm.registry, key)
		dictObj.Keys[h] = key
		dictObj.Data[h] = val
	}

	return dict
}

// rowToDict reads one row from rows and returns it as a Dictionary.
// Returns nil if no rows. Caller must close rows.
func (vm *VM) rowToDict(rows *sql.Rows) Value {
	if !rows.Next() {
		return Nil
	}

	cols, _ := rows.Columns()
	colCount := len(cols)
	values := make([]interface{}, colCount)
	valuePtrs := make([]interface{}, colCount)
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rows.Scan(valuePtrs...); err != nil {
		return vm.newFailureResult("Error scanning row: " + err.Error())
	}

	dict := vm.registry.NewDictionaryValue()
	dictObj := vm.registry.GetDictionaryObject(dict)
	if dictObj == nil {
		return vm.newFailureResult("Cannot create dictionary")
	}

	for i, col := range cols {
		key := vm.registry.NewStringValue(col)
		val := vm.sqlValueToMaggie(values[i])
		h := hashValue(vm.registry, key)
		dictObj.Keys[h] = key
		dictObj.Data[h] = val
	}

	return dict
}

// queryAllRows executes a query and returns all rows as an Array of Dictionaries.
func (vm *VM) queryAllRows(dbObj *SqliteDatabaseObject, sqlStr string, args []interface{}) Value {
	if sqlStr == "" {
		return vm.newFailureResult("queryAll: requires a SQL string")
	}

	var rows *sql.Rows
	var err error
	if args == nil {
		rows, err = dbObj.db.Query(sqlStr)
	} else {
		rows, err = dbObj.db.Query(sqlStr, args...)
	}
	if err != nil {
		return vm.newFailureResult("SQL error: " + err.Error())
	}
	defer rows.Close()

	cols, _ := rows.Columns()
	var results []Value

	for rows.Next() {
		colCount := len(cols)
		values := make([]interface{}, colCount)
		valuePtrs := make([]interface{}, colCount)
		for i := range values {
			valuePtrs[i] = &values[i]
		}

		if err := rows.Scan(valuePtrs...); err != nil {
			return vm.newFailureResult("Error scanning row: " + err.Error())
		}

		dict := vm.registry.NewDictionaryValue()
		dictObj := vm.registry.GetDictionaryObject(dict)
		if dictObj == nil {
			return vm.newFailureResult("Cannot create dictionary")
		}

		for i, col := range cols {
			key := vm.registry.NewStringValue(col)
			val := vm.sqlValueToMaggie(values[i])
			h := hashValue(vm.registry, key)
			dictObj.Keys[h] = key
			dictObj.Data[h] = val
		}

		results = append(results, dict)
	}

	return vm.NewArrayWithElements(results)
}

// sqlValueToMaggie converts a SQL value (from database/sql Scan) to a Maggie Value.
// Type mapping: INTEGER→SmallInteger, REAL→Float, TEXT→String, BLOB→String, NULL→nil
func (vm *VM) sqlValueToMaggie(v interface{}) Value {
	if v == nil {
		return Nil
	}
	switch val := v.(type) {
	case int64:
		return FromSmallInt(val)
	case float64:
		return FromFloat64(val)
	case string:
		return vm.registry.NewStringValue(val)
	case []byte:
		return vm.registry.NewStringValue(string(val))
	case bool:
		if val {
			return True
		}
		return False
	default:
		return vm.registry.NewStringValue(fmt.Sprintf("%v", val))
	}
}

