package sqlite

import (
	"database/sql"
	"fmt"
	"reflect"
	"strings"
	"sync"

	vm "github.com/chazu/maggie/vm"

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

func RegisterSqlitePrimitives(v *vm.VM) {
	// Register Go types for GoObject dispatch
	dbType := reflect.TypeOf((*SqliteDatabaseObject)(nil))
	stmtType := reflect.TypeOf((*SqliteStatementObject)(nil))
	rowsType := reflect.TypeOf((*SqliteRowsObject)(nil))

	dbClass := v.RegisterGoType("SqliteDatabase", dbType)
	stmtClass := v.RegisterGoType("SqliteStatement", stmtType)
	rowsClass := v.RegisterGoType("SqliteRows", rowsType)

	registerSqliteDatabasePrimitives(v, dbClass)
	registerSqliteStatementPrimitives(v, stmtClass)
	registerSqliteRowsPrimitives(v, rowsClass)
}

// ---------------------------------------------------------------------------
// SqliteDatabase primitives
// ---------------------------------------------------------------------------

func registerSqliteDatabasePrimitives(vmInst *vm.VM, dbClass *vm.Class) {
	// open: path — Open a SQLite database at the given path
	dbClass.AddClassMethod1(vmInst.Selectors, "primOpen:", func(v *vm.VM, recv vm.Value, pathVal vm.Value) vm.Value {
		path := v.ValueToString(pathVal)
		if path == "" {
			return v.NewFailureResult("SqliteDatabase open: requires a path string")
		}

		db, err := sql.Open("sqlite", path)
		if err != nil {
			return v.NewFailureResult("Cannot open database: " + err.Error())
		}

		// Ping to verify connection
		if err := db.Ping(); err != nil {
			db.Close()
			return v.NewFailureResult("Cannot connect to database: " + err.Error())
		}

		obj := &SqliteDatabaseObject{db: db, path: path}
		val, regErr := v.RegisterGoObject(obj)
		if regErr != nil {
			db.Close()
			return v.NewFailureResult("Cannot register database: " + regErr.Error())
		}
		return val
	})

	// openMemory — Open an in-memory SQLite database
	dbClass.AddClassMethod0(vmInst.Selectors, "primOpenMemory", func(v *vm.VM, recv vm.Value) vm.Value {

		db, err := sql.Open("sqlite", ":memory:")
		if err != nil {
			return v.NewFailureResult("Cannot open in-memory database: " + err.Error())
		}

		obj := &SqliteDatabaseObject{db: db, path: ":memory:"}
		val, regErr := v.RegisterGoObject(obj)
		if regErr != nil {
			db.Close()
			return v.NewFailureResult("Cannot register database: " + regErr.Error())
		}
		return val
	})

	// close — Close the database
	dbClass.AddMethod0(vmInst.Selectors, "primClose", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		dbObj.mu.Lock()
		defer dbObj.mu.Unlock()
		if dbObj.closed {
			return v.NewFailureResult("Database already closed")
		}
		err := dbObj.db.Close()
		dbObj.closed = true
		if err != nil {
			return v.NewFailureResult("Error closing database: " + err.Error())
		}
		return vm.True
	})

	// execute: sql — Execute SQL that returns no rows (CREATE, INSERT, UPDATE, DELETE)
	dbClass.AddMethod1(vmInst.Selectors, "primExecute:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("execute: requires a SQL string")
		}

		result, err := dbObj.db.Exec(sqlStr)
		if err != nil {
			return v.NewFailureResult("SQL error: " + err.Error())
		}

		rowsAffected, _ := result.RowsAffected()
		return vm.FromSmallInt(rowsAffected)
	})

	// execute:with: sql params — Execute SQL with positional parameters (array)
	dbClass.AddMethod2(vmInst.Selectors, "primExecuteWith:params:", func(v *vm.VM, recv vm.Value, sqlVal, paramsVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("execute:with: requires a SQL string")
		}

		args := valueToGoArgs(v, paramsVal)
		result, err := dbObj.db.Exec(sqlStr, args...)
		if err != nil {
			return v.NewFailureResult("SQL error: " + err.Error())
		}

		rowsAffected, _ := result.RowsAffected()
		return vm.FromSmallInt(rowsAffected)
	})

	// query: sql — Execute SQL that returns rows (SELECT)
	dbClass.AddMethod1(vmInst.Selectors, "primQuery:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("query: requires a SQL string")
		}

		rows, err := dbObj.db.Query(sqlStr)
		if err != nil {
			return v.NewFailureResult("SQL error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.NewFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// query:with: sql params — Execute SQL query with positional parameters
	dbClass.AddMethod2(vmInst.Selectors, "primQueryWith:params:", func(v *vm.VM, recv vm.Value, sqlVal, paramsVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("query:with: requires a SQL string")
		}

		args := valueToGoArgs(v, paramsVal)
		rows, err := dbObj.db.Query(sqlStr, args...)
		if err != nil {
			return v.NewFailureResult("SQL error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.NewFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// queryRow: sql — Execute SQL and return a single row as a Dictionary
	dbClass.AddMethod1(vmInst.Selectors, "primQueryRow:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("queryRow: requires a SQL string")
		}

		rows, err := dbObj.db.Query(sqlStr)
		if err != nil {
			return v.NewFailureResult("SQL error: " + err.Error())
		}
		defer rows.Close()

		return rowToDict(v, rows)
	})

	// queryRow:with: sql params — Execute SQL with params and return a single row
	dbClass.AddMethod2(vmInst.Selectors, "primQueryRowWith:params:", func(v *vm.VM, recv vm.Value, sqlVal, paramsVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("queryRow:with: requires a SQL string")
		}

		args := valueToGoArgs(v, paramsVal)
		rows, err := dbObj.db.Query(sqlStr, args...)
		if err != nil {
			return v.NewFailureResult("SQL error: " + err.Error())
		}
		defer rows.Close()

		return rowToDict(v, rows)
	})

	// queryAll: sql — Execute SQL and return all rows as an Array of Dictionaries
	dbClass.AddMethod1(vmInst.Selectors, "primQueryAll:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		return queryAllRows(v, dbObj, sqlStr, nil)
	})

	// queryAll:with: sql params — Execute SQL with params and return all rows
	dbClass.AddMethod2(vmInst.Selectors, "primQueryAllWith:params:", func(v *vm.VM, recv vm.Value, sqlVal, paramsVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		args := valueToGoArgs(v, paramsVal)
		return queryAllRows(v, dbObj, sqlStr, args)
	})

	// prepare: sql — Prepare a statement for repeated execution
	dbClass.AddMethod1(vmInst.Selectors, "primPrepare:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("prepare: requires a SQL string")
		}

		stmt, err := dbObj.db.Prepare(sqlStr)
		if err != nil {
			return v.NewFailureResult("Cannot prepare statement: " + err.Error())
		}

		stmtObj := &SqliteStatementObject{stmt: stmt, query: sqlStr, dbObj: dbObj}
		val, regErr := v.RegisterGoObject(stmtObj)
		if regErr != nil {
			stmt.Close()
			return v.NewFailureResult("Cannot register statement: " + regErr.Error())
		}
		return val
	})

	// pragma: name — Get a PRAGMA value
	dbClass.AddMethod1(vmInst.Selectors, "primPragma:", func(v *vm.VM, recv vm.Value, nameVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		name := v.ValueToString(nameVal)
		var result string
		err := dbObj.db.QueryRow("PRAGMA " + name).Scan(&result)
		if err != nil {
			return v.NewFailureResult("PRAGMA error: " + err.Error())
		}
		return v.Registry().NewStringValue(result)
	})

	// pragma:set: name value — Set a PRAGMA value
	dbClass.AddMethod2(vmInst.Selectors, "primPragmaSet:value:", func(v *vm.VM, recv vm.Value, nameVal, valVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		name := v.ValueToString(nameVal)
		valStr := v.ValueToString(valVal)
		_, err := dbObj.db.Exec(fmt.Sprintf("PRAGMA %s = %s", name, valStr))
		if err != nil {
			return v.NewFailureResult("PRAGMA error: " + err.Error())
		}
		return vm.True
	})

	// enableWAL — Enable WAL journal mode
	dbClass.AddMethod0(vmInst.Selectors, "primEnableWAL", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		var result string
		err := dbObj.db.QueryRow("PRAGMA journal_mode=WAL").Scan(&result)
		if err != nil {
			return v.NewFailureResult("Cannot enable WAL: " + err.Error())
		}
		return v.Registry().NewStringValue(result)
	})

	// beginTransaction — Start a new transaction, returns the db for chaining
	dbClass.AddMethod0(vmInst.Selectors, "primBeginTransaction", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		_, err := dbObj.db.Exec("BEGIN TRANSACTION")
		if err != nil {
			return v.NewFailureResult("Cannot begin transaction: " + err.Error())
		}
		return recv
	})

	// commitTransaction — Commit the current transaction
	dbClass.AddMethod0(vmInst.Selectors, "primCommitTransaction", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		_, err := dbObj.db.Exec("COMMIT")
		if err != nil {
			return v.NewFailureResult("Cannot commit transaction: " + err.Error())
		}
		return vm.True
	})

	// rollbackTransaction — Rollback the current transaction
	dbClass.AddMethod0(vmInst.Selectors, "primRollbackTransaction", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		_, err := dbObj.db.Exec("ROLLBACK")
		if err != nil {
			return v.NewFailureResult("Cannot rollback transaction: " + err.Error())
		}
		return vm.True
	})

	// path — Get the database path
	dbClass.AddMethod0(vmInst.Selectors, "primPath", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		return v.Registry().NewStringValue(dbObj.path)
	})

	// isClosed — Check if the database is closed
	dbClass.AddMethod0(vmInst.Selectors, "primIsClosed", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return vm.False
		}
		if dbObj.closed {
			return vm.True
		}
		return vm.False
	})

	// lastInsertId — Get the last insert row ID (via a query)
	dbClass.AddMethod0(vmInst.Selectors, "primLastInsertId", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		var id int64
		err := dbObj.db.QueryRow("SELECT last_insert_rowid()").Scan(&id)
		if err != nil {
			return v.NewFailureResult("Cannot get last insert ID: " + err.Error())
		}
		return vm.FromSmallInt(id)
	})

	// tableExists: name — Check if a table exists
	dbClass.AddMethod1(vmInst.Selectors, "primTableExists:", func(v *vm.VM, recv vm.Value, nameVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		name := v.ValueToString(nameVal)
		var count int
		err := dbObj.db.QueryRow(
			"SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?", name,
		).Scan(&count)
		if err != nil {
			return v.NewFailureResult("Error checking table: " + err.Error())
		}
		if count > 0 {
			return vm.True
		}
		return vm.False
	})

	// tables — List all table names
	dbClass.AddMethod0(vmInst.Selectors, "primTables", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		rows, err := dbObj.db.Query("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
		if err != nil {
			return v.NewFailureResult("Error listing tables: " + err.Error())
		}
		defer rows.Close()

		var names []vm.Value
		for rows.Next() {
			var name string
			if err := rows.Scan(&name); err != nil {
				return v.NewFailureResult("Error scanning table name: " + err.Error())
			}
			names = append(names, v.Registry().NewStringValue(name))
		}
		return v.NewArrayWithElements(names)
	})

	// version — Get SQLite version string
	dbClass.AddMethod0(vmInst.Selectors, "primVersion", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		var version string
		err := dbObj.db.QueryRow("SELECT sqlite_version()").Scan(&version)
		if err != nil {
			return v.NewFailureResult("Cannot get version: " + err.Error())
		}
		return v.Registry().NewStringValue(version)
	})

	// migrate:version: sql version — Run a migration if not already applied
	dbClass.AddMethod2(vmInst.Selectors, "primMigrate:version:", func(v *vm.VM, recv vm.Value, sqlVal, versionVal vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		sqlStr := v.ValueToString(sqlVal)
		var version int64
		if versionVal.IsSmallInt() {
			version = versionVal.SmallInt()
		} else {
			return v.NewFailureResult("migrate:version: version must be an integer")
		}

		// Create migrations table if it doesn't exist
		_, err := dbObj.db.Exec(`CREATE TABLE IF NOT EXISTS _maggie_migrations (
			version INTEGER PRIMARY KEY,
			applied_at TEXT DEFAULT (datetime('now'))
		)`)
		if err != nil {
			return v.NewFailureResult("Cannot create migrations table: " + err.Error())
		}

		// Check if migration already applied
		var count int
		err = dbObj.db.QueryRow("SELECT COUNT(*) FROM _maggie_migrations WHERE version = ?", version).Scan(&count)
		if err != nil {
			return v.NewFailureResult("Cannot check migration status: " + err.Error())
		}
		if count > 0 {
			return vm.False // Already applied
		}

		// Apply migration in a transaction
		tx, err := dbObj.db.Begin()
		if err != nil {
			return v.NewFailureResult("Cannot begin migration transaction: " + err.Error())
		}

		_, err = tx.Exec(sqlStr)
		if err != nil {
			tx.Rollback()
			return v.NewFailureResult("Migration failed: " + err.Error())
		}

		_, err = tx.Exec("INSERT INTO _maggie_migrations (version) VALUES (?)", version)
		if err != nil {
			tx.Rollback()
			return v.NewFailureResult("Cannot record migration: " + err.Error())
		}

		if err := tx.Commit(); err != nil {
			return v.NewFailureResult("Cannot commit migration: " + err.Error())
		}

		return vm.True // Migration applied
	})

	// migrationVersion — Get the highest applied migration version
	dbClass.AddMethod0(vmInst.Selectors, "primMigrationVersion", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getSqliteDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("Not a SqliteDatabase")
		}
		if dbObj.closed {
			return v.NewFailureResult("Database is closed")
		}

		// Check if migrations table exists
		var count int
		err := dbObj.db.QueryRow(
			"SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='_maggie_migrations'",
		).Scan(&count)
		if err != nil {
			return v.NewFailureResult("Error checking migrations table: " + err.Error())
		}
		if count == 0 {
			return vm.FromSmallInt(0)
		}

		var version int64
		err = dbObj.db.QueryRow("SELECT COALESCE(MAX(version), 0) FROM _maggie_migrations").Scan(&version)
		if err != nil {
			return v.NewFailureResult("Cannot get migration version: " + err.Error())
		}
		return vm.FromSmallInt(version)
	})
}

// ---------------------------------------------------------------------------
// SqliteStatement primitives
// ---------------------------------------------------------------------------

func registerSqliteStatementPrimitives(vmInst *vm.VM, stmtClass *vm.Class) {
	// execute — Execute the prepared statement with no params
	stmtClass.AddMethod0(vmInst.Selectors, "primExecute", func(v *vm.VM, recv vm.Value) vm.Value {
		stmtObj := getSqliteStmt(v, recv)
		if stmtObj == nil {
			return v.NewFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.NewFailureResult("Statement is closed")
		}

		result, err := stmtObj.stmt.Exec()
		if err != nil {
			return v.NewFailureResult("Statement execute error: " + err.Error())
		}
		rowsAffected, _ := result.RowsAffected()
		return vm.FromSmallInt(rowsAffected)
	})

	// executeWith: params — Execute the prepared statement with params (array)
	stmtClass.AddMethod1(vmInst.Selectors, "primExecuteWith:", func(v *vm.VM, recv vm.Value, paramsVal vm.Value) vm.Value {
		stmtObj := getSqliteStmt(v, recv)
		if stmtObj == nil {
			return v.NewFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.NewFailureResult("Statement is closed")
		}

		args := valueToGoArgs(v, paramsVal)
		result, err := stmtObj.stmt.Exec(args...)
		if err != nil {
			return v.NewFailureResult("Statement execute error: " + err.Error())
		}
		rowsAffected, _ := result.RowsAffected()
		return vm.FromSmallInt(rowsAffected)
	})

	// query — Query with the prepared statement, no params
	stmtClass.AddMethod0(vmInst.Selectors, "primQuery", func(v *vm.VM, recv vm.Value) vm.Value {
		stmtObj := getSqliteStmt(v, recv)
		if stmtObj == nil {
			return v.NewFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.NewFailureResult("Statement is closed")
		}

		rows, err := stmtObj.stmt.Query()
		if err != nil {
			return v.NewFailureResult("Statement query error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.NewFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// queryWith: params — Query with the prepared statement and params
	stmtClass.AddMethod1(vmInst.Selectors, "primQueryWith:", func(v *vm.VM, recv vm.Value, paramsVal vm.Value) vm.Value {
		stmtObj := getSqliteStmt(v, recv)
		if stmtObj == nil {
			return v.NewFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.NewFailureResult("Statement is closed")
		}

		args := valueToGoArgs(v, paramsVal)
		rows, err := stmtObj.stmt.Query(args...)
		if err != nil {
			return v.NewFailureResult("Statement query error: " + err.Error())
		}

		cols, _ := rows.Columns()
		rowsObj := &SqliteRowsObject{rows: rows, columns: cols}
		val, regErr := v.RegisterGoObject(rowsObj)
		if regErr != nil {
			rows.Close()
			return v.NewFailureResult("Cannot register rows: " + regErr.Error())
		}
		return val
	})

	// close — Close the prepared statement
	stmtClass.AddMethod0(vmInst.Selectors, "primClose", func(v *vm.VM, recv vm.Value) vm.Value {
		stmtObj := getSqliteStmt(v, recv)
		if stmtObj == nil {
			return v.NewFailureResult("Not a SqliteStatement")
		}
		if stmtObj.closed {
			return v.NewFailureResult("Statement already closed")
		}
		err := stmtObj.stmt.Close()
		stmtObj.closed = true
		if err != nil {
			return v.NewFailureResult("Error closing statement: " + err.Error())
		}
		return vm.True
	})

	// sql — Get the SQL string used to create this statement
	stmtClass.AddMethod0(vmInst.Selectors, "primSql", func(v *vm.VM, recv vm.Value) vm.Value {
		stmtObj := getSqliteStmt(v, recv)
		if stmtObj == nil {
			return v.NewFailureResult("Not a SqliteStatement")
		}
		return v.Registry().NewStringValue(stmtObj.query)
	})
}

// ---------------------------------------------------------------------------
// SqliteRows primitives
// ---------------------------------------------------------------------------

func registerSqliteRowsPrimitives(vmInst *vm.VM, rowsClass *vm.Class) {
	// next — Advance to next row. Returns true if a row is available, false if done.
	rowsClass.AddMethod0(vmInst.Selectors, "primNext", func(v *vm.VM, recv vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}
		if rowsObj.closed {
			return vm.False
		}
		if rowsObj.rows.Next() {
			return vm.True
		}
		return vm.False
	})

	// columnAt: index — Get column value at zero-based index for current row
	rowsClass.AddMethod1(vmInst.Selectors, "primColumnAt:", func(v *vm.VM, recv vm.Value, indexVal vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}

		if !indexVal.IsSmallInt() {
			return v.NewFailureResult("columnAt: requires an integer index")
		}
		idx := int(indexVal.SmallInt())
		if idx < 0 || idx >= len(rowsObj.columns) {
			return v.NewFailureResult(fmt.Sprintf("columnAt: index %d out of range (0..%d)", idx, len(rowsObj.columns)-1))
		}

		return scanCurrentRow(v, rowsObj, idx)
	})

	// columnNamed: name — Get column value by name for current row
	rowsClass.AddMethod1(vmInst.Selectors, "primColumnNamed:", func(v *vm.VM, recv vm.Value, nameVal vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}

		name := v.ValueToString(nameVal)
		idx := -1
		for i, col := range rowsObj.columns {
			if strings.EqualFold(col, name) {
				idx = i
				break
			}
		}
		if idx < 0 {
			return v.NewFailureResult("columnNamed: no column named '" + name + "'")
		}

		return scanCurrentRow(v, rowsObj, idx)
	})

	// asDict — Get current row as a Dictionary
	rowsClass.AddMethod0(vmInst.Selectors, "primAsDict", func(v *vm.VM, recv vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}

		return currentRowToDict(v, rowsObj)
	})

	// columns — Get column names as an Array of Strings
	rowsClass.AddMethod0(vmInst.Selectors, "primColumns", func(v *vm.VM, recv vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}

		vals := make([]vm.Value, len(rowsObj.columns))
		for i, col := range rowsObj.columns {
			vals[i] = v.Registry().NewStringValue(col)
		}
		return v.NewArrayWithElements(vals)
	})

	// columnCount — Get number of columns
	rowsClass.AddMethod0(vmInst.Selectors, "primColumnCount", func(v *vm.VM, recv vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}
		return vm.FromSmallInt(int64(len(rowsObj.columns)))
	})

	// close — Close the rows cursor
	rowsClass.AddMethod0(vmInst.Selectors, "primClose", func(v *vm.VM, recv vm.Value) vm.Value {
		rowsObj := getSqliteRows(v, recv)
		if rowsObj == nil {
			return v.NewFailureResult("Not a SqliteRows")
		}
		if rowsObj.closed {
			return vm.True
		}
		err := rowsObj.rows.Close()
		rowsObj.closed = true
		if err != nil {
			return v.NewFailureResult("Error closing rows: " + err.Error())
		}
		return vm.True
	})
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

// getSqliteDB extracts a SqliteDatabaseObject from a GoObject Value.
func getSqliteDB(vmInst *vm.VM, val vm.Value) *SqliteDatabaseObject {
	goVal, ok := vmInst.GetGoObject(val)
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
func getSqliteStmt(vmInst *vm.VM, val vm.Value) *SqliteStatementObject {
	goVal, ok := vmInst.GetGoObject(val)
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
func getSqliteRows(vmInst *vm.VM, val vm.Value) *SqliteRowsObject {
	goVal, ok := vmInst.GetGoObject(val)
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
func valueToGoArgs(vmInst *vm.VM, val vm.Value) []interface{} {
	if !val.IsObject() {
		// Single value
		return []interface{}{vmInst.ValueToGo(val)}
	}
	obj := vm.ObjectFromValue(val)
	if obj == nil {
		return []interface{}{vmInst.ValueToGo(val)}
	}

	n := obj.NumSlots()
	args := make([]interface{}, n)
	for i := 0; i < n; i++ {
		args[i] = vmInst.ValueToGo(obj.GetSlot(i))
	}
	return args
}

// scanCurrentRow scans all columns of the current row and returns the value at idx.
func scanCurrentRow(vmInst *vm.VM, rowsObj *SqliteRowsObject, idx int) vm.Value {
	colCount := len(rowsObj.columns)
	values := make([]interface{}, colCount)
	valuePtrs := make([]interface{}, colCount)
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rowsObj.rows.Scan(valuePtrs...); err != nil {
		return vmInst.NewFailureResult("Error scanning row: " + err.Error())
	}

	return sqlValueToMaggie(vmInst, values[idx])
}

// currentRowToDict scans the current row and returns it as a Dictionary.
func currentRowToDict(vmInst *vm.VM, rowsObj *SqliteRowsObject) vm.Value {
	colCount := len(rowsObj.columns)
	values := make([]interface{}, colCount)
	valuePtrs := make([]interface{}, colCount)
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rowsObj.rows.Scan(valuePtrs...); err != nil {
		return vmInst.NewFailureResult("Error scanning row: " + err.Error())
	}

	dict := vmInst.Registry().NewDictionaryValue()
	dictObj := vmInst.Registry().GetDictionaryObject(dict)
	if dictObj == nil {
		return vmInst.NewFailureResult("Cannot create dictionary")
	}

	for i, col := range rowsObj.columns {
		key := vmInst.Registry().NewStringValue(col)
		val := sqlValueToMaggie(vmInst, values[i])
		dictObj.Put(vmInst.Registry(), key, val)
	}

	return dict
}

// rowToDict reads one row from rows and returns it as a Dictionary.
// Returns nil if no rows. Caller must close rows.
func rowToDict(vmInst *vm.VM, rows *sql.Rows) vm.Value {
	if !rows.Next() {
		return vm.Nil
	}

	cols, _ := rows.Columns()
	colCount := len(cols)
	values := make([]interface{}, colCount)
	valuePtrs := make([]interface{}, colCount)
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rows.Scan(valuePtrs...); err != nil {
		return vmInst.NewFailureResult("Error scanning row: " + err.Error())
	}

	dict := vmInst.Registry().NewDictionaryValue()
	dictObj := vmInst.Registry().GetDictionaryObject(dict)
	if dictObj == nil {
		return vmInst.NewFailureResult("Cannot create dictionary")
	}

	for i, col := range cols {
		key := vmInst.Registry().NewStringValue(col)
		val := sqlValueToMaggie(vmInst, values[i])
		dictObj.Put(vmInst.Registry(), key, val)
	}

	return dict
}

// queryAllRows executes a query and returns all rows as an Array of Dictionaries.
func queryAllRows(vmInst *vm.VM, dbObj *SqliteDatabaseObject, sqlStr string, args []interface{}) vm.Value {
	if sqlStr == "" {
		return vmInst.NewFailureResult("queryAll: requires a SQL string")
	}

	var rows *sql.Rows
	var err error
	if args == nil {
		rows, err = dbObj.db.Query(sqlStr)
	} else {
		rows, err = dbObj.db.Query(sqlStr, args...)
	}
	if err != nil {
		return vmInst.NewFailureResult("SQL error: " + err.Error())
	}
	defer rows.Close()

	cols, _ := rows.Columns()
	var results []vm.Value

	for rows.Next() {
		colCount := len(cols)
		values := make([]interface{}, colCount)
		valuePtrs := make([]interface{}, colCount)
		for i := range values {
			valuePtrs[i] = &values[i]
		}

		if err := rows.Scan(valuePtrs...); err != nil {
			return vmInst.NewFailureResult("Error scanning row: " + err.Error())
		}

		dict := vmInst.Registry().NewDictionaryValue()
		dictObj := vmInst.Registry().GetDictionaryObject(dict)
		if dictObj == nil {
			return vmInst.NewFailureResult("Cannot create dictionary")
		}

		for i, col := range cols {
			key := vmInst.Registry().NewStringValue(col)
			val := sqlValueToMaggie(vmInst, values[i])
			dictObj.Put(vmInst.Registry(), key, val)
		}

		results = append(results, dict)
	}

	return vmInst.NewArrayWithElements(results)
}

// sqlValueToMaggie converts a SQL value (from database/sql Scan) to a Maggie Value.
// Type mapping: INTEGER->SmallInteger, REAL->Float, TEXT->String, BLOB->String, NULL->nil
func sqlValueToMaggie(vmInst *vm.VM, v interface{}) vm.Value {
	if v == nil {
		return vm.Nil
	}
	switch val := v.(type) {
	case int64:
		return vm.FromSmallInt(val)
	case float64:
		return vm.FromFloat64(val)
	case string:
		return vmInst.Registry().NewStringValue(val)
	case []byte:
		return vmInst.Registry().NewStringValue(string(val))
	case bool:
		if val {
			return vm.True
		}
		return vm.False
	default:
		return vmInst.Registry().NewStringValue(fmt.Sprintf("%v", val))
	}
}
