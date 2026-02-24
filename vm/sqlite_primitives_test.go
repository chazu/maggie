package vm

import (
	"os"
	"path/filepath"
	"testing"
)

// ---------------------------------------------------------------------------
// SqliteDatabase Tests
// ---------------------------------------------------------------------------

func TestSqliteOpenMemory(t *testing.T) {
	vm := NewVM()
	dbClassVal := vm.Globals["SqliteDatabase"]
	if dbClassVal == Nil {
		t.Fatal("SqliteDatabase class not in Globals")
	}

	result := vm.Send(dbClassVal, "openMemory", nil)
	if result == Nil {
		t.Fatal("openMemory returned nil")
	}

	dbObj := vm.getSqliteDB(result)
	if dbObj == nil {
		t.Fatal("could not extract SqliteDatabaseObject")
	}
	if dbObj.path != ":memory:" {
		t.Errorf("path = %q, want :memory:", dbObj.path)
	}
	if dbObj.closed {
		t.Error("database should not be closed initially")
	}
}

func TestSqliteOpenFile(t *testing.T) {
	vm := NewVM()
	dbClassVal := vm.Globals["SqliteDatabase"]

	tmpDir := t.TempDir()
	dbPath := filepath.Join(tmpDir, "test.db")

	pathVal := vm.registry.NewStringValue(dbPath)
	result := vm.Send(dbClassVal, "open:", []Value{pathVal})

	dbObj := vm.getSqliteDB(result)
	if dbObj == nil {
		t.Fatal("could not open file database")
	}
	if dbObj.path != dbPath {
		t.Errorf("path = %q, want %q", dbObj.path, dbPath)
	}

	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		t.Error("database file was not created")
	}
}

func TestSqliteClose(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	result := vm.Send(db, "close", nil)
	if result != True {
		t.Error("close should return true")
	}

	dbObj := vm.getSqliteDB(db)
	if !dbObj.closed {
		t.Error("database should be closed")
	}

	// Close again should fail
	result = vm.Send(db, "close", nil)
	if result == True {
		t.Error("double close should return failure")
	}
}

func TestSqliteExecuteAndQuery(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	// Create table
	sqliteExec(t, vm, db, "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)")

	// Insert
	sqlVal := vm.registry.NewStringValue("INSERT INTO users (name, age) VALUES ('Alice', 30)")
	result := vm.Send(db, "execute:", []Value{sqlVal})
	if sqliteIsFailure(vm, result) {
		t.Fatal("INSERT failed:", sqliteFailureMsg(vm, result))
	}
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("expected 1 row affected, got %v", result)
	}

	// Insert with params
	sqlVal = vm.registry.NewStringValue("INSERT INTO users (name, age) VALUES (?, ?)")
	params := vm.NewArrayWithElements([]Value{
		vm.registry.NewStringValue("Bob"),
		FromSmallInt(25),
	})
	result = vm.Send(db, "execute:with:", []Value{sqlVal, params})
	if sqliteIsFailure(vm, result) {
		t.Fatal("INSERT with params failed:", sqliteFailureMsg(vm, result))
	}

	// Query all
	sqlVal = vm.registry.NewStringValue("SELECT * FROM users ORDER BY id")
	result = vm.Send(db, "queryAll:", []Value{sqlVal})
	if sqliteIsFailure(vm, result) {
		t.Fatal("queryAll: failed:", sqliteFailureMsg(vm, result))
	}

	if !result.IsObject() {
		t.Fatal("queryAll: should return an array")
	}
	arr := ObjectFromValue(result)
	if arr == nil || arr.NumSlots() != 2 {
		slots := 0
		if arr != nil {
			slots = arr.NumSlots()
		}
		t.Fatalf("expected 2 rows, got %d", slots)
	}
}

func TestSqliteQueryRow(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT)")
	sqliteExec(t, vm, db, "INSERT INTO items (name) VALUES ('Widget')")

	sqlVal := vm.registry.NewStringValue("SELECT * FROM items WHERE id = 1")
	result := vm.Send(db, "queryRow:", []Value{sqlVal})
	if result == Nil {
		t.Fatal("queryRow: returned nil")
	}
	if sqliteIsFailure(vm, result) {
		t.Fatal("queryRow: failed:", sqliteFailureMsg(vm, result))
	}

	dictObj := vm.registry.GetDictionaryObject(result)
	if dictObj == nil {
		t.Fatal("queryRow: should return a Dictionary")
	}

	// Query row that doesn't exist
	sqlVal = vm.registry.NewStringValue("SELECT * FROM items WHERE id = 999")
	result = vm.Send(db, "queryRow:", []Value{sqlVal})
	if result != Nil {
		t.Error("queryRow: for no results should return nil")
	}
}

func TestSqliteQueryRowWithParams(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT)")
	sqliteExec(t, vm, db, "INSERT INTO items (name) VALUES ('Widget')")

	sqlVal := vm.registry.NewStringValue("SELECT * FROM items WHERE name = ?")
	params := vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("Widget")})
	result := vm.Send(db, "queryRow:with:", []Value{sqlVal, params})
	if result == Nil {
		t.Fatal("queryRow:with: returned nil")
	}
	if sqliteIsFailure(vm, result) {
		t.Fatal("queryRow:with: failed:", sqliteFailureMsg(vm, result))
	}
}

func TestSqliteTransaction(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE txtest (id INTEGER PRIMARY KEY, val TEXT)")

	// Begin transaction
	result := vm.Send(db, "beginTransaction", nil)
	if result != db {
		t.Error("beginTransaction should return the database for chaining")
	}

	// Insert inside transaction
	sqliteExec(t, vm, db, "INSERT INTO txtest (val) VALUES ('inside-tx')")

	// Rollback
	result = vm.Send(db, "rollbackTransaction", nil)
	if result != True {
		t.Error("rollbackTransaction should return true")
	}

	// Verify data was rolled back
	sqlVal := vm.registry.NewStringValue("SELECT COUNT(*) as cnt FROM txtest")
	rowResult := vm.Send(db, "queryRow:", []Value{sqlVal})
	if rowResult == Nil {
		t.Fatal("queryRow returned nil after rollback")
	}

	// Test commit
	vm.Send(db, "beginTransaction", nil)
	sqliteExec(t, vm, db, "INSERT INTO txtest (val) VALUES ('committed')")
	vm.Send(db, "commitTransaction", nil)

	sqlVal = vm.registry.NewStringValue("SELECT COUNT(*) as cnt FROM txtest")
	rowResult = vm.Send(db, "queryRow:", []Value{sqlVal})
	if rowResult == Nil {
		t.Fatal("queryRow returned nil after commit")
	}
}

func TestSqlitePreparedStatement(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE prep_test (id INTEGER PRIMARY KEY, name TEXT)")

	// Prepare an insert statement
	sqlVal := vm.registry.NewStringValue("INSERT INTO prep_test (name) VALUES (?)")
	stmt := vm.Send(db, "prepare:", []Value{sqlVal})
	if sqliteIsFailure(vm, stmt) {
		t.Fatal("prepare: failed:", sqliteFailureMsg(vm, stmt))
	}

	stmtObj := vm.getSqliteStmt(stmt)
	if stmtObj == nil {
		t.Fatal("could not extract SqliteStatementObject")
	}

	// Execute with different params
	for _, name := range []string{"Alice", "Bob", "Charlie"} {
		params := vm.NewArrayWithElements([]Value{vm.registry.NewStringValue(name)})
		result := vm.Send(stmt, "executeWith:", []Value{params})
		if sqliteIsFailure(vm, result) {
			t.Fatalf("executeWith: failed for %s: %s", name, sqliteFailureMsg(vm, result))
		}
	}

	// Close statement
	result := vm.Send(stmt, "close", nil)
	if result != True {
		t.Error("close should return true")
	}

	// Verify rows were inserted
	sqlVal = vm.registry.NewStringValue("SELECT COUNT(*) as cnt FROM prep_test")
	row := vm.Send(db, "queryRow:", []Value{sqlVal})
	if row == Nil {
		t.Fatal("queryRow returned nil")
	}
}

func TestSqliteRows(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE row_test (id INTEGER PRIMARY KEY, name TEXT, score REAL)")
	sqliteExec(t, vm, db, "INSERT INTO row_test (name, score) VALUES ('Alice', 95.5)")
	sqliteExec(t, vm, db, "INSERT INTO row_test (name, score) VALUES ('Bob', 87.3)")

	// Query returning rows
	sqlVal := vm.registry.NewStringValue("SELECT * FROM row_test ORDER BY id")
	rows := vm.Send(db, "query:", []Value{sqlVal})
	if sqliteIsFailure(vm, rows) {
		t.Fatal("query: failed:", sqliteFailureMsg(vm, rows))
	}

	rowsObj := vm.getSqliteRows(rows)
	if rowsObj == nil {
		t.Fatal("could not extract SqliteRowsObject")
	}

	// Check columns
	cols := vm.Send(rows, "columns", nil)
	if !cols.IsObject() {
		t.Fatal("columns should return an array")
	}
	colCount := vm.Send(rows, "columnCount", nil)
	if !colCount.IsSmallInt() || colCount.SmallInt() != 3 {
		t.Errorf("expected 3 columns, got %v", colCount)
	}

	// Iterate rows
	count := 0
	for {
		hasNext := vm.Send(rows, "next", nil)
		if hasNext != True {
			break
		}
		count++

		// Get current row as dict
		dict := vm.Send(rows, "asDict", nil)
		if sqliteIsFailure(vm, dict) {
			t.Fatal("asDict failed:", sqliteFailureMsg(vm, dict))
		}
		dictObj := vm.registry.GetDictionaryObject(dict)
		if dictObj == nil {
			t.Fatal("asDict should return a Dictionary")
		}
	}
	if count != 2 {
		t.Errorf("expected 2 rows, got %d", count)
	}

	// Close rows
	result := vm.Send(rows, "close", nil)
	if result != True {
		t.Error("rows close should return true")
	}
}

func TestSqliteTypeMapping(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, `CREATE TABLE types_test (
		int_val INTEGER,
		real_val REAL,
		text_val TEXT,
		blob_val BLOB,
		null_val TEXT
	)`)
	sqliteExec(t, vm, db, "INSERT INTO types_test VALUES (42, 3.14, 'hello', X'DEADBEEF', NULL)")

	sqlVal := vm.registry.NewStringValue("SELECT * FROM types_test")
	row := vm.Send(db, "queryRow:", []Value{sqlVal})
	if row == Nil || sqliteIsFailure(vm, row) {
		t.Fatal("queryRow: returned nil or failure")
	}

	dictObj := vm.registry.GetDictionaryObject(row)
	if dictObj == nil {
		t.Fatal("expected dictionary")
	}

	if len(dictObj.Data) != 5 {
		t.Errorf("expected 5 columns in dict, got %d", len(dictObj.Data))
	}
}

func TestSqliteWAL(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	result := vm.Send(db, "enableWAL", nil)
	if sqliteIsFailure(vm, result) {
		t.Fatal("enableWAL failed:", sqliteFailureMsg(vm, result))
	}
}

func TestSqlitePragma(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	nameVal := vm.registry.NewStringValue("journal_mode")
	result := vm.Send(db, "pragma:", []Value{nameVal})
	if sqliteIsFailure(vm, result) {
		t.Fatal("pragma: failed:", sqliteFailureMsg(vm, result))
	}

	nameVal = vm.registry.NewStringValue("cache_size")
	valVal := vm.registry.NewStringValue("-2000")
	result = vm.Send(db, "pragma:set:", []Value{nameVal, valVal})
	if result != True {
		t.Error("pragma:set: should return true")
	}
}

func TestSqliteTableExists(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE exists_test (id INTEGER)")

	nameVal := vm.registry.NewStringValue("exists_test")
	result := vm.Send(db, "tableExists:", []Value{nameVal})
	if result != True {
		t.Error("tableExists: should return true for existing table")
	}

	nameVal = vm.registry.NewStringValue("nonexistent")
	result = vm.Send(db, "tableExists:", []Value{nameVal})
	if result != False {
		t.Error("tableExists: should return false for nonexistent table")
	}
}

func TestSqliteTables(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE alpha (id INTEGER)")
	sqliteExec(t, vm, db, "CREATE TABLE beta (id INTEGER)")

	result := vm.Send(db, "tables", nil)
	if !result.IsObject() {
		t.Fatal("tables should return an array")
	}
	arr := ObjectFromValue(result)
	if arr == nil || arr.NumSlots() < 2 {
		t.Errorf("expected at least 2 tables")
	}
}

func TestSqliteVersion(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	result := vm.Send(db, "version", nil)
	if sqliteIsFailure(vm, result) {
		t.Fatal("version failed:", sqliteFailureMsg(vm, result))
	}
	ver := vm.registry.GetStringContent(result)
	if ver == "" {
		t.Error("version should return a non-empty string")
	}
}

func TestSqliteMigration(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqlVal := vm.registry.NewStringValue("CREATE TABLE migration_test (id INTEGER PRIMARY KEY, name TEXT)")
	verVal := FromSmallInt(1)
	result := vm.Send(db, "migrate:version:", []Value{sqlVal, verVal})
	if result != True {
		t.Error("first migration should return true (applied)")
	}

	result = vm.Send(db, "migrate:version:", []Value{sqlVal, verVal})
	if result != False {
		t.Error("duplicate migration should return false (already applied)")
	}

	result = vm.Send(db, "migrationVersion", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("migrationVersion = %v, want 1", result)
	}

	sqlVal = vm.registry.NewStringValue("ALTER TABLE migration_test ADD COLUMN age INTEGER")
	verVal = FromSmallInt(2)
	result = vm.Send(db, "migrate:version:", []Value{sqlVal, verVal})
	if result != True {
		t.Error("migration v2 should return true")
	}

	result = vm.Send(db, "migrationVersion", nil)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("migrationVersion = %v, want 2", result)
	}
}

func TestSqliteLastInsertId(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE lid_test (id INTEGER PRIMARY KEY, name TEXT)")
	sqliteExec(t, vm, db, "INSERT INTO lid_test (name) VALUES ('first')")

	result := vm.Send(db, "lastInsertId", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("lastInsertId = %v, want 1", result)
	}

	sqliteExec(t, vm, db, "INSERT INTO lid_test (name) VALUES ('second')")
	result = vm.Send(db, "lastInsertId", nil)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("lastInsertId = %v, want 2", result)
	}
}

func TestSqliteIsClosed(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	result := vm.Send(db, "isClosed", nil)
	if result != False {
		t.Error("isClosed should return false for open database")
	}

	vm.Send(db, "close", nil)

	result = vm.Send(db, "isClosed", nil)
	if result != True {
		t.Error("isClosed should return true after close")
	}
}

func TestSqliteClosedDatabaseErrors(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)
	vm.Send(db, "close", nil)

	sqlVal := vm.registry.NewStringValue("SELECT 1")
	result := vm.Send(db, "execute:", []Value{sqlVal})
	if !sqliteIsFailure(vm, result) {
		t.Error("execute on closed db should return failure")
	}

	result = vm.Send(db, "query:", []Value{sqlVal})
	if !sqliteIsFailure(vm, result) {
		t.Error("query on closed db should return failure")
	}
}

func TestSqlitePath(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	result := vm.Send(db, "path", nil)
	path := vm.registry.GetStringContent(result)
	if path != ":memory:" {
		t.Errorf("path = %q, want :memory:", path)
	}
}

func TestSqliteStmtSQL(t *testing.T) {
	vm := NewVM()
	db := sqliteOpenMemory(t, vm)

	sqliteExec(t, vm, db, "CREATE TABLE sql_test (id INTEGER)")

	sqlStr := "INSERT INTO sql_test (id) VALUES (?)"
	sqlVal := vm.registry.NewStringValue(sqlStr)
	stmt := vm.Send(db, "prepare:", []Value{sqlVal})
	if sqliteIsFailure(vm, stmt) {
		t.Fatal("prepare: failed:", sqliteFailureMsg(vm, stmt))
	}

	result := vm.Send(stmt, "sql", nil)
	got := vm.registry.GetStringContent(result)
	if got != sqlStr {
		t.Errorf("sql = %q, want %q", got, sqlStr)
	}

	vm.Send(stmt, "close", nil)
}

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

func sqliteOpenMemory(t *testing.T, vm *VM) Value {
	t.Helper()
	dbClassVal := vm.Globals["SqliteDatabase"]
	if dbClassVal == Nil {
		t.Fatal("SqliteDatabase class not in Globals")
	}
	result := vm.Send(dbClassVal, "openMemory", nil)
	if result == Nil || sqliteIsFailure(vm, result) {
		t.Fatal("openMemory failed")
	}
	return result
}

func sqliteExec(t *testing.T, vm *VM, db Value, sql string) {
	t.Helper()
	sqlVal := vm.registry.NewStringValue(sql)
	result := vm.Send(db, "execute:", []Value{sqlVal})
	if sqliteIsFailure(vm, result) {
		t.Fatal("execute failed:", sqliteFailureMsg(vm, result), "sql:", sql)
	}
}

func sqliteIsFailure(vm *VM, v Value) bool {
	r := vm.registry.GetResultFromValue(v)
	if r == nil {
		return false
	}
	return r.resultType == ResultFailure
}

func sqliteFailureMsg(vm *VM, v Value) string {
	r := vm.registry.GetResultFromValue(v)
	if r == nil {
		return "<not a result>"
	}
	return vm.valueToString(r.value)
}
