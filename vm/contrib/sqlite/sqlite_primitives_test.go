package sqlite

import (
	"os"
	"path/filepath"
	"testing"

	vm "github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// SqliteDatabase Tests
// ---------------------------------------------------------------------------

func TestSqliteOpenMemory(t *testing.T) {
	vmInst := vm.NewVM()
	dbClassVal := vmInst.MustGlobal("SqliteDatabase")
	if dbClassVal == vm.Nil {
		t.Fatal("SqliteDatabase class not in Globals")
	}

	result := vmInst.Send(dbClassVal, "primOpenMemory", nil)
	if result == vm.Nil {
		t.Fatal("openMemory returned nil")
	}

	dbObj := getSqliteDB(vmInst, result)
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
	vmInst := vm.NewVM()
	dbClassVal := vmInst.MustGlobal("SqliteDatabase")

	tmpDir := t.TempDir()
	dbPath := filepath.Join(tmpDir, "test.db")

	pathVal := vmInst.Registry().NewStringValue(dbPath)
	result := vmInst.Send(dbClassVal, "primOpen:", []vm.Value{pathVal})

	dbObj := getSqliteDB(vmInst, result)
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
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	result := vmInst.Send(db, "primClose", nil)
	if result != vm.True {
		t.Error("close should return true")
	}

	dbObj := getSqliteDB(vmInst, db)
	if !dbObj.closed {
		t.Error("database should be closed")
	}

	// Close again should fail
	result = vmInst.Send(db, "primClose", nil)
	if result == vm.True {
		t.Error("double close should return failure")
	}
}

func TestSqliteExecuteAndQuery(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	// Create table
	sqliteExec(t, vmInst, db, "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, age INTEGER)")

	// Insert
	sqlVal := vmInst.Registry().NewStringValue("INSERT INTO users (name, age) VALUES ('Alice', 30)")
	result := vmInst.Send(db, "primExecute:", []vm.Value{sqlVal})
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("INSERT failed:", sqliteFailureMsg(vmInst, result))
	}
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("expected 1 row affected, got %v", result)
	}

	// Insert with params
	sqlVal = vmInst.Registry().NewStringValue("INSERT INTO users (name, age) VALUES (?, ?)")
	params := vmInst.NewArrayWithElements([]vm.Value{
		vmInst.Registry().NewStringValue("Bob"),
		vm.FromSmallInt(25),
	})
	result = vmInst.Send(db, "primExecuteWith:params:", []vm.Value{sqlVal, params})
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("INSERT with params failed:", sqliteFailureMsg(vmInst, result))
	}

	// Query all
	sqlVal = vmInst.Registry().NewStringValue("SELECT * FROM users ORDER BY id")
	result = vmInst.Send(db, "primQueryAll:", []vm.Value{sqlVal})
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("queryAll: failed:", sqliteFailureMsg(vmInst, result))
	}

	if !result.IsObject() {
		t.Fatal("queryAll: should return an array")
	}
	arr := vm.ObjectFromValue(result)
	if arr == nil || arr.NumSlots() != 2 {
		slots := 0
		if arr != nil {
			slots = arr.NumSlots()
		}
		t.Fatalf("expected 2 rows, got %d", slots)
	}
}

func TestSqliteQueryRow(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT)")
	sqliteExec(t, vmInst, db, "INSERT INTO items (name) VALUES ('Widget')")

	sqlVal := vmInst.Registry().NewStringValue("SELECT * FROM items WHERE id = 1")
	result := vmInst.Send(db, "primQueryRow:", []vm.Value{sqlVal})
	if result == vm.Nil {
		t.Fatal("queryRow: returned nil")
	}
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("queryRow: failed:", sqliteFailureMsg(vmInst, result))
	}

	dictObj := vmInst.Registry().GetDictionaryObject(result)
	if dictObj == nil {
		t.Fatal("queryRow: should return a Dictionary")
	}

	// Query row that doesn't exist
	sqlVal = vmInst.Registry().NewStringValue("SELECT * FROM items WHERE id = 999")
	result = vmInst.Send(db, "primQueryRow:", []vm.Value{sqlVal})
	if result != vm.Nil {
		t.Error("queryRow: for no results should return nil")
	}
}

func TestSqliteQueryRowWithParams(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE items (id INTEGER PRIMARY KEY, name TEXT)")
	sqliteExec(t, vmInst, db, "INSERT INTO items (name) VALUES ('Widget')")

	sqlVal := vmInst.Registry().NewStringValue("SELECT * FROM items WHERE name = ?")
	params := vmInst.NewArrayWithElements([]vm.Value{vmInst.Registry().NewStringValue("Widget")})
	result := vmInst.Send(db, "primQueryRowWith:params:", []vm.Value{sqlVal, params})
	if result == vm.Nil {
		t.Fatal("queryRow:with: returned nil")
	}
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("queryRow:with: failed:", sqliteFailureMsg(vmInst, result))
	}
}

func TestSqliteTransaction(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE txtest (id INTEGER PRIMARY KEY, val TEXT)")

	// Begin transaction
	result := vmInst.Send(db, "primBeginTransaction", nil)
	if result != db {
		t.Error("beginTransaction should return the database for chaining")
	}

	// Insert inside transaction
	sqliteExec(t, vmInst, db, "INSERT INTO txtest (val) VALUES ('inside-tx')")

	// Rollback
	result = vmInst.Send(db, "primRollbackTransaction", nil)
	if result != vm.True {
		t.Error("rollbackTransaction should return true")
	}

	// Verify data was rolled back
	sqlVal := vmInst.Registry().NewStringValue("SELECT COUNT(*) as cnt FROM txtest")
	rowResult := vmInst.Send(db, "primQueryRow:", []vm.Value{sqlVal})
	if rowResult == vm.Nil {
		t.Fatal("queryRow returned nil after rollback")
	}

	// Test commit
	vmInst.Send(db, "primBeginTransaction", nil)
	sqliteExec(t, vmInst, db, "INSERT INTO txtest (val) VALUES ('committed')")
	vmInst.Send(db, "primCommitTransaction", nil)

	sqlVal = vmInst.Registry().NewStringValue("SELECT COUNT(*) as cnt FROM txtest")
	rowResult = vmInst.Send(db, "primQueryRow:", []vm.Value{sqlVal})
	if rowResult == vm.Nil {
		t.Fatal("queryRow returned nil after commit")
	}
}

func TestSqlitePreparedStatement(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE prep_test (id INTEGER PRIMARY KEY, name TEXT)")

	// Prepare an insert statement
	sqlVal := vmInst.Registry().NewStringValue("INSERT INTO prep_test (name) VALUES (?)")
	stmt := vmInst.Send(db, "primPrepare:", []vm.Value{sqlVal})
	if sqliteIsFailure(vmInst, stmt) {
		t.Fatal("prepare: failed:", sqliteFailureMsg(vmInst, stmt))
	}

	stmtObj := getSqliteStmt(vmInst, stmt)
	if stmtObj == nil {
		t.Fatal("could not extract SqliteStatementObject")
	}

	// Execute with different params
	for _, name := range []string{"Alice", "Bob", "Charlie"} {
		params := vmInst.NewArrayWithElements([]vm.Value{vmInst.Registry().NewStringValue(name)})
		result := vmInst.Send(stmt, "primExecuteWith:", []vm.Value{params})
		if sqliteIsFailure(vmInst, result) {
			t.Fatalf("executeWith: failed for %s: %s", name, sqliteFailureMsg(vmInst, result))
		}
	}

	// Close statement
	result := vmInst.Send(stmt, "primClose", nil)
	if result != vm.True {
		t.Error("close should return true")
	}

	// Verify rows were inserted
	sqlVal = vmInst.Registry().NewStringValue("SELECT COUNT(*) as cnt FROM prep_test")
	row := vmInst.Send(db, "primQueryRow:", []vm.Value{sqlVal})
	if row == vm.Nil {
		t.Fatal("queryRow returned nil")
	}
}

func TestSqliteRows(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE row_test (id INTEGER PRIMARY KEY, name TEXT, score REAL)")
	sqliteExec(t, vmInst, db, "INSERT INTO row_test (name, score) VALUES ('Alice', 95.5)")
	sqliteExec(t, vmInst, db, "INSERT INTO row_test (name, score) VALUES ('Bob', 87.3)")

	// Query returning rows
	sqlVal := vmInst.Registry().NewStringValue("SELECT * FROM row_test ORDER BY id")
	rows := vmInst.Send(db, "primQuery:", []vm.Value{sqlVal})
	if sqliteIsFailure(vmInst, rows) {
		t.Fatal("query: failed:", sqliteFailureMsg(vmInst, rows))
	}

	rowsObj := getSqliteRows(vmInst, rows)
	if rowsObj == nil {
		t.Fatal("could not extract SqliteRowsObject")
	}

	// Check columns
	cols := vmInst.Send(rows, "primColumns", nil)
	if !cols.IsObject() {
		t.Fatal("columns should return an array")
	}
	colCount := vmInst.Send(rows, "primColumnCount", nil)
	if !colCount.IsSmallInt() || colCount.SmallInt() != 3 {
		t.Errorf("expected 3 columns, got %v", colCount)
	}

	// Iterate rows
	count := 0
	for {
		hasNext := vmInst.Send(rows, "primNext", nil)
		if hasNext != vm.True {
			break
		}
		count++

		// Get current row as dict
		dict := vmInst.Send(rows, "primAsDict", nil)
		if sqliteIsFailure(vmInst, dict) {
			t.Fatal("asDict failed:", sqliteFailureMsg(vmInst, dict))
		}
		dictObj := vmInst.Registry().GetDictionaryObject(dict)
		if dictObj == nil {
			t.Fatal("asDict should return a Dictionary")
		}
	}
	if count != 2 {
		t.Errorf("expected 2 rows, got %d", count)
	}

	// Close rows
	result := vmInst.Send(rows, "primClose", nil)
	if result != vm.True {
		t.Error("rows close should return true")
	}
}

func TestSqliteTypeMapping(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, `CREATE TABLE types_test (
		int_val INTEGER,
		real_val REAL,
		text_val TEXT,
		blob_val BLOB,
		null_val TEXT
	)`)
	sqliteExec(t, vmInst, db, "INSERT INTO types_test VALUES (42, 3.14, 'hello', X'DEADBEEF', NULL)")

	sqlVal := vmInst.Registry().NewStringValue("SELECT * FROM types_test")
	row := vmInst.Send(db, "primQueryRow:", []vm.Value{sqlVal})
	if row == vm.Nil || sqliteIsFailure(vmInst, row) {
		t.Fatal("queryRow: returned nil or failure")
	}

	dictObj := vmInst.Registry().GetDictionaryObject(row)
	if dictObj == nil {
		t.Fatal("expected dictionary")
	}

	if dictObj.Size() != 5 {
		t.Errorf("expected 5 columns in dict, got %d", dictObj.Size())
	}
}

func TestSqliteWAL(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	result := vmInst.Send(db, "primEnableWAL", nil)
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("enableWAL failed:", sqliteFailureMsg(vmInst, result))
	}
}

func TestSqlitePragma(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	nameVal := vmInst.Registry().NewStringValue("journal_mode")
	result := vmInst.Send(db, "primPragma:", []vm.Value{nameVal})
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("pragma: failed:", sqliteFailureMsg(vmInst, result))
	}

	nameVal = vmInst.Registry().NewStringValue("cache_size")
	valVal := vmInst.Registry().NewStringValue("-2000")
	result = vmInst.Send(db, "primPragmaSet:value:", []vm.Value{nameVal, valVal})
	if result != vm.True {
		t.Error("pragma:set: should return true")
	}
}

func TestSqliteTableExists(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE exists_test (id INTEGER)")

	nameVal := vmInst.Registry().NewStringValue("exists_test")
	result := vmInst.Send(db, "primTableExists:", []vm.Value{nameVal})
	if result != vm.True {
		t.Error("tableExists: should return true for existing table")
	}

	nameVal = vmInst.Registry().NewStringValue("nonexistent")
	result = vmInst.Send(db, "primTableExists:", []vm.Value{nameVal})
	if result != vm.False {
		t.Error("tableExists: should return false for nonexistent table")
	}
}

func TestSqliteTables(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE alpha (id INTEGER)")
	sqliteExec(t, vmInst, db, "CREATE TABLE beta (id INTEGER)")

	result := vmInst.Send(db, "primTables", nil)
	if !result.IsObject() {
		t.Fatal("tables should return an array")
	}
	arr := vm.ObjectFromValue(result)
	if arr == nil || arr.NumSlots() < 2 {
		t.Errorf("expected at least 2 tables")
	}
}

func TestSqliteVersion(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	result := vmInst.Send(db, "primVersion", nil)
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("version failed:", sqliteFailureMsg(vmInst, result))
	}
	ver := vmInst.Registry().GetStringContent(result)
	if ver == "" {
		t.Error("version should return a non-empty string")
	}
}

func TestSqliteMigration(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqlVal := vmInst.Registry().NewStringValue("CREATE TABLE migration_test (id INTEGER PRIMARY KEY, name TEXT)")
	verVal := vm.FromSmallInt(1)
	result := vmInst.Send(db, "primMigrate:version:", []vm.Value{sqlVal, verVal})
	if result != vm.True {
		t.Error("first migration should return true (applied)")
	}

	result = vmInst.Send(db, "primMigrate:version:", []vm.Value{sqlVal, verVal})
	if result != vm.False {
		t.Error("duplicate migration should return false (already applied)")
	}

	result = vmInst.Send(db, "primMigrationVersion", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("migrationVersion = %v, want 1", result)
	}

	sqlVal = vmInst.Registry().NewStringValue("ALTER TABLE migration_test ADD COLUMN age INTEGER")
	verVal = vm.FromSmallInt(2)
	result = vmInst.Send(db, "primMigrate:version:", []vm.Value{sqlVal, verVal})
	if result != vm.True {
		t.Error("migration v2 should return true")
	}

	result = vmInst.Send(db, "primMigrationVersion", nil)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("migrationVersion = %v, want 2", result)
	}
}

func TestSqliteLastInsertId(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE lid_test (id INTEGER PRIMARY KEY, name TEXT)")
	sqliteExec(t, vmInst, db, "INSERT INTO lid_test (name) VALUES ('first')")

	result := vmInst.Send(db, "primLastInsertId", nil)
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("lastInsertId = %v, want 1", result)
	}

	sqliteExec(t, vmInst, db, "INSERT INTO lid_test (name) VALUES ('second')")
	result = vmInst.Send(db, "primLastInsertId", nil)
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("lastInsertId = %v, want 2", result)
	}
}

func TestSqliteIsClosed(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	result := vmInst.Send(db, "primIsClosed", nil)
	if result != vm.False {
		t.Error("isClosed should return false for open database")
	}

	vmInst.Send(db, "primClose", nil)

	result = vmInst.Send(db, "primIsClosed", nil)
	if result != vm.True {
		t.Error("isClosed should return true after close")
	}
}

func TestSqliteClosedDatabaseErrors(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)
	vmInst.Send(db, "primClose", nil)

	sqlVal := vmInst.Registry().NewStringValue("SELECT 1")
	result := vmInst.Send(db, "primExecute:", []vm.Value{sqlVal})
	if !sqliteIsFailure(vmInst, result) {
		t.Error("execute on closed db should return failure")
	}

	result = vmInst.Send(db, "primQuery:", []vm.Value{sqlVal})
	if !sqliteIsFailure(vmInst, result) {
		t.Error("query on closed db should return failure")
	}
}

func TestSqlitePath(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	result := vmInst.Send(db, "primPath", nil)
	path := vmInst.Registry().GetStringContent(result)
	if path != ":memory:" {
		t.Errorf("path = %q, want :memory:", path)
	}
}

func TestSqliteStmtSQL(t *testing.T) {
	vmInst := vm.NewVM()
	db := sqliteOpenMemory(t, vmInst)

	sqliteExec(t, vmInst, db, "CREATE TABLE sql_test (id INTEGER)")

	sqlStr := "INSERT INTO sql_test (id) VALUES (?)"
	sqlVal := vmInst.Registry().NewStringValue(sqlStr)
	stmt := vmInst.Send(db, "primPrepare:", []vm.Value{sqlVal})
	if sqliteIsFailure(vmInst, stmt) {
		t.Fatal("prepare: failed:", sqliteFailureMsg(vmInst, stmt))
	}

	result := vmInst.Send(stmt, "primSql", nil)
	got := vmInst.Registry().GetStringContent(result)
	if got != sqlStr {
		t.Errorf("sql = %q, want %q", got, sqlStr)
	}

	vmInst.Send(stmt, "primClose", nil)
}

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

func sqliteOpenMemory(t *testing.T, vmInst *vm.VM) vm.Value {
	t.Helper()
	dbClassVal := vmInst.MustGlobal("SqliteDatabase")
	if dbClassVal == vm.Nil {
		t.Fatal("SqliteDatabase class not in Globals")
	}
	result := vmInst.Send(dbClassVal, "primOpenMemory", nil)
	if result == vm.Nil || sqliteIsFailure(vmInst, result) {
		t.Fatal("openMemory failed")
	}
	return result
}

func sqliteExec(t *testing.T, vmInst *vm.VM, db vm.Value, sqlStr string) {
	t.Helper()
	sqlVal := vmInst.Registry().NewStringValue(sqlStr)
	result := vmInst.Send(db, "primExecute:", []vm.Value{sqlVal})
	if sqliteIsFailure(vmInst, result) {
		t.Fatal("execute failed:", sqliteFailureMsg(vmInst, result), "sql:", sqlStr)
	}
}

func sqliteIsFailure(vmInst *vm.VM, v vm.Value) bool {
	r := vmInst.Registry().GetResultFromValue(v)
	if r == nil {
		return false
	}
	return r.Type() == vm.ResultFailure
}

func sqliteFailureMsg(vmInst *vm.VM, v vm.Value) string {
	r := vmInst.Registry().GetResultFromValue(v)
	if r == nil {
		return "<not a result>"
	}
	return vmInst.ValueToString(r.WrappedValue())
}
