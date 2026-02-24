package vm

import (
	"os"
	"path/filepath"
	"testing"
)

// duckDBClass returns the DuckDatabase class value from the VM globals.
func duckDBClass(vm *VM) Value {
	return vm.Globals["DuckDatabase"]
}

// ---------------------------------------------------------------------------
// In-memory open / close
// ---------------------------------------------------------------------------

func TestDuckDBNewInMemory(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	if db == Nil {
		t.Fatal("DuckDatabase new returned nil")
	}

	// Should not be closed
	isClosed := vm.Send(db, "isClosed", nil)
	if isClosed != False {
		t.Error("New database should not be closed")
	}

	// Close it
	result := vm.Send(db, "close", nil)
	assertSuccess(t, vm, result, "close")

	// Now should be closed
	isClosed = vm.Send(db, "isClosed", nil)
	if isClosed != True {
		t.Error("Database should be closed after close")
	}
}

func TestDuckDBCloseIdempotent(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	vm.Send(db, "close", nil)

	// Closing again should succeed
	result := vm.Send(db, "close", nil)
	assertSuccess(t, vm, result, "double close")
}

// ---------------------------------------------------------------------------
// File-backed open
// ---------------------------------------------------------------------------

func TestDuckDBOpenFile(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)
	tmpDir := t.TempDir()

	dbPath := filepath.Join(tmpDir, "test.duckdb")
	db := vm.Send(dc, "open:", []Value{vm.registry.NewStringValue(dbPath)})
	if db == Nil {
		t.Fatal("DuckDatabase open: returned nil")
	}

	// Create a table and insert data
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE t (id INTEGER)")})
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("INSERT INTO t VALUES (42)")})
	vm.Send(db, "close", nil)

	// Verify the file was created
	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		t.Error("Database file was not created")
	}

	// Reopen and verify data persists
	db2 := vm.Send(dc, "open:", []Value{vm.registry.NewStringValue(dbPath)})
	rows := vm.Send(db2, "query:", []Value{vm.registry.NewStringValue("SELECT id FROM t")})
	arr := ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatalf("Expected 1 row from reopened db, got %v", arr)
	}
	vm.Send(db2, "close", nil)
}

// ---------------------------------------------------------------------------
// CRUD operations
// ---------------------------------------------------------------------------

func TestDuckDBCreateInsertQuery(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	// CREATE TABLE
	result := vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"CREATE TABLE users (id INTEGER, name VARCHAR, score DOUBLE)",
	)})
	assertSuccess(t, vm, result, "CREATE TABLE")

	// INSERT
	result = vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"INSERT INTO users VALUES (1, 'alice', 95.5), (2, 'bob', 87.3), (3, 'charlie', 72.1)",
	)})
	assertSuccess(t, vm, result, "INSERT")

	// SELECT all
	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT * FROM users ORDER BY id")})
	arr := ObjectFromValue(rows)
	if arr == nil {
		t.Fatal("query: returned non-array")
	}
	if arr.NumSlots() != 3 {
		t.Fatalf("Expected 3 rows, got %d", arr.NumSlots())
	}

	// Check first row
	row0 := arr.GetSlot(0)
	dictObj := vm.registry.GetDictionaryObject(row0)
	if dictObj == nil {
		t.Fatal("Row is not a Dictionary")
	}

	// Verify 'name' column in first row
	nameKey := vm.registry.NewStringValue("name")
	h := hashValue(vm.registry, nameKey)
	nameVal, exists := dictObj.Data[h]
	if !exists {
		t.Fatal("Missing 'name' key in row")
	}
	name := vm.registry.GetStringContent(nameVal)
	if name != "alice" {
		t.Errorf("First row name = %q, want 'alice'", name)
	}
}

func TestDuckDBUpdate(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE t (id INTEGER, val INTEGER)")})
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("INSERT INTO t VALUES (1, 10), (2, 20)")})

	// UPDATE
	result := vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("UPDATE t SET val = 99 WHERE id = 1")})
	assertSuccess(t, vm, result, "UPDATE")

	// Verify
	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT val FROM t WHERE id = 1")})
	arr := ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 row after UPDATE")
	}
	row := vm.registry.GetDictionaryObject(arr.GetSlot(0))
	valKey := vm.registry.NewStringValue("val")
	h := hashValue(vm.registry, valKey)
	val := row.Data[h]
	if val.SmallInt() != 99 {
		t.Errorf("Updated val = %d, want 99", val.SmallInt())
	}
}

func TestDuckDBDelete(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE t (id INTEGER)")})
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("INSERT INTO t VALUES (1), (2), (3)")})

	// DELETE
	result := vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("DELETE FROM t WHERE id = 2")})
	assertSuccess(t, vm, result, "DELETE")

	// Verify
	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT count(*) as n FROM t")})
	arr := ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 row for count")
	}
	row := vm.registry.GetDictionaryObject(arr.GetSlot(0))
	nKey := vm.registry.NewStringValue("n")
	h := hashValue(vm.registry, nKey)
	n := row.Data[h]
	if n.SmallInt() != 2 {
		t.Errorf("Count after delete = %d, want 2", n.SmallInt())
	}
}

// ---------------------------------------------------------------------------
// Type mapping
// ---------------------------------------------------------------------------

func TestDuckDBTypeMapping(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"CREATE TABLE types (i INTEGER, d DOUBLE, v VARCHAR, b BOOLEAN, n INTEGER)",
	)})
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"INSERT INTO types VALUES (42, 3.14, 'hello', true, NULL)",
	)})

	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT * FROM types")})
	arr := ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 row")
	}

	row := vm.registry.GetDictionaryObject(arr.GetSlot(0))

	// INTEGER -> SmallInt
	iKey := vm.registry.NewStringValue("i")
	iVal := row.Data[hashValue(vm.registry, iKey)]
	if !iVal.IsSmallInt() || iVal.SmallInt() != 42 {
		t.Errorf("INTEGER mapping: got %v, want 42", iVal)
	}

	// DOUBLE -> Float
	dKey := vm.registry.NewStringValue("d")
	dVal := row.Data[hashValue(vm.registry, dKey)]
	if !dVal.IsFloat() {
		t.Errorf("DOUBLE mapping: got non-float %v", dVal)
	}

	// VARCHAR -> String
	vKey := vm.registry.NewStringValue("v")
	vVal := row.Data[hashValue(vm.registry, vKey)]
	if !IsStringValue(vVal) || vm.registry.GetStringContent(vVal) != "hello" {
		t.Errorf("VARCHAR mapping: got %v, want 'hello'", vVal)
	}

	// BOOLEAN -> True/False
	bKey := vm.registry.NewStringValue("b")
	bVal := row.Data[hashValue(vm.registry, bKey)]
	if bVal != True {
		t.Errorf("BOOLEAN mapping: got %v, want True", bVal)
	}

	// NULL -> Nil
	nKey := vm.registry.NewStringValue("n")
	nVal := row.Data[hashValue(vm.registry, nKey)]
	if nVal != Nil {
		t.Errorf("NULL mapping: got %v, want Nil", nVal)
	}
}

// ---------------------------------------------------------------------------
// Parquet read/write
// ---------------------------------------------------------------------------

func TestDuckDBParquetRoundTrip(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)
	tmpDir := t.TempDir()

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	// Create and populate a table
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"CREATE TABLE source (id INTEGER, label VARCHAR)",
	)})
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"INSERT INTO source VALUES (1, 'one'), (2, 'two'), (3, 'three')",
	)})

	// Export to Parquet
	parquetPath := filepath.Join(tmpDir, "output.parquet")
	result := vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"COPY source TO '" + parquetPath + "' (FORMAT PARQUET)",
	)})
	assertSuccess(t, vm, result, "COPY TO PARQUET")

	// Verify file exists
	if _, err := os.Stat(parquetPath); os.IsNotExist(err) {
		t.Fatal("Parquet file was not created")
	}

	// Query directly from Parquet file
	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue(
		"SELECT * FROM read_parquet('" + parquetPath + "') ORDER BY id",
	)})
	arr := ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 3 {
		t.Fatalf("Expected 3 rows from Parquet, got %v", arr)
	}

	// Verify first row
	row0 := vm.registry.GetDictionaryObject(arr.GetSlot(0))
	labelKey := vm.registry.NewStringValue("label")
	labelVal := row0.Data[hashValue(vm.registry, labelKey)]
	if vm.registry.GetStringContent(labelVal) != "one" {
		t.Errorf("First Parquet row label = %q, want 'one'", vm.registry.GetStringContent(labelVal))
	}
}

// ---------------------------------------------------------------------------
// Appender bulk insert
// ---------------------------------------------------------------------------

func TestDuckDBAppenderBulkInsert(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"CREATE TABLE bulk (id INTEGER, name VARCHAR, value DOUBLE)",
	)})

	// Create appender
	appVal := vm.Send(db, "appender:", []Value{vm.registry.NewStringValue("bulk")})
	if appVal == Nil {
		t.Fatal("appender: returned nil")
	}

	// Append rows
	for i := int64(1); i <= 100; i++ {
		row := vm.NewArrayWithElements([]Value{
			FromSmallInt(i),
			vm.registry.NewStringValue("item"),
			FromFloat64(float64(i) * 0.1),
		})
		result := vm.Send(appVal, "appendRow:", []Value{row})
		assertSuccess(t, vm, result, "appendRow:")
	}

	// Close appender (flushes)
	result := vm.Send(appVal, "close", nil)
	assertSuccess(t, vm, result, "appender close")

	// Verify
	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT count(*) as n FROM bulk")})
	arr := ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 count row")
	}
	row := vm.registry.GetDictionaryObject(arr.GetSlot(0))
	nKey := vm.registry.NewStringValue("n")
	n := row.Data[hashValue(vm.registry, nKey)]
	if n.SmallInt() != 100 {
		t.Errorf("Bulk insert count = %d, want 100", n.SmallInt())
	}
}

func TestDuckDBAppenderFlush(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE f (x INTEGER)")})

	appVal := vm.Send(db, "appender:", []Value{vm.registry.NewStringValue("f")})

	// Append some rows
	for i := int64(1); i <= 5; i++ {
		row := vm.NewArrayWithElements([]Value{FromSmallInt(i)})
		vm.Send(appVal, "appendRow:", []Value{row})
	}

	// Flush (not close)
	result := vm.Send(appVal, "flush", nil)
	assertSuccess(t, vm, result, "flush")

	// Data should be visible after flush
	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT count(*) as n FROM f")})
	arr := ObjectFromValue(rows)
	row := vm.registry.GetDictionaryObject(arr.GetSlot(0))
	nKey := vm.registry.NewStringValue("n")
	n := row.Data[hashValue(vm.registry, nKey)]
	if n.SmallInt() != 5 {
		t.Errorf("After flush count = %d, want 5", n.SmallInt())
	}

	// Can still append after flush
	row2 := vm.NewArrayWithElements([]Value{FromSmallInt(6)})
	vm.Send(appVal, "appendRow:", []Value{row2})
	vm.Send(appVal, "close", nil)

	rows = vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT count(*) as n FROM f")})
	arr = ObjectFromValue(rows)
	row3 := vm.registry.GetDictionaryObject(arr.GetSlot(0))
	n = row3.Data[hashValue(vm.registry, nKey)]
	if n.SmallInt() != 6 {
		t.Errorf("After second close count = %d, want 6", n.SmallInt())
	}
}

func TestDuckDBAppenderCloseIdempotent(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE c (x INTEGER)")})
	appVal := vm.Send(db, "appender:", []Value{vm.registry.NewStringValue("c")})

	vm.Send(appVal, "close", nil)

	// Second close should succeed
	result := vm.Send(appVal, "close", nil)
	assertSuccess(t, vm, result, "double appender close")
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

func TestDuckDBQueryOnClosed(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	vm.Send(db, "close", nil)

	result := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT 1")})
	assertFailure(t, vm, result, "query on closed db")
}

func TestDuckDBExecuteOnClosed(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	vm.Send(db, "close", nil)

	result := vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE t (x INTEGER)")})
	assertFailure(t, vm, result, "execute on closed db")
}

func TestDuckDBInvalidSQL(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	result := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("INVALID SQL GIBBERISH")})
	assertFailure(t, vm, result, "invalid SQL")
}

func TestDuckDBAppenderOnClosedDB(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE t (x INTEGER)")})
	vm.Send(db, "close", nil)

	result := vm.Send(db, "appender:", []Value{vm.registry.NewStringValue("t")})
	assertFailure(t, vm, result, "appender on closed db")
}

func TestDuckDBAppenderOnClosedAppender(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE t (x INTEGER)")})
	appVal := vm.Send(db, "appender:", []Value{vm.registry.NewStringValue("t")})
	vm.Send(appVal, "close", nil)

	// appendRow on closed appender
	row := vm.NewArrayWithElements([]Value{FromSmallInt(1)})
	result := vm.Send(appVal, "appendRow:", []Value{row})
	assertFailure(t, vm, result, "appendRow on closed appender")
}

// ---------------------------------------------------------------------------
// Concurrent query safety
// ---------------------------------------------------------------------------

func TestDuckDBConcurrentQueries(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE conc (id INTEGER)")})
	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue(
		"INSERT INTO conc SELECT * FROM range(100)",
	)})

	// Run multiple queries (not truly concurrent in VM, but exercises thread safety)
	for i := 0; i < 10; i++ {
		rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue(
			"SELECT count(*) as n FROM conc",
		)})
		arr := ObjectFromValue(rows)
		if arr == nil || arr.NumSlots() != 1 {
			t.Fatalf("Iteration %d: expected 1 row", i)
		}
	}
}

// ---------------------------------------------------------------------------
// Empty result set
// ---------------------------------------------------------------------------

func TestDuckDBEmptyResult(t *testing.T) {
	vm := NewVM()
	dc := duckDBClass(vm)

	db := vm.Send(dc, "new", nil)
	defer vm.Send(db, "close", nil)

	vm.Send(db, "execute:", []Value{vm.registry.NewStringValue("CREATE TABLE empty (id INTEGER)")})

	rows := vm.Send(db, "query:", []Value{vm.registry.NewStringValue("SELECT * FROM empty")})
	arr := ObjectFromValue(rows)
	if arr == nil {
		t.Fatal("query: returned nil for empty table")
	}
	if arr.NumSlots() != 0 {
		t.Errorf("Expected 0 rows for empty table, got %d", arr.NumSlots())
	}
}
