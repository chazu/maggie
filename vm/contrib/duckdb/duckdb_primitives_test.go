package duckdb

import (
	"os"
	"path/filepath"
	"testing"

	vm "github.com/chazu/maggie/vm"
)

// duckDBClass returns the DuckDatabase class value from the VM globals.
func duckDBClass(vmInst *vm.VM) vm.Value {
	return vmInst.MustGlobal("DuckDatabase")
}

// assertSuccess checks that a value is a Success result and returns the unwrapped value.
func assertSuccess(t *testing.T, vmInst *vm.VM, result vm.Value, context string) vm.Value {
	t.Helper()
	if !vm.IsResultValue(result) {
		t.Fatalf("%s: expected a Result value, got non-result", context)
	}
	isSuccess := vmInst.Send(result, "isSuccess", nil)
	if isSuccess != vm.True {
		// Extract error message for better diagnostics
		errVal := vmInst.Send(result, "error", nil)
		errMsg := ""
		if vm.IsStringValue(errVal) {
			errMsg = vmInst.Registry().GetStringContent(errVal)
		}
		t.Fatalf("%s: expected Success result, got Failure: %s", context, errMsg)
	}
	return vmInst.Send(result, "value", nil)
}

// assertFailure checks that a value is a Failure result and returns the error value.
func assertFailure(t *testing.T, vmInst *vm.VM, result vm.Value, context string) vm.Value {
	t.Helper()
	if !vm.IsResultValue(result) {
		t.Fatalf("%s: expected a Result value, got non-result", context)
	}
	isFailure := vmInst.Send(result, "isFailure", nil)
	if isFailure != vm.True {
		t.Fatalf("%s: expected Failure result, got Success", context)
	}
	return vmInst.Send(result, "error", nil)
}

// ---------------------------------------------------------------------------
// In-memory open / close
// ---------------------------------------------------------------------------

func TestDuckDBNewInMemory(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	if db == vm.Nil {
		t.Fatal("DuckDatabase new returned nil")
	}

	// Should not be closed
	isClosed := vmInst.Send(db, "isClosed", nil)
	if isClosed != vm.False {
		t.Error("New database should not be closed")
	}

	// Close it
	result := vmInst.Send(db, "close", nil)
	assertSuccess(t, vmInst, result, "close")

	// Now should be closed
	isClosed = vmInst.Send(db, "isClosed", nil)
	if isClosed != vm.True {
		t.Error("Database should be closed after close")
	}
}

func TestDuckDBCloseIdempotent(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	vmInst.Send(db, "close", nil)

	// Closing again should succeed
	result := vmInst.Send(db, "close", nil)
	assertSuccess(t, vmInst, result, "double close")
}

// ---------------------------------------------------------------------------
// File-backed open
// ---------------------------------------------------------------------------

func TestDuckDBOpenFile(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)
	tmpDir := t.TempDir()

	dbPath := filepath.Join(tmpDir, "test.duckdb")
	db := vmInst.Send(dc, "open:", []vm.Value{vmInst.Registry().NewStringValue(dbPath)})
	if db == vm.Nil {
		t.Fatal("DuckDatabase open: returned nil")
	}

	// Create a table and insert data
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE t (id INTEGER)")})
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("INSERT INTO t VALUES (42)")})
	vmInst.Send(db, "close", nil)

	// Verify the file was created
	if _, err := os.Stat(dbPath); os.IsNotExist(err) {
		t.Error("Database file was not created")
	}

	// Reopen and verify data persists
	db2 := vmInst.Send(dc, "open:", []vm.Value{vmInst.Registry().NewStringValue(dbPath)})
	rows := vmInst.Send(db2, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT id FROM t")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatalf("Expected 1 row from reopened db, got %v", arr)
	}
	vmInst.Send(db2, "close", nil)
}

// ---------------------------------------------------------------------------
// CRUD operations
// ---------------------------------------------------------------------------

func TestDuckDBCreateInsertQuery(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	// CREATE TABLE
	result := vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"CREATE TABLE users (id INTEGER, name VARCHAR, score DOUBLE)",
	)})
	assertSuccess(t, vmInst, result, "CREATE TABLE")

	// INSERT
	result = vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"INSERT INTO users VALUES (1, 'alice', 95.5), (2, 'bob', 87.3), (3, 'charlie', 72.1)",
	)})
	assertSuccess(t, vmInst, result, "INSERT")

	// SELECT all
	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT * FROM users ORDER BY id")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil {
		t.Fatal("query: returned non-array")
	}
	if arr.NumSlots() != 3 {
		t.Fatalf("Expected 3 rows, got %d", arr.NumSlots())
	}

	// Check first row
	row0 := arr.GetSlot(0)
	dictObj := vmInst.Registry().GetDictionaryObject(row0)
	if dictObj == nil {
		t.Fatal("Row is not a Dictionary")
	}

	// Verify 'name' column in first row
	nameKey := vmInst.Registry().NewStringValue("name")
	h := vm.HashValue(vmInst.Registry(), nameKey)
	nameVal, exists := dictObj.Data[h]
	if !exists {
		t.Fatal("Missing 'name' key in row")
	}
	name := vmInst.Registry().GetStringContent(nameVal)
	if name != "alice" {
		t.Errorf("First row name = %q, want 'alice'", name)
	}
}

func TestDuckDBUpdate(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE t (id INTEGER, val INTEGER)")})
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("INSERT INTO t VALUES (1, 10), (2, 20)")})

	// UPDATE
	result := vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("UPDATE t SET val = 99 WHERE id = 1")})
	assertSuccess(t, vmInst, result, "UPDATE")

	// Verify
	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT val FROM t WHERE id = 1")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 row after UPDATE")
	}
	row := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))
	valKey := vmInst.Registry().NewStringValue("val")
	h := vm.HashValue(vmInst.Registry(), valKey)
	val := row.Data[h]
	if val.SmallInt() != 99 {
		t.Errorf("Updated val = %d, want 99", val.SmallInt())
	}
}

func TestDuckDBDelete(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE t (id INTEGER)")})
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("INSERT INTO t VALUES (1), (2), (3)")})

	// DELETE
	result := vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("DELETE FROM t WHERE id = 2")})
	assertSuccess(t, vmInst, result, "DELETE")

	// Verify
	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT count(*) as n FROM t")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 row for count")
	}
	row := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))
	nKey := vmInst.Registry().NewStringValue("n")
	h := vm.HashValue(vmInst.Registry(), nKey)
	n := row.Data[h]
	if n.SmallInt() != 2 {
		t.Errorf("Count after delete = %d, want 2", n.SmallInt())
	}
}

// ---------------------------------------------------------------------------
// Type mapping
// ---------------------------------------------------------------------------

func TestDuckDBTypeMapping(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"CREATE TABLE types (i INTEGER, d DOUBLE, v VARCHAR, b BOOLEAN, n INTEGER)",
	)})
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"INSERT INTO types VALUES (42, 3.14, 'hello', true, NULL)",
	)})

	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT * FROM types")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 row")
	}

	row := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))

	// INTEGER -> SmallInt
	iKey := vmInst.Registry().NewStringValue("i")
	iVal := row.Data[vm.HashValue(vmInst.Registry(), iKey)]
	if !iVal.IsSmallInt() || iVal.SmallInt() != 42 {
		t.Errorf("INTEGER mapping: got %v, want 42", iVal)
	}

	// DOUBLE -> Float
	dKey := vmInst.Registry().NewStringValue("d")
	dVal := row.Data[vm.HashValue(vmInst.Registry(), dKey)]
	if !dVal.IsFloat() {
		t.Errorf("DOUBLE mapping: got non-float %v", dVal)
	}

	// VARCHAR -> String
	vKey := vmInst.Registry().NewStringValue("v")
	vVal := row.Data[vm.HashValue(vmInst.Registry(), vKey)]
	if !vm.IsStringValue(vVal) || vmInst.Registry().GetStringContent(vVal) != "hello" {
		t.Errorf("VARCHAR mapping: got %v, want 'hello'", vVal)
	}

	// BOOLEAN -> True/False
	bKey := vmInst.Registry().NewStringValue("b")
	bVal := row.Data[vm.HashValue(vmInst.Registry(), bKey)]
	if bVal != vm.True {
		t.Errorf("BOOLEAN mapping: got %v, want True", bVal)
	}

	// NULL -> Nil
	nKey := vmInst.Registry().NewStringValue("n")
	nVal := row.Data[vm.HashValue(vmInst.Registry(), nKey)]
	if nVal != vm.Nil {
		t.Errorf("NULL mapping: got %v, want Nil", nVal)
	}
}

// ---------------------------------------------------------------------------
// Parquet read/write
// ---------------------------------------------------------------------------

func TestDuckDBParquetRoundTrip(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)
	tmpDir := t.TempDir()

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	// Create and populate a table
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"CREATE TABLE source (id INTEGER, label VARCHAR)",
	)})
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"INSERT INTO source VALUES (1, 'one'), (2, 'two'), (3, 'three')",
	)})

	// Export to Parquet
	parquetPath := filepath.Join(tmpDir, "output.parquet")
	result := vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"COPY source TO '" + parquetPath + "' (FORMAT PARQUET)",
	)})
	assertSuccess(t, vmInst, result, "COPY TO PARQUET")

	// Verify file exists
	if _, err := os.Stat(parquetPath); os.IsNotExist(err) {
		t.Fatal("Parquet file was not created")
	}

	// Query directly from Parquet file
	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue(
		"SELECT * FROM read_parquet('" + parquetPath + "') ORDER BY id",
	)})
	arr := vm.ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 3 {
		t.Fatalf("Expected 3 rows from Parquet, got %v", arr)
	}

	// Verify first row
	row0 := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))
	labelKey := vmInst.Registry().NewStringValue("label")
	labelVal := row0.Data[vm.HashValue(vmInst.Registry(), labelKey)]
	if vmInst.Registry().GetStringContent(labelVal) != "one" {
		t.Errorf("First Parquet row label = %q, want 'one'", vmInst.Registry().GetStringContent(labelVal))
	}
}

// ---------------------------------------------------------------------------
// Appender bulk insert
// ---------------------------------------------------------------------------

func TestDuckDBAppenderBulkInsert(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"CREATE TABLE bulk (id INTEGER, name VARCHAR, value DOUBLE)",
	)})

	// Create appender
	appVal := vmInst.Send(db, "appender:", []vm.Value{vmInst.Registry().NewStringValue("bulk")})
	if appVal == vm.Nil {
		t.Fatal("appender: returned nil")
	}

	// Append rows
	for i := int64(1); i <= 100; i++ {
		row := vmInst.NewArrayWithElements([]vm.Value{
			vm.FromSmallInt(i),
			vmInst.Registry().NewStringValue("item"),
			vm.FromFloat64(float64(i) * 0.1),
		})
		result := vmInst.Send(appVal, "appendRow:", []vm.Value{row})
		assertSuccess(t, vmInst, result, "appendRow:")
	}

	// Close appender (flushes)
	result := vmInst.Send(appVal, "close", nil)
	assertSuccess(t, vmInst, result, "appender close")

	// Verify
	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT count(*) as n FROM bulk")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil || arr.NumSlots() != 1 {
		t.Fatal("Expected 1 count row")
	}
	row := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))
	nKey := vmInst.Registry().NewStringValue("n")
	n := row.Data[vm.HashValue(vmInst.Registry(), nKey)]
	if n.SmallInt() != 100 {
		t.Errorf("Bulk insert count = %d, want 100", n.SmallInt())
	}
}

func TestDuckDBAppenderFlush(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE f (x INTEGER)")})

	appVal := vmInst.Send(db, "appender:", []vm.Value{vmInst.Registry().NewStringValue("f")})

	// Append some rows
	for i := int64(1); i <= 5; i++ {
		row := vmInst.NewArrayWithElements([]vm.Value{vm.FromSmallInt(i)})
		vmInst.Send(appVal, "appendRow:", []vm.Value{row})
	}

	// Flush (not close)
	result := vmInst.Send(appVal, "flush", nil)
	assertSuccess(t, vmInst, result, "flush")

	// Data should be visible after flush
	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT count(*) as n FROM f")})
	arr := vm.ObjectFromValue(rows)
	row := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))
	nKey := vmInst.Registry().NewStringValue("n")
	n := row.Data[vm.HashValue(vmInst.Registry(), nKey)]
	if n.SmallInt() != 5 {
		t.Errorf("After flush count = %d, want 5", n.SmallInt())
	}

	// Can still append after flush
	row2 := vmInst.NewArrayWithElements([]vm.Value{vm.FromSmallInt(6)})
	vmInst.Send(appVal, "appendRow:", []vm.Value{row2})
	vmInst.Send(appVal, "close", nil)

	rows = vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT count(*) as n FROM f")})
	arr = vm.ObjectFromValue(rows)
	row3 := vmInst.Registry().GetDictionaryObject(arr.GetSlot(0))
	n = row3.Data[vm.HashValue(vmInst.Registry(), nKey)]
	if n.SmallInt() != 6 {
		t.Errorf("After second close count = %d, want 6", n.SmallInt())
	}
}

func TestDuckDBAppenderCloseIdempotent(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE c (x INTEGER)")})
	appVal := vmInst.Send(db, "appender:", []vm.Value{vmInst.Registry().NewStringValue("c")})

	vmInst.Send(appVal, "close", nil)

	// Second close should succeed
	result := vmInst.Send(appVal, "close", nil)
	assertSuccess(t, vmInst, result, "double appender close")
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

func TestDuckDBQueryOnClosed(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	vmInst.Send(db, "close", nil)

	result := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT 1")})
	assertFailure(t, vmInst, result, "query on closed db")
}

func TestDuckDBExecuteOnClosed(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	vmInst.Send(db, "close", nil)

	result := vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE t (x INTEGER)")})
	assertFailure(t, vmInst, result, "execute on closed db")
}

func TestDuckDBInvalidSQL(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	result := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("INVALID SQL GIBBERISH")})
	assertFailure(t, vmInst, result, "invalid SQL")
}

func TestDuckDBAppenderOnClosedDB(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE t (x INTEGER)")})
	vmInst.Send(db, "close", nil)

	result := vmInst.Send(db, "appender:", []vm.Value{vmInst.Registry().NewStringValue("t")})
	assertFailure(t, vmInst, result, "appender on closed db")
}

func TestDuckDBAppenderOnClosedAppender(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE t (x INTEGER)")})
	appVal := vmInst.Send(db, "appender:", []vm.Value{vmInst.Registry().NewStringValue("t")})
	vmInst.Send(appVal, "close", nil)

	// appendRow on closed appender
	row := vmInst.NewArrayWithElements([]vm.Value{vm.FromSmallInt(1)})
	result := vmInst.Send(appVal, "appendRow:", []vm.Value{row})
	assertFailure(t, vmInst, result, "appendRow on closed appender")
}

// ---------------------------------------------------------------------------
// Concurrent query safety
// ---------------------------------------------------------------------------

func TestDuckDBConcurrentQueries(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE conc (id INTEGER)")})
	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue(
		"INSERT INTO conc SELECT * FROM range(100)",
	)})

	// Run multiple queries (not truly concurrent in VM, but exercises thread safety)
	for i := 0; i < 10; i++ {
		rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue(
			"SELECT count(*) as n FROM conc",
		)})
		arr := vm.ObjectFromValue(rows)
		if arr == nil || arr.NumSlots() != 1 {
			t.Fatalf("Iteration %d: expected 1 row", i)
		}
	}
}

// ---------------------------------------------------------------------------
// Empty result set
// ---------------------------------------------------------------------------

func TestDuckDBEmptyResult(t *testing.T) {
	vmInst := vm.NewVM()
	dc := duckDBClass(vmInst)

	db := vmInst.Send(dc, "new", nil)
	defer vmInst.Send(db, "close", nil)

	vmInst.Send(db, "execute:", []vm.Value{vmInst.Registry().NewStringValue("CREATE TABLE empty (id INTEGER)")})

	rows := vmInst.Send(db, "query:", []vm.Value{vmInst.Registry().NewStringValue("SELECT * FROM empty")})
	arr := vm.ObjectFromValue(rows)
	if arr == nil {
		t.Fatal("query: returned nil for empty table")
	}
	if arr.NumSlots() != 0 {
		t.Errorf("Expected 0 rows for empty table, got %d", arr.NumSlots())
	}
}
