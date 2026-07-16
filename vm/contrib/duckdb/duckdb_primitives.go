package duckdb

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"reflect"
	"sync"

	"github.com/marcboeker/go-duckdb"

	vm "github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// DuckDB Object Types
// ---------------------------------------------------------------------------

// DuckDatabaseObject wraps a DuckDB database for use in Maggie.
type DuckDatabaseObject struct {
	connector *duckdb.Connector
	db        *sql.DB
	closed    bool
	mu        sync.Mutex
}

// DuckAppenderObject wraps a DuckDB appender for bulk inserts.
type DuckAppenderObject struct {
	appender *duckdb.Appender
	closed   bool
	mu       sync.Mutex
}

// ---------------------------------------------------------------------------
// DuckDB Primitives Registration
// ---------------------------------------------------------------------------

func RegisterDuckDBPrimitives(vmInst *vm.VM) {
	duckDBClass := vmInst.RegisterGoType("DuckDatabase", reflect.TypeOf(&DuckDatabaseObject{}))
	appenderClass := vmInst.RegisterGoType("DuckAppender", reflect.TypeOf(&DuckAppenderObject{}))

	// -----------------------------------------------------------------------
	// DuckDatabase class methods
	// -----------------------------------------------------------------------

	// new — Open an in-memory DuckDB database
	duckDBClass.AddClassMethod0(vmInst.Selectors, "new", func(v *vm.VM, recv vm.Value) vm.Value {
		return openDuckDB(v, "")
	})

	// open: path — Open a file-backed DuckDB database
	duckDBClass.AddClassMethod1(vmInst.Selectors, "open:", func(v *vm.VM, recv vm.Value, pathVal vm.Value) vm.Value {
		path := v.ValueToString(pathVal)
		if path == "" {
			return v.NewFailureResult("open: requires a path string")
		}
		return openDuckDB(v, path)
	})

	// -----------------------------------------------------------------------
	// DuckDatabase instance methods
	// -----------------------------------------------------------------------

	// query: sql — Execute a SELECT and return results as an array of dictionaries
	duckDBClass.AddMethod1(vmInst.Selectors, "query:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getDuckDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("query: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		if dbObj.closed {
			dbObj.mu.Unlock()
			return v.NewFailureResult("query: database is closed")
		}
		db := dbObj.db
		dbObj.mu.Unlock()

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("query: requires a SQL string")
		}

		rows, err := db.Query(sqlStr)
		if err != nil {
			return v.NewFailureResult("query: " + err.Error())
		}
		defer rows.Close()

		return rowsToArray(v, rows)
	})

	// execute: sql — Execute DDL/DML with no result set
	duckDBClass.AddMethod1(vmInst.Selectors, "execute:", func(v *vm.VM, recv vm.Value, sqlVal vm.Value) vm.Value {
		dbObj := getDuckDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("execute: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		if dbObj.closed {
			dbObj.mu.Unlock()
			return v.NewFailureResult("execute: database is closed")
		}
		db := dbObj.db
		dbObj.mu.Unlock()

		sqlStr := v.ValueToString(sqlVal)
		if sqlStr == "" {
			return v.NewFailureResult("execute: requires a SQL string")
		}

		result, err := db.Exec(sqlStr)
		if err != nil {
			return v.NewFailureResult("execute: " + err.Error())
		}

		affected, _ := result.RowsAffected()
		return v.NewSuccessResult(vm.FromSmallInt(affected))
	})

	// close — Close the database
	duckDBClass.AddMethod0(vmInst.Selectors, "close", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getDuckDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("close: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		defer dbObj.mu.Unlock()

		if dbObj.closed {
			return v.NewSuccessResult(recv)
		}

		var firstErr error
		if dbObj.db != nil {
			if err := dbObj.db.Close(); err != nil {
				firstErr = err
			}
		}
		if dbObj.connector != nil {
			if err := dbObj.connector.Close(); err != nil && firstErr == nil {
				firstErr = err
			}
		}
		dbObj.closed = true

		if firstErr != nil {
			return v.NewFailureResult("close: " + firstErr.Error())
		}
		return v.NewSuccessResult(recv)
	})

	// isClosed — Check if database is closed
	duckDBClass.AddMethod0(vmInst.Selectors, "isClosed", func(v *vm.VM, recv vm.Value) vm.Value {
		dbObj := getDuckDB(v, recv)
		if dbObj == nil {
			return vm.True
		}
		dbObj.mu.Lock()
		defer dbObj.mu.Unlock()
		if dbObj.closed {
			return vm.True
		}
		return vm.False
	})

	// appender: tableName — Create an appender for bulk inserts
	duckDBClass.AddMethod1(vmInst.Selectors, "appender:", func(v *vm.VM, recv vm.Value, tableVal vm.Value) vm.Value {
		dbObj := getDuckDB(v, recv)
		if dbObj == nil {
			return v.NewFailureResult("appender: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		if dbObj.closed {
			dbObj.mu.Unlock()
			return v.NewFailureResult("appender: database is closed")
		}
		connector := dbObj.connector
		dbObj.mu.Unlock()

		tableName := v.ValueToString(tableVal)
		if tableName == "" {
			return v.NewFailureResult("appender: requires a table name string")
		}

		conn, err := connector.Connect(context.Background())
		if err != nil {
			return v.NewFailureResult("appender: " + err.Error())
		}

		appender, err := duckdb.NewAppenderFromConn(conn, "", tableName)
		if err != nil {
			conn.Close()
			return v.NewFailureResult("appender: " + err.Error())
		}

		appObj := &DuckAppenderObject{appender: appender}
		val, regErr := v.RegisterGoObject(appObj)
		if regErr != nil {
			appender.Close()
			return v.NewFailureResult("appender: " + regErr.Error())
		}
		return val
	})

	// -----------------------------------------------------------------------
	// DuckAppender instance methods
	// -----------------------------------------------------------------------

	// appendRow: anArray — Append a row of values
	appenderClass.AddMethod1(vmInst.Selectors, "appendRow:", func(v *vm.VM, recv vm.Value, rowVal vm.Value) vm.Value {
		appObj := getDuckAppender(v, recv)
		if appObj == nil {
			return v.NewFailureResult("appendRow: receiver is not a DuckAppender")
		}

		appObj.mu.Lock()
		defer appObj.mu.Unlock()

		if appObj.closed {
			return v.NewFailureResult("appendRow: appender is closed")
		}

		// Convert Maggie array to Go values
		goArgs := arrayToDriverValues(v, rowVal)
		if goArgs == nil {
			return v.NewFailureResult("appendRow: requires an Array")
		}

		if err := appObj.appender.AppendRow(goArgs...); err != nil {
			return v.NewFailureResult("appendRow: " + err.Error())
		}

		return v.NewSuccessResult(recv)
	})

	// flush — Flush buffered rows to the table
	appenderClass.AddMethod0(vmInst.Selectors, "flush", func(v *vm.VM, recv vm.Value) vm.Value {
		appObj := getDuckAppender(v, recv)
		if appObj == nil {
			return v.NewFailureResult("flush: receiver is not a DuckAppender")
		}

		appObj.mu.Lock()
		defer appObj.mu.Unlock()

		if appObj.closed {
			return v.NewFailureResult("flush: appender is closed")
		}

		if err := appObj.appender.Flush(); err != nil {
			return v.NewFailureResult("flush: " + err.Error())
		}

		return v.NewSuccessResult(recv)
	})

	// close — Close the appender (flushes remaining rows)
	appenderClass.AddMethod0(vmInst.Selectors, "close", func(v *vm.VM, recv vm.Value) vm.Value {
		appObj := getDuckAppender(v, recv)
		if appObj == nil {
			return v.NewFailureResult("close: receiver is not a DuckAppender")
		}

		appObj.mu.Lock()
		defer appObj.mu.Unlock()

		if appObj.closed {
			return v.NewSuccessResult(recv)
		}

		if err := appObj.appender.Close(); err != nil {
			appObj.closed = true
			return v.NewFailureResult("close: " + err.Error())
		}

		appObj.closed = true
		return v.NewSuccessResult(recv)
	})

	// isClosed — Check if appender is closed
	appenderClass.AddMethod0(vmInst.Selectors, "isClosed", func(v *vm.VM, recv vm.Value) vm.Value {
		appObj := getDuckAppender(v, recv)
		if appObj == nil {
			return vm.True
		}
		appObj.mu.Lock()
		defer appObj.mu.Unlock()
		if appObj.closed {
			return vm.True
		}
		return vm.False
	})
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// openDuckDB opens a DuckDB database (in-memory if dsn is empty).
func openDuckDB(vmInst *vm.VM, dsn string) vm.Value {
	connector, err := duckdb.NewConnector(dsn, nil)
	if err != nil {
		return vmInst.NewFailureResult("DuckDatabase open: " + err.Error())
	}

	db := sql.OpenDB(connector)

	obj := &DuckDatabaseObject{
		connector: connector,
		db:        db,
	}

	val, regErr := vmInst.RegisterGoObject(obj)
	if regErr != nil {
		db.Close()
		connector.Close()
		return vmInst.NewFailureResult("DuckDatabase: " + regErr.Error())
	}
	return val
}

// getDuckDB extracts a DuckDatabaseObject from a Maggie Value.
func getDuckDB(vmInst *vm.VM, v vm.Value) *DuckDatabaseObject {
	goVal, ok := vmInst.GetGoObject(v)
	if !ok {
		return nil
	}
	dbObj, ok := goVal.(*DuckDatabaseObject)
	if !ok {
		return nil
	}
	return dbObj
}

// getDuckAppender extracts a DuckAppenderObject from a Maggie Value.
func getDuckAppender(vmInst *vm.VM, v vm.Value) *DuckAppenderObject {
	goVal, ok := vmInst.GetGoObject(v)
	if !ok {
		return nil
	}
	appObj, ok := goVal.(*DuckAppenderObject)
	if !ok {
		return nil
	}
	return appObj
}

// rowsToArray converts sql.Rows into a Maggie Array of Dictionaries.
func rowsToArray(vmInst *vm.VM, rows *sql.Rows) vm.Value {
	cols, err := rows.Columns()
	if err != nil {
		return vmInst.NewFailureResult("query: " + err.Error())
	}

	var results []vm.Value

	for rows.Next() {
		// Create scan targets
		scanValues := make([]interface{}, len(cols))
		scanPtrs := make([]interface{}, len(cols))
		for i := range scanValues {
			scanPtrs[i] = &scanValues[i]
		}

		if err := rows.Scan(scanPtrs...); err != nil {
			return vmInst.NewFailureResult("query: " + err.Error())
		}

		// Build a Dictionary for this row
		dict := vmInst.Registry().NewDictionaryValue()
		dictObj := vmInst.Registry().GetDictionaryObject(dict)
		if dictObj == nil {
			continue
		}

		for i, colName := range cols {
			key := vmInst.Registry().NewStringValue(colName)
			val := goScanValueToMaggie(vmInst, scanValues[i])
			dictObj.Put(vmInst.Registry(), key, val)
		}

		results = append(results, dict)
	}

	if err := rows.Err(); err != nil {
		return vmInst.NewFailureResult("query: " + err.Error())
	}

	return vmInst.NewArrayWithElements(results)
}

// goScanValueToMaggie converts a database scan value to a Maggie Value.
func goScanValueToMaggie(vmInst *vm.VM, v interface{}) vm.Value {
	if v == nil {
		return vm.Nil
	}
	switch val := v.(type) {
	case int64:
		return vm.FromSmallInt(val)
	case int32:
		return vm.FromSmallInt(int64(val))
	case int:
		return vm.FromSmallInt(int64(val))
	case float64:
		return vm.FromFloat64(val)
	case float32:
		return vm.FromFloat64(float64(val))
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
		// Try reflect-based conversion for other types
		rv := reflect.ValueOf(v)
		switch rv.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			return vm.FromSmallInt(rv.Int())
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			return vm.FromSmallInt(int64(rv.Uint()))
		case reflect.Float32, reflect.Float64:
			return vm.FromFloat64(rv.Float())
		case reflect.String:
			return vmInst.Registry().NewStringValue(rv.String())
		default:
			// Use Stringer if available, otherwise nil
			if s, ok := v.(interface{ String() string }); ok {
				return vmInst.Registry().NewStringValue(s.String())
			}
			return vm.Nil
		}
	}
}

// valueToDuckDB converts a Maggie Value to a Go value suitable for DuckDB.
func valueToDuckDB(vmInst *vm.VM, v vm.Value) interface{} {
	switch {
	case v == vm.Nil:
		return nil
	case v == vm.True:
		return true
	case v == vm.False:
		return false
	case v.IsSmallInt():
		return v.SmallInt()
	case v.IsFloat():
		return v.Float64()
	case vm.IsStringValue(v):
		return vmInst.Registry().GetStringContent(v)
	default:
		return nil
	}
}

// arrayToDriverValues converts a Maggie Array value to []driver.Value for AppendRow.
func arrayToDriverValues(vmInst *vm.VM, v vm.Value) []driver.Value {
	if !v.IsObject() {
		return nil
	}
	obj := vm.ObjectFromValue(v)
	if obj == nil {
		return nil
	}
	n := obj.NumSlots()
	result := make([]driver.Value, n)
	for i := 0; i < n; i++ {
		result[i] = valueToDuckDB(vmInst, obj.GetSlot(i))
	}
	return result
}
