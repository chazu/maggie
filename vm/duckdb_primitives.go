package vm

import (
	"context"
	"database/sql"
	"database/sql/driver"
	"reflect"
	"sync"

	"github.com/marcboeker/go-duckdb"
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

func (vm *VM) registerDuckDBPrimitives() {
	duckDBClass := vm.RegisterGoType("DuckDatabase", reflect.TypeOf(&DuckDatabaseObject{}))
	appenderClass := vm.RegisterGoType("DuckAppender", reflect.TypeOf(&DuckAppenderObject{}))

	// -----------------------------------------------------------------------
	// DuckDatabase class methods
	// -----------------------------------------------------------------------

	// new — Open an in-memory DuckDB database
	duckDBClass.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.openDuckDB("")
	})

	// open: path — Open a file-backed DuckDB database
	duckDBClass.AddClassMethod1(vm.Selectors, "open:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("open: requires a path string")
		}
		return v.openDuckDB(path)
	})

	// -----------------------------------------------------------------------
	// DuckDatabase instance methods
	// -----------------------------------------------------------------------

	// query: sql — Execute a SELECT and return results as an array of dictionaries
	duckDBClass.AddMethod1(vm.Selectors, "query:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getDuckDB(recv)
		if dbObj == nil {
			return v.newFailureResult("query: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		if dbObj.closed {
			dbObj.mu.Unlock()
			return v.newFailureResult("query: database is closed")
		}
		db := dbObj.db
		dbObj.mu.Unlock()

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("query: requires a SQL string")
		}

		rows, err := db.Query(sqlStr)
		if err != nil {
			return v.newFailureResult("query: " + err.Error())
		}
		defer rows.Close()

		return v.rowsToArray(rows)
	})

	// execute: sql — Execute DDL/DML with no result set
	duckDBClass.AddMethod1(vm.Selectors, "execute:", func(vmPtr interface{}, recv Value, sqlVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getDuckDB(recv)
		if dbObj == nil {
			return v.newFailureResult("execute: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		if dbObj.closed {
			dbObj.mu.Unlock()
			return v.newFailureResult("execute: database is closed")
		}
		db := dbObj.db
		dbObj.mu.Unlock()

		sqlStr := v.valueToString(sqlVal)
		if sqlStr == "" {
			return v.newFailureResult("execute: requires a SQL string")
		}

		result, err := db.Exec(sqlStr)
		if err != nil {
			return v.newFailureResult("execute: " + err.Error())
		}

		affected, _ := result.RowsAffected()
		return v.newSuccessResult(FromSmallInt(affected))
	})

	// close — Close the database
	duckDBClass.AddMethod0(vm.Selectors, "close", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getDuckDB(recv)
		if dbObj == nil {
			return v.newFailureResult("close: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		defer dbObj.mu.Unlock()

		if dbObj.closed {
			return v.newSuccessResult(recv)
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
			return v.newFailureResult("close: " + firstErr.Error())
		}
		return v.newSuccessResult(recv)
	})

	// isClosed — Check if database is closed
	duckDBClass.AddMethod0(vm.Selectors, "isClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getDuckDB(recv)
		if dbObj == nil {
			return True
		}
		dbObj.mu.Lock()
		defer dbObj.mu.Unlock()
		if dbObj.closed {
			return True
		}
		return False
	})

	// appender: tableName — Create an appender for bulk inserts
	duckDBClass.AddMethod1(vm.Selectors, "appender:", func(vmPtr interface{}, recv Value, tableVal Value) Value {
		v := vmPtr.(*VM)
		dbObj := v.getDuckDB(recv)
		if dbObj == nil {
			return v.newFailureResult("appender: receiver is not a DuckDatabase")
		}

		dbObj.mu.Lock()
		if dbObj.closed {
			dbObj.mu.Unlock()
			return v.newFailureResult("appender: database is closed")
		}
		connector := dbObj.connector
		dbObj.mu.Unlock()

		tableName := v.valueToString(tableVal)
		if tableName == "" {
			return v.newFailureResult("appender: requires a table name string")
		}

		conn, err := connector.Connect(context.Background())
		if err != nil {
			return v.newFailureResult("appender: " + err.Error())
		}

		appender, err := duckdb.NewAppenderFromConn(conn, "", tableName)
		if err != nil {
			conn.Close()
			return v.newFailureResult("appender: " + err.Error())
		}

		appObj := &DuckAppenderObject{appender: appender}
		val, regErr := v.RegisterGoObject(appObj)
		if regErr != nil {
			appender.Close()
			return v.newFailureResult("appender: " + regErr.Error())
		}
		return val
	})

	// -----------------------------------------------------------------------
	// DuckAppender instance methods
	// -----------------------------------------------------------------------

	// appendRow: anArray — Append a row of values
	appenderClass.AddMethod1(vm.Selectors, "appendRow:", func(vmPtr interface{}, recv Value, rowVal Value) Value {
		v := vmPtr.(*VM)
		appObj := v.getDuckAppender(recv)
		if appObj == nil {
			return v.newFailureResult("appendRow: receiver is not a DuckAppender")
		}

		appObj.mu.Lock()
		defer appObj.mu.Unlock()

		if appObj.closed {
			return v.newFailureResult("appendRow: appender is closed")
		}

		// Convert Maggie array to Go values
		goArgs := v.arrayToDriverValues(rowVal)
		if goArgs == nil {
			return v.newFailureResult("appendRow: requires an Array")
		}

		if err := appObj.appender.AppendRow(goArgs...); err != nil {
			return v.newFailureResult("appendRow: " + err.Error())
		}

		return v.newSuccessResult(recv)
	})

	// flush — Flush buffered rows to the table
	appenderClass.AddMethod0(vm.Selectors, "flush", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		appObj := v.getDuckAppender(recv)
		if appObj == nil {
			return v.newFailureResult("flush: receiver is not a DuckAppender")
		}

		appObj.mu.Lock()
		defer appObj.mu.Unlock()

		if appObj.closed {
			return v.newFailureResult("flush: appender is closed")
		}

		if err := appObj.appender.Flush(); err != nil {
			return v.newFailureResult("flush: " + err.Error())
		}

		return v.newSuccessResult(recv)
	})

	// close — Close the appender (flushes remaining rows)
	appenderClass.AddMethod0(vm.Selectors, "close", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		appObj := v.getDuckAppender(recv)
		if appObj == nil {
			return v.newFailureResult("close: receiver is not a DuckAppender")
		}

		appObj.mu.Lock()
		defer appObj.mu.Unlock()

		if appObj.closed {
			return v.newSuccessResult(recv)
		}

		if err := appObj.appender.Close(); err != nil {
			appObj.closed = true
			return v.newFailureResult("close: " + err.Error())
		}

		appObj.closed = true
		return v.newSuccessResult(recv)
	})

	// isClosed — Check if appender is closed
	appenderClass.AddMethod0(vm.Selectors, "isClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		appObj := v.getDuckAppender(recv)
		if appObj == nil {
			return True
		}
		appObj.mu.Lock()
		defer appObj.mu.Unlock()
		if appObj.closed {
			return True
		}
		return False
	})
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// openDuckDB opens a DuckDB database (in-memory if dsn is empty).
func (vm *VM) openDuckDB(dsn string) Value {
	connector, err := duckdb.NewConnector(dsn, nil)
	if err != nil {
		return vm.newFailureResult("DuckDatabase open: " + err.Error())
	}

	db := sql.OpenDB(connector)

	obj := &DuckDatabaseObject{
		connector: connector,
		db:        db,
	}

	val, regErr := vm.RegisterGoObject(obj)
	if regErr != nil {
		db.Close()
		connector.Close()
		return vm.newFailureResult("DuckDatabase: " + regErr.Error())
	}
	return val
}

// getDuckDB extracts a DuckDatabaseObject from a Maggie Value.
func (vm *VM) getDuckDB(v Value) *DuckDatabaseObject {
	goVal, ok := vm.GetGoObject(v)
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
func (vm *VM) getDuckAppender(v Value) *DuckAppenderObject {
	goVal, ok := vm.GetGoObject(v)
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
func (vm *VM) rowsToArray(rows *sql.Rows) Value {
	cols, err := rows.Columns()
	if err != nil {
		return vm.newFailureResult("query: " + err.Error())
	}

	var results []Value

	for rows.Next() {
		// Create scan targets
		scanValues := make([]interface{}, len(cols))
		scanPtrs := make([]interface{}, len(cols))
		for i := range scanValues {
			scanPtrs[i] = &scanValues[i]
		}

		if err := rows.Scan(scanPtrs...); err != nil {
			return vm.newFailureResult("query: " + err.Error())
		}

		// Build a Dictionary for this row
		dict := vm.registry.NewDictionaryValue()
		dictObj := vm.registry.GetDictionaryObject(dict)
		if dictObj == nil {
			continue
		}

		for i, colName := range cols {
			key := vm.registry.NewStringValue(colName)
			val := vm.goScanValueToMaggie(scanValues[i])
			h := hashValue(vm.registry, key)
			dictObj.Keys[h] = key
			dictObj.Data[h] = val
		}

		results = append(results, dict)
	}

	if err := rows.Err(); err != nil {
		return vm.newFailureResult("query: " + err.Error())
	}

	return vm.NewArrayWithElements(results)
}

// goScanValueToMaggie converts a database scan value to a Maggie Value.
func (vm *VM) goScanValueToMaggie(v interface{}) Value {
	if v == nil {
		return Nil
	}
	switch val := v.(type) {
	case int64:
		return FromSmallInt(val)
	case int32:
		return FromSmallInt(int64(val))
	case int:
		return FromSmallInt(int64(val))
	case float64:
		return FromFloat64(val)
	case float32:
		return FromFloat64(float64(val))
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
		// Try reflect-based conversion for other types
		rv := reflect.ValueOf(v)
		switch rv.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			return FromSmallInt(rv.Int())
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			return FromSmallInt(int64(rv.Uint()))
		case reflect.Float32, reflect.Float64:
			return FromFloat64(rv.Float())
		case reflect.String:
			return vm.registry.NewStringValue(rv.String())
		default:
			// Use Stringer if available, otherwise nil
			if s, ok := v.(interface{ String() string }); ok {
				return vm.registry.NewStringValue(s.String())
			}
			return Nil
		}
	}
}

// valueToDuckDB converts a Maggie Value to a Go value suitable for DuckDB.
func (vm *VM) valueToDuckDB(v Value) interface{} {
	switch {
	case v == Nil:
		return nil
	case v == True:
		return true
	case v == False:
		return false
	case v.IsSmallInt():
		return v.SmallInt()
	case v.IsFloat():
		return v.Float64()
	case IsStringValue(v):
		return vm.registry.GetStringContent(v)
	default:
		return nil
	}
}

// arrayToDriverValues converts a Maggie Array value to []driver.Value for AppendRow.
func (vm *VM) arrayToDriverValues(v Value) []driver.Value {
	if !v.IsObject() {
		return nil
	}
	obj := ObjectFromValue(v)
	if obj == nil {
		return nil
	}
	n := obj.NumSlots()
	result := make([]driver.Value, n)
	for i := 0; i < n; i++ {
		result[i] = vm.valueToDuckDB(obj.GetSlot(i))
	}
	return result
}
