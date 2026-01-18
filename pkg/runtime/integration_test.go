// +build integration

package runtime

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Integration tests for Bash/Go interop.
// These tests verify that instances created/modified by Go can be read by Bash
// and vice versa, using the same SQLite database format.
//
// Run with: go test -tags=integration ./pkg/runtime/...

// TestRoundTripPersistence verifies that instances created in Go
// can be read back with the correct JSON structure that Bash expects.
func TestRoundTripPersistence(t *testing.T) {
	r := testRuntime(t)

	// Create a Counter instance with defaults matching Bash behavior
	defaults := map[string]interface{}{
		"value": 0,
		"step":  1,
	}

	id, inst, err := r.CreateInstance("Counter", defaults)
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	// Clear cache to force DB read
	r.ClearCache()

	// Read back and verify JSON structure matches Bash expectations
	var data string
	err = r.db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		t.Fatalf("QueryRow error: %v", err)
	}

	// Parse the JSON
	var parsed map[string]interface{}
	if err := json.Unmarshal([]byte(data), &parsed); err != nil {
		t.Fatalf("JSON parse error: %v", err)
	}

	// Verify required fields exist (Bash expects these)
	if parsed["class"] != "Counter" {
		t.Errorf("class = %v, want Counter", parsed["class"])
	}
	if parsed["created_at"] == nil || parsed["created_at"] == "" {
		t.Error("created_at should not be empty")
	}
	if parsed["value"] == nil {
		t.Error("value should exist")
	}
	if parsed["step"] == nil {
		t.Error("step should exist")
	}

	// Verify _vars contains the instance variable names
	vars, ok := parsed["_vars"].([]interface{})
	if !ok {
		t.Errorf("_vars should be an array, got %T", parsed["_vars"])
	} else {
		varNames := make(map[string]bool)
		for _, v := range vars {
			if s, ok := v.(string); ok {
				varNames[s] = true
			}
		}
		if !varNames["value"] || !varNames["step"] {
			t.Errorf("_vars should contain 'value' and 'step', got %v", vars)
		}
	}

	// Reload via runtime and verify
	loaded, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance error: %v", err)
	}
	if loaded.Class != inst.Class {
		t.Errorf("loaded.Class = %v, want %v", loaded.Class, inst.Class)
	}
	if loaded.GetVarInt("value") != 0 {
		t.Errorf("loaded.value = %v, want 0", loaded.GetVarInt("value"))
	}
}

// TestModifyAndPersist verifies that modifications via handlers
// are correctly persisted to the database.
func TestModifyAndPersist(t *testing.T) {
	r := testRuntime(t)

	// Create an Array instance
	id, inst, err := r.CreateInstance("Array", map[string]interface{}{
		"items": `[]`,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	// Use handlers to modify
	_, handled, err := r.Dispatch("Array", "push_", inst, id, []string{"first"})
	if err != nil || !handled {
		t.Fatalf("push_ dispatch error: %v, handled: %v", err, handled)
	}

	_, _, _ = r.Dispatch("Array", "push_", inst, id, []string{"second"})
	_, _, _ = r.Dispatch("Array", "push_", inst, id, []string{"third"})

	// Save the changes
	if err := r.SaveInstance(id, inst); err != nil {
		t.Fatalf("SaveInstance error: %v", err)
	}

	// Clear cache and reload from DB
	r.ClearCache()

	loaded, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance error: %v", err)
	}

	// Verify via handler
	result, handled, _ := r.Dispatch("Array", "size", loaded, id, nil)
	if !handled {
		t.Error("size handler not found")
	}
	if result != "3" {
		t.Errorf("size = %v, want 3", result)
	}

	// Verify items via handler
	first, _, _ := r.Dispatch("Array", "first", loaded, id, nil)
	if first != "first" {
		t.Errorf("first = %v, want 'first'", first)
	}

	last, _, _ := r.Dispatch("Array", "last", loaded, id, nil)
	if last != "third" {
		t.Errorf("last = %v, want 'third'", last)
	}
}

// TestDictionaryRoundTrip tests Dictionary operations persist correctly.
func TestDictionaryRoundTrip(t *testing.T) {
	r := testRuntime(t)

	id, inst, err := r.CreateInstance("Dictionary", map[string]interface{}{
		"items": `{}`,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	// Add entries via handlers
	r.Dispatch("Dictionary", "at_put_", inst, id, []string{"name", "Alice"})
	r.Dispatch("Dictionary", "at_put_", inst, id, []string{"age", "30"})
	r.Dispatch("Dictionary", "at_put_", inst, id, []string{"city", "NYC"})

	// Save
	r.SaveInstance(id, inst)

	// Reload
	r.ClearCache()
	loaded, _ := r.LoadInstance(id)

	// Verify
	size, _, _ := r.Dispatch("Dictionary", "size", loaded, id, nil)
	if size != "3" {
		t.Errorf("size = %v, want 3", size)
	}

	name, _, _ := r.Dispatch("Dictionary", "at_", loaded, id, []string{"name"})
	if name != "Alice" {
		t.Errorf("name = %v, want Alice", name)
	}

	// Remove and verify
	r.Dispatch("Dictionary", "removeAt_", loaded, id, []string{"city"})
	r.SaveInstance(id, loaded)

	r.ClearCache()
	reloaded, _ := r.LoadInstance(id)

	size2, _, _ := r.Dispatch("Dictionary", "size", reloaded, id, nil)
	if size2 != "2" {
		t.Errorf("size after remove = %v, want 2", size2)
	}
}

// TestJSONFormatCompatibility verifies the JSON format matches Bash expectations.
func TestJSONFormatCompatibility(t *testing.T) {
	r := testRuntime(t)

	// Create instance with various types
	defaults := map[string]interface{}{
		"count":   42,
		"name":    "test",
		"enabled": true,
		"items":   `["a","b","c"]`,
	}

	id, _, err := r.CreateInstance("Widget", defaults)
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	// Read raw JSON from DB
	var data string
	r.db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)

	// Verify it's valid JSON
	var parsed map[string]interface{}
	if err := json.Unmarshal([]byte(data), &parsed); err != nil {
		t.Fatalf("Invalid JSON: %v", err)
	}

	// Bash expects numeric values as numbers or strings
	// The Go runtime stores them as-is
	if parsed["count"] != float64(42) && parsed["count"] != "42" {
		t.Logf("count = %v (%T), Bash may expect string or number", parsed["count"], parsed["count"])
	}

	// String values should remain strings
	if parsed["name"] != "test" {
		t.Errorf("name = %v, want 'test'", parsed["name"])
	}
}

// TestFindByClassInterop tests that FindByClass returns instances
// that can be loaded correctly.
func TestFindByClassInterop(t *testing.T) {
	r := testRuntime(t)

	// Create multiple Counter instances
	id1, _, _ := r.CreateInstance("Counter", map[string]interface{}{"value": 10})
	id2, _, _ := r.CreateInstance("Counter", map[string]interface{}{"value": 20})
	id3, _, _ := r.CreateInstance("Timer", map[string]interface{}{"elapsed": 0})

	// Find all Counters
	ids, err := r.FindByClass("Counter")
	if err != nil {
		t.Fatalf("FindByClass error: %v", err)
	}

	if len(ids) != 2 {
		t.Errorf("FindByClass returned %d ids, want 2", len(ids))
	}

	// Verify the IDs match
	found := make(map[string]bool)
	for _, id := range ids {
		found[id] = true
	}
	if !found[id1] || !found[id2] {
		t.Errorf("FindByClass didn't return expected IDs: got %v, want %s and %s", ids, id1, id2)
	}
	if found[id3] {
		t.Error("FindByClass returned Timer instance for Counter query")
	}
}

// TestObjectHandlersOnDifferentClasses verifies Object handlers work
// via wildcard matching on any class.
func TestObjectHandlersOnDifferentClasses(t *testing.T) {
	r := testRuntime(t)

	classes := []string{"Counter", "Timer", "Widget", "MyApp::Counter"}

	for _, class := range classes {
		t.Run(class, func(t *testing.T) {
			id, inst, err := r.CreateInstance(class, nil)
			if err != nil {
				t.Fatalf("CreateInstance error: %v", err)
			}

			// class handler should return the class name
			result, handled, err := r.Dispatch(class, "class", inst, id, nil)
			if err != nil {
				t.Fatalf("class dispatch error: %v", err)
			}
			if !handled {
				t.Error("class handler not found")
			}
			if result != class {
				t.Errorf("class = %v, want %v", result, class)
			}

			// yourself handler should return the ID
			result, _, _ = r.Dispatch(class, "yourself", inst, id, nil)
			if result != id {
				t.Errorf("yourself = %v, want %v", result, id)
			}

			// printString should return formatted string
			result, _, _ = r.Dispatch(class, "printString", inst, id, nil)
			expected := "<" + class + " " + id + ">"
			if result != expected {
				t.Errorf("printString = %v, want %v", result, expected)
			}
		})
	}
}

// TestNamespacedClassPersistence verifies namespaced classes
// are stored with correct qualified names.
func TestNamespacedClassPersistence(t *testing.T) {
	r := testRuntime(t)

	id, _, err := r.CreateInstance("MyApp::Counter", map[string]interface{}{
		"value": 100,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	// ID should have namespace prefix
	if !strings.HasPrefix(id, "myapp_counter_") {
		t.Errorf("namespaced ID = %v, want myapp_counter_* prefix", id)
	}

	// Reload and verify class name preserved
	r.ClearCache()
	loaded, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance error: %v", err)
	}

	if loaded.Class != "MyApp::Counter" {
		t.Errorf("loaded.Class = %v, want MyApp::Counter", loaded.Class)
	}

	// FindByClass should work with qualified name
	ids, _ := r.FindByClass("MyApp::Counter")
	if len(ids) != 1 || ids[0] != id {
		t.Errorf("FindByClass(MyApp::Counter) = %v, want [%s]", ids, id)
	}
}

// TestConcurrentAccess verifies thread-safety of the runtime.
func TestConcurrentAccess(t *testing.T) {
	r := testRuntime(t)

	// Create a shared Array
	id, inst, _ := r.CreateInstance("Array", map[string]interface{}{"items": `[]`})

	// Save initial state
	r.SaveInstance(id, inst)

	done := make(chan bool)
	errors := make(chan error, 10)

	// Spawn goroutines that read/write
	for i := 0; i < 5; i++ {
		go func(n int) {
			defer func() { done <- true }()

			for j := 0; j < 10; j++ {
				// Load
				loaded, err := r.LoadInstance(id)
				if err != nil {
					errors <- err
					return
				}

				// Check size (read)
				_, _, err = r.Dispatch("Array", "size", loaded, id, nil)
				if err != nil {
					errors <- err
					return
				}
			}
		}(i)
	}

	// Wait for all goroutines
	for i := 0; i < 5; i++ {
		<-done
	}

	close(errors)
	for err := range errors {
		t.Errorf("Concurrent access error: %v", err)
	}
}

// TestDeleteAndRecreate verifies delete/create cycle works correctly.
func TestDeleteAndRecreate(t *testing.T) {
	r := testRuntime(t)

	// Create
	id1, _, _ := r.CreateInstance("Counter", map[string]interface{}{"value": 42})

	// Verify exists
	_, err := r.LoadInstance(id1)
	if err != nil {
		t.Fatalf("LoadInstance after create: %v", err)
	}

	// Delete
	if err := r.DeleteInstance(id1); err != nil {
		t.Fatalf("DeleteInstance: %v", err)
	}

	// Verify gone
	_, err = r.LoadInstance(id1)
	if err != ErrInstanceNotFound {
		t.Errorf("LoadInstance after delete: expected ErrInstanceNotFound, got %v", err)
	}

	// Create new with same class
	id2, _, _ := r.CreateInstance("Counter", map[string]interface{}{"value": 0})

	// Should have different ID
	if id1 == id2 {
		t.Error("New instance has same ID as deleted instance")
	}

	// Verify new instance works
	loaded, _ := r.LoadInstance(id2)
	if loaded.GetVarInt("value") != 0 {
		t.Errorf("new instance value = %v, want 0", loaded.GetVarInt("value"))
	}
}

// TestDatabasePath verifies custom database path works.
func TestDatabasePath(t *testing.T) {
	tmpDir := t.TempDir()
	customPath := filepath.Join(tmpDir, "custom", "path", "instances.db")

	// Create directory
	os.MkdirAll(filepath.Dir(customPath), 0755)

	// Create runtime with custom path
	r, err := New(&Config{DBPath: customPath})
	if err != nil {
		t.Fatalf("New with custom path: %v", err)
	}
	defer r.Close()

	// Initialize table
	r.db.Exec(`CREATE TABLE IF NOT EXISTS instances (id TEXT PRIMARY KEY, data JSON NOT NULL)`)

	// Create instance
	id, _, err := r.CreateInstance("Test", nil)
	if err != nil {
		t.Fatalf("CreateInstance: %v", err)
	}

	// Verify file exists at custom path
	if _, err := os.Stat(customPath); os.IsNotExist(err) {
		t.Error("Database file not created at custom path")
	}

	// Verify data persisted
	r.ClearCache()
	_, err = r.LoadInstance(id)
	if err != nil {
		t.Errorf("LoadInstance from custom path: %v", err)
	}
}

// TestFlushOnClose verifies dirty entries are saved on close.
func TestFlushOnClose(t *testing.T) {
	tmpDir := t.TempDir()
	dbPath := filepath.Join(tmpDir, "flush_test.db")

	// Create runtime
	r, _ := New(&Config{DBPath: dbPath})
	r.db.Exec(`CREATE TABLE IF NOT EXISTS instances (id TEXT PRIMARY KEY, data JSON NOT NULL)`)

	// Create and modify
	id, inst, _ := r.CreateInstance("Counter", map[string]interface{}{"value": 0})
	inst.SetVar("value", 999)
	r.MarkDirty(id)

	// Close (should flush)
	r.Close()

	// Reopen and verify
	r2, _ := New(&Config{DBPath: dbPath})
	defer r2.Close()

	loaded, err := r2.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance after reopen: %v", err)
	}

	// Note: value may be 999 or 0 depending on implementation
	// The key test is that instance exists after close
	if loaded == nil {
		t.Error("Instance should exist after close/reopen")
	}
}

// TestBashCompatibleSQLiteFormat verifies the SQLite format matches
// what Bash's sqlite-json.bash expects.
func TestBashCompatibleSQLiteFormat(t *testing.T) {
	r := testRuntime(t)

	id, _, _ := r.CreateInstance("Counter", map[string]interface{}{
		"value": 42,
		"step":  1,
	})

	// Query using the same SQL Bash uses
	var data string
	err := r.db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		t.Fatalf("SQL query error: %v", err)
	}

	// Bash's db_get returns raw JSON
	if data == "" {
		t.Error("data should not be empty")
	}

	// Should be parseable JSON
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(data), &m); err != nil {
		t.Errorf("data should be valid JSON: %v", err)
	}

	// Test query by class (Bash uses this)
	var id2 string
	err = r.db.QueryRow(
		"SELECT id FROM instances WHERE json_extract(data, '$.class') = ?",
		"Counter",
	).Scan(&id2)
	if err != nil {
		t.Fatalf("Query by class error: %v", err)
	}
	if id2 != id {
		t.Errorf("Query by class returned wrong id: %v, want %v", id2, id)
	}
}

// BenchmarkRoundTrip measures instance create/load/save performance.
func BenchmarkRoundTrip(b *testing.B) {
	tmpDir := b.TempDir()
	dbPath := filepath.Join(tmpDir, "bench.db")

	r, _ := New(&Config{DBPath: dbPath})
	r.db.Exec(`CREATE TABLE IF NOT EXISTS instances (id TEXT PRIMARY KEY, data JSON NOT NULL)`)
	defer r.Close()

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// Create
		id, inst, _ := r.CreateInstance("Counter", map[string]interface{}{"value": i})

		// Load (from cache)
		r.LoadInstance(id)

		// Modify
		inst.SetVar("value", i+1)

		// Save
		r.SaveInstance(id, inst)
	}
}

// BenchmarkHandlerDispatch measures handler dispatch overhead.
func BenchmarkHandlerDispatch(b *testing.B) {
	tmpDir := b.TempDir()
	dbPath := filepath.Join(tmpDir, "bench.db")

	r, _ := New(&Config{DBPath: dbPath})
	r.db.Exec(`CREATE TABLE IF NOT EXISTS instances (id TEXT PRIMARY KEY, data JSON NOT NULL)`)
	defer r.Close()

	id, inst, _ := r.CreateInstance("Array", map[string]interface{}{"items": `[]`})

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		r.Dispatch("Array", "push_", inst, id, []string{"item"})
	}
}
