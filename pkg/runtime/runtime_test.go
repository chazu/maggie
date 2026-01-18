package runtime

import (
	"os"
	"path/filepath"
	"testing"
)

// testRuntime creates a runtime with a temp database for testing.
func testRuntime(t *testing.T) *Runtime {
	t.Helper()

	tmpDir := t.TempDir()
	dbPath := filepath.Join(tmpDir, "test.db")

	r, err := New(&Config{
		DBPath:        dbPath,
		TrashtalkRoot: tmpDir,
	})
	if err != nil {
		t.Fatalf("creating runtime: %v", err)
	}

	// Initialize the instances table
	_, err = r.db.Exec(`CREATE TABLE IF NOT EXISTS instances (
		id TEXT PRIMARY KEY,
		data JSON NOT NULL
	)`)
	if err != nil {
		t.Fatalf("creating table: %v", err)
	}

	t.Cleanup(func() {
		r.Close()
	})

	return r
}

func TestNew(t *testing.T) {
	tmpDir := t.TempDir()
	dbPath := filepath.Join(tmpDir, "test.db")

	r, err := New(&Config{DBPath: dbPath})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}
	defer r.Close()

	if r.db == nil {
		t.Error("New() db is nil")
	}
	if r.cache == nil {
		t.Error("New() cache is nil")
	}
}

func TestNewWithEnvVar(t *testing.T) {
	tmpDir := t.TempDir()
	dbPath := filepath.Join(tmpDir, "env_test.db")

	// Set environment variable
	oldVal := os.Getenv("SQLITE_JSON_DB")
	os.Setenv("SQLITE_JSON_DB", dbPath)
	defer os.Setenv("SQLITE_JSON_DB", oldVal)

	r, err := New(nil)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}
	defer r.Close()

	if r.dbPath != dbPath {
		t.Errorf("dbPath = %v, want %v", r.dbPath, dbPath)
	}
}

func TestCreateInstance(t *testing.T) {
	r := testRuntime(t)

	defaults := map[string]interface{}{
		"value": 0,
		"step":  1,
	}

	id, instance, err := r.CreateInstance("Counter", defaults)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Check ID format
	if id == "" {
		t.Error("CreateInstance() returned empty id")
	}
	if len(id) < 10 {
		t.Errorf("CreateInstance() id too short: %v", id)
	}

	// Check instance fields
	if instance.Class != "Counter" {
		t.Errorf("instance.Class = %v, want Counter", instance.Class)
	}
	if instance.CreatedAt == "" {
		t.Error("instance.CreatedAt is empty")
	}
	if instance.GetVarInt("value") != 0 {
		t.Errorf("instance.value = %v, want 0", instance.GetVarInt("value"))
	}
	if instance.GetVarInt("step") != 1 {
		t.Errorf("instance.step = %v, want 1", instance.GetVarInt("step"))
	}
}

func TestCreateInstanceNamespaced(t *testing.T) {
	r := testRuntime(t)

	id, _, err := r.CreateInstance("MyApp::Counter", nil)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Check ID format for namespaced class
	if len(id) < 13 || id[:13] != "myapp_counter" {
		t.Errorf("namespaced id prefix = %v, want myapp_counter...", id)
	}
}

func TestLoadInstance(t *testing.T) {
	r := testRuntime(t)

	// Create an instance
	defaults := map[string]interface{}{
		"value": 42,
	}
	id, _, err := r.CreateInstance("Counter", defaults)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Clear cache to force DB load
	r.ClearCache()

	// Load it back
	instance, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance() error = %v", err)
	}

	if instance.Class != "Counter" {
		t.Errorf("loaded instance.Class = %v, want Counter", instance.Class)
	}
	if instance.GetVarInt("value") != 42 {
		t.Errorf("loaded instance.value = %v, want 42", instance.GetVarInt("value"))
	}
}

func TestLoadInstanceNotFound(t *testing.T) {
	r := testRuntime(t)

	_, err := r.LoadInstance("nonexistent_123")
	if err != ErrInstanceNotFound {
		t.Errorf("LoadInstance() error = %v, want ErrInstanceNotFound", err)
	}
}

func TestLoadInstanceFromCache(t *testing.T) {
	r := testRuntime(t)

	// Create an instance
	id, _, err := r.CreateInstance("Counter", nil)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Should be in cache now
	if !r.IsCached(id) {
		t.Error("instance should be cached after create")
	}

	// Load again - should come from cache
	instance, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance() error = %v", err)
	}
	if instance == nil {
		t.Error("LoadInstance() returned nil")
	}
}

func TestSaveInstance(t *testing.T) {
	r := testRuntime(t)

	// Create an instance
	id, instance, err := r.CreateInstance("Counter", map[string]interface{}{"value": 0})
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Modify and save
	instance.SetVar("value", 100)
	if err := r.SaveInstance(id, instance); err != nil {
		t.Fatalf("SaveInstance() error = %v", err)
	}

	// Clear cache and reload
	r.ClearCache()
	loaded, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance() error = %v", err)
	}

	if loaded.GetVarInt("value") != 100 {
		t.Errorf("saved value = %v, want 100", loaded.GetVarInt("value"))
	}
}

func TestDeleteInstance(t *testing.T) {
	r := testRuntime(t)

	// Create an instance
	id, _, err := r.CreateInstance("Counter", nil)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Delete it
	if err := r.DeleteInstance(id); err != nil {
		t.Fatalf("DeleteInstance() error = %v", err)
	}

	// Should not be in cache
	if r.IsCached(id) {
		t.Error("instance should not be cached after delete")
	}

	// Should not be loadable
	_, err = r.LoadInstance(id)
	if err != ErrInstanceNotFound {
		t.Errorf("LoadInstance() error = %v, want ErrInstanceNotFound", err)
	}
}

func TestMarkDirty(t *testing.T) {
	r := testRuntime(t)

	// Create an instance
	id, _, err := r.CreateInstance("Counter", nil)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// After create, cache entry exists but is clean
	size, dirty := r.CacheStats()
	if size != 1 {
		t.Errorf("cache size = %v, want 1", size)
	}
	if dirty != 0 {
		t.Errorf("dirty count = %v, want 0", dirty)
	}

	// Mark dirty
	r.MarkDirty(id)
	_, dirty = r.CacheStats()
	if dirty != 1 {
		t.Errorf("dirty count after MarkDirty = %v, want 1", dirty)
	}
}

func TestFlushCache(t *testing.T) {
	r := testRuntime(t)

	// Create and modify an instance
	id, instance, err := r.CreateInstance("Counter", map[string]interface{}{"value": 0})
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Modify in cache and mark dirty
	instance.SetVar("value", 999)
	r.MarkDirty(id)

	// Flush
	if err := r.FlushCache(); err != nil {
		t.Fatalf("FlushCache() error = %v", err)
	}

	// Check dirty count is 0
	_, dirty := r.CacheStats()
	if dirty != 0 {
		t.Errorf("dirty count after flush = %v, want 0", dirty)
	}

	// Clear cache and reload to verify save
	r.ClearCache()
	loaded, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance() error = %v", err)
	}
	if loaded.GetVarInt("value") != 999 {
		t.Errorf("flushed value = %v, want 999", loaded.GetVarInt("value"))
	}
}

func TestFindByClass(t *testing.T) {
	r := testRuntime(t)

	// Create multiple instances of different classes
	_, _, _ = r.CreateInstance("Counter", nil)
	_, _, _ = r.CreateInstance("Counter", nil)
	_, _, _ = r.CreateInstance("Timer", nil)

	// Find Counters
	ids, err := r.FindByClass("Counter")
	if err != nil {
		t.Fatalf("FindByClass() error = %v", err)
	}
	if len(ids) != 2 {
		t.Errorf("FindByClass(Counter) returned %v ids, want 2", len(ids))
	}

	// Find Timers
	ids, err = r.FindByClass("Timer")
	if err != nil {
		t.Fatalf("FindByClass() error = %v", err)
	}
	if len(ids) != 1 {
		t.Errorf("FindByClass(Timer) returned %v ids, want 1", len(ids))
	}
}

func TestInstanceJSON(t *testing.T) {
	r := testRuntime(t)

	// Create instance with various types
	defaults := map[string]interface{}{
		"count":   42,
		"name":    "test",
		"enabled": true,
	}
	id, _, err := r.CreateInstance("Widget", defaults)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	// Clear cache and reload
	r.ClearCache()
	loaded, err := r.LoadInstance(id)
	if err != nil {
		t.Fatalf("LoadInstance() error = %v", err)
	}

	// Check values survived JSON round-trip
	if loaded.GetVarInt("count") != 42 {
		t.Errorf("count = %v, want 42", loaded.GetVarInt("count"))
	}
	if loaded.GetVarString("name") != "test" {
		t.Errorf("name = %v, want test", loaded.GetVarString("name"))
	}
}

func TestEvict(t *testing.T) {
	r := testRuntime(t)

	id, _, err := r.CreateInstance("Counter", nil)
	if err != nil {
		t.Fatalf("CreateInstance() error = %v", err)
	}

	if !r.IsCached(id) {
		t.Error("should be cached after create")
	}

	r.Evict(id)

	if r.IsCached(id) {
		t.Error("should not be cached after evict")
	}
}

func TestCacheStats(t *testing.T) {
	r := testRuntime(t)

	size, dirty := r.CacheStats()
	if size != 0 || dirty != 0 {
		t.Errorf("initial stats = (%v, %v), want (0, 0)", size, dirty)
	}

	// Create some instances
	id1, _, _ := r.CreateInstance("A", nil)
	id2, _, _ := r.CreateInstance("B", nil)

	size, dirty = r.CacheStats()
	if size != 2 || dirty != 0 {
		t.Errorf("after creates = (%v, %v), want (2, 0)", size, dirty)
	}

	// Mark one dirty
	r.MarkDirty(id1)
	size, dirty = r.CacheStats()
	if size != 2 || dirty != 1 {
		t.Errorf("after dirty = (%v, %v), want (2, 1)", size, dirty)
	}

	// Evict one
	r.Evict(id2)
	size, dirty = r.CacheStats()
	if size != 1 || dirty != 1 {
		t.Errorf("after evict = (%v, %v), want (1, 1)", size, dirty)
	}
}
