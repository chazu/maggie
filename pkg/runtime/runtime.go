// Package runtime provides shared runtime functionality for compiled Trashtalk classes.
// It handles instance persistence via SQLite, caching, and message dispatch.
package runtime

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"github.com/google/uuid"
	_ "github.com/mattn/go-sqlite3"
)

// ErrUnknownSelector indicates the native binary doesn't implement this method.
// Exit code 200 signals the Bash dispatcher to fall back to interpreted execution.
var ErrUnknownSelector = errors.New("unknown selector")

// ErrInstanceNotFound indicates the requested instance doesn't exist in the database.
var ErrInstanceNotFound = errors.New("instance not found")

// Instance represents a generic Trashtalk instance as stored in the database.
// Instance variables are stored in the Data map.
type Instance struct {
	ID        string                 `json:"-"`          // Instance ID (not serialized, used as DB key)
	Class     string                 `json:"class"`      // Fully qualified class name
	CreatedAt string                 `json:"created_at"` // RFC3339 timestamp
	Vars      []string               `json:"_vars"`      // List of instance variable names
	Data      map[string]interface{} `json:"-"`          // All fields including instance vars
}

// MarshalJSON implements custom JSON marshaling that includes all Data fields.
func (i *Instance) MarshalJSON() ([]byte, error) {
	// Start with data map
	m := make(map[string]interface{})
	for k, v := range i.Data {
		m[k] = v
	}
	// Ensure standard fields are present
	m["class"] = i.Class
	m["created_at"] = i.CreatedAt
	if i.Vars != nil {
		m["_vars"] = i.Vars
	}
	return json.Marshal(m)
}

// UnmarshalJSON implements custom JSON unmarshaling that captures all fields.
func (i *Instance) UnmarshalJSON(data []byte) error {
	// Parse into generic map first
	m := make(map[string]interface{})
	if err := json.Unmarshal(data, &m); err != nil {
		return err
	}
	i.Data = m

	// Extract standard fields
	if class, ok := m["class"].(string); ok {
		i.Class = class
	}
	if createdAt, ok := m["created_at"].(string); ok {
		i.CreatedAt = createdAt
	}
	if vars, ok := m["_vars"].([]interface{}); ok {
		i.Vars = make([]string, len(vars))
		for idx, v := range vars {
			if s, ok := v.(string); ok {
				i.Vars[idx] = s
			}
		}
	}
	return nil
}

// GetVar retrieves an instance variable value.
func (i *Instance) GetVar(name string) interface{} {
	if i.Data == nil {
		return nil
	}
	return i.Data[name]
}

// SetVar sets an instance variable value.
func (i *Instance) SetVar(name string, value interface{}) {
	if i.Data == nil {
		i.Data = make(map[string]interface{})
	}
	i.Data[name] = value
}

// GetVarString retrieves an instance variable as a string.
func (i *Instance) GetVarString(name string) string {
	v := i.GetVar(name)
	if v == nil {
		return ""
	}
	return fmt.Sprintf("%v", v)
}

// GetVarInt retrieves an instance variable as an int.
func (i *Instance) GetVarInt(name string) int {
	v := i.GetVar(name)
	if v == nil {
		return 0
	}
	switch x := v.(type) {
	case int:
		return x
	case int64:
		return int(x)
	case float64:
		return int(x)
	case string:
		var n int
		fmt.Sscanf(x, "%d", &n)
		return n
	default:
		return 0
	}
}

// cacheEntry holds a cached instance and its metadata.
type cacheEntry struct {
	instance  *Instance
	dirty     bool      // Has been modified since last save
	loadedAt  time.Time // When loaded from DB
	accessedAt time.Time // Last access time
}

// Runtime manages instance lifecycle and persistence.
type Runtime struct {
	db          *sql.DB
	dbPath      string
	cache       map[string]*cacheEntry
	cacheMu     sync.RWMutex
	trashtalkRoot string // Path to ~/.trashtalk

	// Block management for bytecode execution
	blockRegistry   map[string]BlockInterface // blockID -> block implementation
	blockRegistryMu sync.RWMutex
	blockCounter    int // For generating unique block IDs
}

// Config holds runtime configuration options.
type Config struct {
	DBPath        string // Path to instances.db (defaults to ~/.trashtalk/instances.db)
	TrashtalkRoot string // Path to trashtalk installation (defaults to ~/.trashtalk)
}

// New creates a new Runtime with the given configuration.
// If cfg is nil, defaults are used.
func New(cfg *Config) (*Runtime, error) {
	r := &Runtime{
		cache:         make(map[string]*cacheEntry),
		blockRegistry: make(map[string]BlockInterface),
	}

	// Determine trashtalk root
	if cfg != nil && cfg.TrashtalkRoot != "" {
		r.trashtalkRoot = cfg.TrashtalkRoot
	} else {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("getting home dir: %w", err)
		}
		r.trashtalkRoot = filepath.Join(home, ".trashtalk")
	}

	// Determine database path
	if cfg != nil && cfg.DBPath != "" {
		r.dbPath = cfg.DBPath
	} else if dbPath := os.Getenv("SQLITE_JSON_DB"); dbPath != "" {
		r.dbPath = dbPath
	} else {
		r.dbPath = filepath.Join(r.trashtalkRoot, "instances.db")
	}

	// Open database
	db, err := sql.Open("sqlite3", r.dbPath)
	if err != nil {
		return nil, fmt.Errorf("opening database: %w", err)
	}
	r.db = db

	// Set busy timeout for concurrent access
	_, err = db.Exec("PRAGMA busy_timeout = 5000")
	if err != nil {
		db.Close()
		return nil, fmt.Errorf("setting busy timeout: %w", err)
	}

	return r, nil
}

// Close closes the runtime and its database connection.
func (r *Runtime) Close() error {
	// Flush any dirty cache entries
	r.cacheMu.Lock()
	defer r.cacheMu.Unlock()

	for id, entry := range r.cache {
		if entry.dirty {
			if err := r.saveInstanceLocked(id, entry.instance); err != nil {
				// Log but don't fail - we want to close anyway
				fmt.Fprintf(os.Stderr, "Warning: failed to save dirty instance %s: %v\n", id, err)
			}
		}
	}
	r.cache = nil

	if r.db != nil {
		return r.db.Close()
	}
	return nil
}

// LoadInstance loads an instance from cache or database.
func (r *Runtime) LoadInstance(id string) (*Instance, error) {
	// Check cache first
	r.cacheMu.RLock()
	if entry, ok := r.cache[id]; ok {
		entry.accessedAt = time.Now()
		r.cacheMu.RUnlock()
		return entry.instance, nil
	}
	r.cacheMu.RUnlock()

	// Load from database
	var data string
	err := r.db.QueryRow("SELECT data FROM instances WHERE id = ?", id).Scan(&data)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, ErrInstanceNotFound
		}
		return nil, fmt.Errorf("querying instance: %w", err)
	}

	var instance Instance
	if err := json.Unmarshal([]byte(data), &instance); err != nil {
		return nil, fmt.Errorf("unmarshaling instance: %w", err)
	}
	instance.ID = id

	// Cache it
	r.cacheMu.Lock()
	r.cache[id] = &cacheEntry{
		instance:   &instance,
		dirty:      false,
		loadedAt:   time.Now(),
		accessedAt: time.Now(),
	}
	r.cacheMu.Unlock()

	return &instance, nil
}

// SaveInstance saves an instance to the database and marks cache as clean.
func (r *Runtime) SaveInstance(id string, instance *Instance) error {
	r.cacheMu.Lock()
	defer r.cacheMu.Unlock()

	if err := r.saveInstanceLocked(id, instance); err != nil {
		return err
	}

	// Update cache
	if entry, ok := r.cache[id]; ok {
		entry.dirty = false
		entry.accessedAt = time.Now()
	} else {
		r.cache[id] = &cacheEntry{
			instance:   instance,
			dirty:      false,
			loadedAt:   time.Now(),
			accessedAt: time.Now(),
		}
	}

	return nil
}

// saveInstanceLocked saves to DB without acquiring locks (caller must hold lock).
func (r *Runtime) saveInstanceLocked(id string, instance *Instance) error {
	data, err := json.Marshal(instance)
	if err != nil {
		return fmt.Errorf("marshaling instance: %w", err)
	}

	_, err = r.db.Exec(
		"INSERT OR REPLACE INTO instances (id, data) VALUES (?, json(?))",
		id, string(data),
	)
	if err != nil {
		return fmt.Errorf("saving instance: %w", err)
	}

	return nil
}

// CreateInstance creates a new instance with the given class name and default values.
// Returns the new instance ID.
func (r *Runtime) CreateInstance(className string, defaults map[string]interface{}) (string, *Instance, error) {
	// Generate instance ID: lowercase class name + UUID
	// For namespaced classes like "MyApp::Counter", use "myapp_counter_uuid"
	idPrefix := strings.ToLower(strings.ReplaceAll(className, "::", "_"))
	id := idPrefix + "_" + uuid.New().String()

	instance := &Instance{
		ID:        id,
		Class:     className,
		CreatedAt: time.Now().Format(time.RFC3339),
		Data:      make(map[string]interface{}),
	}

	// Copy defaults
	if defaults != nil {
		for k, v := range defaults {
			instance.Data[k] = v
		}
	}

	// Ensure standard fields are in Data
	instance.Data["class"] = instance.Class
	instance.Data["created_at"] = instance.CreatedAt

	// Build _vars list from defaults
	var vars []string
	for k := range defaults {
		if k != "class" && k != "created_at" && k != "_vars" {
			vars = append(vars, k)
		}
	}
	instance.Vars = vars
	instance.Data["_vars"] = vars

	// Save to database
	if err := r.SaveInstance(id, instance); err != nil {
		return "", nil, err
	}

	return id, instance, nil
}

// DeleteInstance removes an instance from the database and cache.
func (r *Runtime) DeleteInstance(id string) error {
	r.cacheMu.Lock()
	delete(r.cache, id)
	r.cacheMu.Unlock()

	_, err := r.db.Exec("DELETE FROM instances WHERE id = ?", id)
	if err != nil {
		return fmt.Errorf("deleting instance: %w", err)
	}
	return nil
}

// MarkDirty marks a cached instance as modified (needs saving).
func (r *Runtime) MarkDirty(id string) {
	r.cacheMu.Lock()
	defer r.cacheMu.Unlock()
	if entry, ok := r.cache[id]; ok {
		entry.dirty = true
	}
}

// SendMessage sends a message to another Trashtalk object via the Bash runtime.
// This is used for cross-class calls when the target class isn't compiled to Go.
func (r *Runtime) SendMessage(receiver string, selector string, args ...string) (string, error) {
	dispatchScript := filepath.Join(r.trashtalkRoot, "bin", "trash-send")

	// Build command arguments
	cmdArgs := append([]string{receiver, selector}, args...)

	cmd := exec.Command(dispatchScript, cmdArgs...)
	output, err := cmd.Output()
	if err != nil {
		// Check for exit code 200 (unknown selector)
		if exitErr, ok := err.(*exec.ExitError); ok {
			if exitErr.ExitCode() == 200 {
				return "", ErrUnknownSelector
			}
		}
		return "", fmt.Errorf("sending message %s to %s: %w", selector, receiver, err)
	}

	return strings.TrimSpace(string(output)), nil
}

// FindByClass returns all instance IDs for a given class.
func (r *Runtime) FindByClass(className string) ([]string, error) {
	rows, err := r.db.Query("SELECT id FROM instances WHERE json_extract(data, '$.class') = ?", className)
	if err != nil {
		return nil, fmt.Errorf("querying instances by class: %w", err)
	}
	defer rows.Close()

	var ids []string
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("scanning instance id: %w", err)
		}
		ids = append(ids, id)
	}
	return ids, rows.Err()
}

// CacheStats returns statistics about the instance cache.
func (r *Runtime) CacheStats() (size int, dirty int) {
	r.cacheMu.RLock()
	defer r.cacheMu.RUnlock()

	size = len(r.cache)
	for _, entry := range r.cache {
		if entry.dirty {
			dirty++
		}
	}
	return
}

// FlushCache writes all dirty cache entries to the database.
func (r *Runtime) FlushCache() error {
	r.cacheMu.Lock()
	defer r.cacheMu.Unlock()

	for id, entry := range r.cache {
		if entry.dirty {
			if err := r.saveInstanceLocked(id, entry.instance); err != nil {
				return fmt.Errorf("flushing %s: %w", id, err)
			}
			entry.dirty = false
		}
	}
	return nil
}

// ClearCache removes all entries from the cache (does not save dirty entries).
func (r *Runtime) ClearCache() {
	r.cacheMu.Lock()
	defer r.cacheMu.Unlock()
	r.cache = make(map[string]*cacheEntry)
}

// Evict removes a specific instance from the cache (does not save if dirty).
func (r *Runtime) Evict(id string) {
	r.cacheMu.Lock()
	defer r.cacheMu.Unlock()
	delete(r.cache, id)
}

// IsCached returns whether an instance is currently in the cache.
func (r *Runtime) IsCached(id string) bool {
	r.cacheMu.RLock()
	defer r.cacheMu.RUnlock()
	_, ok := r.cache[id]
	return ok
}

// ============================================================================
// Block Registry - for bytecode block management
// ============================================================================

// RegisterBlock registers a block implementation for later invocation.
// Returns the assigned block ID.
func (r *Runtime) RegisterBlock(block BlockInterface) string {
	r.blockRegistryMu.Lock()
	defer r.blockRegistryMu.Unlock()

	r.blockCounter++
	id := fmt.Sprintf("block_%d", r.blockCounter)
	r.blockRegistry[id] = block
	return id
}

// RegisterBlockWithID registers a block with a specific ID.
// Useful for restoring serialized blocks or cross-process transfer.
func (r *Runtime) RegisterBlockWithID(id string, block BlockInterface) {
	r.blockRegistryMu.Lock()
	defer r.blockRegistryMu.Unlock()
	r.blockRegistry[id] = block
}

// GetBlock retrieves a registered block by ID.
// Returns nil if the block is not registered.
func (r *Runtime) GetBlock(id string) BlockInterface {
	r.blockRegistryMu.RLock()
	defer r.blockRegistryMu.RUnlock()
	return r.blockRegistry[id]
}

// UnregisterBlock removes a block from the registry.
func (r *Runtime) UnregisterBlock(id string) {
	r.blockRegistryMu.Lock()
	defer r.blockRegistryMu.Unlock()
	delete(r.blockRegistry, id)
}

// InvokeBlock invokes a registered block with the given arguments.
// Falls back to Bash execution for unregistered blocks.
func (r *Runtime) InvokeBlock(id string, args ...string) (string, error) {
	r.blockRegistryMu.RLock()
	block, ok := r.blockRegistry[id]
	r.blockRegistryMu.RUnlock()

	if !ok {
		// Fall back to Bash for unregistered blocks
		return r.InvokeBashBlock(id, args...)
	}

	// Create execution context
	ctx := &BlockContext{
		Runtime:  r,
		Captured: make(map[string]*CapturedVar),
	}

	return block.Invoke(ctx, args...)
}

// InvokeBlockWithContext invokes a block with an explicit context.
// This allows passing captured variables and instance information.
func (r *Runtime) InvokeBlockWithContext(id string, ctx *BlockContext, args ...string) (string, error) {
	r.blockRegistryMu.RLock()
	block, ok := r.blockRegistry[id]
	r.blockRegistryMu.RUnlock()

	if !ok {
		// Fall back to Bash for unregistered blocks
		return r.InvokeBashBlock(id, args...)
	}

	return block.Invoke(ctx, args...)
}

// BlockRegistryStats returns statistics about the block registry.
func (r *Runtime) BlockRegistryStats() (count int) {
	r.blockRegistryMu.RLock()
	defer r.blockRegistryMu.RUnlock()
	return len(r.blockRegistry)
}

// ClearBlockRegistry removes all blocks from the registry.
func (r *Runtime) ClearBlockRegistry() {
	r.blockRegistryMu.Lock()
	defer r.blockRegistryMu.Unlock()
	r.blockRegistry = make(map[string]BlockInterface)
	r.blockCounter = 0
}
