package runtime

import (
	"os"
	"path/filepath"
	"sync"

	"github.com/chazu/procyon/pkg/bytecode"
)

// Runtime is the main entry point for the shared runtime.
// It coordinates object space, dispatch, blocks, and persistence.
type Runtime struct {
	OS          *ObjectSpace
	Dispatcher  *Dispatcher
	BlockRunner *BlockRunner
	Persistence *Persistence
	BashBridge  *BashBridge

	trashDir    string
	initialized bool
	mu          sync.Mutex
}

// Config holds runtime configuration
type Config struct {
	TrashDir   string // Path to ~/.trashtalk (defaults from env/home)
	DBPath     string // Path to instances.db (defaults from TrashDir)
	Debug      bool   // Enable debug output
	NoBash     bool   // Disable bash fallback
	NoPersist  bool   // Disable persistence
}

// DefaultConfig returns a configuration with default values
func DefaultConfig() *Config {
	trashDir := os.Getenv("TRASHTALK_ROOT")
	if trashDir == "" {
		home, _ := os.UserHomeDir()
		trashDir = filepath.Join(home, ".trashtalk")
	}

	dbPath := os.Getenv("SQLITE_JSON_DB")
	if dbPath == "" {
		dbPath = filepath.Join(trashDir, "instances.db")
	}

	return &Config{
		TrashDir: trashDir,
		DBPath:   dbPath,
		Debug:    os.Getenv("TRASHTALK_DEBUG") != "",
	}
}

// New creates a new runtime with the given configuration
func New(cfg *Config) (*Runtime, error) {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	r := &Runtime{
		trashDir: cfg.TrashDir,
	}

	// Create object space
	r.OS = NewObjectSpace()

	// Create dispatcher
	r.Dispatcher = NewDispatcher(r.OS)

	// Create block runner
	r.BlockRunner = NewBlockRunner(r.OS)
	r.BlockRunner.SetDispatcher(r.Dispatcher)
	r.Dispatcher.SetBlockRunner(r.BlockRunner)

	// Create bash bridge
	if !cfg.NoBash {
		r.BashBridge = NewBashBridge(cfg.TrashDir)
		r.BashBridge.SetDebug(cfg.Debug)
		r.Dispatcher.SetBashBridge(r.BashBridge)
	}

	// Create persistence
	if !cfg.NoPersist {
		var err error
		r.Persistence, err = NewPersistence(cfg.DBPath, r.OS)
		if err != nil {
			return nil, err
		}
		// Connect persistence to block runner for block reference resolution
		r.Persistence.SetBlockRunner(r.BlockRunner)
	}

	// Register base Object class
	RegisterObjectClass(r)

	r.initialized = true
	return r, nil
}

// Close shuts down the runtime
func (r *Runtime) Close() error {
	r.mu.Lock()
	defer r.mu.Unlock()

	if r.Persistence != nil {
		// Save all dirty instances
		r.Persistence.SaveAll()
		r.Persistence.Close()
	}

	if r.BashBridge != nil {
		r.BashBridge.Close()
	}

	r.BlockRunner.ClearRegistry()
	r.initialized = false
	return nil
}

// RegisterClass registers a native class with the runtime
func (r *Runtime) RegisterClass(name, superclass string, instanceVars []string, methods *MethodTable) *Class {
	return r.OS.RegisterClass(name, superclass, instanceVars, methods)
}

// Send dispatches a message
func (r *Runtime) Send(receiver, selector string, args []Value) Value {
	return r.Dispatcher.Send(receiver, selector, args)
}

// SendDirect dispatches a message with an instance pointer
func (r *Runtime) SendDirect(inst *Instance, selector string, args []Value) Value {
	return r.Dispatcher.SendDirect(inst, selector, args)
}

// NewInstance creates a new instance of a class
func (r *Runtime) NewInstance(className string) (*Instance, error) {
	return r.OS.NewInstance(className)
}

// GetInstance retrieves an instance by ID
func (r *Runtime) GetInstance(id string) *Instance {
	// Try object space first
	if inst := r.OS.GetInstance(id); inst != nil {
		return inst
	}

	// Try loading from persistence
	if r.Persistence != nil {
		inst, _ := r.Persistence.Load(id)
		return inst
	}

	return nil
}

// RegisterBlock registers a bytecode block
func (r *Runtime) RegisterBlock(chunk interface{}, captures []*CaptureCell, instanceID, className string) string {
	// Type assert to *bytecode.Chunk
	bc, ok := chunk.(*bytecode.Chunk)
	if !ok {
		return ""
	}
	return r.BlockRunner.RegisterBlock(bc, captures, instanceID, className)
}

// InvokeBlock invokes a block by ID
func (r *Runtime) InvokeBlock(blockID string, args []Value) Value {
	return r.BlockRunner.Invoke(blockID, args)
}

// InvokeBlockDirect invokes a block with a pointer
func (r *Runtime) InvokeBlockDirect(block *Block, args []Value) Value {
	return r.BlockRunner.InvokeDirect(block, args)
}

// LookupBlock finds a block by ID
func (r *Runtime) LookupBlock(blockID string) *Block {
	return r.BlockRunner.GetBlock(blockID)
}

// Persist saves an instance to the database
func (r *Runtime) Persist(id string) error {
	if r.Persistence == nil {
		return nil
	}
	inst := r.OS.GetInstance(id)
	if inst == nil {
		return ErrInstanceNotFound
	}
	return r.Persistence.Save(inst)
}

// Load retrieves an instance from the database
func (r *Runtime) Load(id string) (*Instance, error) {
	if r.Persistence == nil {
		return nil, ErrInstanceNotFound
	}
	return r.Persistence.Load(id)
}

// DeserializeInstance creates or updates an instance from JSON
func (r *Runtime) DeserializeInstance(jsonStr string) (*Instance, error) {
	if r.Persistence != nil {
		return r.Persistence.instanceFromJSON("", jsonStr)
	}
	// Fall back to creating a new instance from JSON without persistence
	return instanceFromJSONNoID(jsonStr, r.OS)
}

// Stats returns runtime statistics
func (r *Runtime) Stats() RuntimeStats {
	return RuntimeStats{
		Classes:   r.OS.ClassCount(),
		Instances: r.OS.InstanceCount(),
		Blocks:    r.BlockRunner.BlockStats(),
	}
}

// RuntimeStats contains runtime statistics
type RuntimeStats struct {
	Classes   int
	Instances int
	Blocks    int
}

// ============================================================================
// Global runtime instance (for C ABI)
// ============================================================================

var (
	globalRuntime *Runtime
	globalMu      sync.Mutex
)

// GlobalRuntime returns the global runtime instance
func GlobalRuntime() *Runtime {
	return globalRuntime
}

// InitGlobal initializes the global runtime
func InitGlobal(cfg *Config) error {
	globalMu.Lock()
	defer globalMu.Unlock()

	if globalRuntime != nil {
		return nil // Already initialized
	}

	r, err := New(cfg)
	if err != nil {
		return err
	}

	globalRuntime = r
	return nil
}

// CloseGlobal shuts down the global runtime
func CloseGlobal() error {
	globalMu.Lock()
	defer globalMu.Unlock()

	if globalRuntime != nil {
		err := globalRuntime.Close()
		globalRuntime = nil
		return err
	}
	return nil
}
