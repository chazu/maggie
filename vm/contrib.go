package vm

import "sync"

// ContribPlugin defines the interface for optional VM extensions that can be
// registered at init time. This allows heavy dependencies (CUE, DuckDB, etc.)
// to live in separate packages and only be linked when imported.
type ContribPlugin interface {
	Name() string
	Register(vm *VM)
}

var (
	contribMu      sync.Mutex
	contribPlugins []ContribPlugin
)

// RegisterContrib registers a plugin to be initialized when NewVM() is called.
// Typically called from init() in a contrib package.
func RegisterContrib(p ContribPlugin) {
	contribMu.Lock()
	defer contribMu.Unlock()
	contribPlugins = append(contribPlugins, p)
}

func initContribPlugins(vm *VM) {
	contribMu.Lock()
	plugins := make([]ContribPlugin, len(contribPlugins))
	copy(plugins, contribPlugins)
	contribMu.Unlock()

	for _, p := range plugins {
		p.Register(vm)
	}
}
