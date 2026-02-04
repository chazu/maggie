package vm

import (
	"sync"
	"sync/atomic"
	"time"
)

// ---------------------------------------------------------------------------
// RegistryGC: Periodic garbage collection for concurrency registries
// ---------------------------------------------------------------------------

// RegistryGCStats holds statistics from a single GC sweep.
type RegistryGCStats struct {
	Channels             int
	Processes            int
	CancellationContexts int
	Exceptions           int
	GlobalChannels       int
	GlobalProcesses      int
	TotalSwept           int
	SweepDuration        time.Duration
	Timestamp            time.Time
}

// RegistryGC periodically sweeps global and VM-local registries to reclaim
// entries for completed/closed objects. This prevents memory leaks in
// long-running programs (servers, REPLs, IDE sessions).
type RegistryGC struct {
	vm       *VM
	interval time.Duration
	enabled  atomic.Bool
	stop     chan struct{}
	stopped  chan struct{}
	mu       sync.Mutex // protects start/stop lifecycle

	// Statistics
	sweepCount atomic.Uint64
	lastStats  atomic.Value // *RegistryGCStats
}

// DefaultGCInterval is the default sweep interval for registry GC.
const DefaultGCInterval = 30 * time.Second

// NewRegistryGC creates a new RegistryGC for the given VM with the specified
// sweep interval. Use DefaultGCInterval for the default (30s).
func NewRegistryGC(vm *VM, interval time.Duration) *RegistryGC {
	if interval <= 0 {
		interval = DefaultGCInterval
	}
	gc := &RegistryGC{
		vm:       vm,
		interval: interval,
	}
	gc.enabled.Store(true)
	return gc
}

// Start begins the periodic sweep goroutine. It is safe to call Start
// multiple times; only one sweep loop will run.
func (gc *RegistryGC) Start() {
	gc.mu.Lock()
	defer gc.mu.Unlock()

	if gc.stop != nil {
		return // already running
	}

	gc.stop = make(chan struct{})
	gc.stopped = make(chan struct{})

	// Capture channels locally so the goroutine does not read gc.stop/gc.stopped
	// after Stop() has nilled them out.
	stopCh := gc.stop
	stoppedCh := gc.stopped
	go gc.loop(stopCh, stoppedCh)
}

// Stop halts the periodic sweep goroutine and waits for it to finish.
// It is safe to call Stop multiple times or on a GC that was never started.
func (gc *RegistryGC) Stop() {
	gc.mu.Lock()
	stopCh := gc.stop
	stoppedCh := gc.stopped
	gc.stop = nil
	gc.stopped = nil
	gc.mu.Unlock()

	if stopCh != nil {
		close(stopCh)
		<-stoppedCh
	}
}

// SetEnabled enables or disables sweeping. When disabled, the goroutine
// still runs but skips sweep operations.
func (gc *RegistryGC) SetEnabled(enabled bool) {
	gc.enabled.Store(enabled)
}

// IsEnabled returns whether sweeping is currently enabled.
func (gc *RegistryGC) IsEnabled() bool {
	return gc.enabled.Load()
}

// Interval returns the current sweep interval.
func (gc *RegistryGC) Interval() time.Duration {
	return gc.interval
}

// SweepCount returns the total number of sweeps performed.
func (gc *RegistryGC) SweepCount() uint64 {
	return gc.sweepCount.Load()
}

// LastStats returns statistics from the most recent sweep, or nil if no
// sweep has been performed yet.
func (gc *RegistryGC) LastStats() *RegistryGCStats {
	v := gc.lastStats.Load()
	if v == nil {
		return nil
	}
	return v.(*RegistryGCStats)
}

// SweepNow performs an immediate sweep regardless of the timer.
// This is useful for testing and manual cleanup.
func (gc *RegistryGC) SweepNow() *RegistryGCStats {
	return gc.sweep()
}

// loop is the main GC goroutine that periodically invokes sweep.
// stopCh and stoppedCh are captured copies of gc.stop and gc.stopped
// to avoid reading struct fields that may be nilled by Stop().
func (gc *RegistryGC) loop(stopCh <-chan struct{}, stoppedCh chan struct{}) {
	defer close(stoppedCh)

	ticker := time.NewTicker(gc.interval)
	defer ticker.Stop()

	for {
		select {
		case <-stopCh:
			return
		case <-ticker.C:
			if gc.enabled.Load() {
				gc.sweep()
			}
		}
	}
}

// sweep performs one pass over all registries and removes stale entries.
func (gc *RegistryGC) sweep() *RegistryGCStats {
	start := time.Now()
	stats := &RegistryGCStats{
		Timestamp: start,
	}

	// 1. Sweep VM-local concurrency registries
	if gc.vm.registry != nil {
		stats.Channels = gc.vm.registry.SweepChannels()
		stats.Processes = gc.vm.registry.SweepProcesses()
		stats.CancellationContexts = gc.vm.registry.SweepCancellationContexts()
	}

	// 2. Sweep global channel registry (legacy)
	stats.GlobalChannels = sweepGlobalChannels()

	// 3. Sweep global process registry (legacy)
	stats.GlobalProcesses = sweepGlobalProcesses()

	// 4. Sweep VM-local exception registry
	if gc.vm.registry != nil {
		stats.Exceptions = gc.vm.registry.SweepExceptions()
	}

	// Note: We do NOT sweep the block registry here.
	// Blocks are cleaned up by ReleaseBlocksForFrame on the VM-local registry
	// when frames are popped. Detached blocks (HomeFrame == -1) are intentionally
	// long-lived. Sweeping blocks based on home frame validity requires interpreter
	// state that is not safely accessible from a background goroutine.

	stats.TotalSwept = stats.Channels + stats.Processes +
		stats.CancellationContexts + stats.Exceptions +
		stats.GlobalChannels + stats.GlobalProcesses
	stats.SweepDuration = time.Since(start)

	gc.sweepCount.Add(1)
	gc.lastStats.Store(stats)

	return stats
}

// ---------------------------------------------------------------------------
// Global registry sweep functions
// ---------------------------------------------------------------------------

// sweepGlobalChannels removes closed channels from the global channelRegistry.
func sweepGlobalChannels() int {
	channelRegistryMu.Lock()
	defer channelRegistryMu.Unlock()

	swept := 0
	for id, ch := range channelRegistry {
		if ch.closed.Load() {
			delete(channelRegistry, id)
			swept++
		}
	}
	return swept
}

// sweepGlobalProcesses removes terminated processes from the global processRegistry.
func sweepGlobalProcesses() int {
	processRegistryMu.Lock()
	defer processRegistryMu.Unlock()

	swept := 0
	for id, proc := range processRegistry {
		if proc.isDone() {
			delete(processRegistry, id)
			swept++
		}
	}
	return swept
}

