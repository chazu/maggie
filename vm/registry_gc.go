package vm

import (
	"os"
	"runtime/debug"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

// ---------------------------------------------------------------------------
// RegistryGC: Pressure-aware periodic garbage collection for VM registries
// ---------------------------------------------------------------------------
//
// Sweeps fire on whichever of these comes first:
//
//   1. Growth pressure: an AutoIDRegistry has grown by more than its
//      growthThreshold entries since the last sweep. Caught by a pressure
//      hook installed on each registry that does a single atomic compare
//      on every Register and a non-blocking channel send when it trips.
//
//   2. Absolute ceiling: an AutoIDRegistry's live size exceeds its
//      absoluteCeiling. Catches programs that allocate steadily but never
//      enough between sweeps to trip growth pressure.
//
//   3. Wall-clock floor: timeFloor elapses since the last sweep. Safety
//      net for idle programs (catches the "process held a few exceptions
//      for an hour" case) and for non-AutoIDRegistry registries
//      (channels/processes/cancellation contexts) that have no hook.
//
// Coalescing: the trigger channel has capacity 1 and we use non-blocking
// send. Many concurrent triggers collapse into at most one pending wake.
// While a sweep is running, additional triggers buffer one (next iteration
// will sweep again immediately) and any further ones drop. This prevents
// thundering-herd back-to-back sweeps under burst.

// triggerReason explains what caused the most recent sweep.
type triggerReason uint8

const (
	TriggerNone     triggerReason = 0
	TriggerTimer    triggerReason = 1 // wall-clock floor elapsed
	TriggerGrowth   triggerReason = 2 // delta-since-last-sweep crossed threshold
	TriggerCeiling  triggerReason = 3 // absolute live-size ceiling exceeded
	TriggerManual   triggerReason = 4 // SweepNow() called explicitly
	TriggerShutdown triggerReason = 5 // forced sweep on Stop (not used yet)
)

func (r triggerReason) String() string {
	switch r {
	case TriggerTimer:
		return "timer"
	case TriggerGrowth:
		return "growth"
	case TriggerCeiling:
		return "ceiling"
	case TriggerManual:
		return "manual"
	case TriggerShutdown:
		return "shutdown"
	default:
		return "none"
	}
}

// RegistryGCStats holds statistics from a single GC sweep.
type RegistryGCStats struct {
	Channels             int
	Processes            int
	CancellationContexts int
	Strings              int
	Dictionaries         int
	Blocks               int
	TotalSwept           int
	SweepDuration        time.Duration
	Timestamp            time.Time
	Reason               triggerReason
	// RegistryName is the name of the registry whose pressure tripped this
	// sweep (only set for TriggerGrowth/TriggerCeiling). Empty otherwise.
	RegistryName string
}

// monitoredRegistry holds the per-registry state RegistryGC tracks for
// pressure decisions. It's a thin wrapper around an AutoIDRegistry that
// remembers the size at the last sweep and the thresholds for growth
// and absolute ceiling.
type monitoredRegistry struct {
	name            string
	growthThreshold int32
	absoluteCeiling int32
	lastSweepSize   atomic.Int32 // live size immediately after the last sweep
	uninstall       func()       // clears the pressure hook on Stop
	currentSize     func() int32 // reads the registry's liveSize
	resetAfterSweep func()       // captures liveSize back into lastSweepSize
}

// DefaultGCInterval is the wall-clock floor between sweeps for idle
// processes. Pressure-driven sweeps may fire much sooner. Bumped from the
// historical 30s now that pressure-driven sweeps catch bursty allocation.
const DefaultGCInterval = 60 * time.Second

// Default thresholds — chosen as compromises that work for the steady-state
// of most workloads. Programs with extreme allocation patterns can tune
// per-registry via WithRegistryThresholds.
const (
	defaultGrowthThreshold int32 = 10_000  // delta since last sweep
	defaultAbsoluteCeiling int32 = 100_000 // hard cap regardless of growth
)

// RegistryGC periodically sweeps VM-local registries to reclaim
// entries for completed/closed objects. Sweeps are pressure-driven where
// possible, with a wall-clock floor as a safety net.
type RegistryGC struct {
	vm        *VM
	timeFloor time.Duration
	enabled   atomic.Bool
	stop      chan struct{}
	stopped   chan struct{}
	trigger   chan triggerReason // capacity 1; non-blocking send
	mu        sync.Mutex         // protects start/stop lifecycle

	// monitored registries (AutoIDRegistry-backed). Built in Start().
	monitored []*monitoredRegistry

	// Statistics
	sweepCount        atomic.Uint64
	growthSweeps      atomic.Uint64
	ceilingSweeps     atomic.Uint64
	timerSweeps       atomic.Uint64
	manualSweeps      atomic.Uint64
	lastStats         atomic.Value // *RegistryGCStats
	lastScavengeNanos atomic.Int64 // unix nanos of the last debug.FreeOSMemory()
}

// minScavengeInterval bounds how often a pressure/manual sweep may force a full
// Go GC + scavenge. Without it, a workload allocating fast enough to trip a
// growth sweep repeatedly would call debug.FreeOSMemory() on every sweep —
// many forced full GCs per second. Timer sweeps ignore this floor so idle
// processes still return RSS promptly.
const minScavengeInterval = 10 * time.Second

// NewRegistryGC creates a new RegistryGC for the given VM. interval sets
// the wall-clock floor (use DefaultGCInterval for the default).
func NewRegistryGC(vm *VM, interval time.Duration) *RegistryGC {
	if interval <= 0 {
		interval = DefaultGCInterval
	}
	gc := &RegistryGC{
		vm:        vm,
		timeFloor: interval,
		trigger:   make(chan triggerReason, 1),
	}
	gc.enabled.Store(true)
	return gc
}

// Start begins the sweep goroutine and installs pressure hooks on the VM's
// AutoIDRegistry-backed registries. Safe to call multiple times.
func (gc *RegistryGC) Start() {
	gc.mu.Lock()
	defer gc.mu.Unlock()

	if gc.stop != nil {
		return // already running
	}

	gc.installHooks()

	gc.stop = make(chan struct{})
	gc.stopped = make(chan struct{})

	stopCh := gc.stop
	stoppedCh := gc.stopped
	go gc.loop(stopCh, stoppedCh)
}

// installHooks wires a pressure callback onto each AutoIDRegistry we know
// about on the VM. The callback compares delta-since-last-sweep against
// the per-registry growth threshold (cheap path) and live-size against
// absolute ceiling (also cheap), and does a non-blocking send on the
// trigger channel if either condition trips.
func (gc *RegistryGC) installHooks() {
	if gc.vm == nil || gc.vm.registry == nil {
		return
	}
	or := gc.vm.registry
	gc.monitored = gc.monitored[:0]

	add := func(name string, growth, ceiling int32, reg interface {
		LiveSize() int32
		SetPressureHook(func(int32))
	}) {
		mr := &monitoredRegistry{
			name:            name,
			growthThreshold: growth,
			absoluteCeiling: ceiling,
			currentSize:     reg.LiveSize,
			uninstall:       func() { reg.SetPressureHook(nil) },
		}
		mr.resetAfterSweep = func() { mr.lastSweepSize.Store(reg.LiveSize()) }
		gc.monitored = append(gc.monitored, mr)

		// Capture mr in the hook. The decision logic is shared with the
		// block-pressure hook below; see onPressure.
		reg.SetPressureHook(func(liveSize int32) { gc.onPressure(mr, liveSize) })
	}

	// Dictionaries are among the most-allocated registry kinds still using ids.
	add("dictionaries", 20_000, 200_000, or.Dictionaries)

	// Contexts are the last remaining error-handling registry.
	add("contexts", defaultGrowthThreshold, defaultAbsoluteCeiling, or.Contexts)

	// The rest are domain-specific and usually have low cardinality.
	// Default thresholds are fine.
	add("goObjects", defaultGrowthThreshold, defaultAbsoluteCeiling, or.GoObjects)
	// classValues is monotonic and intentionally append-only — no point
	// installing pressure hooks since sweeping won't reclaim anything.

	// Blocks are not an AutoIDRegistry (custom slice-backed slots), so wire
	// pressure manually. Every non-inlined ifTrue:/whileTrue:/do: argument
	// allocates a block; in hot loops they accumulate quickly, so use a
	// modest growth threshold to keep the slot slices small and cache-hot.
	{
		mr := &monitoredRegistry{
			name:            "blocks",
			growthThreshold: 250_000,
			absoluteCeiling: 1_000_000,
			currentSize:     func() int32 { return int32(or.BlockCount()) },
			uninstall:       func() { or.SetBlockPressureHook(nil) },
		}
		mr.resetAfterSweep = func() { mr.lastSweepSize.Store(int32(or.BlockCount())) }
		gc.monitored = append(gc.monitored, mr)
		or.SetBlockPressureHook(func(liveSize int32) { gc.onPressure(mr, liveSize) })
	}
}

// onPressure is the per-registry sweep-trigger decision, shared by the
// AutoIDRegistry pressure hook and the block pressure hook. Branch order:
// cheapest test first.
//
// Two rules:
//
//  1. Growth: the live set has grown by more than growthThreshold since the
//     last sweep. Edge-triggered against the post-sweep baseline, so it can
//     never fire twice without growthThreshold fresh allocations in between.
//
//  2. Ceiling: the live set has crossed absoluteCeiling — but ONLY when the
//     previous sweep left us at or below the ceiling (last <= ceiling). Once a
//     sweep leaves the live set ABOVE the ceiling, the registry is saturated
//     with genuinely-live entries and re-sweeping reclaims nothing; firing on
//     every subsequent Register busy-loops a core. In that saturated state we
//     defer entirely to the growth rule (and the wall-clock timer floor), which
//     paces sweeps to real allocation rather than absolute level. This was the
//     pp-serve regression: under dashboard load the live string set sat
//     persistently above the 500k ceiling and the collector pegged a core
//     sweeping back-to-back to no effect.
func (gc *RegistryGC) onPressure(mr *monitoredRegistry, liveSize int32) {
	last := mr.lastSweepSize.Load()
	if liveSize-last > mr.growthThreshold {
		gc.signal(TriggerGrowth, mr.name)
		return
	}
	if liveSize > mr.absoluteCeiling && last <= mr.absoluteCeiling {
		gc.signal(TriggerCeiling, mr.name)
	}
}

// signal performs a non-blocking send on the trigger channel. If the
// channel is full, the wake is already pending and we drop — coalesces
// thundering-herd triggers under burst.
func (gc *RegistryGC) signal(reason triggerReason, _ string) {
	select {
	case gc.trigger <- reason:
	default:
		// already pending, drop
	}
}

// Stop halts the sweep goroutine, uninstalls pressure hooks, and waits
// for the goroutine to finish. Safe to call multiple times.
func (gc *RegistryGC) Stop() {
	gc.mu.Lock()
	stopCh := gc.stop
	stoppedCh := gc.stopped
	gc.stop = nil
	gc.stopped = nil
	monitored := gc.monitored
	gc.monitored = nil
	gc.mu.Unlock()

	if stopCh != nil {
		close(stopCh)
		<-stoppedCh
	}
	// Uninstall hooks AFTER the loop has exited so we don't race with a
	// hook firing into a closed/nil channel.
	for _, mr := range monitored {
		if mr.uninstall != nil {
			mr.uninstall()
		}
	}
}

// SetEnabled enables or disables sweeping. When disabled, the goroutine
// still runs but skips sweeps. Pressure triggers still fire but are no-ops.
func (gc *RegistryGC) SetEnabled(enabled bool) {
	gc.enabled.Store(enabled)
}

// IsEnabled returns whether sweeping is currently enabled.
func (gc *RegistryGC) IsEnabled() bool {
	return gc.enabled.Load()
}

// Interval returns the wall-clock floor between sweeps.
func (gc *RegistryGC) Interval() time.Duration {
	return gc.timeFloor
}

// SweepCount returns the total number of sweeps performed.
func (gc *RegistryGC) SweepCount() uint64 {
	return gc.sweepCount.Load()
}

// SweepCounts returns the count of sweeps broken down by trigger reason.
// Useful for diagnosing whether pressure triggers are firing as expected.
func (gc *RegistryGC) SweepCounts() (timer, growth, ceiling, manual uint64) {
	return gc.timerSweeps.Load(), gc.growthSweeps.Load(),
		gc.ceilingSweeps.Load(), gc.manualSweeps.Load()
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

// SweepNow performs an immediate sweep regardless of the timer or pressure.
func (gc *RegistryGC) SweepNow() *RegistryGCStats {
	return gc.sweep(TriggerManual, "")
}

// loop is the main GC goroutine. It selects on three sources: stop,
// wall-clock floor (time.After), and the trigger channel.
//
// Note: time.After is recreated each iteration so a pressure-driven sweep
// resets the floor — we don't want a sweep at t=5s followed by another at
// t=60s for the same idle reason.
func (gc *RegistryGC) loop(stopCh <-chan struct{}, stoppedCh chan struct{}) {
	defer close(stoppedCh)

	for {
		select {
		case <-stopCh:
			return
		case <-time.After(gc.timeFloor):
			if gc.enabled.Load() {
				gc.sweep(TriggerTimer, "")
			}
		case reason := <-gc.trigger:
			if gc.enabled.Load() {
				// We don't track which registry tripped the trigger
				// through the channel (capacity 1, dedup-by-overwrite
				// would be racy). Accept that under burst we lose the
				// per-registry attribution; the count of growth vs
				// ceiling triggers is still useful.
				gc.sweep(reason, "")
			}
		}
	}
}

// sweep performs one pass over all registries and removes stale entries.
// reason and registryName are recorded in the stats for diagnostics.
func (gc *RegistryGC) sweep(reason triggerReason, registryName string) *RegistryGCStats {
	start := time.Now()
	stats := &RegistryGCStats{
		Timestamp:    start,
		Reason:       reason,
		RegistryName: registryName,
	}

	if gc.vm.registry != nil {
		// 1. Concurrency registries (channels/processes/CC).
		// These are not AutoIDRegistry-backed and have no pressure hook;
		// they ride the timer floor only. That's fine — they're typically
		// low-cardinality and not the bursty-allocation problem case.
		stats.Channels = gc.vm.registry.SweepChannels()
		stats.Processes = gc.vm.registry.SweepProcesses()
		stats.CancellationContexts = gc.vm.registry.SweepCancellationContexts()
	}

	// 3. String/dictionary tracing collector (opt-in). This runs a
	// stop-the-world mark-sweep; it is invoked here, on the dedicated
	// RegistryGC goroutine, which is never a Maggie mutator. On barrier
	// timeout it is a no-op (ran == false).
	if gc.vm.gcEnabled.Load() {
		s, d, b, _ := gc.vm.CollectStringGarbage()
		stats.Strings = s
		stats.Dictionaries = d
		stats.Blocks = b
	}

	// Frame-bound blocks (HomeFrame >= 0) are swept by the same stop-the-world
	// tracing collector that reclaims strings/dictionaries (CollectStringGarbage
	// -> collectHeapGarbageLocked -> SweepBlocksLive): a block survives only if
	// the trace reaches its Value from a live root. Detached blocks (HomeFrame
	// == -1, used by [block] fork and friends) are treated as unconditional
	// roots there and release their slot in the fork goroutine's defer once the
	// goroutine exits — see RegisterBlock/ReleaseBlock and the fork primitives
	// in vm/concurrency.go.

	stats.TotalSwept = stats.Channels + stats.Processes +
		stats.CancellationContexts +
		stats.Strings + stats.Dictionaries + stats.Blocks
	stats.SweepDuration = time.Since(start)

	// Reset per-registry baselines so the next sweep's growth threshold is
	// measured from the post-sweep size. Without this, registries that
	// continue to hold N entries after sweep would re-trigger immediately.
	// Snapshot gc.monitored under the lock: Stop() nils it concurrently, and
	// this sweep runs on the background goroutine that races Stop().
	gc.mu.Lock()
	monitored := gc.monitored
	gc.mu.Unlock()
	for _, mr := range monitored {
		mr.resetAfterSweep()
	}

	gc.sweepCount.Add(1)
	switch reason {
	case TriggerTimer:
		gc.timerSweeps.Add(1)
	case TriggerGrowth:
		gc.growthSweeps.Add(1)
	case TriggerCeiling:
		gc.ceilingSweeps.Add(1)
	case TriggerManual:
		gc.manualSweeps.Add(1)
	}
	gc.lastStats.Store(stats)

	// Return reclaimed heap to the OS. The tracing collector above frees Maggie
	// registry objects, but the Go runtime holds their backing pages at the
	// process high-water mark and only returns them lazily — so a long-running
	// server ratchets RSS upward and never falls, even as the live set shrinks
	// (this is the Go<->Maggie GC interaction that made pp serve drift to GBs).
	// FreeOSMemory forces a scavenge here, tied to the already-throttled sweep
	// cadence (timer floor + growth thresholds) so the cost is bounded and every
	// Maggie project gets bounded RSS for free. Opt out with MAGGIE_SCAVENGE=0
	// for latency-critical workloads that prefer to keep pages mapped.
	if scavengeAfterSweep() && gc.shouldScavenge(reason, stats) {
		debug.FreeOSMemory()
		gc.lastScavengeNanos.Store(time.Now().UnixNano())
	}

	return stats
}

// shouldScavenge decides whether this sweep should force a full Go GC +
// scavenge. Timer sweeps always do (idle RSS return). Pressure/manual sweeps
// do so only when they actually reclaimed something and not more than once per
// minScavengeInterval, so a burst of growth-triggered sweeps cannot force a
// full GC on every one.
func (gc *RegistryGC) shouldScavenge(reason triggerReason, stats *RegistryGCStats) bool {
	if reason == TriggerTimer {
		return true
	}
	if stats.TotalSwept == 0 {
		return false
	}
	last := gc.lastScavengeNanos.Load()
	return time.Now().UnixNano()-last >= int64(minScavengeInterval)
}

// scavengeAfterSweep reports whether the GC sweep should return freed memory to
// the OS via debug.FreeOSMemory(). On by default; disable with MAGGIE_SCAVENGE
// set to 0/off/false/no.
func scavengeAfterSweep() bool {
	switch strings.ToLower(strings.TrimSpace(os.Getenv("MAGGIE_SCAVENGE"))) {
	case "0", "off", "false", "no":
		return false
	default:
		return true
	}
}
