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
// RegistryGC: periodic sweep of the functional channel/process id maps
// ---------------------------------------------------------------------------
//
// After the pointer-value migration, heap objects (strings, dicts, collections,
// blocks, objects, contexts) are pointer-carrying Values reclaimed by Go's GC —
// there is no custom mark-sweep and no allocation-pressure signal to react to.
// What remains are two id→object maps kept for *functional* reasons, not
// liveness: `channels` and `processes` in the ConcurrencyRegistry. Terminated
// processes and closed channels linger in those maps until reaped, so a single
// background goroutine sweeps them on a wall-clock floor and then returns freed
// pages to the OS via debug.FreeOSMemory().
//
// (Historically RegistryGC also drove allocation-pressure sweeps of the
// AutoIDRegistry-backed heap registries; those registries and the whole
// growth/ceiling pressure apparatus were removed with the migration. This is now
// just a periodic reaper + OS scavenger.)

// triggerReason explains what caused the most recent sweep.
type triggerReason uint8

const (
	TriggerNone   triggerReason = 0
	TriggerTimer  triggerReason = 1 // wall-clock floor elapsed
	TriggerManual triggerReason = 2 // SweepNow() called explicitly
)

func (r triggerReason) String() string {
	switch r {
	case TriggerTimer:
		return "timer"
	case TriggerManual:
		return "manual"
	default:
		return "none"
	}
}

// RegistryGCStats holds statistics from a single GC sweep.
type RegistryGCStats struct {
	Channels      int
	Processes     int
	TotalSwept    int
	SweepDuration time.Duration
	Timestamp     time.Time
	Reason        triggerReason
}

// DefaultGCInterval is the wall-clock floor between sweeps.
const DefaultGCInterval = 60 * time.Second

// RegistryGC periodically sweeps the VM's channel/process id maps to reclaim
// entries for terminated/closed objects, then scavenges OS memory.
type RegistryGC struct {
	vm        *VM
	timeFloor time.Duration
	enabled   atomic.Bool
	stop      chan struct{}
	stopped   chan struct{}
	mu        sync.Mutex // protects start/stop lifecycle

	// Statistics
	sweepCount        atomic.Uint64
	timerSweeps       atomic.Uint64
	manualSweeps      atomic.Uint64
	lastStats         atomic.Value // *RegistryGCStats
	lastScavengeNanos atomic.Int64 // unix nanos of the last debug.FreeOSMemory()
}

// minScavengeInterval bounds how often a manual sweep may force a full Go GC +
// scavenge. Timer sweeps ignore this floor so idle processes still return RSS
// promptly.
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
	}
	gc.enabled.Store(true)
	return gc
}

// Start begins the sweep goroutine. Safe to call multiple times.
func (gc *RegistryGC) Start() {
	gc.mu.Lock()
	defer gc.mu.Unlock()

	if gc.stop != nil {
		return // already running
	}

	gc.stop = make(chan struct{})
	gc.stopped = make(chan struct{})

	stopCh := gc.stop
	stoppedCh := gc.stopped
	go gc.loop(stopCh, stoppedCh)
}

// Stop halts the sweep goroutine and waits for it to finish. Safe to call
// multiple times.
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
// still runs but skips sweeps.
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
func (gc *RegistryGC) SweepCounts() (timer, manual uint64) {
	return gc.timerSweeps.Load(), gc.manualSweeps.Load()
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
func (gc *RegistryGC) SweepNow() *RegistryGCStats {
	return gc.sweep(TriggerManual)
}

// loop is the main GC goroutine. It selects on stop and the wall-clock floor.
func (gc *RegistryGC) loop(stopCh <-chan struct{}, stoppedCh chan struct{}) {
	defer close(stoppedCh)

	for {
		select {
		case <-stopCh:
			return
		case <-time.After(gc.timeFloor):
			if gc.enabled.Load() {
				gc.sweep(TriggerTimer)
			}
		}
	}
}

// sweep reaps terminated processes and closed channels from the id maps and
// records stats. reason is recorded for diagnostics.
func (gc *RegistryGC) sweep(reason triggerReason) *RegistryGCStats {
	start := time.Now()
	stats := &RegistryGCStats{
		Timestamp: start,
		Reason:    reason,
	}

	if gc.vm.registry != nil {
		// Channel/process id maps are swept for terminated entries so they don't
		// grow unbounded. Strings/dicts/blocks/objects are pointer-carrying heap
		// Values reclaimed by Go's GC — there is no custom mark-sweep to run.
		stats.Channels = gc.vm.registry.SweepChannels()
		stats.Processes = gc.vm.registry.SweepProcesses()
	}

	stats.TotalSwept = stats.Channels + stats.Processes
	stats.SweepDuration = time.Since(start)

	gc.sweepCount.Add(1)
	switch reason {
	case TriggerTimer:
		gc.timerSweeps.Add(1)
	case TriggerManual:
		gc.manualSweeps.Add(1)
	}
	gc.lastStats.Store(stats)

	// Return reclaimed heap to the OS. Go's collector frees the swept map entries'
	// backing objects, but the runtime holds their pages at the process
	// high-water mark and returns them only lazily — so a long-running server
	// ratchets RSS upward and never falls (the Go<->Maggie GC interaction that
	// made pp serve drift to GBs). FreeOSMemory forces a scavenge here, tied to
	// the throttled sweep cadence so the cost is bounded. Opt out with
	// MAGGIE_SCAVENGE=0 for latency-critical workloads that prefer mapped pages.
	if scavengeAfterSweep() && gc.shouldScavenge(reason, stats) {
		debug.FreeOSMemory()
		gc.lastScavengeNanos.Store(time.Now().UnixNano())
	}

	return stats
}

// shouldScavenge decides whether this sweep should force a full Go GC +
// scavenge. Timer sweeps always do (idle RSS return). Manual sweeps do so only
// when they actually reclaimed something and not more than once per
// minScavengeInterval.
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
