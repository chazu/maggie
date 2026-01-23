package vm

import (
	"sync"
	"sync/atomic"
)

// Profiler tracks method and block invocation counts to identify hot code
// for JIT compilation. Based on Cog VM's approach where:
// - Methods are compiled after a low threshold (Cog uses 2, we use 100)
// - Blocks are critical for loops and callbacks
// - Profile at method/block level, not call-site level

// MethodProfile holds profiling data for a single method.
type MethodProfile struct {
	InvocationCount uint64 // Atomic counter for invocations
	IsHot           bool   // True if threshold exceeded
}

// BlockProfile holds profiling data for a single block.
type BlockProfile struct {
	InvocationCount uint64          // Atomic counter for invocations
	IsHot           bool            // True if threshold exceeded
	OwningMethod    *CompiledMethod // The method containing this block
	BlockIndex      int             // Index within the method's Blocks slice
}

// Profiler manages profiling for all methods and blocks in the VM.
type Profiler struct {
	// Profile storage (thread-safe)
	methodProfiles sync.Map // *CompiledMethod -> *MethodProfile
	blockProfiles  sync.Map // *BlockMethod -> *BlockProfile

	// Configuration thresholds
	// Cog uses 2 for methods, but we have more compilation overhead
	MethodHotThreshold uint64 // Default: 100
	BlockHotThreshold  uint64 // Default: 500 (blocks often in tight loops)

	// Callback when code becomes hot
	// Called with either *CompiledMethod or *BlockMethod
	OnHot func(code interface{}, profile interface{})

	// Statistics
	hotMethodCount uint64
	hotBlockCount  uint64
}

// NewProfiler creates a new profiler with default thresholds.
func NewProfiler() *Profiler {
	return &Profiler{
		MethodHotThreshold: 100,
		BlockHotThreshold:  500,
	}
}

// RecordMethodInvocation increments the invocation count for a method.
// Returns true if this invocation caused the method to become hot.
func (p *Profiler) RecordMethodInvocation(method *CompiledMethod) bool {
	if method == nil {
		return false
	}

	// Get or create profile
	val, _ := p.methodProfiles.LoadOrStore(method, &MethodProfile{})
	profile := val.(*MethodProfile)

	// Atomic increment
	count := atomic.AddUint64(&profile.InvocationCount, 1)

	// Check if just became hot
	if !profile.IsHot && count >= p.MethodHotThreshold {
		profile.IsHot = true
		atomic.AddUint64(&p.hotMethodCount, 1)

		if p.OnHot != nil {
			p.OnHot(method, profile)
		}
		return true
	}

	return false
}

// RecordBlockInvocation increments the invocation count for a block.
// Returns true if this invocation caused the block to become hot.
func (p *Profiler) RecordBlockInvocation(block *BlockMethod, owner *CompiledMethod, blockIndex int) bool {
	if block == nil {
		return false
	}

	// Get or create profile
	val, loaded := p.blockProfiles.LoadOrStore(block, &BlockProfile{
		OwningMethod: owner,
		BlockIndex:   blockIndex,
	})
	profile := val.(*BlockProfile)

	// Update owner info if this is an existing profile without it
	if loaded && profile.OwningMethod == nil {
		profile.OwningMethod = owner
		profile.BlockIndex = blockIndex
	}

	// Atomic increment
	count := atomic.AddUint64(&profile.InvocationCount, 1)

	// Check if just became hot
	if !profile.IsHot && count >= p.BlockHotThreshold {
		profile.IsHot = true
		atomic.AddUint64(&p.hotBlockCount, 1)

		if p.OnHot != nil {
			p.OnHot(block, profile)
		}
		return true
	}

	return false
}

// GetMethodProfile returns the profile for a method, or nil if not tracked.
func (p *Profiler) GetMethodProfile(method *CompiledMethod) *MethodProfile {
	if val, ok := p.methodProfiles.Load(method); ok {
		return val.(*MethodProfile)
	}
	return nil
}

// GetBlockProfile returns the profile for a block, or nil if not tracked.
func (p *Profiler) GetBlockProfile(block *BlockMethod) *BlockProfile {
	if val, ok := p.blockProfiles.Load(block); ok {
		return val.(*BlockProfile)
	}
	return nil
}

// IsMethodHot returns true if the method has exceeded the hot threshold.
func (p *Profiler) IsMethodHot(method *CompiledMethod) bool {
	profile := p.GetMethodProfile(method)
	return profile != nil && profile.IsHot
}

// IsBlockHot returns true if the block has exceeded the hot threshold.
func (p *Profiler) IsBlockHot(block *BlockMethod) bool {
	profile := p.GetBlockProfile(block)
	return profile != nil && profile.IsHot
}

// ProfilerStats holds aggregate profiling statistics.
type ProfilerStats struct {
	TotalMethods      int    // Number of methods profiled
	TotalBlocks       int    // Number of blocks profiled
	HotMethods        int    // Number of hot methods
	HotBlocks         int    // Number of hot blocks
	TotalInvocations  uint64 // Total method + block invocations
	MethodInvocations uint64 // Total method invocations
	BlockInvocations  uint64 // Total block invocations
}

// Stats returns aggregate profiling statistics.
func (p *Profiler) Stats() ProfilerStats {
	var stats ProfilerStats

	p.methodProfiles.Range(func(key, value interface{}) bool {
		profile := value.(*MethodProfile)
		stats.TotalMethods++
		stats.MethodInvocations += atomic.LoadUint64(&profile.InvocationCount)
		if profile.IsHot {
			stats.HotMethods++
		}
		return true
	})

	p.blockProfiles.Range(func(key, value interface{}) bool {
		profile := value.(*BlockProfile)
		stats.TotalBlocks++
		stats.BlockInvocations += atomic.LoadUint64(&profile.InvocationCount)
		if profile.IsHot {
			stats.HotBlocks++
		}
		return true
	})

	stats.TotalInvocations = stats.MethodInvocations + stats.BlockInvocations
	return stats
}

// HotMethods returns all methods that have exceeded the hot threshold.
func (p *Profiler) HotMethods() []*CompiledMethod {
	var hot []*CompiledMethod
	p.methodProfiles.Range(func(key, value interface{}) bool {
		method := key.(*CompiledMethod)
		profile := value.(*MethodProfile)
		if profile.IsHot {
			hot = append(hot, method)
		}
		return true
	})
	return hot
}

// HotBlocks returns all blocks that have exceeded the hot threshold.
func (p *Profiler) HotBlocks() []*BlockMethod {
	var hot []*BlockMethod
	p.blockProfiles.Range(func(key, value interface{}) bool {
		block := key.(*BlockMethod)
		profile := value.(*BlockProfile)
		if profile.IsHot {
			hot = append(hot, block)
		}
		return true
	})
	return hot
}

// TopMethods returns the N most frequently invoked methods.
func (p *Profiler) TopMethods(n int) []*CompiledMethod {
	type methodCount struct {
		method *CompiledMethod
		count  uint64
	}

	var all []methodCount
	p.methodProfiles.Range(func(key, value interface{}) bool {
		method := key.(*CompiledMethod)
		profile := value.(*MethodProfile)
		all = append(all, methodCount{method, atomic.LoadUint64(&profile.InvocationCount)})
		return true
	})

	// Simple selection sort for top N (fine for small N)
	for i := 0; i < n && i < len(all); i++ {
		maxIdx := i
		for j := i + 1; j < len(all); j++ {
			if all[j].count > all[maxIdx].count {
				maxIdx = j
			}
		}
		all[i], all[maxIdx] = all[maxIdx], all[i]
	}

	result := make([]*CompiledMethod, 0, n)
	for i := 0; i < n && i < len(all); i++ {
		result = append(result, all[i].method)
	}
	return result
}

// TopBlocks returns the N most frequently invoked blocks.
func (p *Profiler) TopBlocks(n int) []*BlockMethod {
	type blockCount struct {
		block *BlockMethod
		count uint64
	}

	var all []blockCount
	p.blockProfiles.Range(func(key, value interface{}) bool {
		block := key.(*BlockMethod)
		profile := value.(*BlockProfile)
		all = append(all, blockCount{block, atomic.LoadUint64(&profile.InvocationCount)})
		return true
	})

	// Simple selection sort for top N
	for i := 0; i < n && i < len(all); i++ {
		maxIdx := i
		for j := i + 1; j < len(all); j++ {
			if all[j].count > all[maxIdx].count {
				maxIdx = j
			}
		}
		all[i], all[maxIdx] = all[maxIdx], all[i]
	}

	result := make([]*BlockMethod, 0, n)
	for i := 0; i < n && i < len(all); i++ {
		result = append(result, all[i].block)
	}
	return result
}

// Reset clears all profiling data.
func (p *Profiler) Reset() {
	p.methodProfiles = sync.Map{}
	p.blockProfiles = sync.Map{}
	atomic.StoreUint64(&p.hotMethodCount, 0)
	atomic.StoreUint64(&p.hotBlockCount, 0)
}
