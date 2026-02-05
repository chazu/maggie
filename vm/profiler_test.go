package vm

import (
	"sync"
	"sync/atomic"
	"testing"
)

func TestProfilerMethodInvocation(t *testing.T) {
	p := NewProfiler()
	p.MethodHotThreshold = 5

	method := &CompiledMethod{name: "test"}

	// First invocation
	becameHot := p.RecordMethodInvocation(method)
	if becameHot {
		t.Error("Method should not be hot after 1 invocation")
	}

	profile := p.GetMethodProfile(method)
	if profile == nil {
		t.Fatal("Profile should exist after invocation")
	}
	if profile.InvocationCount != 1 {
		t.Errorf("Expected 1 invocation, got %d", profile.InvocationCount)
	}

	// Invoke 4 more times (total 5)
	for i := 0; i < 4; i++ {
		becameHot = p.RecordMethodInvocation(method)
	}

	// Should become hot at exactly threshold
	if !becameHot {
		t.Error("Method should become hot at threshold")
	}
	if !p.IsMethodHot(method) {
		t.Error("IsMethodHot should return true")
	}

	// Additional invocations should not re-trigger hot
	becameHot = p.RecordMethodInvocation(method)
	if becameHot {
		t.Error("Method should not re-trigger hot")
	}
}

func TestProfilerBlockInvocation(t *testing.T) {
	p := NewProfiler()
	p.BlockHotThreshold = 3

	method := &CompiledMethod{name: "test"}
	block := &BlockMethod{Arity: 0}
	method.Blocks = []*BlockMethod{block}

	// First invocation
	becameHot := p.RecordBlockInvocation(block, method, 0)
	if becameHot {
		t.Error("Block should not be hot after 1 invocation")
	}

	profile := p.GetBlockProfile(block)
	if profile == nil {
		t.Fatal("Profile should exist after invocation")
	}
	if profile.OwningMethod != method {
		t.Error("Profile should track owning method")
	}
	if profile.BlockIndex != 0 {
		t.Errorf("Expected block index 0, got %d", profile.BlockIndex)
	}

	// Invoke 2 more times (total 3)
	for i := 0; i < 2; i++ {
		becameHot = p.RecordBlockInvocation(block, method, 0)
	}

	// Should become hot at exactly threshold
	if !becameHot {
		t.Error("Block should become hot at threshold")
	}
	if !p.IsBlockHot(block) {
		t.Error("IsBlockHot should return true")
	}
}

func TestProfilerStats(t *testing.T) {
	p := NewProfiler()
	p.MethodHotThreshold = 5
	p.BlockHotThreshold = 10

	// Create test data
	method1 := &CompiledMethod{name: "method1"}
	method2 := &CompiledMethod{name: "method2"}
	block1 := &BlockMethod{Arity: 0}
	block2 := &BlockMethod{Arity: 1}

	// Invoke methods
	for i := 0; i < 10; i++ {
		p.RecordMethodInvocation(method1) // Will be hot
	}
	for i := 0; i < 3; i++ {
		p.RecordMethodInvocation(method2) // Won't be hot
	}

	// Invoke blocks
	for i := 0; i < 15; i++ {
		p.RecordBlockInvocation(block1, nil, 0) // Will be hot
	}
	for i := 0; i < 5; i++ {
		p.RecordBlockInvocation(block2, nil, 0) // Won't be hot
	}

	stats := p.Stats()

	if stats.TotalMethods != 2 {
		t.Errorf("Expected 2 methods, got %d", stats.TotalMethods)
	}
	if stats.TotalBlocks != 2 {
		t.Errorf("Expected 2 blocks, got %d", stats.TotalBlocks)
	}
	if stats.HotMethods != 1 {
		t.Errorf("Expected 1 hot method, got %d", stats.HotMethods)
	}
	if stats.HotBlocks != 1 {
		t.Errorf("Expected 1 hot block, got %d", stats.HotBlocks)
	}
	if stats.MethodInvocations != 13 {
		t.Errorf("Expected 13 method invocations, got %d", stats.MethodInvocations)
	}
	if stats.BlockInvocations != 20 {
		t.Errorf("Expected 20 block invocations, got %d", stats.BlockInvocations)
	}
}

func TestProfilerHotMethods(t *testing.T) {
	p := NewProfiler()
	p.MethodHotThreshold = 3

	method1 := &CompiledMethod{name: "method1"}
	method2 := &CompiledMethod{name: "method2"}
	method3 := &CompiledMethod{name: "method3"}

	// Make method1 and method3 hot
	for i := 0; i < 5; i++ {
		p.RecordMethodInvocation(method1)
		p.RecordMethodInvocation(method3)
	}
	// method2 stays cold
	p.RecordMethodInvocation(method2)

	hot := p.HotMethods()
	if len(hot) != 2 {
		t.Errorf("Expected 2 hot methods, got %d", len(hot))
	}

	// Check both hot methods are in the list
	found1, found3 := false, false
	for _, m := range hot {
		if m == method1 {
			found1 = true
		}
		if m == method3 {
			found3 = true
		}
	}
	if !found1 || !found3 {
		t.Error("Expected method1 and method3 in hot methods")
	}
}

func TestProfilerTopMethods(t *testing.T) {
	p := NewProfiler()

	method1 := &CompiledMethod{name: "method1"}
	method2 := &CompiledMethod{name: "method2"}
	method3 := &CompiledMethod{name: "method3"}

	// Different invocation counts
	for i := 0; i < 100; i++ {
		p.RecordMethodInvocation(method1)
	}
	for i := 0; i < 50; i++ {
		p.RecordMethodInvocation(method2)
	}
	for i := 0; i < 25; i++ {
		p.RecordMethodInvocation(method3)
	}

	top := p.TopMethods(2)
	if len(top) != 2 {
		t.Fatalf("Expected 2 top methods, got %d", len(top))
	}
	if top[0] != method1 {
		t.Error("Expected method1 to be top")
	}
	if top[1] != method2 {
		t.Error("Expected method2 to be second")
	}
}

func TestProfilerReset(t *testing.T) {
	p := NewProfiler()
	p.MethodHotThreshold = 2

	method := &CompiledMethod{name: "test"}
	for i := 0; i < 5; i++ {
		p.RecordMethodInvocation(method)
	}

	if !p.IsMethodHot(method) {
		t.Error("Method should be hot before reset")
	}

	p.Reset()

	if p.IsMethodHot(method) {
		t.Error("Method should not be hot after reset")
	}
	if p.GetMethodProfile(method) != nil {
		t.Error("Profile should be cleared after reset")
	}

	stats := p.Stats()
	if stats.TotalMethods != 0 || stats.TotalBlocks != 0 {
		t.Error("Stats should be zero after reset")
	}
}

func TestProfilerConcurrentAccess(t *testing.T) {
	p := NewProfiler()
	p.MethodHotThreshold = 1000

	method := &CompiledMethod{name: "test"}

	// Concurrent invocations from multiple goroutines
	var wg sync.WaitGroup
	goroutines := 10
	invocationsPerGoroutine := 100

	for g := 0; g < goroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < invocationsPerGoroutine; i++ {
				p.RecordMethodInvocation(method)
			}
		}()
	}

	wg.Wait()

	profile := p.GetMethodProfile(method)
	expected := uint64(goroutines * invocationsPerGoroutine)
	if profile.InvocationCount != expected {
		t.Errorf("Expected %d invocations, got %d", expected, profile.InvocationCount)
	}
}

// BenchmarkProfilerMethodInvocation measures profiling overhead.
func BenchmarkProfilerMethodInvocation(b *testing.B) {
	p := NewProfiler()
	method := &CompiledMethod{name: "test"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p.RecordMethodInvocation(method)
	}
}

// BenchmarkProfilerBlockInvocation measures block profiling overhead.
func BenchmarkProfilerBlockInvocation(b *testing.B) {
	p := NewProfiler()
	method := &CompiledMethod{name: "test"}
	block := &BlockMethod{Arity: 0}
	method.Blocks = []*BlockMethod{block}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		p.RecordBlockInvocation(block, method, 0)
	}
}

// BenchmarkProfilerConcurrent measures concurrent profiling overhead.
func BenchmarkProfilerConcurrent(b *testing.B) {
	p := NewProfiler()
	methods := make([]*CompiledMethod, 100)
	for i := range methods {
		methods[i] = &CompiledMethod{name: "test"}
	}

	b.ResetTimer()
	b.RunParallel(func(pb *testing.PB) {
		var counter uint64
		for pb.Next() {
			idx := atomic.AddUint64(&counter, 1) % 100
			p.RecordMethodInvocation(methods[idx])
		}
	})
}

// BenchmarkNoProfiler measures baseline without profiling.
func BenchmarkNoProfiler(b *testing.B) {
	method := &CompiledMethod{name: "test"}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		// Just access the method to have comparable baseline
		_ = method.Name()
	}
}
