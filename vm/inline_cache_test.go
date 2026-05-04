package vm

import (
	"testing"
)

// newTestIC returns an empty InlineCache. The IC starts with no
// snapshot; Lookup returns nil and counts a miss.
func newTestIC() *InlineCache { return &InlineCache{} }

// newTestICTable builds an InlineCacheTable pre-registered with the
// given PCs. Used by tests that don't want to assemble real bytecode.
func newTestICTable(pcs ...int) *InlineCacheTable {
	t := &InlineCacheTable{sites: make(map[int]*InlineCache, len(pcs))}
	for _, pc := range pcs {
		t.sites[pc] = &InlineCache{}
	}
	return t
}

// installTestICTable installs a prebuilt table on a CompiledMethod and
// marks the lazy-build as completed so subsequent GetInlineCaches calls
// won't overwrite it.
func installTestICTable(m *CompiledMethod, t *InlineCacheTable) {
	m.inlineCachesOnce.Do(func() {
		m.inlineCaches.Store(t)
	})
}

func TestInlineCacheEmpty(t *testing.T) {
	ic := newTestIC()

	class := &Class{Name: "Test"}
	method := ic.Lookup(class)

	if method != nil {
		t.Error("Expected nil from empty cache")
	}
	if got := ic.Misses.Load(); got != 1 {
		t.Errorf("Expected 1 miss, got %d", got)
	}
}

func TestInlineCacheMonomorphic(t *testing.T) {
	ic := newTestIC()

	class := &Class{Name: "Test"}
	testMethod := &CompiledMethod{name: "test"}

	ic.Update(class, testMethod)

	if ic.State() != CacheMonomorphic {
		t.Errorf("Expected monomorphic state, got %v", ic.State())
	}
	if ic.Count() != 1 {
		t.Errorf("Expected count 1, got %d", ic.Count())
	}

	method := ic.Lookup(class)
	if method != testMethod {
		t.Error("Expected cache hit")
	}
	if got := ic.Hits.Load(); got != 1 {
		t.Errorf("Expected 1 hit, got %d", got)
	}

	otherClass := &Class{Name: "Other"}
	method = ic.Lookup(otherClass)
	if method != nil {
		t.Error("Expected cache miss for different class")
	}
	if got := ic.Misses.Load(); got != 1 {
		t.Errorf("Expected 1 miss, got %d", got)
	}
}

func TestInlineCacheUpgradeToPolymorphic(t *testing.T) {
	ic := newTestIC()

	class1 := &Class{Name: "Class1"}
	class2 := &Class{Name: "Class2"}
	method1 := &CompiledMethod{name: "method1"}
	method2 := &CompiledMethod{name: "method2"}

	ic.Update(class1, method1)
	if ic.State() != CacheMonomorphic {
		t.Errorf("Expected monomorphic, got %v", ic.State())
	}

	ic.Update(class2, method2)
	if ic.State() != CachePolymorphic {
		t.Errorf("Expected polymorphic, got %v", ic.State())
	}
	if ic.Count() != 2 {
		t.Errorf("Expected count 2, got %d", ic.Count())
	}

	if m := ic.Lookup(class1); m != method1 {
		t.Error("Expected hit for class1")
	}
	if m := ic.Lookup(class2); m != method2 {
		t.Error("Expected hit for class2")
	}
}

func TestInlineCachePolymorphicGrowth(t *testing.T) {
	ic := newTestIC()

	classes := make([]*Class, MaxPICEntries)
	methods := make([]*CompiledMethod, MaxPICEntries)

	for i := 0; i < MaxPICEntries; i++ {
		classes[i] = &Class{Name: "Class"}
		methods[i] = &CompiledMethod{name: "method"}
		ic.Update(classes[i], methods[i])
	}

	if ic.State() != CachePolymorphic {
		t.Errorf("Expected polymorphic, got %v", ic.State())
	}
	if ic.Count() != MaxPICEntries {
		t.Errorf("Expected count %d, got %d", MaxPICEntries, ic.Count())
	}

	for i := 0; i < MaxPICEntries; i++ {
		if m := ic.Lookup(classes[i]); m != methods[i] {
			t.Errorf("Expected hit for class %d", i)
		}
	}
}

func TestInlineCacheUpgradeToMegamorphic(t *testing.T) {
	ic := newTestIC()

	for i := 0; i < MaxPICEntries; i++ {
		class := &Class{Name: "Class"}
		method := &CompiledMethod{name: "method"}
		ic.Update(class, method)
	}

	extraClass := &Class{Name: "Extra"}
	extraMethod := &CompiledMethod{name: "extra"}
	ic.Update(extraClass, extraMethod)

	if ic.State() != CacheMegamorphic {
		t.Errorf("Expected megamorphic, got %v", ic.State())
	}

	if m := ic.Lookup(extraClass); m != nil {
		t.Error("Expected miss from megamorphic cache")
	}
}

func TestInlineCacheHitRate(t *testing.T) {
	ic := newTestIC()

	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}
	ic.Update(class, method)

	for i := 0; i < 10; i++ {
		ic.Lookup(class)
	}

	other := &Class{Name: "Other"}
	ic.Lookup(other)
	ic.Lookup(other)

	hitRate := ic.HitRate()
	if hitRate < 83.0 || hitRate > 84.0 {
		t.Errorf("Expected ~83%% hit rate, got %.2f%%", hitRate)
	}
}

func TestInlineCacheTable(t *testing.T) {
	table := newTestICTable(100, 200)

	ic1 := table.Get(100)
	if ic1 == nil {
		t.Error("Expected pre-registered cache at PC 100")
	}

	ic2 := table.Get(100)
	if ic1 != ic2 {
		t.Error("Expected same cache for same PC")
	}

	ic3 := table.Get(200)
	if ic1 == ic3 {
		t.Error("Expected different cache for different PC")
	}

	if got := table.Get(999); got != nil {
		t.Error("Expected nil for unregistered PC")
	}
}

func TestInlineCacheTableStats(t *testing.T) {
	table := newTestICTable(100, 200, 300)

	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}

	ic1 := table.Get(100)
	ic1.Update(class, method)
	ic1.Lookup(class)

	// 200 stays empty.

	ic3 := table.Get(300)
	ic3.Update(&Class{Name: "A"}, method)
	ic3.Update(&Class{Name: "B"}, method)
	ic3.Lookup(&Class{Name: "C"}) // miss

	mono, poly, mega, empty, hits, misses := table.Stats()

	if mono != 1 {
		t.Errorf("Expected 1 mono, got %d", mono)
	}
	if poly != 1 {
		t.Errorf("Expected 1 poly, got %d", poly)
	}
	if mega != 0 {
		t.Errorf("Expected 0 mega, got %d", mega)
	}
	if empty != 1 {
		t.Errorf("Expected 1 empty, got %d", empty)
	}
	if hits != 1 {
		t.Errorf("Expected 1 hit, got %d", hits)
	}
	if misses != 1 {
		t.Errorf("Expected 1 miss, got %d", misses)
	}
}

func TestInlineCacheResetClearsState(t *testing.T) {
	ic := newTestIC()

	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}

	ic.Update(class, method)
	class2 := &Class{Name: "Test2"}
	method2 := &CompiledMethod{name: "test2"}
	ic.Update(class2, method2)

	ic.Lookup(class)
	ic.Lookup(&Class{Name: "Unknown"})

	if ic.State() != CachePolymorphic {
		t.Fatalf("Expected polymorphic, got %v", ic.State())
	}

	ic.Reset()

	if ic.State() != CacheEmpty {
		t.Errorf("Expected empty state after reset, got %v", ic.State())
	}
	if ic.Count() != 0 {
		t.Errorf("Expected count 0 after reset, got %d", ic.Count())
	}
	if got := ic.Hits.Load(); got != 0 {
		t.Errorf("Expected hits 0 after reset, got %d", got)
	}
	if got := ic.Misses.Load(); got != 0 {
		t.Errorf("Expected misses 0 after reset, got %d", got)
	}
	if entries := ic.Entries(); len(entries) != 0 {
		t.Errorf("Expected zero entries after reset, got %d", len(entries))
	}
}

func TestInlineCacheResetForcesRemiss(t *testing.T) {
	ic := newTestIC()

	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}

	ic.Update(class, method)
	if m := ic.Lookup(class); m != method {
		t.Fatal("Expected cache hit before reset")
	}

	ic.Reset()

	if m := ic.Lookup(class); m != nil {
		t.Error("Expected cache miss after reset, got a hit")
	}
	if got := ic.Misses.Load(); got != 1 {
		t.Errorf("Expected 1 miss after reset, got %d", got)
	}
}

func TestInvalidateAllCaches(t *testing.T) {
	ct := NewClassTable()

	class1 := &Class{Name: "Alpha"}
	class1.VTable = NewVTable(class1, nil)
	class1.ClassVTable = NewVTable(class1, nil)

	method1 := &CompiledMethod{name: "m1"}
	installTestICTable(method1, newTestICTable(10))
	ic1 := method1.GetInlineCaches().Get(10)
	ic1.Update(class1, method1)
	class1.VTable.AddMethod(1, method1)

	classMethod := &CompiledMethod{name: "cm1"}
	installTestICTable(classMethod, newTestICTable(20))
	icCM := classMethod.GetInlineCaches().Get(20)
	icCM.Update(class1, classMethod)
	class1.ClassVTable.AddMethod(2, classMethod)

	class2 := &Class{Name: "Beta"}
	class2.VTable = NewVTable(class2, nil)

	method2 := &CompiledMethod{name: "m2"}
	installTestICTable(method2, newTestICTable(30))
	ic2 := method2.GetInlineCaches().Get(30)
	ic2.Update(class2, method2)
	ic2.Update(class1, method1)
	class2.VTable.AddMethod(3, method2)

	ct.Register(class1)
	ct.Register(class2)

	if ic1.State() != CacheMonomorphic {
		t.Fatalf("Expected monomorphic, got %v", ic1.State())
	}
	if ic2.State() != CachePolymorphic {
		t.Fatalf("Expected polymorphic, got %v", ic2.State())
	}

	InvalidateAllCaches(ct)

	if ic1.State() != CacheEmpty {
		t.Errorf("ic1: expected empty after invalidation, got %v", ic1.State())
	}
	if icCM.State() != CacheEmpty {
		t.Errorf("icCM: expected empty after invalidation, got %v", icCM.State())
	}
	if ic2.State() != CacheEmpty {
		t.Errorf("ic2: expected empty after invalidation, got %v", ic2.State())
	}

	if m := ic1.Lookup(class1); m != nil {
		t.Error("Expected miss after invalidation")
	}
	if m := ic2.Lookup(class2); m != nil {
		t.Error("Expected miss after invalidation")
	}
}

// BenchmarkInlineCacheLookup measures the overhead of inline cache lookup.
func BenchmarkInlineCacheLookup(b *testing.B) {
	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}

	b.Run("Monomorphic_Hit", func(b *testing.B) {
		ic := newTestIC()
		ic.Update(class, method)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			ic.Lookup(class)
		}
	})

	b.Run("Polymorphic_Hit_First", func(b *testing.B) {
		ic := newTestIC()
		classes := make([]*Class, 4)
		for i := range classes {
			classes[i] = &Class{Name: "Class"}
			ic.Update(classes[i], method)
		}
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			ic.Lookup(classes[0])
		}
	})

	b.Run("Polymorphic_Hit_Last", func(b *testing.B) {
		ic := newTestIC()
		classes := make([]*Class, 4)
		for i := range classes {
			classes[i] = &Class{Name: "Class"}
			ic.Update(classes[i], method)
		}
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			ic.Lookup(classes[3])
		}
	})

	b.Run("Megamorphic_Miss", func(b *testing.B) {
		ic := newTestIC()
		// Force into megamorphic state by exceeding MaxPICEntries.
		for i := 0; i < MaxPICEntries+1; i++ {
			ic.Update(&Class{Name: "C"}, method)
		}
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			ic.Lookup(class)
		}
	})
}

// BenchmarkInlineCacheVsVTable compares inline cache to direct vtable lookup.
func BenchmarkInlineCacheVsVTable(b *testing.B) {
	class := &Class{Name: "Test"}
	class.VTable = NewVTable(class, nil)

	method := &CompiledMethod{name: "test"}
	selectorID := 5
	class.VTable.AddMethod(selectorID, method)

	b.Run("VTable_Direct", func(b *testing.B) {
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			_ = class.VTable.Lookup(selectorID)
		}
	})

	b.Run("InlineCache_Then_VTable", func(b *testing.B) {
		ic := newTestIC()
		ic.Update(class, method)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			if m := ic.Lookup(class); m == nil {
				_ = class.VTable.Lookup(selectorID)
			}
		}
	})
}
