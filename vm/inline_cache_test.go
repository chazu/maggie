package vm

import (
	"testing"
)

func TestInlineCacheEmpty(t *testing.T) {
	ic := &InlineCache{State: CacheEmpty}

	class := &Class{Name: "Test"}
	method := ic.Lookup(class)

	if method != nil {
		t.Error("Expected nil from empty cache")
	}
	if ic.Misses != 1 {
		t.Errorf("Expected 1 miss, got %d", ic.Misses)
	}
}

func TestInlineCacheMonomorphic(t *testing.T) {
	ic := &InlineCache{State: CacheEmpty}

	class := &Class{Name: "Test"}
	testMethod := &CompiledMethod{name: "test"}

	// First update - becomes monomorphic
	ic.Update(class, testMethod)

	if ic.State != CacheMonomorphic {
		t.Errorf("Expected monomorphic state, got %v", ic.State)
	}
	if ic.Count != 1 {
		t.Errorf("Expected count 1, got %d", ic.Count)
	}

	// Lookup should hit
	method := ic.Lookup(class)
	if method != testMethod {
		t.Error("Expected cache hit")
	}
	if ic.Hits != 1 {
		t.Errorf("Expected 1 hit, got %d", ic.Hits)
	}

	// Different class should miss
	otherClass := &Class{Name: "Other"}
	method = ic.Lookup(otherClass)
	if method != nil {
		t.Error("Expected cache miss for different class")
	}
	if ic.Misses != 1 {
		t.Errorf("Expected 1 miss, got %d", ic.Misses)
	}
}

func TestInlineCacheUpgradeToPolymorphic(t *testing.T) {
	ic := &InlineCache{State: CacheEmpty}

	class1 := &Class{Name: "Class1"}
	class2 := &Class{Name: "Class2"}
	method1 := &CompiledMethod{name: "method1"}
	method2 := &CompiledMethod{name: "method2"}

	// First class - monomorphic
	ic.Update(class1, method1)
	if ic.State != CacheMonomorphic {
		t.Errorf("Expected monomorphic, got %v", ic.State)
	}

	// Second class - upgrade to polymorphic
	ic.Update(class2, method2)
	if ic.State != CachePolymorphic {
		t.Errorf("Expected polymorphic, got %v", ic.State)
	}
	if ic.Count != 2 {
		t.Errorf("Expected count 2, got %d", ic.Count)
	}

	// Both should hit
	if m := ic.Lookup(class1); m != method1 {
		t.Error("Expected hit for class1")
	}
	if m := ic.Lookup(class2); m != method2 {
		t.Error("Expected hit for class2")
	}
}

func TestInlineCachePolymorphicGrowth(t *testing.T) {
	ic := &InlineCache{State: CacheEmpty}

	classes := make([]*Class, MaxPICEntries)
	methods := make([]*CompiledMethod, MaxPICEntries)

	for i := 0; i < MaxPICEntries; i++ {
		classes[i] = &Class{Name: "Class"}
		methods[i] = &CompiledMethod{name: "method"}
		ic.Update(classes[i], methods[i])
	}

	// Should be polymorphic with max entries
	if ic.State != CachePolymorphic {
		t.Errorf("Expected polymorphic, got %v", ic.State)
	}
	if ic.Count != MaxPICEntries {
		t.Errorf("Expected count %d, got %d", MaxPICEntries, ic.Count)
	}

	// All should hit
	for i := 0; i < MaxPICEntries; i++ {
		if m := ic.Lookup(classes[i]); m != methods[i] {
			t.Errorf("Expected hit for class %d", i)
		}
	}
}

func TestInlineCacheUpgradeToMegamorphic(t *testing.T) {
	ic := &InlineCache{State: CacheEmpty}

	// Fill up to max entries
	for i := 0; i < MaxPICEntries; i++ {
		class := &Class{Name: "Class"}
		method := &CompiledMethod{name: "method"}
		ic.Update(class, method)
	}

	// One more should go megamorphic
	extraClass := &Class{Name: "Extra"}
	extraMethod := &CompiledMethod{name: "extra"}
	ic.Update(extraClass, extraMethod)

	if ic.State != CacheMegamorphic {
		t.Errorf("Expected megamorphic, got %v", ic.State)
	}

	// Megamorphic always misses
	if m := ic.Lookup(extraClass); m != nil {
		t.Error("Expected miss from megamorphic cache")
	}
}

func TestInlineCacheHitRate(t *testing.T) {
	ic := &InlineCache{State: CacheEmpty}

	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}
	ic.Update(class, method)

	// 10 hits
	for i := 0; i < 10; i++ {
		ic.Lookup(class)
	}

	// 2 misses
	other := &Class{Name: "Other"}
	ic.Lookup(other)
	ic.Lookup(other)

	hitRate := ic.HitRate()
	// 10 hits / 12 total = 83.33%
	if hitRate < 83.0 || hitRate > 84.0 {
		t.Errorf("Expected ~83%% hit rate, got %.2f%%", hitRate)
	}
}

func TestInlineCacheTable(t *testing.T) {
	table := NewInlineCacheTable()

	// Get creates on first access
	ic1 := table.GetOrCreate(100)
	if ic1 == nil {
		t.Error("Expected cache to be created")
	}

	// Same PC returns same cache
	ic2 := table.GetOrCreate(100)
	if ic1 != ic2 {
		t.Error("Expected same cache for same PC")
	}

	// Different PC returns different cache
	ic3 := table.GetOrCreate(200)
	if ic1 == ic3 {
		t.Error("Expected different cache for different PC")
	}
}

func TestInlineCacheTableStats(t *testing.T) {
	table := NewInlineCacheTable()

	// Create some caches in different states
	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}

	// Monomorphic
	ic1 := table.GetOrCreate(100)
	ic1.Update(class, method)
	ic1.Lookup(class) // 1 hit

	// Empty
	table.GetOrCreate(200)

	// Polymorphic
	ic3 := table.GetOrCreate(300)
	ic3.Update(&Class{Name: "A"}, method)
	ic3.Update(&Class{Name: "B"}, method)
	ic3.Lookup(&Class{Name: "C"}) // 1 miss

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

// BenchmarkInlineCacheLookup measures the overhead of inline cache lookup.
func BenchmarkInlineCacheLookup(b *testing.B) {
	class := &Class{Name: "Test"}
	method := &CompiledMethod{name: "test"}

	b.Run("Monomorphic_Hit", func(b *testing.B) {
		ic := &InlineCache{State: CacheEmpty}
		ic.Update(class, method)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			ic.Lookup(class)
		}
	})

	b.Run("Polymorphic_Hit_First", func(b *testing.B) {
		ic := &InlineCache{State: CacheEmpty}
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
		ic := &InlineCache{State: CacheEmpty}
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
		ic := &InlineCache{State: CacheMegamorphic}
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
		ic := &InlineCache{State: CacheEmpty}
		ic.Update(class, method)
		b.ResetTimer()
		for i := 0; i < b.N; i++ {
			if m := ic.Lookup(class); m == nil {
				_ = class.VTable.Lookup(selectorID)
			}
		}
	})
}
