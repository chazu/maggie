package vm

import (
	"sync"
	"testing"
)

// These tests exist to fail under `go test -race` if DictionaryObject or
// ArrayListObject ever lose their internal locking. Shared collections in
// globals are reachable from concurrent server requests (RunIsolated) and
// forked processes; a bare concurrent map write is an uncatchable Go
// runtime fatal (review finding VM-1).

func TestDictionaryObject_ConcurrentAccess(t *testing.T) {
	vm := NewVM()
	dictVal := vm.registry.NewDictionaryValue()
	dict := vm.registry.GetDictionaryObject(dictVal)

	const goroutines = 8
	const ops = 500

	var wg sync.WaitGroup
	for g := 0; g < goroutines; g++ {
		wg.Add(1)
		go func(g int) {
			defer wg.Done()
			for i := 0; i < ops; i++ {
				h := uint64(g*ops + i)
				key := FromSmallInt(int64(h))
				dict.SetByHash(h, key, FromSmallInt(int64(i)))
				dict.GetByHash(h)
				if i%3 == 0 {
					dict.DeleteByHash(h)
				}
				if i%7 == 0 {
					for range dict.Entries() {
					}
					dict.Size()
				}
			}
		}(g)
	}
	wg.Wait()
}

func TestArrayListObject_ConcurrentAccess(t *testing.T) {
	al := createArrayList(0)

	const goroutines = 8
	const ops = 500

	var wg sync.WaitGroup
	for g := 0; g < goroutines; g++ {
		wg.Add(1)
		go func(g int) {
			defer wg.Done()
			for i := 0; i < ops; i++ {
				al.Add(FromSmallInt(int64(i)))
				al.At(i % 16)
				al.AtPut(i%16, FromSmallInt(int64(g)))
				if i%5 == 0 {
					al.RemoveLast()
				}
				if i%9 == 0 {
					for range al.Snapshot() {
					}
					al.Size()
				}
			}
		}(g)
	}
	wg.Wait()

	if al.Size() == 0 {
		t.Error("expected surviving elements after concurrent churn")
	}
}
