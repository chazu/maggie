package vm

import (
	"sync"
	"testing"
)

// A Random instance shared across forked processes must be safe for
// concurrent use (run with -race).
func TestRandomInstanceConcurrentUse(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	rng := v.Send(v.MustGlobal("Random"), "new:", []Value{FromSmallInt(42)})

	var wg sync.WaitGroup
	for g := 0; g < 8; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < 200; i++ {
				v.Send(rng, "next", nil)
				v.Send(rng, "nextInt:", []Value{FromSmallInt(100)})
				v.Send(rng, "nextBetween:and:", []Value{FromSmallInt(1), FromSmallInt(6)})
			}
		}()
	}
	wg.Wait()
}
