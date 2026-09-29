package vm

import (
	"io"
	"sync"
	"testing"
)

// Saving an image while another process writes a class variable must not race
// on the class-variable map (run with -race).
func TestSaveImageConcurrentWithClassVarWrites(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()
	c := v.Classes.Lookup("Object")

	stop := make(chan struct{})
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := int64(0); ; i++ {
			select {
			case <-stop:
				return
			default:
				c.SetClassVar(v.registry, "Counter", FromSmallInt(i%1000))
			}
		}
	}()
	for i := 0; i < 5; i++ {
		if err := v.SaveImageTo(io.Discard); err != nil {
			t.Fatalf("SaveImageTo: %v", err)
		}
	}
	close(stop)
	wg.Wait()
}
