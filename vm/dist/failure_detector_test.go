package dist

import (
	"sync"
	"testing"

	"github.com/chazu/maggie/vm"
)

// Stop closes the event channel, but the down-observer stays registered on the
// VM's shared health monitor; a node declared dead afterwards must not panic
// with "send on closed channel" (which would kill the process).
func TestDirectHeartbeatDetector_DownAfterStopDoesNotPanic(t *testing.T) {
	v := vm.NewVM()
	defer v.Shutdown()
	d := NewDirectHeartbeatDetector(v)
	d.Stop()
	d.onDown([32]byte{1}) // must be a no-op, not a panic
	d.Stop()              // idempotent

	if _, ok := <-d.Events(); ok {
		t.Fatal("events channel should be closed after Stop")
	}
}

func TestDirectHeartbeatDetector_ConcurrentDownAndStop(t *testing.T) {
	v := vm.NewVM()
	defer v.Shutdown()
	for i := 0; i < 50; i++ {
		d := NewDirectHeartbeatDetector(v)
		var wg sync.WaitGroup
		wg.Add(2)
		go func() {
			defer wg.Done()
			for j := 0; j < 100; j++ {
				d.onDown([32]byte{byte(j)})
			}
		}()
		go func() { defer wg.Done(); d.Stop() }()
		wg.Wait()
	}
}
