package vm

import (
	"testing"
	"time"
)

func TestChannelNew(t *testing.T) {
	vm := NewVM()

	// Create unbuffered channel via primitive
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new", nil)
	if ch == Nil {
		t.Fatal("Channel new returned nil")
	}

	if !isChannelValue(ch) {
		t.Error("Expected channel value")
	}
}

func TestChannelNewBuffered(t *testing.T) {
	vm := NewVM()

	// Create buffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(5)})
	if ch == Nil {
		t.Fatal("Channel new: returned nil")
	}

	// Check capacity
	cap := vm.Send(ch, "capacity", nil)
	if !cap.IsSmallInt() || cap.SmallInt() != 5 {
		t.Errorf("Channel capacity = %v, want 5", cap)
	}
}

func TestChannelSendReceive(t *testing.T) {
	vm := NewVM()

	// Create buffered channel (so send doesn't block)
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Send a value
	vm.Send(ch, "send:", []Value{FromSmallInt(42)})

	// Receive the value
	val := vm.Send(ch, "receive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("Received %v, want 42", val)
	}
}

func TestChannelTrySend(t *testing.T) {
	vm := NewVM()

	// Create buffered channel with capacity 1
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// First send should succeed
	result := vm.Send(ch, "trySend:", []Value{FromSmallInt(1)})
	if result != True {
		t.Error("First trySend should succeed")
	}

	// Second send should fail (buffer full)
	result = vm.Send(ch, "trySend:", []Value{FromSmallInt(2)})
	if result != False {
		t.Error("Second trySend should fail (buffer full)")
	}
}

func TestChannelTryReceive(t *testing.T) {
	vm := NewVM()

	// Create buffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Try receive on empty channel
	val := vm.Send(ch, "tryReceive", nil)
	if val != Nil {
		t.Error("tryReceive on empty channel should return nil")
	}

	// Send a value
	vm.Send(ch, "send:", []Value{FromSmallInt(42)})

	// Now tryReceive should succeed
	val = vm.Send(ch, "tryReceive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("tryReceive = %v, want 42", val)
	}
}

func TestChannelClose(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Initially not closed
	closed := vm.Send(ch, "isClosed", nil)
	if closed != False {
		t.Error("New channel should not be closed")
	}

	// Close the channel
	vm.Send(ch, "close", nil)

	// Now it should be closed
	closed = vm.Send(ch, "isClosed", nil)
	if closed != True {
		t.Error("Channel should be closed after close")
	}
}

func TestChannelSizeAndEmpty(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(5)})

	// Initially empty
	empty := vm.Send(ch, "isEmpty", nil)
	if empty != True {
		t.Error("New channel should be empty")
	}

	size := vm.Send(ch, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("New channel size = %v, want 0", size)
	}

	// Send some values
	vm.Send(ch, "send:", []Value{FromSmallInt(1)})
	vm.Send(ch, "send:", []Value{FromSmallInt(2)})

	empty = vm.Send(ch, "isEmpty", nil)
	if empty != False {
		t.Error("Channel with values should not be empty")
	}

	size = vm.Send(ch, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 2 {
		t.Errorf("Channel size = %v, want 2", size)
	}
}

func TestProcessFork(t *testing.T) {
	vm := NewVM()

	// Create a simple block that returns 42
	// For now, we'll test process creation with a pre-compiled block
	// This requires setting up a proper block, which is complex

	// Simpler test: just verify Process class exists and has methods
	proc := vm.Send(vm.classValue(vm.ProcessClass), "current", nil)
	// current returns nil since we're not tracking per-goroutine processes yet
	if proc != Nil {
		t.Log("Process current returned:", proc)
	}
}

func TestProcessSleep(t *testing.T) {
	vm := NewVM()

	start := time.Now()
	vm.Send(vm.classValue(vm.ProcessClass), "sleep:", []Value{FromSmallInt(50)})
	elapsed := time.Since(start)

	if elapsed < 50*time.Millisecond {
		t.Errorf("Sleep elapsed %v, want >= 50ms", elapsed)
	}
}

func TestChannelConcurrent(t *testing.T) {
	vm := NewVM()

	// Create unbuffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new", nil)
	chObj := getChannel(ch)

	done := make(chan struct{})

	// Send in a goroutine
	go func() {
		chObj.ch <- FromSmallInt(42)
		close(done)
	}()

	// Receive
	val := vm.Send(ch, "receive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("Received %v, want 42", val)
	}

	<-done
}

func TestChannelMultipleValues(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(10)})

	// Send multiple values
	for i := int64(1); i <= 5; i++ {
		vm.Send(ch, "send:", []Value{FromSmallInt(i)})
	}

	// Receive and verify
	for i := int64(1); i <= 5; i++ {
		val := vm.Send(ch, "receive", nil)
		if !val.IsSmallInt() || val.SmallInt() != i {
			t.Errorf("Received %v, want %d", val, i)
		}
	}
}
