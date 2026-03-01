package vm

import (
	"syscall"
	"testing"
	"time"
)

func TestSignalTrapToChannel(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sig := v.Globals["Signal"]
	if sig == Nil {
		t.Fatal("Signal class not in Globals")
	}

	// Create a buffered channel
	chClass := v.Globals["Channel"]
	bufSize := FromSmallInt(1)
	ch := v.Send(chClass, "new:", []Value{bufSize})

	// Trap SIGUSR1 to the channel
	sigName := v.registry.NewStringValue("SIGUSR1")
	result := v.Send(sig, "trap:toChannel:", []Value{sigName, ch})
	if result == Nil {
		t.Fatal("trap:toChannel: returned nil")
	}

	// Send the signal to ourselves
	syscall.Kill(syscall.Getpid(), syscall.SIGUSR1)

	// Give goroutine time to deliver
	time.Sleep(50 * time.Millisecond)

	// Try to receive from the channel
	received := v.Send(ch, "tryReceive", nil)
	if received == Nil {
		t.Fatal("expected to receive signal on channel, got nil")
	}
	// Should be a symbol #SIGUSR1
	if !received.IsSymbol() {
		t.Fatalf("expected symbol, got %v", received)
	}
}

func TestSignalTrapAllToChannel(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sig := v.Globals["Signal"]
	chClass := v.Globals["Channel"]
	bufSize := FromSmallInt(2)
	ch := v.Send(chClass, "new:", []Value{bufSize})

	// Create array of signal names
	names := v.NewArrayWithElements([]Value{
		v.registry.NewStringValue("SIGUSR1"),
		v.registry.NewStringValue("SIGUSR2"),
	})

	result := v.Send(sig, "trapAll:toChannel:", []Value{names, ch})
	if result == Nil {
		t.Fatal("trapAll:toChannel: returned nil")
	}

	// Send SIGUSR2
	syscall.Kill(syscall.Getpid(), syscall.SIGUSR2)
	time.Sleep(50 * time.Millisecond)

	received := v.Send(ch, "tryReceive", nil)
	if received == Nil {
		t.Fatal("expected to receive signal on channel, got nil")
	}
}

func TestSignalUnknownSignal(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sig := v.Globals["Signal"]
	chClass := v.Globals["Channel"]
	bufSize := FromSmallInt(1)
	ch := v.Send(chClass, "new:", []Value{bufSize})

	badName := v.registry.NewStringValue("SIGFAKE")
	result := v.Send(sig, "trap:toChannel:", []Value{badName, ch})

	// Should return a Failure
	isFailure := v.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Error("expected failure for unknown signal, got non-failure")
	}
}
