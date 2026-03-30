package vm

import (
	"sync"
	"testing"
	"time"
)

func TestMailbox_TrySendReceive(t *testing.T) {
	mb := NewMailbox(10)
	msg := FromSmallInt(42)

	if !mb.TrySend(msg) {
		t.Fatal("TrySend should succeed on empty mailbox")
	}
	if mb.Count() != 1 {
		t.Errorf("Count: got %d, want 1", mb.Count())
	}

	got, ok := mb.TryReceive()
	if !ok {
		t.Fatal("TryReceive should succeed")
	}
	if !got.IsSmallInt() || got.SmallInt() != 42 {
		t.Errorf("got %v, want 42", got)
	}
	if mb.Count() != 0 {
		t.Errorf("Count after receive: got %d, want 0", mb.Count())
	}
}

func TestMailbox_TryReceive_Empty(t *testing.T) {
	mb := NewMailbox(10)
	_, ok := mb.TryReceive()
	if ok {
		t.Error("TryReceive on empty mailbox should return false")
	}
}

func TestMailbox_BoundedCapacity(t *testing.T) {
	mb := NewMailbox(3)

	mb.TrySend(FromSmallInt(1))
	mb.TrySend(FromSmallInt(2))
	mb.TrySend(FromSmallInt(3))

	if mb.TrySend(FromSmallInt(4)) {
		t.Error("TrySend should fail when mailbox is full")
	}
}

func TestMailbox_FIFO(t *testing.T) {
	mb := NewMailbox(10)
	for i := int64(0); i < 5; i++ {
		mb.TrySend(FromSmallInt(i))
	}
	for i := int64(0); i < 5; i++ {
		got, ok := mb.TryReceive()
		if !ok {
			t.Fatalf("TryReceive %d failed", i)
		}
		if got.SmallInt() != i {
			t.Errorf("FIFO order: got %d, want %d", got.SmallInt(), i)
		}
	}
}

func TestMailbox_BlockingReceive(t *testing.T) {
	mb := NewMailbox(10)
	var got Value
	var wg sync.WaitGroup
	wg.Add(1)

	go func() {
		defer wg.Done()
		val, ok := mb.Receive()
		if ok {
			got = val
		}
	}()

	// Give receiver time to block
	time.Sleep(10 * time.Millisecond)
	mb.TrySend(FromSmallInt(99))
	wg.Wait()

	if !got.IsSmallInt() || got.SmallInt() != 99 {
		t.Errorf("blocking receive: got %v, want 99", got)
	}
}

func TestMailbox_ReceiveTimeout_Success(t *testing.T) {
	mb := NewMailbox(10)
	mb.TrySend(FromSmallInt(42))

	got, ok := mb.ReceiveTimeout(100 * time.Millisecond)
	if !ok {
		t.Fatal("ReceiveTimeout should succeed when message available")
	}
	if got.SmallInt() != 42 {
		t.Errorf("got %d, want 42", got.SmallInt())
	}
}

func TestMailbox_ReceiveTimeout_Timeout(t *testing.T) {
	mb := NewMailbox(10)

	start := time.Now()
	_, ok := mb.ReceiveTimeout(50 * time.Millisecond)
	elapsed := time.Since(start)

	if ok {
		t.Error("ReceiveTimeout should return false on timeout")
	}
	if elapsed < 40*time.Millisecond {
		t.Errorf("timed out too fast: %v", elapsed)
	}
}

func TestMailbox_Close_WakesReceiver(t *testing.T) {
	mb := NewMailbox(10)
	var wg sync.WaitGroup
	wg.Add(1)

	var ok bool
	go func() {
		defer wg.Done()
		_, ok = mb.Receive()
	}()

	time.Sleep(10 * time.Millisecond)
	mb.Close()
	wg.Wait()

	if ok {
		t.Error("Receive on closed empty mailbox should return false")
	}
}

func TestMailbox_Close_RejectsSend(t *testing.T) {
	mb := NewMailbox(10)
	mb.Close()
	if mb.TrySend(FromSmallInt(1)) {
		t.Error("TrySend on closed mailbox should return false")
	}
}

func TestMailbox_Close_DrainRemaining(t *testing.T) {
	mb := NewMailbox(10)
	mb.TrySend(FromSmallInt(1))
	mb.TrySend(FromSmallInt(2))
	mb.Close()

	// Can still read existing messages
	got, ok := mb.TryReceive()
	if !ok || got.SmallInt() != 1 {
		t.Error("should be able to read existing messages after close")
	}
	got, ok = mb.TryReceive()
	if !ok || got.SmallInt() != 2 {
		t.Error("should be able to read second message after close")
	}
	// Now empty
	_, ok = mb.TryReceive()
	if ok {
		t.Error("should be empty after draining")
	}
}

func TestMailbox_ConcurrentSenders(t *testing.T) {
	mb := NewMailbox(1000)
	var wg sync.WaitGroup
	n := 100

	for i := 0; i < n; i++ {
		wg.Add(1)
		go func(val int64) {
			defer wg.Done()
			mb.TrySend(FromSmallInt(val))
		}(int64(i))
	}
	wg.Wait()

	if mb.Count() != n {
		t.Errorf("Count after %d concurrent sends: got %d", n, mb.Count())
	}
}

func TestMailbox_Snapshot(t *testing.T) {
	mb := NewMailbox(10)
	mb.TrySend(FromSmallInt(1))
	mb.TrySend(FromSmallInt(2))
	mb.TrySend(FromSmallInt(3))

	snap, ver := mb.Snapshot()
	if len(snap) != 3 {
		t.Fatalf("snapshot length: got %d, want 3", len(snap))
	}
	if snap[0].SmallInt() != 1 || snap[1].SmallInt() != 2 || snap[2].SmallInt() != 3 {
		t.Error("snapshot values wrong")
	}

	// Snapshot should not consume messages
	if mb.Count() != 3 {
		t.Errorf("Count after snapshot: got %d, want 3", mb.Count())
	}
	_ = ver
}

func TestMailbox_RemoveAtVersion(t *testing.T) {
	mb := NewMailbox(10)
	mb.TrySend(FromSmallInt(10))
	mb.TrySend(FromSmallInt(20))
	mb.TrySend(FromSmallInt(30))

	_, ver := mb.Snapshot()

	// Remove middle element
	got, ok := mb.RemoveAtVersion(1, ver)
	if !ok {
		t.Fatal("RemoveAtVersion should succeed with correct version")
	}
	if got.SmallInt() != 20 {
		t.Errorf("removed value: got %d, want 20", got.SmallInt())
	}
	if mb.Count() != 2 {
		t.Errorf("Count after remove: got %d, want 2", mb.Count())
	}

	// Remaining should be [10, 30]
	v1, _ := mb.TryReceive()
	v2, _ := mb.TryReceive()
	if v1.SmallInt() != 10 || v2.SmallInt() != 30 {
		t.Errorf("remaining: got [%d, %d], want [10, 30]", v1.SmallInt(), v2.SmallInt())
	}
}

func TestMailbox_RemoveAtVersion_VersionMismatch(t *testing.T) {
	mb := NewMailbox(10)
	mb.TrySend(FromSmallInt(1))

	_, ver := mb.Snapshot()

	// Mutate mailbox (changes version)
	mb.TrySend(FromSmallInt(2))

	// Attempt remove with stale version
	_, ok := mb.RemoveAtVersion(0, ver)
	if ok {
		t.Error("RemoveAtVersion should fail with stale version")
	}
}

func TestMailbox_WaitForChange(t *testing.T) {
	mb := NewMailbox(10)
	_, ver := mb.Snapshot()

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		mb.WaitForChange(ver)
	}()

	time.Sleep(10 * time.Millisecond)
	mb.TrySend(FromSmallInt(1)) // changes version
	wg.Wait() // should unblock
}

func TestMailbox_RingBufferWrapAround(t *testing.T) {
	mb := NewMailbox(3)

	// Fill and drain to advance head/tail pointers
	mb.TrySend(FromSmallInt(1))
	mb.TrySend(FromSmallInt(2))
	mb.TryReceive()
	mb.TryReceive()
	// Now head=2, tail=2, count=0

	// Send 3 more (wraps around)
	mb.TrySend(FromSmallInt(10))
	mb.TrySend(FromSmallInt(20))
	mb.TrySend(FromSmallInt(30))

	for _, want := range []int64{10, 20, 30} {
		got, ok := mb.TryReceive()
		if !ok {
			t.Fatalf("TryReceive failed for %d", want)
		}
		if got.SmallInt() != want {
			t.Errorf("got %d, want %d", got.SmallInt(), want)
		}
	}
}
