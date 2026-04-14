package vm

import (
	"sync"
	"time"
)

// DefaultMailboxCapacity is the default bounded capacity for process mailboxes.
const DefaultMailboxCapacity = 4096

// Mailbox is a bounded, mutex-protected FIFO with selective receive support.
// Each process gets one mailbox. Senders enqueue messages with TrySend;
// receivers dequeue with Receive (blocking), ReceiveTimeout, TryReceive,
// or ReceiveMatching (selective, version-checked).
type Mailbox struct {
	mu       sync.Mutex
	cond     *sync.Cond
	buf      []Value
	head     int
	tail     int
	count    int
	capacity int
	closed   bool
	version  uint64 // incremented on every mutation for selective receive
}

// NewMailbox creates a mailbox with the given capacity.
func NewMailbox(capacity int) *Mailbox {
	m := &Mailbox{
		buf:      make([]Value, capacity),
		capacity: capacity,
	}
	m.cond = sync.NewCond(&m.mu)
	return m
}

// TrySend enqueues a message. Returns false if the mailbox is full or closed.
// This is the only send operation — it never blocks. Backpressure is the
// caller's responsibility.
func (m *Mailbox) TrySend(msg Value) bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.closed || m.count >= m.capacity {
		return false
	}
	m.buf[m.tail] = msg
	m.tail = (m.tail + 1) % m.capacity
	m.count++
	m.version++
	m.cond.Broadcast()
	return true
}

// Receive blocks until a message is available or the mailbox is closed.
// Returns (value, true) on success, (Nil, false) if closed with no messages.
func (m *Mailbox) Receive() (Value, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	for m.count == 0 && !m.closed {
		m.cond.Wait()
	}
	if m.count == 0 {
		return Nil, false
	}
	return m.dequeue(), true
}

// ReceiveTimeout blocks for at most the given duration.
// Returns (value, true) on success, (Nil, false) on timeout or closed.
func (m *Mailbox) ReceiveTimeout(timeout time.Duration) (Value, bool) {
	deadline := time.Now().Add(timeout)
	m.mu.Lock()
	defer m.mu.Unlock()

	for m.count == 0 && !m.closed {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			return Nil, false
		}
		// Timer goroutine wakes us on timeout
		done := make(chan struct{})
		go func() {
			timer := time.NewTimer(remaining)
			defer timer.Stop()
			select {
			case <-timer.C:
				m.cond.Broadcast()
			case <-done:
			}
		}()
		m.cond.Wait()
		close(done)
	}
	if m.count == 0 {
		return Nil, false
	}
	return m.dequeue(), true
}

// TryReceive is non-blocking. Returns (value, true) if a message was
// available, (Nil, false) otherwise.
func (m *Mailbox) TryReceive() (Value, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.count == 0 {
		return Nil, false
	}
	return m.dequeue(), true
}

// Snapshot returns a copy of current messages and the current version.
// Used by the two-phase selective receive: snapshot outside the lock,
// evaluate predicate, then remove under version check.
func (m *Mailbox) Snapshot() ([]Value, uint64) {
	m.mu.Lock()
	defer m.mu.Unlock()
	snap := make([]Value, m.count)
	for i := 0; i < m.count; i++ {
		snap[i] = m.buf[(m.head+i)%m.capacity]
	}
	return snap, m.version
}

// RemoveAtVersion removes the message at logicalIndex if version matches.
// Returns (msg, true) on success, (Nil, false) if version changed.
func (m *Mailbox) RemoveAtVersion(logicalIndex int, expectedVersion uint64) (Value, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.version != expectedVersion || logicalIndex >= m.count {
		return Nil, false
	}
	idx := (m.head + logicalIndex) % m.capacity
	val := m.buf[idx]
	m.removeAt(logicalIndex)
	m.version++
	return val, true
}

// WaitForChange blocks until the version changes from expectedVersion
// or the mailbox is closed.
func (m *Mailbox) WaitForChange(expectedVersion uint64) {
	m.mu.Lock()
	defer m.mu.Unlock()
	for m.version == expectedVersion && !m.closed {
		m.cond.Wait()
	}
}

// Close marks the mailbox as closed. Wakes all waiting receivers.
// Future TrySend calls return false. Remaining messages can still be read.
func (m *Mailbox) Close() {
	m.mu.Lock()
	m.closed = true
	m.cond.Broadcast()
	m.mu.Unlock()
}

// IsClosed returns whether the mailbox has been closed.
func (m *Mailbox) IsClosed() bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.closed
}

// Count returns the number of messages currently in the mailbox.
func (m *Mailbox) Count() int {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.count
}

// dequeue removes and returns the head element. Caller must hold mu.
func (m *Mailbox) dequeue() Value {
	val := m.buf[m.head]
	m.buf[m.head] = Nil // clear for GC
	m.head = (m.head + 1) % m.capacity
	m.count--
	m.version++
	return val
}

// removeAt removes element at logical index i (0 = head). Caller must hold mu.
func (m *Mailbox) removeAt(logicalIndex int) {
	if logicalIndex == 0 {
		m.dequeue()
		return
	}
	// Shift elements toward the head to fill the gap
	for j := logicalIndex; j > 0; j-- {
		dst := (m.head + j) % m.capacity
		src := (m.head + j - 1) % m.capacity
		m.buf[dst] = m.buf[src]
	}
	m.buf[m.head] = Nil // clear for GC
	m.head = (m.head + 1) % m.capacity
	m.count--
}
