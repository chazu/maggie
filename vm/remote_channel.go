package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// RemoteChannelRef: proxy for a channel on another node
// ---------------------------------------------------------------------------

// RemoteChannelRef is the Go-side data for a channel that lives on a remote
// node. Operations are forwarded via injected callback functions, following
// the same pattern as NodeRefData.
type RemoteChannelRef struct {
	OwnerNode [32]byte // NodeID of the owning node
	ChannelID uint64   // export ID on the owning node
	Capacity  int      // buffer capacity (0 = unbuffered)
	closed    atomic.Bool

	// RPC callbacks — injected by cmd/mag to avoid import cycles.
	SendFunc       func(channelID uint64, data []byte) error
	ReceiveFunc    func(channelID uint64) ([]byte, bool, error) // (data, ok, error)
	TrySendFunc    func(channelID uint64, data []byte) (bool, error)
	TryReceiveFunc func(channelID uint64) ([]byte, bool, bool, error) // (data, gotValue, ok, error)
	CloseFunc      func(channelID uint64) error
	StatusFunc     func(channelID uint64) (size int, capacity int, closed bool, err error)
}

// IsClosed returns true if the remote channel is known to be closed.
func (r *RemoteChannelRef) IsClosed() bool { return r.closed.Load() }

// MarkClosed sets the closed flag (called on close notification or node death).
func (r *RemoteChannelRef) MarkClosed() { r.closed.Store(true) }

// ---------------------------------------------------------------------------
// NaN-boxing: RemoteChannel values use remoteChannelMarker (55 << 24)
// ---------------------------------------------------------------------------

func remoteChannelToValue(id int) Value {
	return FromSymbolID(uint32(id) | remoteChannelMarker)
}

func isRemoteChannelValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == remoteChannelMarker
}

func remoteChannelIDFromValue(v Value) int {
	return int(v.SymbolID() & ^uint32(0xFF<<24))
}

// IsRemoteChannelValue returns true if v is a remote channel reference.
func IsRemoteChannelValue(v Value) bool { return isRemoteChannelValue(v) }

// ---------------------------------------------------------------------------
// VM-local remote channel registry
// ---------------------------------------------------------------------------

type remoteChannelRegistry struct {
	mu       sync.RWMutex
	channels map[int]*RemoteChannelRef
	nextID   atomic.Int32
}

func newRemoteChannelRegistry() *remoteChannelRegistry {
	r := &remoteChannelRegistry{
		channels: make(map[int]*RemoteChannelRef),
	}
	r.nextID.Store(1)
	return r
}

func (r *remoteChannelRegistry) register(ref *RemoteChannelRef) int {
	id := int(r.nextID.Add(1) - 1)
	r.mu.Lock()
	r.channels[id] = ref
	r.mu.Unlock()
	return id
}

func (r *remoteChannelRegistry) get(id int) *RemoteChannelRef {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.channels[id]
}

// drainNode marks all remote channels from the given node as closed.
func (r *remoteChannelRegistry) drainNode(nodeID [32]byte) {
	r.mu.RLock()
	defer r.mu.RUnlock()
	for _, ref := range r.channels {
		if ref.OwnerNode == nodeID {
			ref.MarkClosed()
		}
	}
}

// ---------------------------------------------------------------------------
// Channel export registry: maps local channels to export IDs for remote access
// ---------------------------------------------------------------------------

type channelExportRegistry struct {
	mu      sync.RWMutex
	exports map[uint64]*ChannelObject
	nextID  atomic.Uint64
}

func newChannelExportRegistry() *channelExportRegistry {
	r := &channelExportRegistry{
		exports: make(map[uint64]*ChannelObject),
	}
	r.nextID.Store(1)
	return r
}

// Export registers a local channel for remote access and returns its export ID.
// If the channel is already exported, returns the existing ID.
func (r *channelExportRegistry) Export(ch *ChannelObject) uint64 {
	r.mu.Lock()
	defer r.mu.Unlock()

	// Dedup: check if already exported
	for id, existing := range r.exports {
		if existing == ch {
			return id
		}
	}

	id := r.nextID.Add(1) - 1
	r.exports[id] = ch
	return id
}

// Lookup returns the local channel for an export ID, or nil.
func (r *channelExportRegistry) Lookup(id uint64) *ChannelObject {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.exports[id]
}

// ---------------------------------------------------------------------------
// VM helpers
// ---------------------------------------------------------------------------

func (vm *VM) registerRemoteChannel(ref *RemoteChannelRef) Value {
	id := vm.remoteChannels.register(ref)
	return remoteChannelToValue(id)
}

func (vm *VM) getRemoteChannel(v Value) *RemoteChannelRef {
	if !isRemoteChannelValue(v) {
		return nil
	}
	id := remoteChannelIDFromValue(v)
	return vm.remoteChannels.get(id)
}

// ExportChannel registers a local channel for remote access.
func (vm *VM) ExportChannel(ch *ChannelObject) uint64 {
	return vm.channelExports.Export(ch)
}

// LookupExportedChannel returns the local channel for an export ID.
func (vm *VM) LookupExportedChannel(id uint64) *ChannelObject {
	return vm.channelExports.Lookup(id)
}

// RegisterRemoteChannelValue creates and registers a RemoteChannelRef.
// Exported for use by integration tests and cmd/mag wiring.
func (vm *VM) RegisterRemoteChannelValue(ref *RemoteChannelRef) Value {
	return vm.registerRemoteChannel(ref)
}

// GetRemoteChannel returns the RemoteChannelRef for a remote channel value.
func (vm *VM) GetRemoteChannel(v Value) *RemoteChannelRef {
	return vm.getRemoteChannel(v)
}

// findChannelValue finds the NaN-boxed Value for a ChannelObject by scanning
// the concurrency registry. Used when deserializing a channel reference that
// turns out to be local.
func (vm *VM) findChannelValue(target *ChannelObject) Value {
	vm.registry.channelsMu.RLock()
	defer vm.registry.channelsMu.RUnlock()
	for id, ch := range vm.registry.channels {
		if ch == target {
			return channelToValue(id)
		}
	}
	return Nil
}

// DrainRemoteChannels marks all remote channels from the given node as closed.
func (vm *VM) DrainRemoteChannels(nodeID [32]byte) {
	vm.remoteChannels.drainNode(nodeID)
}

// ---------------------------------------------------------------------------
// RemoteChannel primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerRemoteChannelPrimitives() {
	c := vm.createClass("RemoteChannel", vm.ObjectClass)
	vm.RemoteChannelClass = c
	vm.Globals["RemoteChannel"] = vm.classValue(c)
	vm.symbolDispatch.Register(remoteChannelMarker, &SymbolTypeEntry{Class: c})

	// RemoteChannel>>send: value — blocking send to remote channel
	c.AddMethod1(vm.Selectors, "send:", func(vmPtr interface{}, recv, val Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.SendFunc == nil {
			return Nil
		}
		if ref.IsClosed() {
			return Nil
		}
		data, err := v.SerializeValue(val)
		if err != nil {
			return Nil
		}
		if err := ref.SendFunc(ref.ChannelID, data); err != nil {
			return Nil
		}
		return recv
	})

	// RemoteChannel>>receive — blocking receive from remote channel
	c.AddMethod0(vm.Selectors, "receive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.ReceiveFunc == nil {
			return Nil
		}
		if ref.IsClosed() {
			return Nil
		}
		data, ok, err := ref.ReceiveFunc(ref.ChannelID)
		if err != nil || !ok {
			ref.MarkClosed()
			return Nil
		}
		val, err := v.DeserializeValue(data)
		if err != nil {
			return Nil
		}
		return val
	})

	// RemoteChannel>>trySend: value — non-blocking send
	c.AddMethod1(vm.Selectors, "trySend:", func(vmPtr interface{}, recv, val Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.TrySendFunc == nil {
			return False
		}
		if ref.IsClosed() {
			return False
		}
		data, err := v.SerializeValue(val)
		if err != nil {
			return False
		}
		sent, err := ref.TrySendFunc(ref.ChannelID, data)
		if err != nil {
			return False
		}
		return FromBool(sent)
	})

	// RemoteChannel>>tryReceive — non-blocking receive
	c.AddMethod0(vm.Selectors, "tryReceive", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.TryReceiveFunc == nil {
			return Nil
		}
		if ref.IsClosed() {
			return Nil
		}
		data, gotValue, ok, err := ref.TryReceiveFunc(ref.ChannelID)
		if err != nil || !gotValue {
			if !ok {
				ref.MarkClosed()
			}
			return Nil
		}
		val, err := v.DeserializeValue(data)
		if err != nil {
			return Nil
		}
		return val
	})

	// RemoteChannel>>close
	c.AddMethod0(vm.Selectors, "close", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.CloseFunc == nil {
			return recv
		}
		if ref.IsClosed() {
			return recv
		}
		ref.CloseFunc(ref.ChannelID)
		ref.MarkClosed()
		return recv
	})

	// RemoteChannel>>isClosed
	c.AddMethod0(vm.Selectors, "isClosed", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil {
			return True
		}
		return FromBool(ref.IsClosed())
	})

	// RemoteChannel>>isEmpty
	c.AddMethod0(vm.Selectors, "isEmpty", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.StatusFunc == nil {
			return True
		}
		size, _, _, err := ref.StatusFunc(ref.ChannelID)
		if err != nil {
			return True
		}
		return FromBool(size == 0)
	})

	// RemoteChannel>>size
	c.AddMethod0(vm.Selectors, "size", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil || ref.StatusFunc == nil {
			return FromSmallInt(0)
		}
		size, _, _, err := ref.StatusFunc(ref.ChannelID)
		if err != nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(size))
	})

	// RemoteChannel>>capacity
	c.AddMethod0(vm.Selectors, "capacity", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(ref.Capacity))
	})

	// RemoteChannel>>printString
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getRemoteChannel(recv)
		if ref == nil {
			return v.registry.NewStringValue("a RemoteChannel (invalid)")
		}
		status := "open"
		if ref.IsClosed() {
			status = "closed"
		}
		return v.registry.NewStringValue(fmt.Sprintf("a RemoteChannel(%d, %s)", ref.ChannelID, status))
	})

	// RemoteChannel>>onReceive: — for Channel select integration
	c.AddMethod1(vm.Selectors, "onReceive:", func(vmPtr interface{}, recv, handler Value) Value {
		v := vmPtr.(*VM)
		return v.createAssociation(recv, handler)
	})
}
