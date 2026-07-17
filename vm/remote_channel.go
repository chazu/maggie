package vm

import (
	"fmt"
	"sync"
	"sync/atomic"
	"unsafe"
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
// RemoteChannel values are pointer-carrying kindRemoteChannel heap Values.
// ---------------------------------------------------------------------------

func isRemoteChannelValue(v Value) bool {
	return v.ptr != nil && v.hi == kindRemoteChannel
}

// IsRemoteChannelValue returns true if v is a remote channel reference.
func IsRemoteChannelValue(v Value) bool { return isRemoteChannelValue(v) }

// ---------------------------------------------------------------------------
// VM-local remote channel registry
// ---------------------------------------------------------------------------

// remoteChannelRegistry tracks every live RemoteChannelRef proxy so that
// drainNode can mark all proxies of a dead node closed. Proxies are
// pointer-carrying kindRemoteChannel Values (Go GC owns their lifetime); this
// set is purely functional (node-death fan-out), not a liveness root.
type remoteChannelRegistry struct {
	mu       sync.RWMutex
	channels map[*RemoteChannelRef]struct{}
}

func newRemoteChannelRegistry() *remoteChannelRegistry {
	return &remoteChannelRegistry{
		channels: make(map[*RemoteChannelRef]struct{}),
	}
}

func (r *remoteChannelRegistry) track(ref *RemoteChannelRef) {
	r.mu.Lock()
	r.channels[ref] = struct{}{}
	r.mu.Unlock()
}

// drainNode marks all remote channels from the given node as closed and
// removes them from the tracking set — once closed, their node-death fan-out
// role is finished, so keeping them would only grow the set forever.
func (r *remoteChannelRegistry) drainNode(nodeID [32]byte) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for ref := range r.channels {
		if ref.OwnerNode == nodeID {
			ref.MarkClosed()
			delete(r.channels, ref)
		}
	}
}

// ---------------------------------------------------------------------------
// Channel export registry: maps local channels to export IDs for remote access
// ---------------------------------------------------------------------------

type channelExportRegistry struct {
	mu      sync.RWMutex
	exports map[uint64]*ChannelObject
	byChan  map[*ChannelObject]uint64 // reverse index for O(1) dedup
	nextID  atomic.Uint64
}

func newChannelExportRegistry() *channelExportRegistry {
	r := &channelExportRegistry{
		exports: make(map[uint64]*ChannelObject),
		byChan:  make(map[*ChannelObject]uint64),
	}
	r.nextID.Store(1)
	return r
}

// Export registers a local channel for remote access and returns its export ID.
// If the channel is already exported, returns the existing ID.
func (r *channelExportRegistry) Export(ch *ChannelObject) uint64 {
	r.mu.RLock()
	if id, ok := r.byChan[ch]; ok {
		r.mu.RUnlock()
		return id
	}
	r.mu.RUnlock()

	r.mu.Lock()
	defer r.mu.Unlock()
	if id, ok := r.byChan[ch]; ok {
		return id
	}
	id := r.nextID.Add(1) - 1
	r.exports[id] = ch
	r.byChan[ch] = id
	return id
}

// Lookup returns the local channel for an export ID, or nil.
func (r *channelExportRegistry) Lookup(id uint64) *ChannelObject {
	r.mu.RLock()
	defer r.mu.RUnlock()
	return r.exports[id]
}

// Unexport removes a channel from the export tables. Serializing a channel
// exports it as a side effect; without an unexport path every channel that
// ever crossed the wire would be pinned against the Go GC for the life of
// the VM (SD-11).
func (r *channelExportRegistry) Unexport(ch *ChannelObject) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if id, ok := r.byChan[ch]; ok {
		delete(r.exports, id)
		delete(r.byChan, ch)
	}
}

// ---------------------------------------------------------------------------
// VM helpers
// ---------------------------------------------------------------------------

func (vm *VM) registerRemoteChannel(ref *RemoteChannelRef) Value {
	// The ref is a pointer-carrying kindRemoteChannel Value (below). It is also
	// tracked in the registry, whose sole remaining role is drainNode: marking
	// every remote channel of a dead node as closed (a proxy can be closed even
	// while a live Value still points at it).
	vm.remoteChannels.track(ref)
	return makeHeap(kindRemoteChannel, unsafe.Pointer(ref))
}

func (vm *VM) getRemoteChannel(v Value) *RemoteChannelRef {
	if !isRemoteChannelValue(v) {
		return nil
	}
	return (*RemoteChannelRef)(v.ptr)
}

// ExportChannel registers a local channel for remote access.
func (vm *VM) ExportChannel(ch *ChannelObject) uint64 {
	return vm.channelExports.Export(ch)
}

// LookupExportedChannel returns the local channel for an export ID.
func (vm *VM) LookupExportedChannel(id uint64) *ChannelObject {
	return vm.channelExports.Lookup(id)
}

// UnexportChannel removes a channel from the export registry. Called when a
// closed exported channel has fully drained — remote peers observing the
// missing export treat it as closed.
func (vm *VM) UnexportChannel(ch *ChannelObject) {
	vm.channelExports.Unexport(ch)
}

// CloseChannel closes ch and, if it has no buffered values left, releases its
// remote export. A closed channel with buffered values stays exported so
// remote receivers can drain it; the drain path unexports on the final
// receive.
func (vm *VM) CloseChannel(ch *ChannelObject) {
	ch.Close()
	if ch.Size() == 0 {
		vm.channelExports.Unexport(ch)
	}
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

// findChannelValue returns the heap Value for a ChannelObject. Used when
// deserializing a channel reference that turns out to be local. With
// pointer-carrying channel Values this is a direct wrap — no registry scan.
func (vm *VM) findChannelValue(target *ChannelObject) Value {
	if target == nil {
		return Nil
	}
	return channelToValue(target)
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
	vm.globals["RemoteChannel"] = vm.classValue(c)
	// Class resolution is via classForHeap's kindRemoteChannel case (pointer Value).

	// RemoteChannel>>send: value — blocking send to remote channel
	c.AddMethod1(vm.Selectors, "send:", func(v *VM, recv, val Value) Value {
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
	c.AddMethod0(vm.Selectors, "receive", func(v *VM, recv Value) Value {
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
	c.AddMethod1(vm.Selectors, "trySend:", func(v *VM, recv, val Value) Value {
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
	c.AddMethod0(vm.Selectors, "tryReceive", func(v *VM, recv Value) Value {
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
	c.AddMethod0(vm.Selectors, "close", func(v *VM, recv Value) Value {
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
	c.AddMethod0(vm.Selectors, "isClosed", func(v *VM, recv Value) Value {
		ref := v.getRemoteChannel(recv)
		if ref == nil {
			return True
		}
		return FromBool(ref.IsClosed())
	})

	// RemoteChannel>>isEmpty
	c.AddMethod0(vm.Selectors, "isEmpty", func(v *VM, recv Value) Value {
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
	c.AddMethod0(vm.Selectors, "size", func(v *VM, recv Value) Value {
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
	c.AddMethod0(vm.Selectors, "capacity", func(v *VM, recv Value) Value {
		ref := v.getRemoteChannel(recv)
		if ref == nil {
			return FromSmallInt(0)
		}
		return FromSmallInt(int64(ref.Capacity))
	})

	// RemoteChannel>>printString
	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
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
	c.AddMethod1(vm.Selectors, "onReceive:", func(v *VM, recv, handler Value) Value {
		return v.createAssociation(recv, handler)
	})
}
