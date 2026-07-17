package vm

import (
	"encoding/hex"
	"fmt"
	"unsafe"
)

// Cluster selectors delivered to the __cluster__ event process by the membership
// core (via DeliverClusterEvent). The Maggie Cluster loop dispatches on these.
const (
	SelectorMemberUp         = "memberUp:"
	SelectorMemberDown       = "memberDown:"
	SelectorMemberDiscovered = "memberDiscovered:"
)

// ClusterEventProcessName is the registered name of the Maggie process that
// receives membership events. Reused as the peer-monitored sentinel so
// mixed-version peers still find a __cluster__ process to monitor.
const ClusterEventProcessName = "__cluster__"

// registerClusterPrimitives bootstraps the ClusterMember data class and the
// Go-only ClusterRuntime class whose class-side primitives delegate to the
// installed ClusterCore. The user-facing Cluster class lives in lib/Cluster.mag
// and calls ClusterRuntime — so there is no dual Go/lib definition of Cluster.
func (vm *VM) registerClusterPrimitives() {
	vm.registerClusterMemberClass()
	vm.registerClusterRuntimeClass()
}

func (vm *VM) registerClusterMemberClass() {
	c := NewClassWithInstVars("ClusterMember", vm.ObjectClass, []string{"id", "addr", "status", "metadata"})
	c.NumSlots = 4
	c.NonSerializable = true // wraps a live node handle; must not cross the wire
	vm.ClusterMemberClass = c
	vm.Classes.Register(c)
	vm.globals["ClusterMember"] = vm.classValue(c)

	sel := vm.Selectors

	// id — hex-encoded 32-byte node id.
	c.AddMethod0(sel, "id", func(v *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return v.registry.NewStringValue(hex.EncodeToString([]byte(v.registry.GetStringContent(obj.GetSlot(0)))))
	})

	// addr — host:port string.
	c.AddMethod0(sel, "addr", func(v *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(1)
	})

	// status — #alive / #suspect / #dead / #left.
	c.AddMethod0(sel, "status", func(v *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		name := "unknown"
		if st := obj.GetSlot(2); st.IsSmallInt() {
			switch uint8(st.SmallInt()) {
			case ClusterStatusAlive:
				name = "alive"
			case ClusterStatusSuspect:
				name = "suspect"
			case ClusterStatusDead:
				name = "dead"
			case ClusterStatusLeft:
				name = "left"
			}
		}
		return v.Symbols.SymbolValue(name)
	})

	// metadata — the gossiped labels as a Dictionary.
	c.AddMethod0(sel, "metadata", func(v *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(3)
	})

	// isConnected — true if the core currently holds a connection to this member.
	c.AddMethod0(sel, "isConnected", func(v *VM, recv Value) Value {
		core := v.GetClusterCore()
		id, ok := clusterMemberID(v, recv)
		if core == nil || !ok {
			return False
		}
		if core.ConnectedRef(id) != nil {
			return True
		}
		return False
	})

	// node — the Node value for this member, reusing the core's existing ref
	// (nil if not connected). Placement (nodeFor: → spawnOn:) uses this.
	c.AddMethod0(sel, "node", func(v *VM, recv Value) Value {
		core := v.GetClusterCore()
		id, ok := clusterMemberID(v, recv)
		if core == nil || !ok {
			return Nil
		}
		ref := core.ConnectedRef(id)
		if ref == nil {
			return Nil
		}
		return makeHeap(kindRemoteRef, unsafe.Pointer(ref))
	})

	// connect — asynchronously establish a connection to this member (safe from
	// an event handler; never blocks). Answers self.
	c.AddMethod0(sel, "connect", func(v *VM, recv Value) Value {
		core := v.GetClusterCore()
		obj := ObjectFromValue(recv)
		if core == nil || obj == nil {
			return recv
		}
		if addr := v.registry.GetStringContent(obj.GetSlot(1)); addr != "" {
			core.ConnectAsync(addr)
		}
		return recv
	})

	c.AddMethod0(sel, "printString", func(v *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return v.registry.NewStringValue("a ClusterMember (invalid)")
		}
		return v.registry.NewStringValue(fmt.Sprintf("a ClusterMember(%s)", v.registry.GetStringContent(obj.GetSlot(1))))
	})
}

func (vm *VM) registerClusterRuntimeClass() {
	c := vm.createClass("ClusterRuntime", vm.ObjectClass)
	vm.globals["ClusterRuntime"] = vm.classValue(c)
	sel := vm.Selectors

	// ClusterRuntime members — Array of ClusterMember (Alive, self-excluded).
	c.AddClassMethod0(sel, "members", func(v *VM, recv Value) Value {
		core := v.GetClusterCore()
		if core == nil {
			return v.NewArrayWithElements(nil)
		}
		infos := core.AliveMemberInfos()
		elems := make([]Value, len(infos))
		for i, info := range infos {
			elems[i] = v.newClusterMemberValue(info)
		}
		return v.NewArrayWithElements(elems)
	})

	// ClusterRuntime hasCore — true when a membership core is installed.
	c.AddClassMethod0(sel, "hasCore", func(v *VM, recv Value) Value {
		if v.GetClusterCore() != nil {
			return True
		}
		return False
	})

	// ClusterRuntime connect: addr — async connect to an address.
	c.AddClassMethod1(sel, "connect:", func(v *VM, recv, addrVal Value) Value {
		core := v.GetClusterCore()
		if core == nil {
			return recv
		}
		if addr := v.registry.GetStringContent(addrVal); addr != "" {
			core.ConnectAsync(addr)
		}
		return recv
	})

	// ClusterRuntime setJoinPolicy: #eager|#lazy|#manual
	c.AddClassMethod1(sel, "setJoinPolicy:", func(v *VM, recv, polVal Value) Value {
		core := v.GetClusterCore()
		if core == nil {
			return recv
		}
		pol := ""
		if polVal.IsSymbol() {
			pol = v.Symbols.Name(polVal.SymbolID())
		} else if IsStringValue(polVal) {
			pol = v.registry.GetStringContent(polVal)
		}
		core.SetJoinPolicy(pol)
		return recv
	})

	// ClusterRuntime setMetadata: aDictionary
	c.AddClassMethod1(sel, "setMetadata:", func(v *VM, recv, dictVal Value) Value {
		core := v.GetClusterCore()
		if core == nil {
			return recv
		}
		core.SetMetadata(v.dictToStringMap(dictVal))
		return recv
	})
}

// newClusterMemberValue builds a ClusterMember instance from a member info,
// entirely with pure-Go constructors so it is safe to build off the core
// goroutine (no interpreter dispatch).
func (vm *VM) newClusterMemberValue(info ClusterMemberInfo) Value {
	obj := NewObject(vm.ClusterMemberClass.VTable, 4)
	obj.SetSlot(0, vm.registry.NewStringValue(string(info.ID[:]))) // raw 32 bytes
	obj.SetSlot(1, vm.registry.NewStringValue(info.Addr))
	obj.SetSlot(2, FromSmallInt(int64(info.Status)))
	dict := vm.NewDictionary()
	for k, val := range info.Metadata {
		vm.DictionaryAtPut(dict, vm.registry.NewStringValue(k), vm.registry.NewStringValue(val))
	}
	obj.SetSlot(3, dict)
	return obj.ToValue()
}

// clusterMemberID extracts the raw 32-byte node id stored in a ClusterMember.
func clusterMemberID(vm *VM, recv Value) ([32]byte, bool) {
	obj := ObjectFromValue(recv)
	if obj == nil {
		return [32]byte{}, false
	}
	s := vm.registry.GetStringContent(obj.GetSlot(0))
	if len(s) != 32 {
		return [32]byte{}, false
	}
	var id [32]byte
	copy(id[:], s)
	return id, true
}

// dictToStringMap converts a Maggie Dictionary of string→string into a Go map,
// skipping non-string entries.
func (vm *VM) dictToStringMap(dictVal Value) map[string]string {
	d := vm.registry.GetDictionaryObject(dictVal)
	if d == nil {
		return nil
	}
	out := make(map[string]string)
	for _, e := range d.Entries() {
		if IsStringValue(e.Key) && IsStringValue(e.Value) {
			out[vm.registry.GetStringContent(e.Key)] = vm.registry.GetStringContent(e.Value)
		}
	}
	return out
}

// DeliverClusterEvent delivers a membership event to the Maggie __cluster__
// process's mailbox as a MailboxMessage carrying a ClusterMember. Best-effort:
// if the event loop is not running or the mailbox is full the event is dropped
// (the authoritative view remains available via ClusterRuntime members). Safe to
// call from the membership core's goroutines — same off-goroutine mailbox path
// the server uses for remote message delivery.
func (vm *VM) DeliverClusterEvent(selector string, info ClusterMemberInfo) {
	procVal := vm.LookupProcessName(ClusterEventProcessName)
	if procVal == Nil {
		return
	}
	proc := vm.ProcessFromValue(procVal)
	if proc == nil || proc.IsDone() {
		return
	}
	member := vm.newClusterMemberValue(info)
	msg := vm.CreateMailboxMessage(Nil, selector, member)
	proc.Mailbox().TrySend(msg)
}
