package vm

import (
	"crypto/ed25519"
	"crypto/rand"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"sync/atomic"
)

// ---------------------------------------------------------------------------
// NodeRefData: connection to a remote node (no dist package dependency)
// ---------------------------------------------------------------------------

// NodeRefData holds the Go-side data for a Node connection value.
// The actual gRPC client is injected via SendFunc to avoid importing
// vm/dist (which imports vm, creating a cycle).
type NodeRefData struct {
	Addr      string
	PublicKey ed25519.PublicKey  // our public key
	privKey   ed25519.PrivateKey // our private key
	nonce     atomic.Uint64

	// SendFunc is injected by the cmd/mag layer to perform the actual
	// gRPC DeliverMessage call. Returns (responsePayload, errorKind, errorMsg, err).
	SendFunc func(envelope []byte) ([]byte, string, string, error)

	// PingFunc checks if the remote node is reachable.
	PingFunc func() bool

	// Cross-node monitor/link RPCs (injected by cmd/mag)
	MonitorFunc   func(watcherID, refID uint64, targetName string) (*MonitorResponse, error)
	DemonitorFunc func(refID uint64) error

	// SpawnFunc sends a SpawnProcess RPC. Returns (processName, error).
	SpawnFunc SpawnFunc
}

// nodeIdentityHolder wraps node identity keys.
type nodeIdentityHolder struct {
	pub  ed25519.PublicKey
	priv ed25519.PrivateKey
}

// NewNodeRefData creates a NodeRefData. SendFunc and PingFunc must be set
// separately by the wiring layer, or use VM.SetNodeRefFactory to auto-wire.
func NewNodeRefData(addr string, pub ed25519.PublicKey, priv ed25519.PrivateKey) *NodeRefData {
	return &NodeRefData{
		Addr:      addr,
		PublicKey: pub,
		privKey:   priv,
	}
}

// NodeRefFactory creates a fully-wired NodeRefData (with SendFunc/PingFunc).
// Injected by cmd/mag to avoid import cycles.
type NodeRefFactory func(addr string, pub ed25519.PublicKey, priv ed25519.PrivateKey) *NodeRefData

// NextNonce returns a monotonically increasing nonce.
func (n *NodeRefData) NextNonce() uint64 { return n.nonce.Add(1) }

// SignEnvelope signs data with the node's private key.
func (n *NodeRefData) Sign(data []byte) []byte {
	return ed25519.Sign(n.privKey, data)
}

// NodeID returns the 32-byte node identifier.
func (n *NodeRefData) NodeID() [32]byte {
	var nid [32]byte
	copy(nid[:], n.PublicKey)
	return nid
}

// ---------------------------------------------------------------------------
// Lightweight envelope builder (avoids importing vm/dist)
// ---------------------------------------------------------------------------

// buildEnvelopeBytes constructs a CBOR-encoded MessageEnvelope without
// importing the dist package. Uses the same CBOR encoding as dist.MarshalEnvelope.
// This is a temporary solution until the package structure is refactored.
//
// For now, we build a simple envelope struct and marshal it with the same
// CBOR enc mode used by serial.go.
type envelopeData struct {
	SenderNode    [32]byte `cbor:"1,keyasint"`
	TargetProcess uint64   `cbor:"2,keyasint"`
	TargetName    string   `cbor:"3,keyasint,omitempty"`
	ReplyTo       *replyAddr `cbor:"4,keyasint,omitempty"`
	Selector      string   `cbor:"5,keyasint,omitempty"`
	Payload       []byte   `cbor:"6,keyasint"`
	ClassHints    [][32]byte `cbor:"7,keyasint,omitempty"`
	Nonce         uint64   `cbor:"8,keyasint"`
	Signature     []byte   `cbor:"9,keyasint"`
}

type replyAddr struct {
	NodeID    [32]byte `cbor:"1,keyasint"`
	ProcessID uint64   `cbor:"2,keyasint"`
}

// BuildSignedEnvelope constructs a signed CBOR-encoded envelope. Exported for
// use by cmd/mag wiring layer.
func BuildSignedEnvelope(ref *NodeRefData, targetName, selector string, payload []byte, wantReply bool) ([]byte, error) {
	return buildSignedEnvelope(ref, targetName, selector, payload, wantReply)
}

func buildSignedEnvelope(ref *NodeRefData, targetName, selector string, payload []byte, wantReply bool) ([]byte, error) {
	env := &envelopeData{
		SenderNode: ref.NodeID(),
		TargetName: targetName,
		Selector:   selector,
		Payload:    payload,
		Nonce:      ref.NextNonce(),
	}
	if wantReply {
		env.ReplyTo = &replyAddr{NodeID: ref.NodeID()}
	}

	// Sign: payload || nonce || targetProcess
	var sigBuf []byte
	sigBuf = append(sigBuf, payload...)
	var nonceBuf [8]byte
	binary.BigEndian.PutUint64(nonceBuf[:], env.Nonce)
	sigBuf = append(sigBuf, nonceBuf[:]...)
	var procBuf [8]byte
	binary.BigEndian.PutUint64(procBuf[:], env.TargetProcess)
	sigBuf = append(sigBuf, procBuf[:]...)
	env.Signature = ref.Sign(sigBuf)

	return cborSerialEncMode.Marshal(env)
}

// ---------------------------------------------------------------------------
// NaN-boxing: Node values use remoteRefMarker (43 << 24)
// ---------------------------------------------------------------------------

func nodeRefToValue(id int) Value {
	return FromSymbolID(uint32(id) | remoteRefMarker)
}

func isNodeRefValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == remoteRefMarker
}

func nodeRefIDFromValue(v Value) int {
	return int(v.SymbolID() & ^uint32(0xFF<<24))
}

// ---------------------------------------------------------------------------
// VM helpers
// ---------------------------------------------------------------------------

func (vm *VM) registerNodeRef(ref *NodeRefData) Value {
	vm.nodeRefsMu.Lock()
	id := int(vm.nodeRefID.Add(1) - 1)
	vm.nodeRefs[id] = ref
	vm.nodeRefsMu.Unlock()
	return nodeRefToValue(id)
}

func (vm *VM) getNodeRef(v Value) *NodeRefData {
	if !isNodeRefValue(v) {
		return nil
	}
	id := nodeRefIDFromValue(v)
	vm.nodeRefsMu.RLock()
	defer vm.nodeRefsMu.RUnlock()
	return vm.nodeRefs[id]
}

func (vm *VM) nodeIdentity() (ed25519.PublicKey, ed25519.PrivateKey) {
	if vm.localIdentity != nil {
		return vm.localIdentity.pub, vm.localIdentity.priv
	}
	// Generate ephemeral identity
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		return nil, nil
	}
	vm.localIdentity = &nodeIdentityHolder{pub: pub, priv: priv}
	return pub, priv
}

// SetNodeIdentityKeys sets the VM's local node identity keys.
func (vm *VM) SetNodeIdentityKeys(pub ed25519.PublicKey, priv ed25519.PrivateKey) {
	vm.localIdentity = &nodeIdentityHolder{pub: pub, priv: priv}
}

// ---------------------------------------------------------------------------
// Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerNodePrimitives() {
	c := vm.createClass("Node", vm.ObjectClass)
	vm.NodeClass = c
	vm.Globals["Node"] = vm.classValue(c)
	vm.symbolDispatch.Register(remoteRefMarker, &SymbolTypeEntry{Class: c})

	// Node class>>connect: addr
	c.AddClassMethod1(vm.Selectors, "connect:", func(vmPtr interface{}, recv, addr Value) Value {
		v := vmPtr.(*VM)
		addrStr := v.registry.GetStringContent(addr)
		if addrStr == "" {
			return Nil
		}
		pub, priv := v.nodeIdentity()
		if pub == nil {
			return Nil
		}
		var ref *NodeRefData
		if v.NodeRefFactory != nil {
			ref = v.NodeRefFactory(addrStr, pub, priv)
		} else {
			ref = NewNodeRefData(addrStr, pub, priv)
		}
		return v.registerNodeRef(ref)
	})

	// Node>>primAddr
	c.AddMethod0(vm.Selectors, "primAddr", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getNodeRef(recv)
		if ref == nil {
			return Nil
		}
		return v.registry.NewStringValue(ref.Addr)
	})

	// Node>>primPing
	c.AddMethod0(vm.Selectors, "primPing", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getNodeRef(recv)
		if ref == nil || ref.PingFunc == nil {
			return False
		}
		if ref.PingFunc() {
			return True
		}
		return False
	})

	// Node>>primProcessNamed: name
	c.AddMethod1(vm.Selectors, "primProcessNamed:", func(vmPtr interface{}, recv, name Value) Value {
		v := vmPtr.(*VM)
		ref := v.getNodeRef(recv)
		if ref == nil {
			return Nil
		}
		nameStr := v.registry.GetStringContent(name)
		if nameStr == "" {
			return Nil
		}
		return v.createRemoteProcess(recv, nameStr)
	})

	// Node>>primNodeID — hex-encoded 32-byte public key
	c.AddMethod0(vm.Selectors, "primNodeID", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getNodeRef(recv)
		if ref == nil {
			return Nil
		}
		nid := ref.NodeID()
		return v.registry.NewStringValue(hex.EncodeToString(nid[:]))
	})

	// Node>>printString
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ref := v.getNodeRef(recv)
		if ref == nil {
			return v.registry.NewStringValue("a Node (invalid)")
		}
		return v.registry.NewStringValue(fmt.Sprintf("a Node(%s)", ref.Addr))
	})
}

// ---------------------------------------------------------------------------
// RemoteProcess
// ---------------------------------------------------------------------------

func (vm *VM) createRemoteProcess(nodeRefVal Value, name string) Value {
	instance := NewObject(vm.RemoteProcessClass.VTable, 2)
	instance.SetSlot(0, nodeRefVal)
	instance.SetSlot(1, vm.registry.NewStringValue(name))
	vm.KeepAlive(instance)
	return instance.ToValue()
}

func (vm *VM) registerRemoteProcessPrimitives() {
	c := NewClassWithInstVars("RemoteProcess", vm.ObjectClass, []string{"node", "name"})
	c.NumSlots = 2
	vm.RemoteProcessClass = c
	vm.Classes.Register(c)
	vm.Globals["RemoteProcess"] = vm.classValue(c)

	// RemoteProcess>>primName
	c.AddMethod0(vm.Selectors, "primName", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(1)
	})

	// RemoteProcess>>primNode
	c.AddMethod0(vm.Selectors, "primNode", func(_ interface{}, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(0)
	})

	// RemoteProcess>>primCast:with: — fire-and-forget
	c.AddMethod2(vm.Selectors, "primCast:with:", func(vmPtr interface{}, recv, selectorVal, payload Value) Value {
		v := vmPtr.(*VM)
		return v.remoteSend(recv, selectorVal, payload, false)
	})

	// RemoteProcess>>primAsyncSend:with: — returns Future
	c.AddMethod2(vm.Selectors, "primAsyncSend:with:", func(vmPtr interface{}, recv, selectorVal, payload Value) Value {
		v := vmPtr.(*VM)
		return v.remoteSend(recv, selectorVal, payload, true)
	})

	// RemoteProcess>>printString
	c.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		obj := ObjectFromValue(recv)
		if obj == nil {
			return v.registry.NewStringValue("a RemoteProcess (invalid)")
		}
		name := v.registry.GetStringContent(obj.GetSlot(1))
		return v.registry.NewStringValue(fmt.Sprintf("a RemoteProcess(%s)", name))
	})
}

// RegisterNodeRef registers a NodeRefData and returns its NaN-boxed value.
// Exported for use by cmd/mag and integration tests.
func (vm *VM) RegisterNodeRef(ref *NodeRefData) Value {
	return vm.registerNodeRef(ref)
}

// CreateRemoteProcessValue creates a RemoteProcess object targeting a named
// process on the given node. Exported for integration tests.
func (vm *VM) CreateRemoteProcessValue(nodeRefVal Value, name string) Value {
	return vm.createRemoteProcess(nodeRefVal, name)
}

// RemoteSend is the exported version of remoteSend for integration tests.
func (vm *VM) RemoteSend(recv, selectorVal, payload Value, wantReply bool) Value {
	return vm.remoteSend(recv, selectorVal, payload, wantReply)
}

// remoteSend is the core implementation for both cast:with: and asyncSend:with:.
func (vm *VM) remoteSend(recv, selectorVal, payload Value, wantReply bool) Value {
	obj := ObjectFromValue(recv)
	if obj == nil {
		return Nil
	}

	nodeRefVal := obj.GetSlot(0)
	ref := vm.getNodeRef(nodeRefVal)
	if ref == nil || ref.SendFunc == nil {
		return Nil
	}

	nameVal := obj.GetSlot(1)
	targetName := ""
	if IsStringValue(nameVal) {
		targetName = vm.registry.GetStringContent(nameVal)
	}

	sel := ""
	if selectorVal.IsSymbol() && !IsStringValue(selectorVal) {
		sel = vm.Symbols.Name(selectorVal.SymbolID())
	} else if IsStringValue(selectorVal) {
		sel = vm.registry.GetStringContent(selectorVal)
	}

	// Serialize payload
	payloadBytes, err := vm.SerializeValue(payload)
	if err != nil {
		return Nil
	}

	// Build and sign envelope
	envelopeBytes, err := buildSignedEnvelope(ref, targetName, sel, payloadBytes, wantReply)
	if err != nil {
		return Nil
	}

	if !wantReply {
		// Fire-and-forget
		go ref.SendFunc(envelopeBytes)
		return True
	}

	// Request-response: create Future, resolve in background
	future := NewFuture()
	futureVal := vm.registerFuture(future)

	go func() {
		respPayload, errKind, errMsg, netErr := ref.SendFunc(envelopeBytes)
		if netErr != nil {
			future.ResolveError(fmt.Sprintf("network: %v", netErr))
			return
		}
		if errKind != "" {
			future.ResolveError(errKind + ": " + errMsg)
			return
		}
		if len(respPayload) > 0 {
			result, deserErr := vm.DeserializeValue(respPayload)
			if deserErr != nil {
				future.ResolveError(fmt.Sprintf("deserialize: %v", deserErr))
				return
			}
			future.Resolve(result)
		} else {
			future.Resolve(Nil)
		}
	}()

	return futureVal
}
