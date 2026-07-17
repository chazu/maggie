package vm

import (
	"crypto/ed25519"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"sync/atomic"
	"unsafe"

	"github.com/chazu/maggie/vm/wire"
)

// ---------------------------------------------------------------------------
// NodeRefData: connection to a remote node (no dist package dependency)
// ---------------------------------------------------------------------------

// NodeRefData holds the Go-side data for a Node connection value.
// The actual gRPC client is injected via SendFunc to avoid importing
// vm/dist (which imports vm, creating a cycle).
type NodeRefData struct {
	Addr      string
	PublicKey ed25519.PublicKey        // OUR public key (used to sign outgoing envelopes)
	privKey   ed25519.PrivateKey       // OUR private key
	peerID    atomic.Pointer[[32]byte] // the REMOTE peer's node id, learned via Ping handshake
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
	ref := &NodeRefData{
		Addr:      addr,
		PublicKey: pub,
		privKey:   priv,
	}
	// Seed the nonce from wall-clock nanos so it stays increasing across
	// process restarts — receivers reject nonce reuse per peer.
	ref.nonce.Store(wire.NonceSeed())
	return ref
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

// NodeID returns OUR 32-byte node identifier — the sender id stamped on
// outgoing envelopes and request-auth headers. This is the LOCAL identity, not
// the peer's; use PeerNodeID for the remote side's identity.
func (n *NodeRefData) NodeID() [32]byte {
	var nid [32]byte
	copy(nid[:], n.PublicKey)
	return nid
}

// SetPeerID records the remote peer's node identity (learned from the Ping
// handshake). Idempotent and safe for concurrent callers.
func (n *NodeRefData) SetPeerID(id [32]byte) {
	cp := id
	n.peerID.Store(&cp)
}

// PeerNodeID returns the remote peer's node identity and whether it is known
// yet. Health-monitoring, DOWN routing (findNodeRefByID), and node-death
// cleanup (DrainRemoteChannels) must key on THIS, not on our own NodeID().
func (n *NodeRefData) PeerNodeID() (id [32]byte, ok bool) {
	if p := n.peerID.Load(); p != nil {
		return *p, true
	}
	return id, false
}

// peerKey is the identity to use for all peer-scoped bookkeeping
// (health-monitor key, findNodeRefByID matching, pendingSpawns/DrainRemoteChannels
// on node death). After the connect: handshake it is the remote peer's id; if
// the peer was unreachable it degrades to our own id — no worse than the
// pre-handshake behavior, and self-consistent because every site uses this.
func (n *NodeRefData) peerKey() [32]byte {
	if id, ok := n.PeerNodeID(); ok {
		return id
	}
	return n.NodeID()
}

// ---------------------------------------------------------------------------
// Envelope building (shared implementation in vm/wire)
// ---------------------------------------------------------------------------

// BuildSignedEnvelope constructs a signed CBOR-encoded envelope. Exported for
// use by cmd/mag wiring layer.
func BuildSignedEnvelope(ref *NodeRefData, targetName, selector string, payload []byte, wantReply bool) ([]byte, error) {
	return buildSignedEnvelope(ref, targetName, selector, payload, wantReply)
}

func buildSignedEnvelope(ref *NodeRefData, targetName, selector string, payload []byte, wantReply bool) ([]byte, error) {
	env := &wire.Envelope{
		TargetName: targetName,
		Selector:   selector,
		Payload:    payload,
		Nonce:      ref.NextNonce(),
	}
	if wantReply {
		env.ReplyTo = &wire.ReplyAddress{NodeID: ref.NodeID()}
	}
	if err := env.SignWith(ref.NodeID(), ref.Sign); err != nil {
		return nil, err
	}
	return env.Marshal()
}

// ---------------------------------------------------------------------------
// Node values are pointer-carrying kindRemoteRef heap Values.
// ---------------------------------------------------------------------------

func isNodeRefValue(v Value) bool {
	return v.ptr != nil && v.hi == kindRemoteRef
}

// ---------------------------------------------------------------------------
// VM helpers
// ---------------------------------------------------------------------------

// ConnectNode builds a fully-wired NodeRefData for addr, performs the Ping
// handshake so the peer's node id is learned, registers it for reverse lookup,
// and returns it. Returns nil if no local identity is available. This is the
// shared body of the `Node connect:` primitive; the membership layer calls it
// to establish connections to discovered peers. Best-effort handshake: if the
// peer is unreachable now, a later ping populates its id.
func (vm *VM) ConnectNode(addr string) *NodeRefData {
	if addr == "" {
		return nil
	}
	pub, priv := vm.nodeIdentity()
	if pub == nil {
		return nil
	}
	var ref *NodeRefData
	if fn := vm.GetNodeRefFactory(); fn != nil {
		ref = fn(addr, pub, priv)
	} else {
		ref = NewNodeRefData(addr, pub, priv)
	}
	if ref.PingFunc != nil {
		ref.PingFunc()
	}
	vm.registerNodeRef(ref)
	return ref
}

// HealthMonitor returns the VM's node health monitor, creating and starting it
// on first use. The membership failure detector wraps this single instance so
// heartbeating is shared with monitor/link tracking.
func (vm *VM) HealthMonitor() *NodeHealthMonitor {
	vm.healthMonitorMu.Lock()
	defer vm.healthMonitorMu.Unlock()
	if vm.healthMonitor == nil {
		vm.healthMonitor = NewNodeHealthMonitor(vm)
		vm.healthMonitor.Start()
	}
	return vm.healthMonitor
}

func (vm *VM) registerNodeRef(ref *NodeRefData) Value {
	// The ref is a pointer-carrying kindRemoteRef Value (below). It is also
	// tracked in nodeRefs, whose sole remaining role is the public-key reverse
	// lookup (findNodeRefByID) used to route inbound distributed messages.
	vm.nodeRefsMu.Lock()
	vm.nodeRefs[ref] = struct{}{}
	vm.nodeRefsMu.Unlock()
	return makeHeap(kindRemoteRef, unsafe.Pointer(ref))
}

func (vm *VM) getNodeRef(v Value) *NodeRefData {
	if !isNodeRefValue(v) {
		return nil
	}
	return (*NodeRefData)(v.ptr)
}

func (vm *VM) nodeIdentity() (ed25519.PublicKey, ed25519.PrivateKey) {
	if id := vm.localIdentity.Load(); id != nil {
		return id.pub, id.priv
	}
	// Generate an ephemeral identity. The mutex serializes generation so two
	// concurrent Node connect: calls cannot mint two different identities
	// (which would break reply routing).
	vm.localIdentityMu.Lock()
	defer vm.localIdentityMu.Unlock()
	if id := vm.localIdentity.Load(); id != nil {
		return id.pub, id.priv
	}
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		return nil, nil
	}
	vm.localIdentity.Store(&nodeIdentityHolder{pub: pub, priv: priv})
	return pub, priv
}

// SetNodeIdentityKeys sets the VM's local node identity keys.
func (vm *VM) SetNodeIdentityKeys(pub ed25519.PublicKey, priv ed25519.PrivateKey) {
	vm.localIdentity.Store(&nodeIdentityHolder{pub: pub, priv: priv})
}

// SignWithNodeIdentity signs data with the VM's persistent node identity key,
// returning (signature, true). It returns (nil, false) when no identity has been
// installed — it never mints an ephemeral key, so a signature always corresponds
// to LocalNodeID(). Used by the membership layer to self-sign its gossip records.
func (vm *VM) SignWithNodeIdentity(data []byte) ([]byte, bool) {
	holder := vm.localIdentity.Load()
	if holder == nil || holder.priv == nil {
		return nil, false
	}
	return ed25519.Sign(holder.priv, data), true
}

// localNodeID returns the 32-byte local node ID, or the zero array if no
// identity has been set. Safe for concurrent use.
func (vm *VM) localNodeID() (id [32]byte, ok bool) {
	holder := vm.localIdentity.Load()
	if holder == nil {
		return id, false
	}
	copy(id[:], holder.pub)
	return id, true
}

// LocalNodeID exposes this VM's 32-byte node identity for the peer-ID
// handshake (the Ping response advertises it). ok is false before an identity
// is installed.
func (vm *VM) LocalNodeID() (id [32]byte, ok bool) { return vm.localNodeID() }

// ---------------------------------------------------------------------------
// Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerNodePrimitives() {
	c := vm.createClass("Node", vm.ObjectClass)
	vm.NodeClass = c
	vm.globals["Node"] = vm.classValue(c)
	// Class resolution is via classForHeap's kindRemoteRef case (pointer Value).

	// Node class>>connect: addr
	c.AddClassMethod1(vm.Selectors, "connect:", func(v *VM, recv, addr Value) Value {
		addrStr := v.registry.GetStringContent(addr)
		if addrStr == "" {
			return Nil
		}
		ref := v.ConnectNode(addrStr)
		if ref == nil {
			return Nil
		}
		return makeHeap(kindRemoteRef, unsafe.Pointer(ref))
	})

	// Node>>primAddr
	c.AddMethod0(vm.Selectors, "primAddr", func(v *VM, recv Value) Value {
		ref := v.getNodeRef(recv)
		if ref == nil {
			return Nil
		}
		return v.registry.NewStringValue(ref.Addr)
	})

	// Node>>primPing
	c.AddMethod0(vm.Selectors, "primPing", func(v *VM, recv Value) Value {
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
	c.AddMethod1(vm.Selectors, "primProcessNamed:", func(v *VM, recv, name Value) Value {
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

	// Node>>primNodeID — hex-encoded 32-byte identity of the REMOTE node
	// (learned via the connect: handshake; falls back to ours if unreachable).
	c.AddMethod0(vm.Selectors, "primNodeID", func(v *VM, recv Value) Value {
		ref := v.getNodeRef(recv)
		if ref == nil {
			return Nil
		}
		nid := ref.peerKey()
		return v.registry.NewStringValue(hex.EncodeToString(nid[:]))
	})

	// Node>>printString
	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
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
	return instance.ToValue()
}

func (vm *VM) registerRemoteProcessPrimitives() {
	c := NewClassWithInstVars("RemoteProcess", vm.ObjectClass, []string{"node", "name"})
	c.NumSlots = 2
	vm.RemoteProcessClass = c
	vm.Classes.Register(c)
	vm.globals["RemoteProcess"] = vm.classValue(c)

	// RemoteProcess>>primName
	c.AddMethod0(vm.Selectors, "primName", func(_ *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(1)
	})

	// RemoteProcess>>primNode
	c.AddMethod0(vm.Selectors, "primNode", func(_ *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		return obj.GetSlot(0)
	})

	// RemoteProcess>>primCast:with: — fire-and-forget
	c.AddMethod2(vm.Selectors, "primCast:with:", func(v *VM, recv, selectorVal, payload Value) Value {
		return v.remoteSend(recv, selectorVal, payload, false)
	})

	// RemoteProcess>>primAsyncSend:with: — returns Future
	c.AddMethod2(vm.Selectors, "primAsyncSend:with:", func(v *VM, recv, selectorVal, payload Value) Value {
		return v.remoteSend(recv, selectorVal, payload, true)
	})

	// RemoteProcess>>printString
	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
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
// Failure doctrine: local failures (bad receiver, non-serializable payload,
// envelope build) SIGNAL catchable errors instead of answering nil — nil
// would silently discard the reason (SD-14).
func (vm *VM) remoteSend(recv, selectorVal, payload Value, wantReply bool) Value {
	obj := ObjectFromValue(recv)
	if obj == nil {
		return vm.SignalPrimitiveError("asyncSend:with:", "receiver is not a RemoteProcess")
	}

	nodeRefVal := obj.GetSlot(0)
	ref := vm.getNodeRef(nodeRefVal)
	if ref == nil || ref.SendFunc == nil {
		return vm.SignalPrimitiveError("asyncSend:with:", "remote process has no connected node")
	}

	nameVal := obj.GetSlot(1)
	targetName := ""
	if IsStringValue(nameVal) {
		targetName = vm.registry.GetStringContent(nameVal)
	}

	sel := ""
	if selectorVal.IsSymbol() {
		sel = vm.Symbols.Name(selectorVal.SymbolID())
	} else if IsStringValue(selectorVal) {
		sel = vm.registry.GetStringContent(selectorVal)
	}

	// Serialize payload
	payloadBytes, err := vm.SerializeValue(payload)
	if err != nil {
		return vm.SignalPrimitiveError("asyncSend:with:", fmt.Sprintf("cannot serialize payload: %v", err))
	}

	if !wantReply {
		// Fire-and-forget
		envelopeBytes, err := buildSignedEnvelope(ref, targetName, sel, payloadBytes, false)
		if err != nil {
			return vm.SignalPrimitiveError("cast:with:", fmt.Sprintf("cannot build envelope: %v", err))
		}
		go ref.SendFunc(envelopeBytes)
		return True
	}

	// Request-response: register a Future keyed by a correlation id, stamp that
	// id into the envelope's ReplyAddress, and leave the Future pending. The
	// delivery ack only confirms enqueue; the actual answer arrives later as a
	// __reply__ envelope that resolves the Future (or node-death drains it).
	future := NewFuture()
	futureVal := vm.registerFuture(future)
	correlation := vm.pendingReplies.register(future, ref.peerKey())

	replyTo := &wire.ReplyAddress{NodeID: ref.NodeID(), Correlation: correlation}
	envelopeBytes, err := buildSignedEnvelopeWithReply(ref, targetName, sel, payloadBytes, replyTo)
	if err != nil {
		vm.pendingReplies.resolve(correlation, [32]byte{})
		return vm.SignalPrimitiveError("asyncSend:with:", fmt.Sprintf("cannot build envelope: %v", err))
	}

	go func() {
		_, errKind, errMsg, netErr := ref.SendFunc(envelopeBytes)
		if netErr != nil {
			if f, ok := vm.pendingReplies.resolve(correlation, [32]byte{}); ok {
				f.ResolveError(fmt.Sprintf("network: %v", netErr))
			}
			return
		}
		if errKind != "" {
			// Delivery failed (process not found, mailbox full, permission).
			// No reply will ever come, so resolve the Future now.
			if f, ok := vm.pendingReplies.resolve(correlation, [32]byte{}); ok {
				f.ResolveError(errKind + ": " + errMsg)
			}
			return
		}
		// Delivered — the reply resolves the Future asynchronously.
	}()

	return futureVal
}
