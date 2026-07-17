package vm

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/fxamacker/cbor/v2"

	"github.com/chazu/maggie/vm/wire"
)

// SelectorReply is the infrastructure selector for delivering a request-response
// reply back to the node that issued asyncSend:with:. Like __down__ and
// __spawn_result__ it is part of a protocol the peer already initiated, so it is
// not gated by PermMessage — but every reply resolves an existing pending Future
// keyed by a correlation id we minted, and handleReply verifies the replying
// peer matches the node we sent the request to.
const SelectorReply = "__reply__"

// ---------------------------------------------------------------------------
// Pending reply registry: maps correlationID → Future awaiting a remote reply
// ---------------------------------------------------------------------------

// pendingReply associates an asyncSend:with: Future with the peer we sent the
// request to, so handleNodeDown can resolve every future stranded by a dead
// node and handleReply can reject replies forged by a different peer.
type pendingReply struct {
	future *FutureObject
	peerID [32]byte
}

type pendingReplyRegistry struct {
	mu      sync.RWMutex
	replies map[uint64]pendingReply
	nextID  atomic.Uint64
}

func newPendingReplyRegistry() *pendingReplyRegistry {
	return &pendingReplyRegistry{replies: make(map[uint64]pendingReply)}
}

func (r *pendingReplyRegistry) register(f *FutureObject, peerID [32]byte) uint64 {
	id := r.nextID.Add(1)
	r.mu.Lock()
	r.replies[id] = pendingReply{future: f, peerID: peerID}
	r.mu.Unlock()
	return id
}

// resolve removes and returns the pending reply for id. expectedPeer, when
// non-zero, must match the peer recorded at register time; a mismatch returns
// (nil, false) and leaves the entry in place so a forged reply cannot strand
// the real one.
func (r *pendingReplyRegistry) resolve(id uint64, expectedPeer [32]byte) (*FutureObject, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	pr, ok := r.replies[id]
	if !ok {
		return nil, false
	}
	var zero [32]byte
	if expectedPeer != zero && pr.peerID != zero && pr.peerID != expectedPeer {
		return nil, false
	}
	delete(r.replies, id)
	return pr.future, true
}

// drainNode removes and returns every future awaiting a reply from nodeID.
func (r *pendingReplyRegistry) drainNode(nodeID [32]byte) []*FutureObject {
	r.mu.Lock()
	defer r.mu.Unlock()
	var out []*FutureObject
	for id, pr := range r.replies {
		if pr.peerID == nodeID {
			out = append(out, pr.future)
			delete(r.replies, id)
		}
	}
	return out
}

// RegisterPendingReply records a Future awaiting a reply from peer and returns
// the correlation id to stamp into the request envelope's ReplyAddress. This is
// the seam asyncSend:with: uses internally; it is exported so code wiring
// request-response outside the primitive (and integration tests) can share the
// same registry the __reply__ handler resolves against.
func (vm *VM) RegisterPendingReply(f *FutureObject, peer [32]byte) uint64 {
	return vm.pendingReplies.register(f, peer)
}

// ResolvePendingReply looks up and removes the Future awaiting the reply with
// the given correlation id, verifying it came from the expected peer. Called by
// the server's __reply__ handler. Returns nil if unknown, already resolved, or
// the replying peer does not match.
func (vm *VM) ResolvePendingReply(correlation uint64, fromPeer [32]byte) *FutureObject {
	f, _ := vm.pendingReplies.resolve(correlation, fromPeer)
	return f
}

// ---------------------------------------------------------------------------
// Reply payload (wire form of a __reply__ envelope's body)
// ---------------------------------------------------------------------------

// replyPayload is the CBOR body of a __reply__ envelope. The correlation id
// identifies which pending Future to resolve; exactly one of ResultBytes /
// ErrorMsg carries the outcome.
type replyPayload struct {
	Correlation uint64 `cbor:"1,keyasint"`
	ResultBytes []byte `cbor:"2,keyasint,omitempty"`
	ErrorMsg    string `cbor:"3,keyasint,omitempty"`
}

// EncodeReplyPayload / DecodeReplyPayload are exported so the server package can
// build and read __reply__ bodies without duplicating the struct.
func EncodeReplyPayload(correlation uint64, resultBytes []byte, errMsg string) ([]byte, error) {
	return cborSerialEncMode.Marshal(replyPayload{Correlation: correlation, ResultBytes: resultBytes, ErrorMsg: errMsg})
}

func DecodeReplyPayload(data []byte) (correlation uint64, resultBytes []byte, errMsg string, err error) {
	var p replyPayload
	if e := cbor.Unmarshal(data, &p); e != nil {
		return 0, nil, "", e
	}
	return p.Correlation, p.ResultBytes, p.ErrorMsg, nil
}

// ---------------------------------------------------------------------------
// Sending a reply (receiver side, driven by MailboxMessage>>reply:)
// ---------------------------------------------------------------------------

// sendReply routes value back to replyNode as the answer to the request whose
// ReplyTo carried correlation. It requires a live NodeRef back to the requester
// (established when the requester or a bidirectional connect: registered one);
// without one the reply is dropped — the requester's Future then relies on its
// own timeout / node-death drain, exactly as spawn-result delivery does.
func (vm *VM) sendReply(replyNode [32]byte, correlation uint64, value Value) error {
	ref := vm.findNodeRefByID(replyNode)
	if ref == nil || ref.SendFunc == nil {
		return fmt.Errorf("reply: no connected node for requester")
	}

	var body []byte
	if resultBytes, serErr := vm.SerializeValue(value); serErr != nil {
		// A non-serializable reply value becomes a remote error rather than a
		// silently-dropped reply that hangs the requester's Future.
		body, _ = EncodeReplyPayload(correlation, nil, fmt.Sprintf("reply value not serializable: %v", serErr))
	} else {
		body, _ = EncodeReplyPayload(correlation, resultBytes, "")
	}

	envelope, err := buildSignedEnvelopeForProcess(ref, 0, SelectorReply, body)
	if err != nil {
		return err
	}
	go ref.SendFunc(envelope)
	return nil
}

// addMailboxReplyMethods registers MailboxMessage>>reply: and >>isRequest.
// Called from registerMailboxPrimitives once MailboxMessageClass exists.
func (vm *VM) addMailboxReplyMethods() {
	c := vm.MailboxMessageClass

	// MailboxMessage>>isRequest — true when the sender used asyncSend:with:
	// (i.e. is awaiting a reply), false for a fire-and-forget cast:with:/send:.
	c.AddMethod0(vm.Selectors, "isRequest", func(_ *VM, recv Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return False
		}
		if obj.GetSlot(mailboxSlotCorrelation).IsSmallInt() {
			return True
		}
		return False
	})

	// MailboxMessage>>reply: value — answer a request-response message. Signals
	// if the message was not a request (no reply address) or the requester is no
	// longer reachable. Answers the value on success so `^msg reply: x` is idiomatic.
	c.AddMethod1(vm.Selectors, "reply:", func(v *VM, recv, value Value) Value {
		obj := ObjectFromValue(recv)
		if obj == nil {
			return v.SignalPrimitiveError("reply:", "receiver is not a MailboxMessage")
		}
		corrSlot := obj.GetSlot(mailboxSlotCorrelation)
		if !corrSlot.IsSmallInt() {
			return v.SignalPrimitiveError("reply:", "message does not expect a reply (sent with cast:with:/send:, not asyncSend:with:)")
		}
		correlation := uint64(corrSlot.SmallInt())

		nodeStr := v.registry.GetStringContent(obj.GetSlot(mailboxSlotReplyNode))
		if len(nodeStr) != 32 {
			return v.SignalPrimitiveError("reply:", "message has no valid reply address")
		}
		var replyNode [32]byte
		copy(replyNode[:], nodeStr)

		if err := v.sendReply(replyNode, correlation, value); err != nil {
			return v.SignalPrimitiveError("reply:", err.Error())
		}
		return value
	})
}

// buildSignedEnvelopeWithReply constructs a signed envelope carrying an explicit
// ReplyAddress (node + correlation id) so the receiver can route a reply back.
func buildSignedEnvelopeWithReply(ref *NodeRefData, targetName, selector string, payload []byte, replyTo *wire.ReplyAddress) ([]byte, error) {
	env := &wire.Envelope{
		TargetName: targetName,
		Selector:   selector,
		Payload:    payload,
		ReplyTo:    replyTo,
		Nonce:      ref.NextNonce(),
	}
	if err := env.SignWith(ref.NodeID(), ref.Sign); err != nil {
		return nil, err
	}
	return env.Marshal()
}
