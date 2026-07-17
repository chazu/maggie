package vm

import (
	"testing"
	"time"

	"github.com/chazu/maggie/vm/wire"
)

func TestPendingReplyRegistry_ResolveAndPeerCheck(t *testing.T) {
	reg := newPendingReplyRegistry()

	f1 := NewFuture()
	f2 := NewFuture()
	peerA := [32]byte{1}
	peerB := [32]byte{2}

	id1 := reg.register(f1, peerA)
	id2 := reg.register(f2, peerB)
	if id1 == id2 {
		t.Fatal("correlation ids must be unique")
	}

	// A reply claiming to come from the wrong peer must not resolve the future,
	// and must leave the entry intact for the legitimate peer.
	if got, ok := reg.resolve(id1, peerB); ok || got != nil {
		t.Fatal("mismatched peer should not resolve the future")
	}

	// The real peer resolves it.
	if got, ok := reg.resolve(id1, peerA); !ok || got != f1 {
		t.Fatal("matching peer should resolve f1")
	}

	// Second resolve returns nothing (consumed).
	if got, ok := reg.resolve(id1, peerA); ok || got != nil {
		t.Fatal("consumed entry should not resolve again")
	}

	// Zero expectedPeer skips verification (used on the delivery-ack failure path).
	if got, ok := reg.resolve(id2, [32]byte{}); !ok || got != f2 {
		t.Fatal("zero expectedPeer should resolve without a peer check")
	}
}

func TestPendingReplyRegistry_DrainNode(t *testing.T) {
	reg := newPendingReplyRegistry()
	peerA := [32]byte{1}
	peerB := [32]byte{2}

	fA1 := NewFuture()
	fA2 := NewFuture()
	fB := NewFuture()
	reg.register(fA1, peerA)
	reg.register(fA2, peerA)
	idB := reg.register(fB, peerB)

	drained := reg.drainNode(peerA)
	if len(drained) != 2 {
		t.Fatalf("expected 2 futures drained for peerA, got %d", len(drained))
	}

	// peerB's future is untouched.
	if got, ok := reg.resolve(idB, peerB); !ok || got != fB {
		t.Fatal("peerB future should survive draining peerA")
	}
}

func TestReplyPayload_RoundTrip(t *testing.T) {
	body, err := EncodeReplyPayload(42, []byte{0x18, 0x2a}, "")
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	corr, result, errMsg, err := DecodeReplyPayload(body)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if corr != 42 {
		t.Errorf("correlation: got %d, want 42", corr)
	}
	if len(result) != 2 || result[0] != 0x18 || result[1] != 0x2a {
		t.Errorf("result bytes round-trip mismatch: %x", result)
	}
	if errMsg != "" {
		t.Errorf("errMsg: got %q, want empty", errMsg)
	}

	// Error form.
	body, _ = EncodeReplyPayload(7, nil, "boom")
	corr, result, errMsg, _ = DecodeReplyPayload(body)
	if corr != 7 || errMsg != "boom" || len(result) != 0 {
		t.Errorf("error payload round-trip mismatch: corr=%d err=%q result=%x", corr, errMsg, result)
	}
}

// TestMailboxMessage_ReplyRouting drives MailboxMessage>>reply: end to end at
// the VM level: a request-carrying message routes a serialized reply back
// through the requester's NodeRef SendFunc, and the reply resolves the pending
// Future via the same path the server's __reply__ handler uses.
func TestMailboxMessage_ReplyRouting(t *testing.T) {
	// The "requester" VM holds the pending Future and its own identity.
	requester := NewVM()
	defer requester.Shutdown()

	pub, priv := testKeys(t)
	// Register a self-referential NodeRef so findNodeRefByID(replyNode) resolves
	// on the responder side (here the same VM plays both roles for the unit test).
	ref := NewNodeRefData("localhost:9999", pub, priv)
	capturedCh := make(chan []byte, 1)
	ref.SendFunc = func(envelope []byte) ([]byte, string, string, error) {
		capturedCh <- envelope
		return nil, "", "", nil
	}
	ref.SetPeerID(ref.NodeID()) // peerKey resolves to this id
	requester.RegisterNodeRef(ref)

	// A pending Future keyed by a correlation id, as asyncSend:with: would create.
	future := NewFuture()
	correlation := requester.pendingReplies.register(future, ref.NodeID())

	// Build a request-carrying MailboxMessage addressed back to ref.NodeID().
	msg := requester.CreateMailboxMessageWithReply(Nil, "compute:", FromSmallInt(1), ref.NodeID(), correlation)
	mo := ObjectFromValue(msg)
	if mo == nil {
		t.Fatal("mailbox message is not an object")
	}

	// reply: 84 should serialize + route an envelope through SendFunc.
	requester.addMailboxReplyMethods() // idempotent re-register is fine
	replyResult := requester.sendReply(ref.NodeID(), correlation, FromSmallInt(84))
	if replyResult != nil {
		t.Fatalf("sendReply failed: %v", replyResult)
	}
	var captured []byte
	select {
	case captured = <-capturedCh:
	case <-time.After(time.Second):
		t.Fatal("reply envelope was not sent")
	}

	// Decode the captured envelope and feed its body through the resolution path.
	env, err := wire.Unmarshal(captured)
	if err != nil {
		t.Fatalf("decode reply envelope: %v", err)
	}
	if env.Selector != SelectorReply {
		t.Fatalf("reply envelope selector: got %q, want %q", env.Selector, SelectorReply)
	}
	corr, resultBytes, errMsg, err := DecodeReplyPayload(env.Payload)
	if err != nil {
		t.Fatalf("decode reply payload: %v", err)
	}
	if corr != correlation || errMsg != "" {
		t.Fatalf("unexpected reply: corr=%d err=%q", corr, errMsg)
	}

	resolved := requester.ResolvePendingReply(corr, ref.NodeID())
	if resolved != future {
		t.Fatal("ResolvePendingReply did not return the pending future")
	}
	val, derr := requester.DeserializeValue(resultBytes)
	if derr != nil {
		t.Fatalf("deserialize reply result: %v", derr)
	}
	if !val.IsSmallInt() || val.SmallInt() != 84 {
		t.Errorf("reply value: got %v, want 84", val)
	}
}
