package dist

import (
	"testing"
)

func TestMessageEnvelope_SignVerify(t *testing.T) {
	sender, _ := GenerateIdentity()

	env := &MessageEnvelope{
		TargetProcess: 42,
		Selector:      "doWork:",
		Payload:       []byte("test payload"),
		Nonce:         1,
	}
	env.Sign(sender)

	// SenderNode should be set
	if env.SenderNode != sender.NodeID() {
		t.Error("SenderNode not set after Sign")
	}

	// Verify should succeed
	if err := env.Verify(); err != nil {
		t.Errorf("Verify failed: %v", err)
	}
}

func TestMessageEnvelope_TamperedPayload(t *testing.T) {
	sender, _ := GenerateIdentity()

	env := &MessageEnvelope{
		TargetProcess: 42,
		Payload:       []byte("original"),
		Nonce:         1,
	}
	env.Sign(sender)

	// Tamper with payload
	env.Payload = []byte("tampered")
	if err := env.Verify(); err == nil {
		t.Error("tampered payload should fail verification")
	}
}

func TestMessageEnvelope_TamperedTarget(t *testing.T) {
	sender, _ := GenerateIdentity()

	env := &MessageEnvelope{
		TargetProcess: 42,
		Payload:       []byte("data"),
		Nonce:         1,
	}
	env.Sign(sender)

	// Redirect to different process
	env.TargetProcess = 99
	if err := env.Verify(); err == nil {
		t.Error("redirected message should fail verification")
	}
}

func TestMessageEnvelope_TamperedNonce(t *testing.T) {
	sender, _ := GenerateIdentity()

	env := &MessageEnvelope{
		TargetProcess: 42,
		Payload:       []byte("data"),
		Nonce:         1,
	}
	env.Sign(sender)

	// Replay with different nonce
	env.Nonce = 2
	if err := env.Verify(); err == nil {
		t.Error("replayed message should fail verification")
	}
}

func TestMessageEnvelope_WrongSender(t *testing.T) {
	sender, _ := GenerateIdentity()
	impersonator, _ := GenerateIdentity()

	env := &MessageEnvelope{
		TargetProcess: 42,
		Payload:       []byte("data"),
		Nonce:         1,
	}
	env.Sign(sender)

	// Replace sender with impersonator (signature won't match)
	env.SenderNode = impersonator.NodeID()
	if err := env.Verify(); err == nil {
		t.Error("impersonated sender should fail verification")
	}
}

func TestMessageEnvelope_CBORRoundTrip(t *testing.T) {
	sender, _ := GenerateIdentity()
	replyNode, _ := GenerateIdentity()

	env := &MessageEnvelope{
		TargetProcess: 42,
		TargetName:    "worker-1",
		ReplyTo: &ReplyAddress{
			NodeID:    replyNode.NodeID(),
			ProcessID: 7,
		},
		Selector:   "doWork:",
		Payload:    []byte("test payload"),
		ClassHints: [][32]byte{{1, 2, 3}, {4, 5, 6}},
		Nonce:      100,
	}
	env.Sign(sender)

	// Marshal
	data, err := MarshalEnvelope(env)
	if err != nil {
		t.Fatalf("MarshalEnvelope: %v", err)
	}

	// Unmarshal
	got, err := UnmarshalEnvelope(data)
	if err != nil {
		t.Fatalf("UnmarshalEnvelope: %v", err)
	}

	// Check all fields
	if got.SenderNode != env.SenderNode {
		t.Error("SenderNode mismatch")
	}
	if got.TargetProcess != 42 {
		t.Errorf("TargetProcess: got %d, want 42", got.TargetProcess)
	}
	if got.TargetName != "worker-1" {
		t.Errorf("TargetName: got %q, want %q", got.TargetName, "worker-1")
	}
	if got.ReplyTo == nil {
		t.Fatal("ReplyTo is nil")
	}
	if got.ReplyTo.NodeID != replyNode.NodeID() {
		t.Error("ReplyTo.NodeID mismatch")
	}
	if got.ReplyTo.ProcessID != 7 {
		t.Errorf("ReplyTo.ProcessID: got %d, want 7", got.ReplyTo.ProcessID)
	}
	if got.Selector != "doWork:" {
		t.Errorf("Selector: got %q, want %q", got.Selector, "doWork:")
	}
	if string(got.Payload) != "test payload" {
		t.Errorf("Payload: got %q, want %q", got.Payload, "test payload")
	}
	if len(got.ClassHints) != 2 {
		t.Errorf("ClassHints: got %d, want 2", len(got.ClassHints))
	}
	if got.Nonce != 100 {
		t.Errorf("Nonce: got %d, want 100", got.Nonce)
	}

	// Signature should still verify after round-trip
	if err := got.Verify(); err != nil {
		t.Errorf("Verify after round-trip: %v", err)
	}
}

func TestMessageEnvelope_EmptySignature(t *testing.T) {
	env := &MessageEnvelope{
		TargetProcess: 1,
		Payload:       []byte("data"),
		Signature:     nil,
	}
	if err := env.Verify(); err == nil {
		t.Error("empty signature should fail verification")
	}
}

func TestMessageEnvelope_NoReplyTo(t *testing.T) {
	sender, _ := GenerateIdentity()

	// Fire-and-forget: no ReplyTo
	env := &MessageEnvelope{
		TargetProcess: 42,
		Selector:      "logEvent:",
		Payload:       []byte("event data"),
		Nonce:         1,
	}
	env.Sign(sender)

	data, err := MarshalEnvelope(env)
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	got, err := UnmarshalEnvelope(data)
	if err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if got.ReplyTo != nil {
		t.Error("ReplyTo should be nil for fire-and-forget")
	}
	if err := got.Verify(); err != nil {
		t.Errorf("Verify: %v", err)
	}
}
