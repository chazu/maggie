package wire

import (
	"crypto/ed25519"
	"crypto/rand"
	"testing"
	"time"
)

func testKeys(t *testing.T) ([32]byte, ed25519.PrivateKey) {
	t.Helper()
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatalf("GenerateKey: %v", err)
	}
	var sender [32]byte
	copy(sender[:], pub)
	return sender, priv
}

func signedEnvelope(t *testing.T) (*Envelope, [32]byte, ed25519.PrivateKey) {
	t.Helper()
	sender, priv := testKeys(t)
	env := &Envelope{
		TargetName: "worker",
		Selector:   "doWork:",
		Payload:    []byte{1, 2, 3},
		Nonce:      42,
	}
	if err := env.Sign(sender, priv); err != nil {
		t.Fatalf("Sign: %v", err)
	}
	return env, sender, priv
}

func TestEnvelope_SignVerifyRoundTrip(t *testing.T) {
	env, _, _ := signedEnvelope(t)
	if env.Version != Version {
		t.Errorf("Version: got %d, want %d", env.Version, Version)
	}
	if err := env.Verify(); err != nil {
		t.Fatalf("Verify: %v", err)
	}

	// Marshal/unmarshal round trip preserves verifiability.
	data, err := env.Marshal()
	if err != nil {
		t.Fatalf("Marshal: %v", err)
	}
	got, err := Unmarshal(data)
	if err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if err := got.Verify(); err != nil {
		t.Fatalf("Verify after round trip: %v", err)
	}
}

// The signature must cover ROUTING fields, not just the payload — tampering
// with any of them invalidates the envelope. (The old scheme signed only
// payload||nonce||targetProcess, so redirection and selector rewriting went
// undetected.)
func TestEnvelope_RoutingFieldsAreSigned(t *testing.T) {
	tamper := map[string]func(*Envelope){
		"TargetName":    func(e *Envelope) { e.TargetName = "victim" },
		"TargetProcess": func(e *Envelope) { e.TargetProcess = 99 },
		"Selector":      func(e *Envelope) { e.Selector = "__spawn_result__" },
		"ReplyTo":       func(e *Envelope) { e.ReplyTo = &ReplyAddress{ProcessID: 7} },
		"ClassHints":    func(e *Envelope) { e.ClassHints = [][32]byte{{1}} },
		"Payload":       func(e *Envelope) { e.Payload = []byte{9} },
		"Nonce":         func(e *Envelope) { e.Nonce++ },
		"Version":       func(e *Envelope) { e.Version = 0 },
	}
	for field, mutate := range tamper {
		env, _, _ := signedEnvelope(t)
		mutate(env)
		if err := env.Verify(); err == nil {
			t.Errorf("tampering with %s was not detected", field)
		}
	}
}

func TestEnvelope_UnversionedRejected(t *testing.T) {
	sender, priv := testKeys(t)
	env := &Envelope{Payload: []byte{1}, Nonce: 1}
	if err := env.Sign(sender, priv); err != nil {
		t.Fatalf("Sign: %v", err)
	}
	env.Version = 0 // simulate a pre-v1 envelope
	if err := env.Verify(); err == nil {
		t.Error("unversioned envelope should be rejected")
	}
}

func TestRequestAuth_RoundTrip(t *testing.T) {
	sender, priv := testKeys(t)
	now := time.Now()
	ts := now.UnixNano()

	sig := SignRequest(priv, "/svc/Method", ts, 7)
	if err := VerifyRequest(sender, "/svc/Method", ts, 7, sig, now); err != nil {
		t.Fatalf("VerifyRequest: %v", err)
	}

	// Wrong procedure fails.
	if err := VerifyRequest(sender, "/svc/Other", ts, 7, sig, now); err == nil {
		t.Error("signature valid for a different procedure")
	}
	// Wrong nonce fails.
	if err := VerifyRequest(sender, "/svc/Method", ts, 8, sig, now); err == nil {
		t.Error("signature valid for a different nonce")
	}
	// Stale timestamp fails.
	if err := VerifyRequest(sender, "/svc/Method", ts, 7, sig, now.Add(MaxClockSkew*2)); err == nil {
		t.Error("stale timestamp accepted")
	}
}
