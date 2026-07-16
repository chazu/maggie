package wire

import (
	"crypto/ed25519"
	"encoding/binary"
	"fmt"
	"time"
)

// Request authentication
//
// Peer-facing RPCs are authenticated per request: the client signs
// (procedure, timestamp, nonce) with its node key and sends the pieces in
// headers; the server interceptor verifies the signature, checks clock
// skew, and rejects nonce reuse via the trust store's per-peer window.
//
// Scope note: the request signature binds identity and freshness, not the
// request BODY. Body integrity comes from the layers above — message
// payloads ride in fully-signed envelopes, and synced code is content-
// addressed (chunks are verified by re-hashing). Transport confidentiality
// and byte-level integrity against an on-path attacker require TLS in
// front of the listener.

// Header names for request authentication.
const (
	HeaderNodeID    = "X-Maggie-Node-Id"   // hex-encoded Ed25519 public key
	HeaderTimestamp = "X-Maggie-Timestamp" // unix nanoseconds, decimal
	HeaderNonce     = "X-Maggie-Nonce"     // per-sender increasing, decimal
	HeaderSignature = "X-Maggie-Signature" // hex-encoded Ed25519 signature
)

// MaxClockSkew is how far a request timestamp may deviate from the
// receiver's clock before the request is rejected as stale.
const MaxClockSkew = 30 * time.Second

// RequestSigningBytes returns the canonical byte string covered by a
// request signature.
func RequestSigningBytes(procedure string, timestamp int64, nonce uint64) []byte {
	buf := make([]byte, 0, len(procedure)+17)
	buf = append(buf, procedure...)
	buf = append(buf, 0) // separator: procedure names never contain NUL
	var n [8]byte
	binary.BigEndian.PutUint64(n[:], uint64(timestamp))
	buf = append(buf, n[:]...)
	binary.BigEndian.PutUint64(n[:], nonce)
	buf = append(buf, n[:]...)
	return buf
}

// SignRequest produces a request signature.
func SignRequest(priv ed25519.PrivateKey, procedure string, timestamp int64, nonce uint64) []byte {
	return ed25519.Sign(priv, RequestSigningBytes(procedure, timestamp, nonce))
}

// VerifyRequest checks a request signature and timestamp freshness against
// now. Nonce-replay checking is the caller's job (it needs per-peer state).
func VerifyRequest(pub [32]byte, procedure string, timestamp int64, nonce uint64, sig []byte, now time.Time) error {
	if len(sig) != ed25519.SignatureSize {
		return fmt.Errorf("wire: invalid request signature size %d", len(sig))
	}
	skew := now.UnixNano() - timestamp
	if skew < 0 {
		skew = -skew
	}
	if skew > int64(MaxClockSkew) {
		return fmt.Errorf("wire: request timestamp outside ±%s window", MaxClockSkew)
	}
	if !ed25519.Verify(pub[:], RequestSigningBytes(procedure, timestamp, nonce), sig) {
		return fmt.Errorf("wire: request signature verification failed")
	}
	return nil
}

// NonceSeed returns a starting value for a per-sender nonce counter that
// stays increasing across process restarts (wall-clock nanoseconds).
func NonceSeed() uint64 {
	return uint64(time.Now().UnixNano())
}
