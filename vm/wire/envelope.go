// Package wire defines Maggie's versioned inter-node wire formats: the
// fully-signed message envelope and the request-authentication scheme.
//
// This is a LEAF package — it imports only the standard library and the
// CBOR codec — so that vm, vm/dist, server, and cmd/mag all share ONE
// implementation of the envelope layout and signature construction.
// (Historically the envelope was hand-copied in three places because vm
// cannot import vm/dist; a one-field drift between copies silently broke
// signature verification across nodes.)
package wire

import (
	"crypto/ed25519"
	"fmt"

	"github.com/fxamacker/cbor/v2"
)

// Version is the current wire protocol version. Envelopes with any other
// version (including 0, i.e. unversioned pre-v1 envelopes) are rejected —
// version skew fails loudly instead of silently misinterpreting fields.
const Version uint8 = 1

// encMode is the canonical CBOR encoding used for both marshaling and
// signature construction. Canonical form is what makes "sign the encoded
// envelope" well-defined.
var encMode cbor.EncMode

func init() {
	em, err := cbor.CanonicalEncOptions().EncMode()
	if err != nil {
		panic("wire: cbor enc mode: " + err.Error())
	}
	encMode = em
}

// ReplyAddress identifies where to send a response.
type ReplyAddress struct {
	NodeID    [32]byte `cbor:"1,keyasint"` // reply-to node
	ProcessID uint64   `cbor:"2,keyasint"` // reply-to process
}

// Envelope is the wire format for inter-node messages. It wraps a
// serialized Maggie value with routing metadata and an Ed25519 signature.
//
// The signature covers the canonical CBOR encoding of the WHOLE envelope
// with the Signature field empty — routing fields (TargetProcess,
// TargetName, Selector, ReplyTo, ClassHints) and the Version are all
// signed, so an on-path attacker can neither redirect a valid message to a
// different process nor rewrite its selector (e.g. into __spawn_result__ /
// __down__ control messages).
//
// Replay protection: Nonce is strictly increasing per sender (seeded from
// wall-clock nanoseconds so it stays increasing across sender restarts);
// receivers track a per-peer window and reject reuse.
type Envelope struct {
	SenderNode    [32]byte      `cbor:"1,keyasint"`           // sender's Ed25519 public key
	TargetProcess uint64        `cbor:"2,keyasint"`           // local process ID on target node
	TargetName    string        `cbor:"3,keyasint,omitempty"` // registered name (alternative to ID)
	ReplyTo       *ReplyAddress `cbor:"4,keyasint,omitempty"` // where to send response
	Selector      string        `cbor:"5,keyasint,omitempty"` // message selector (method name)
	Payload       []byte        `cbor:"6,keyasint"`           // CBOR-encoded Maggie Value
	ClassHints    [][32]byte    `cbor:"7,keyasint,omitempty"` // class hashes referenced in payload
	Nonce         uint64        `cbor:"8,keyasint"`           // per-sender increasing, for replay prevention
	Signature     []byte        `cbor:"9,keyasint,omitempty"` // Ed25519 signature (excluded from signed bytes)
	Version       uint8         `cbor:"10,keyasint"`          // wire protocol version (see Version)
}

// signedBytes returns the canonical encoding of the envelope with the
// Signature field cleared — the exact bytes that are signed and verified.
func (e *Envelope) signedBytes() ([]byte, error) {
	cp := *e
	cp.Signature = nil
	return encMode.Marshal(&cp)
}

// SignWith stamps the version and sender, then signs the envelope using the
// supplied signer (typically ed25519.Sign closed over a private key —
// callback form so callers in different packages can supply their own
// identity types).
func (e *Envelope) SignWith(sender [32]byte, signer func([]byte) []byte) error {
	e.Version = Version
	e.SenderNode = sender
	b, err := e.signedBytes()
	if err != nil {
		return fmt.Errorf("wire: encode for signing: %w", err)
	}
	e.Signature = signer(b)
	return nil
}

// Sign signs the envelope with an Ed25519 private key whose public key is
// sender.
func (e *Envelope) Sign(sender [32]byte, priv ed25519.PrivateKey) error {
	return e.SignWith(sender, func(b []byte) []byte { return ed25519.Sign(priv, b) })
}

// Verify checks the version and the signature against SenderNode.
func (e *Envelope) Verify() error {
	if e.Version != Version {
		return fmt.Errorf("wire: unsupported envelope version %d (want %d)", e.Version, Version)
	}
	if len(e.Signature) != ed25519.SignatureSize {
		return fmt.Errorf("wire: invalid signature size %d", len(e.Signature))
	}
	b, err := e.signedBytes()
	if err != nil {
		return fmt.Errorf("wire: encode for verification: %w", err)
	}
	if !ed25519.Verify(e.SenderNode[:], b, e.Signature) {
		return fmt.Errorf("wire: signature verification failed")
	}
	return nil
}

// Marshal serializes the envelope to canonical CBOR.
func (e *Envelope) Marshal() ([]byte, error) {
	return encMode.Marshal(e)
}

// Unmarshal deserializes an envelope from CBOR bytes.
func Unmarshal(data []byte) (*Envelope, error) {
	var e Envelope
	if err := cbor.Unmarshal(data, &e); err != nil {
		return nil, fmt.Errorf("wire: unmarshal envelope: %w", err)
	}
	return &e, nil
}
