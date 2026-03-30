package dist

import (
	"crypto/ed25519"
	"encoding/binary"
	"fmt"

	"github.com/fxamacker/cbor/v2"
)

// MessageEnvelope is the wire format for inter-node messages. It wraps a
// serialized Maggie value with routing metadata and an Ed25519 signature.
//
// The sender signs (payload || nonce || targetProcess) to prevent replay
// attacks and message redirection. The receiver verifies the signature
// against the sender's public key before processing.
type MessageEnvelope struct {
	SenderNode    [32]byte `cbor:"1,keyasint"`           // sender's Ed25519 public key
	TargetProcess uint64   `cbor:"2,keyasint"`           // local process ID on target node
	TargetName    string   `cbor:"3,keyasint,omitempty"` // registered name (alternative to ID)
	ReplyTo       *ReplyAddress `cbor:"4,keyasint,omitempty"` // where to send response
	Selector      string   `cbor:"5,keyasint,omitempty"` // message selector (method name)
	Payload       []byte   `cbor:"6,keyasint"`           // CBOR-encoded Maggie Value
	ClassHints    [][32]byte `cbor:"7,keyasint,omitempty"` // class hashes referenced in payload
	Nonce         uint64   `cbor:"8,keyasint"`           // monotonic counter for replay prevention
	Signature     []byte   `cbor:"9,keyasint"`           // Ed25519 signature
}

// ReplyAddress identifies where to send a response.
type ReplyAddress struct {
	NodeID    [32]byte `cbor:"1,keyasint"` // reply-to node
	ProcessID uint64   `cbor:"2,keyasint"` // reply-to process
}

// signaturePayload returns the bytes that are signed/verified.
func (m *MessageEnvelope) signaturePayload() []byte {
	var buf []byte
	buf = append(buf, m.Payload...)
	var nonceBuf [8]byte
	binary.BigEndian.PutUint64(nonceBuf[:], m.Nonce)
	buf = append(buf, nonceBuf[:]...)
	var procBuf [8]byte
	binary.BigEndian.PutUint64(procBuf[:], m.TargetProcess)
	buf = append(buf, procBuf[:]...)
	return buf
}

// Sign signs the envelope with the given node identity.
func (m *MessageEnvelope) Sign(id *NodeIdentity) {
	m.SenderNode = id.NodeID()
	m.Signature = id.Sign(m.signaturePayload())
}

// Verify checks the envelope's signature against the sender's public key.
func (m *MessageEnvelope) Verify() error {
	if len(m.Signature) != ed25519.SignatureSize {
		return fmt.Errorf("dist: invalid signature size %d", len(m.Signature))
	}
	if !VerifyFromNodeID(m.SenderNode, m.signaturePayload(), m.Signature) {
		return fmt.Errorf("dist: signature verification failed")
	}
	return nil
}

// MarshalEnvelope serializes a MessageEnvelope to CBOR bytes.
func MarshalEnvelope(m *MessageEnvelope) ([]byte, error) {
	return cborEncMode.Marshal(m)
}

// UnmarshalEnvelope deserializes a MessageEnvelope from CBOR bytes.
func UnmarshalEnvelope(data []byte) (*MessageEnvelope, error) {
	var m MessageEnvelope
	if err := cbor.Unmarshal(data, &m); err != nil {
		return nil, fmt.Errorf("dist: unmarshal envelope: %w", err)
	}
	return &m, nil
}

// MessageResponse is sent back to the sender after processing a message.
type MessageResponse struct {
	Success bool   `cbor:"1,keyasint"`
	Error   string `cbor:"2,keyasint,omitempty"` // error kind: networkError, processNotFound, mailboxFull, applicationError
	Payload []byte `cbor:"3,keyasint,omitempty"` // CBOR-encoded response value (for asyncSend)
}
