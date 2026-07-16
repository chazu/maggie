package dist

import (
	"github.com/chazu/maggie/vm/wire"
)

// MessageEnvelope is the inter-node message wire format. It is an alias for
// wire.Envelope — the single shared implementation of the envelope layout
// and signature construction (see vm/wire). The signature covers the whole
// envelope (routing fields included) and carries a protocol version.
type MessageEnvelope = wire.Envelope

// ReplyAddress identifies where to send a response.
type ReplyAddress = wire.ReplyAddress

// SignEnvelope signs the envelope with the given node identity.
func SignEnvelope(m *MessageEnvelope, id *NodeIdentity) error {
	return m.SignWith(id.NodeID(), id.Sign)
}

// MarshalEnvelope serializes a MessageEnvelope to CBOR bytes.
func MarshalEnvelope(m *MessageEnvelope) ([]byte, error) {
	return m.Marshal()
}

// UnmarshalEnvelope deserializes a MessageEnvelope from CBOR bytes.
func UnmarshalEnvelope(data []byte) (*MessageEnvelope, error) {
	return wire.Unmarshal(data)
}

// MessageResponse is sent back to the sender after processing a message.
type MessageResponse struct {
	Success bool   `cbor:"1,keyasint"`
	Error   string `cbor:"2,keyasint,omitempty"` // error kind: networkError, processNotFound, mailboxFull, applicationError
	Payload []byte `cbor:"3,keyasint,omitempty"` // CBOR-encoded response value (for asyncSend)
}
