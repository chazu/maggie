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

