package dist

import (
	"fmt"

	"github.com/fxamacker/cbor/v2"
)

// GossipSelector is the infrastructure selector for a membership gossip digest,
// carried by a signed envelope through DeliverMessage exactly like __down__ /
// __reply__. Unlike those, gossip is gated on PermSync (see the server handler):
// it disseminates addresses and liveness, never trust — a peer needs at least
// the baseline sync permission to inject records into our view.
const GossipSelector = "__gossip__"

// maxGossipRecords caps how many member records a single gossip digest may
// carry. Without it a peer could send a digest with an unbounded record count,
// and (under an eager join policy) drive one dial per record to an arbitrary
// address — an amplification / SSRF primitive. A real cluster's view is bounded
// well below this; a digest exceeding it is rejected wholesale.
const maxGossipRecords = 4096

// gossipDigest is the CBOR body of a __gossip__ envelope: the sender's view of
// the cluster (its own record plus every member it knows).
type gossipDigest struct {
	Members []MemberRecord `cbor:"1,keyasint"`
}

// EncodeGossip serializes a membership digest for the wire.
func EncodeGossip(members []MemberRecord) ([]byte, error) {
	return cborEncMode.Marshal(gossipDigest{Members: members})
}

// DecodeGossip parses a __gossip__ envelope body back into member records,
// rejecting a digest that exceeds maxGossipRecords.
func DecodeGossip(data []byte) ([]MemberRecord, error) {
	var d gossipDigest
	if err := cbor.Unmarshal(data, &d); err != nil {
		return nil, err
	}
	if len(d.Members) > maxGossipRecords {
		return nil, fmt.Errorf("gossip digest has %d records, exceeds cap %d", len(d.Members), maxGossipRecords)
	}
	return d.Members, nil
}
