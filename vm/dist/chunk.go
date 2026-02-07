// Package dist implements the content-addressed code distribution protocol
// for Maggie. Two VMs can exchange methods, classes, and modules as
// content-addressed chunks over HTTP/2 using CBOR encoding.
package dist

// ChunkType identifies the kind of content in a Chunk.
type ChunkType uint8

const (
	ChunkMethod ChunkType = 1
	ChunkClass  ChunkType = 2
	ChunkModule ChunkType = 3
)

// Chunk is the atomic unit of code distribution. Each chunk carries source
// text plus a content hash. The receiver compiles the source and verifies
// that the resulting hash matches.
type Chunk struct {
	Hash         [32]byte   `cbor:"1,keyasint"`
	Type         ChunkType  `cbor:"2,keyasint"`
	Content      string     `cbor:"3,keyasint"`           // source text
	Dependencies [][32]byte `cbor:"4,keyasint,omitempty"` // referenced hashes
	Capabilities []string   `cbor:"5,keyasint,omitempty"` // required capabilities
}

// SyncAnnouncement is sent by a peer to advertise what it has available.
type SyncAnnouncement struct {
	RootHash    [32]byte            `cbor:"1,keyasint"`
	AllHashes   [][32]byte          `cbor:"2,keyasint"`
	Capability  *CapabilityManifest `cbor:"3,keyasint,omitempty"`
	HashVersion byte                `cbor:"4,keyasint"`
}

// SyncRequest is the have/want negotiation message.
type SyncRequest struct {
	Have [][32]byte `cbor:"1,keyasint"`
	Want [][32]byte `cbor:"2,keyasint"`
}

// SyncResponse carries the requested chunks.
type SyncResponse struct {
	Chunks []Chunk `cbor:"1,keyasint"`
}

// CapabilityManifest declares what capabilities a set of chunks requires.
type CapabilityManifest struct {
	Required []string `cbor:"1,keyasint"` // e.g., "File", "HTTP", "Network"
}

// AnnounceStatus indicates the result of an announcement.
type AnnounceStatus uint8

const (
	AnnounceAccepted    AnnounceStatus = 0
	AnnounceRejected    AnnounceStatus = 1
	AnnounceAlreadyHave AnnounceStatus = 2
)

// AnnounceResponse is the reply to a SyncAnnouncement.
type AnnounceResponse struct {
	Status       AnnounceStatus `cbor:"1,keyasint"`
	Want         [][32]byte     `cbor:"2,keyasint,omitempty"`
	RejectReason string         `cbor:"3,keyasint,omitempty"`
}

// TransferResult summarizes the outcome of a chunk transfer.
type TransferResult struct {
	Accepted     int        `cbor:"1,keyasint"`
	Rejected     int        `cbor:"2,keyasint"`
	FailedHashes [][32]byte `cbor:"3,keyasint,omitempty"`
}
