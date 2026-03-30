package dist

import (
	"crypto/ed25519"
	"crypto/rand"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/goquint"
)

// NodeIdentity holds an Ed25519 keypair for node authentication.
// The public key (32 bytes) is the node ID. The private key (64 bytes)
// is used to sign outgoing messages.
type NodeIdentity struct {
	PublicKey  ed25519.PublicKey  // 32 bytes — the node ID
	PrivateKey ed25519.PrivateKey // 64 bytes
}

// NodeID returns the 32-byte node identifier (the public key).
func (id *NodeIdentity) NodeID() [32]byte {
	var nid [32]byte
	copy(nid[:], id.PublicKey)
	return nid
}

// NodeIDHex returns the node ID as a 64-character hex string.
func (id *NodeIdentity) NodeIDHex() string {
	return hex.EncodeToString(id.PublicKey)
}

// NodeIDProquint returns the node ID as a human-readable proquint string.
// The 32-byte key is encoded as four 64-bit proquint segments separated
// by dots: "babab-dabab-babab-dabab.babab-dabab-babab-dabab...."
func (id *NodeIdentity) NodeIDProquint() string {
	segments := make([]string, 4)
	for i := 0; i < 4; i++ {
		val := binary.BigEndian.Uint64(id.PublicKey[i*8 : (i+1)*8])
		segments[i] = goquint.Encode64(val)
	}
	return strings.Join(segments, ".")
}

// Sign produces an Ed25519 signature over the given data.
func (id *NodeIdentity) Sign(data []byte) []byte {
	return ed25519.Sign(id.PrivateKey, data)
}

// Verify checks an Ed25519 signature against a public key.
func Verify(publicKey ed25519.PublicKey, data, signature []byte) bool {
	if len(publicKey) != ed25519.PublicKeySize || len(signature) != ed25519.SignatureSize {
		return false
	}
	return ed25519.Verify(publicKey, data, signature)
}

// VerifyFromNodeID checks a signature against a 32-byte node ID.
func VerifyFromNodeID(nodeID [32]byte, data, signature []byte) bool {
	return Verify(ed25519.PublicKey(nodeID[:]), data, signature)
}

// GenerateIdentity creates a new random Ed25519 keypair.
func GenerateIdentity() (*NodeIdentity, error) {
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		return nil, fmt.Errorf("dist: generate keypair: %w", err)
	}
	return &NodeIdentity{PublicKey: pub, PrivateKey: priv}, nil
}

// LoadOrCreateIdentity loads a node identity from the given directory,
// or generates a new one if it doesn't exist. The key is stored as
// "node.key" (64 bytes: 32-byte private key seed + 32-byte public key).
func LoadOrCreateIdentity(dir string) (*NodeIdentity, error) {
	keyPath := filepath.Join(dir, "node.key")
	data, err := os.ReadFile(keyPath)
	if err == nil {
		return parseKeyFile(data)
	}
	if !os.IsNotExist(err) {
		return nil, fmt.Errorf("dist: read key: %w", err)
	}

	// Generate new identity
	id, err := GenerateIdentity()
	if err != nil {
		return nil, err
	}

	// Ensure directory exists
	if err := os.MkdirAll(dir, 0700); err != nil {
		return nil, fmt.Errorf("dist: create key dir: %w", err)
	}

	// Write key file: seed (32 bytes) + public key (32 bytes) = 64 bytes
	seed := id.PrivateKey.Seed()
	keyData := make([]byte, 64)
	copy(keyData[:32], seed)
	copy(keyData[32:], id.PublicKey)

	if err := os.WriteFile(keyPath, keyData, 0600); err != nil {
		return nil, fmt.Errorf("dist: write key: %w", err)
	}

	return id, nil
}

// parseKeyFile reads a 64-byte key file (seed + public key).
func parseKeyFile(data []byte) (*NodeIdentity, error) {
	if len(data) != 64 {
		return nil, fmt.Errorf("dist: invalid key file size %d (want 64)", len(data))
	}
	seed := data[:32]
	privKey := ed25519.NewKeyFromSeed(seed)
	pubKey := privKey.Public().(ed25519.PublicKey)

	// Verify stored public key matches derived public key
	storedPub := data[32:]
	if !pubKey.Equal(ed25519.PublicKey(storedPub)) {
		return nil, fmt.Errorf("dist: key file corrupted (public key mismatch)")
	}

	return &NodeIdentity{PublicKey: pubKey, PrivateKey: privKey}, nil
}

// ParseNodeIDHex parses a 64-character hex string into a 32-byte node ID.
func ParseNodeIDHex(s string) ([32]byte, error) {
	var nid [32]byte
	b, err := hex.DecodeString(s)
	if err != nil {
		return nid, fmt.Errorf("dist: invalid hex node ID: %w", err)
	}
	if len(b) != 32 {
		return nid, fmt.Errorf("dist: node ID must be 32 bytes, got %d", len(b))
	}
	copy(nid[:], b)
	return nid, nil
}
