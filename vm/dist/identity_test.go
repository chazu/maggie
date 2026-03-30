package dist

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestGenerateIdentity(t *testing.T) {
	id, err := GenerateIdentity()
	if err != nil {
		t.Fatalf("GenerateIdentity: %v", err)
	}
	if len(id.PublicKey) != 32 {
		t.Errorf("public key length: got %d, want 32", len(id.PublicKey))
	}
	if len(id.PrivateKey) != 64 {
		t.Errorf("private key length: got %d, want 64", len(id.PrivateKey))
	}
}

func TestNodeID(t *testing.T) {
	id, _ := GenerateIdentity()
	nid := id.NodeID()
	if nid == [32]byte{} {
		t.Error("NodeID should not be zero")
	}
	// NodeID should match the public key
	for i := 0; i < 32; i++ {
		if nid[i] != id.PublicKey[i] {
			t.Errorf("NodeID[%d] mismatch", i)
			break
		}
	}
}

func TestNodeIDHex(t *testing.T) {
	id, _ := GenerateIdentity()
	hex := id.NodeIDHex()
	if len(hex) != 64 {
		t.Errorf("hex length: got %d, want 64", len(hex))
	}
	// Should round-trip
	parsed, err := ParseNodeIDHex(hex)
	if err != nil {
		t.Fatalf("ParseNodeIDHex: %v", err)
	}
	if parsed != id.NodeID() {
		t.Error("hex round-trip failed")
	}
}

func TestNodeIDProquint(t *testing.T) {
	id, _ := GenerateIdentity()
	pq := id.NodeIDProquint()

	// Should be 4 segments separated by dots
	segments := strings.Split(pq, ".")
	if len(segments) != 4 {
		t.Errorf("proquint segments: got %d, want 4", len(segments))
	}

	// Each segment should be a valid 64-bit proquint (4 quintuplets separated by dashes)
	for i, seg := range segments {
		parts := strings.Split(seg, "-")
		if len(parts) != 4 {
			t.Errorf("segment %d has %d parts, want 4", i, len(parts))
		}
	}
}

func TestSignVerify(t *testing.T) {
	id, _ := GenerateIdentity()
	data := []byte("hello world")

	sig := id.Sign(data)
	if len(sig) != 64 {
		t.Fatalf("signature length: got %d, want 64", len(sig))
	}

	// Verify with correct key
	if !VerifyFromNodeID(id.NodeID(), data, sig) {
		t.Error("valid signature should verify")
	}

	// Verify with wrong data
	if VerifyFromNodeID(id.NodeID(), []byte("wrong data"), sig) {
		t.Error("wrong data should not verify")
	}

	// Verify with wrong key
	other, _ := GenerateIdentity()
	if VerifyFromNodeID(other.NodeID(), data, sig) {
		t.Error("wrong key should not verify")
	}

	// Verify with invalid signature
	if VerifyFromNodeID(id.NodeID(), data, []byte("short")) {
		t.Error("invalid signature should not verify")
	}
}

func TestLoadOrCreateIdentity_Create(t *testing.T) {
	dir := t.TempDir()
	id, err := LoadOrCreateIdentity(dir)
	if err != nil {
		t.Fatalf("LoadOrCreateIdentity: %v", err)
	}

	// Key file should exist
	keyPath := filepath.Join(dir, "node.key")
	info, err := os.Stat(keyPath)
	if err != nil {
		t.Fatalf("key file not created: %v", err)
	}
	if info.Size() != 64 {
		t.Errorf("key file size: got %d, want 64", info.Size())
	}
	// Check permissions (owner-only)
	if info.Mode().Perm() != 0600 {
		t.Errorf("key file permissions: got %o, want 0600", info.Mode().Perm())
	}

	_ = id
}

func TestLoadOrCreateIdentity_Load(t *testing.T) {
	dir := t.TempDir()

	// Create
	id1, _ := LoadOrCreateIdentity(dir)

	// Load (should return same identity)
	id2, err := LoadOrCreateIdentity(dir)
	if err != nil {
		t.Fatalf("second LoadOrCreateIdentity: %v", err)
	}

	if id1.NodeID() != id2.NodeID() {
		t.Error("loaded identity should have same NodeID")
	}

	// Signature from one should verify with the other
	data := []byte("test")
	sig := id1.Sign(data)
	if !VerifyFromNodeID(id2.NodeID(), data, sig) {
		t.Error("loaded identity should verify signatures from original")
	}
}

func TestLoadOrCreateIdentity_CorruptedFile(t *testing.T) {
	dir := t.TempDir()
	keyPath := filepath.Join(dir, "node.key")

	// Write a corrupted key file (wrong length)
	os.WriteFile(keyPath, []byte("too short"), 0600)
	_, err := LoadOrCreateIdentity(dir)
	if err == nil {
		t.Error("should error on corrupted key file")
	}
}

func TestParseNodeIDHex_Invalid(t *testing.T) {
	// Wrong length
	_, err := ParseNodeIDHex("abcd")
	if err == nil {
		t.Error("should error on short hex")
	}

	// Not hex
	_, err = ParseNodeIDHex(strings.Repeat("zz", 32))
	if err == nil {
		t.Error("should error on invalid hex")
	}
}

func TestTwoIdentities_Different(t *testing.T) {
	id1, _ := GenerateIdentity()
	id2, _ := GenerateIdentity()
	if id1.NodeID() == id2.NodeID() {
		t.Error("two generated identities should have different NodeIDs")
	}
}
