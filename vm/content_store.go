package vm

import (
	"crypto/sha256"
	"encoding/binary"
	"fmt"
	"sort"
	"sync"
)

// ---------------------------------------------------------------------------
// ContentStore: content-addressed index for methods and classes
// ---------------------------------------------------------------------------

// ContentStore indexes compiled methods and class digests by their content
// hash. It is the VM-local backing store for the distribution protocol.
//
// Methods and classes are indexed by semantic hash (primary key for identity
// and negotiation) and optionally by typed hash (for local verification).
// Semantic hashes are used in have/want negotiation — two nodes with the
// same code but different type annotations see each other as identical.
// Typed hashes travel as metadata and are verified locally after receipt.
type ContentStore struct {
	mu      sync.RWMutex
	methods map[[32]byte]*CompiledMethod
	classes map[[32]byte]*ClassDigest

	// Typed hash → semantic hash reverse indexes for local verification.
	// These allow looking up content by typed hash without changing the
	// primary identity model (semantic hash = identity).
	typedMethods map[[32]byte][32]byte // typed hash → semantic hash
	typedClasses map[[32]byte][32]byte // typed hash → semantic hash
}

// ClassDigest is a compact representation of a class suitable for content
// addressing and distribution. It stores the class's structural metadata
// and the content hashes of all its methods.
type ClassDigest struct {
	Name              string
	Namespace         string
	SuperclassName    string
	InstVars          []string
	ClassVars         []string
	DocString         string
	MethodHashes      [][32]byte // sorted for deterministic hashing
	Hash              [32]byte
	TypedMethodHashes [][32]byte // parallel to MethodHashes, zero entries = no types
	TypedHash         [32]byte   // class digest including typed method hashes
}

// NewContentStore creates an empty content store.
func NewContentStore() *ContentStore {
	return &ContentStore{
		methods:      make(map[[32]byte]*CompiledMethod),
		classes:      make(map[[32]byte]*ClassDigest),
		typedMethods: make(map[[32]byte][32]byte),
		typedClasses: make(map[[32]byte][32]byte),
	}
}

// IndexMethod adds a compiled method to the store, keyed by its ContentHash.
// If the method also has a non-zero TypedHash, the typed hash is indexed as
// a reverse pointer to the semantic hash. Methods with a zero semantic hash
// are silently ignored.
func (cs *ContentStore) IndexMethod(m *CompiledMethod) {
	h := m.GetContentHash()
	if h == [32]byte{} {
		return
	}
	cs.mu.Lock()
	cs.methods[h] = m
	if th := m.GetTypedHash(); th != ([32]byte{}) {
		cs.typedMethods[th] = h
	}
	cs.mu.Unlock()
}

// LookupMethod returns the method for the given semantic hash, or nil.
func (cs *ContentStore) LookupMethod(h [32]byte) *CompiledMethod {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return cs.methods[h]
}

// LookupMethodByTypedHash returns the method whose typed hash matches h,
// or nil if no match. Looks up via the typed→semantic reverse index.
func (cs *ContentStore) LookupMethodByTypedHash(h [32]byte) *CompiledMethod {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	if semantic, ok := cs.typedMethods[h]; ok {
		return cs.methods[semantic]
	}
	return nil
}

// IndexClass adds a class digest to the store. If the digest has a non-zero
// TypedHash, it is indexed as a reverse pointer to the semantic hash.
func (cs *ContentStore) IndexClass(d *ClassDigest) {
	if d.Hash == ([32]byte{}) {
		return
	}
	cs.mu.Lock()
	cs.classes[d.Hash] = d
	if d.TypedHash != ([32]byte{}) {
		cs.typedClasses[d.TypedHash] = d.Hash
	}
	cs.mu.Unlock()
}

// LookupClass returns the class digest for the given semantic hash, or nil.
func (cs *ContentStore) LookupClass(h [32]byte) *ClassDigest {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return cs.classes[h]
}

// LookupClassByTypedHash returns the class digest whose typed hash matches h,
// or nil if no match. Looks up via the typed→semantic reverse index.
func (cs *ContentStore) LookupClassByTypedHash(h [32]byte) *ClassDigest {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	if semantic, ok := cs.typedClasses[h]; ok {
		return cs.classes[semantic]
	}
	return nil
}

// HasHash returns true if the store contains either a method or class with
// the given hash. Checks both semantic and typed hash indexes.
func (cs *ContentStore) HasHash(h [32]byte) bool {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	if _, ok := cs.methods[h]; ok {
		return true
	}
	if _, ok := cs.classes[h]; ok {
		return true
	}
	if _, ok := cs.typedMethods[h]; ok {
		return true
	}
	if _, ok := cs.typedClasses[h]; ok {
		return true
	}
	return false
}

// MethodHashes returns all method content hashes in the store.
func (cs *ContentStore) MethodHashes() [][32]byte {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	hashes := make([][32]byte, 0, len(cs.methods))
	for h := range cs.methods {
		hashes = append(hashes, h)
	}
	return hashes
}

// ClassHashes returns all class digest hashes in the store.
func (cs *ContentStore) ClassHashes() [][32]byte {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	hashes := make([][32]byte, 0, len(cs.classes))
	for h := range cs.classes {
		hashes = append(hashes, h)
	}
	return hashes
}

// AllHashes returns all hashes (methods + classes) in the store.
func (cs *ContentStore) AllHashes() [][32]byte {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	hashes := make([][32]byte, 0, len(cs.methods)+len(cs.classes))
	for h := range cs.methods {
		hashes = append(hashes, h)
	}
	for h := range cs.classes {
		hashes = append(hashes, h)
	}
	return hashes
}

// MethodCount returns the number of indexed methods.
func (cs *ContentStore) MethodCount() int {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return len(cs.methods)
}

// ClassCount returns the number of indexed class digests.
func (cs *ContentStore) ClassCount() int {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return len(cs.classes)
}

// LookupClassByName returns the first ClassDigest whose Name matches the
// given fully-qualified name, or nil if no match is found. This is a linear
// scan and not intended for hot paths.
func (cs *ContentStore) LookupClassByName(name string) *ClassDigest {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	for _, d := range cs.classes {
		if d.Name == name {
			return d
		}
	}
	return nil
}

// AllClassDigests returns all class digests in the store.
func (cs *ContentStore) AllClassDigests() []*ClassDigest {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	result := make([]*ClassDigest, 0, len(cs.classes))
	for _, d := range cs.classes {
		result = append(result, d)
	}
	return result
}

// AllMethodHashes returns all method content hashes in the store.
func (cs *ContentStore) AllMethodHashes() [][32]byte {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	result := make([][32]byte, 0, len(cs.methods))
	for h := range cs.methods {
		result = append(result, h)
	}
	return result
}

// LookupByPrefix finds a hash by its hex prefix (like git short hashes).
// Returns the full hash, the type ("method" or "class"), and an error.
// Errors if prefix is ambiguous (multiple matches) or not found.
// Minimum prefix length is 4 hex characters.
func (cs *ContentStore) LookupByPrefix(prefix string) ([32]byte, string, error) {
	if len(prefix) < 4 {
		return [32]byte{}, "", fmt.Errorf("prefix must be at least 4 hex characters, got %d", len(prefix))
	}

	cs.mu.RLock()
	defer cs.mu.RUnlock()

	var matches [][32]byte
	var types []string

	// Search semantic hashes (primary identity)
	for h := range cs.methods {
		hexStr := fmt.Sprintf("%x", h)
		if len(hexStr) >= len(prefix) && hexStr[:len(prefix)] == prefix {
			matches = append(matches, h)
			types = append(types, "method")
		}
	}
	for h := range cs.classes {
		hexStr := fmt.Sprintf("%x", h)
		if len(hexStr) >= len(prefix) && hexStr[:len(prefix)] == prefix {
			matches = append(matches, h)
			types = append(types, "class")
		}
	}
	// Search typed hashes (resolve to semantic hash for return)
	for th, sh := range cs.typedMethods {
		hexStr := fmt.Sprintf("%x", th)
		if len(hexStr) >= len(prefix) && hexStr[:len(prefix)] == prefix {
			// Avoid duplicate if semantic hash already matched
			dup := false
			for _, m := range matches {
				if m == sh {
					dup = true
					break
				}
			}
			if !dup {
				matches = append(matches, sh)
				types = append(types, "method")
			}
		}
	}
	for th, sh := range cs.typedClasses {
		hexStr := fmt.Sprintf("%x", th)
		if len(hexStr) >= len(prefix) && hexStr[:len(prefix)] == prefix {
			dup := false
			for _, m := range matches {
				if m == sh {
					dup = true
					break
				}
			}
			if !dup {
				matches = append(matches, sh)
				types = append(types, "class")
			}
		}
	}

	switch len(matches) {
	case 0:
		return [32]byte{}, "", fmt.Errorf("no hash found with prefix %q", prefix)
	case 1:
		return matches[0], types[0], nil
	default:
		return [32]byte{}, "", fmt.Errorf("ambiguous prefix %q: matches %d hashes", prefix, len(matches))
	}
}

// ---------------------------------------------------------------------------
// Class hashing
// ---------------------------------------------------------------------------

// HashClass computes the SHA-256 of a ClassDigest from its structural fields.
// The hash covers: name, namespace, superclass, sorted instVars, sorted
// classVars, docstring, and sorted method hashes.
func HashClass(name, namespace, superclass string, instVars, classVars []string, docString string, methodHashes [][32]byte) [32]byte {
	// Sort method hashes for determinism
	sorted := make([][32]byte, len(methodHashes))
	copy(sorted, methodHashes)
	sort.Slice(sorted, func(i, j int) bool {
		for k := 0; k < 32; k++ {
			if sorted[i][k] != sorted[j][k] {
				return sorted[i][k] < sorted[j][k]
			}
		}
		return false
	})

	var buf []byte

	writeString := func(s string) {
		var lenBuf [4]byte
		binary.BigEndian.PutUint32(lenBuf[:], uint32(len(s)))
		buf = append(buf, lenBuf[:]...)
		buf = append(buf, s...)
	}

	writeStringSlice := func(ss []string) {
		var lenBuf [4]byte
		cp := make([]string, len(ss))
		copy(cp, ss)
		sort.Strings(cp)
		binary.BigEndian.PutUint32(lenBuf[:], uint32(len(cp)))
		buf = append(buf, lenBuf[:]...)
		for _, s := range cp {
			writeString(s)
		}
	}

	// Tag byte for class hash format
	buf = append(buf, 0x01)
	writeString(name)
	writeString(namespace)
	writeString(superclass)
	writeStringSlice(instVars)
	writeStringSlice(classVars)
	writeString(docString)

	// Method hashes
	var mhLen [4]byte
	binary.BigEndian.PutUint32(mhLen[:], uint32(len(sorted)))
	buf = append(buf, mhLen[:]...)
	for _, h := range sorted {
		buf = append(buf, h[:]...)
	}

	return sha256.Sum256(buf)
}

// DigestClass builds a ClassDigest from a Class and its methods' content
// hashes. The class hash is computed automatically.
func DigestClass(c *Class) *ClassDigest {
	d := &ClassDigest{
		Name:      c.Name,
		Namespace: c.Namespace,
	}
	if c.Superclass != nil {
		d.SuperclassName = c.Superclass.Name
	}
	if len(c.InstVars) > 0 {
		d.InstVars = make([]string, len(c.InstVars))
		copy(d.InstVars, c.InstVars)
	}
	if len(c.ClassVars) > 0 {
		d.ClassVars = make([]string, len(c.ClassVars))
		copy(d.ClassVars, c.ClassVars)
	}
	d.DocString = c.DocString

	// Collect method content hashes and typed hashes from VTable
	collectHashes := func(vt *VTable) {
		if vt == nil {
			return
		}
		for _, m := range vt.LocalMethods() {
			if cm, ok := m.(*CompiledMethod); ok {
				h := cm.GetContentHash()
				if h != ([32]byte{}) {
					d.MethodHashes = append(d.MethodHashes, h)
					d.TypedMethodHashes = append(d.TypedMethodHashes, cm.GetTypedHash())
				}
			}
		}
	}
	collectHashes(c.VTable)
	collectHashes(c.ClassVTable)

	d.Hash = HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	d.TypedHash = HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.TypedMethodHashes)
	return d
}
