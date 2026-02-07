package vm

import (
	"crypto/sha256"
	"encoding/binary"
	"sort"
	"sync"
)

// ---------------------------------------------------------------------------
// ContentStore: content-addressed index for methods and classes
// ---------------------------------------------------------------------------

// ContentStore indexes compiled methods and class digests by their content
// hash. It is the VM-local backing store for the distribution protocol.
type ContentStore struct {
	mu      sync.RWMutex
	methods map[[32]byte]*CompiledMethod
	classes map[[32]byte]*ClassDigest
}

// ClassDigest is a compact representation of a class suitable for content
// addressing and distribution. It stores the class's structural metadata
// and the content hashes of all its methods.
type ClassDigest struct {
	Name           string
	Namespace      string
	SuperclassName string
	InstVars       []string
	ClassVars      []string
	DocString      string
	MethodHashes   [][32]byte // sorted for deterministic hashing
	Hash           [32]byte
}

// NewContentStore creates an empty content store.
func NewContentStore() *ContentStore {
	return &ContentStore{
		methods: make(map[[32]byte]*CompiledMethod),
		classes: make(map[[32]byte]*ClassDigest),
	}
}

// IndexMethod adds a compiled method to the store, keyed by its ContentHash.
// Methods with a zero hash are silently ignored.
func (cs *ContentStore) IndexMethod(m *CompiledMethod) {
	h := m.GetContentHash()
	if h == [32]byte{} {
		return
	}
	cs.mu.Lock()
	cs.methods[h] = m
	cs.mu.Unlock()
}

// LookupMethod returns the method for the given hash, or nil.
func (cs *ContentStore) LookupMethod(h [32]byte) *CompiledMethod {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return cs.methods[h]
}

// IndexClass adds a class digest to the store.
func (cs *ContentStore) IndexClass(d *ClassDigest) {
	if d.Hash == ([32]byte{}) {
		return
	}
	cs.mu.Lock()
	cs.classes[d.Hash] = d
	cs.mu.Unlock()
}

// LookupClass returns the class digest for the given hash, or nil.
func (cs *ContentStore) LookupClass(h [32]byte) *ClassDigest {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return cs.classes[h]
}

// HasHash returns true if the store contains either a method or class with
// the given hash.
func (cs *ContentStore) HasHash(h [32]byte) bool {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	if _, ok := cs.methods[h]; ok {
		return true
	}
	_, ok := cs.classes[h]
	return ok
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

	// Collect method content hashes from VTable
	collectHashes := func(vt *VTable) {
		if vt == nil {
			return
		}
		for _, m := range vt.LocalMethods() {
			if cm, ok := m.(*CompiledMethod); ok {
				h := cm.GetContentHash()
				if h != ([32]byte{}) {
					d.MethodHashes = append(d.MethodHashes, h)
				}
			}
		}
	}
	collectHashes(c.VTable)
	collectHashes(c.ClassVTable)

	d.Hash = HashClass(d.Name, d.Namespace, d.SuperclassName, d.InstVars, d.ClassVars, d.DocString, d.MethodHashes)
	return d
}
