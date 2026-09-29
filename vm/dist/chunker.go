package dist

import (
	"strings"

	"github.com/chazu/maggie/vm"
)

// MethodToChunk creates a Chunk from a CompiledMethod. The chunk carries
// the method's source text and content hash. The receiver compiles the
// source and verifies the hash matches.
func MethodToChunk(m *vm.CompiledMethod, caps []string) *Chunk {
	h := m.GetContentHash()
	c := &Chunk{
		Hash:         h,
		Type:         ChunkMethod,
		Content:      m.Source,
		Capabilities: caps,
		Selector:     m.Name(),
		IsClassSide:  m.IsClassMethod,
		TypedHash:    m.GetTypedHash(),
	}
	if cls := m.Class(); cls != nil {
		if cls.Namespace != "" {
			c.ClassName = cls.Namespace + "::" + cls.Name
		} else {
			c.ClassName = cls.Name
		}
	}
	return c
}

// MethodChunker returns a MethodToChunk variant that also fills in the owning
// class for detached method stubs. Methods indexed from received sync chunks
// or loaded from the disk cache carry only source and hashes — no *Class — so
// MethodToChunk alone would emit ClassName "" and a receiver would verify the
// chunk with no ivar/namespace context, computing a different hash for any
// method that touches an instance variable or a namespaced global. The owner
// is recovered from the store's class digests (any digest listing the hash
// will do: the same hash under two owners means both contexts hash alike).
//
// The owner index is built once, so reuse one chunker for a whole batch.
func MethodChunker(store *vm.ContentStore) func(m *vm.CompiledMethod, caps []string) *Chunk {
	owners := make(map[[32]byte]string)
	if store != nil {
		for _, d := range store.AllClassDigests() {
			for _, mh := range d.MethodHashes {
				if _, ok := owners[mh]; !ok {
					owners[mh] = d.FQN()
				}
			}
		}
	}
	return func(m *vm.CompiledMethod, caps []string) *Chunk {
		c := MethodToChunk(m, caps)
		if c.ClassName == "" {
			if owner, ok := owners[c.Hash]; ok {
				c.ClassName = owner
				c.IsClassSide = strings.HasPrefix(strings.TrimSpace(m.Source), "classMethod:")
			}
		}
		return c
	}
}

// ClassToChunk creates a Chunk from a ClassDigest. The Content field is
// populated with the deterministic text encoding of the digest's structural
// metadata (name, namespace, superclass, ivars, cvars, docstring).
// Both semantic and typed hashes are propagated to the chunk.
func ClassToChunk(d *vm.ClassDigest, caps []string) *Chunk {
	deps := make([][32]byte, len(d.MethodHashes))
	copy(deps, d.MethodHashes)
	c := &Chunk{
		Hash:         d.Hash,
		Type:         ChunkClass,
		Content:      EncodeClassContent(d),
		Dependencies: deps,
		Capabilities: caps,
		TypedHash:    d.TypedHash,
	}
	if len(d.TypedMethodHashes) > 0 {
		c.TypedDependencies = make([][32]byte, len(d.TypedMethodHashes))
		copy(c.TypedDependencies, d.TypedMethodHashes)
	}
	return c
}

// TransitiveClosure computes all hashes reachable from a root hash by
// following dependency links through the content store.
func TransitiveClosure(root [32]byte, store *vm.ContentStore) [][32]byte {
	seen := make(map[[32]byte]bool)
	var result [][32]byte
	var walk func([32]byte)

	walk = func(h [32]byte) {
		if seen[h] {
			return
		}
		seen[h] = true
		result = append(result, h)

		// If it's a class, its method hashes are dependencies
		if d := store.LookupClass(h); d != nil {
			for _, mh := range d.MethodHashes {
				walk(mh)
			}
		}
	}

	walk(root)
	return result
}

// BuildCapabilityManifest gathers all unique capabilities from all chunks
// reachable from the root hash.
func BuildCapabilityManifest(root [32]byte, store *vm.ContentStore, chunks map[[32]byte]*Chunk) *CapabilityManifest {
	hashes := TransitiveClosure(root, store)
	capSet := make(map[string]bool)
	for _, h := range hashes {
		if c, ok := chunks[h]; ok {
			for _, cap := range c.Capabilities {
				capSet[cap] = true
			}
		}
	}
	if len(capSet) == 0 {
		return nil
	}
	var caps []string
	for c := range capSet {
		caps = append(caps, c)
	}
	return &CapabilityManifest{Required: caps}
}
