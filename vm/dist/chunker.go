package dist

import (
	"github.com/chazu/maggie/vm"
)

// MethodToChunk creates a Chunk from a CompiledMethod. The chunk carries
// the method's source text and content hash. The receiver compiles the
// source and verifies the hash matches.
func MethodToChunk(m *vm.CompiledMethod, caps []string) *Chunk {
	h := m.GetContentHash()
	return &Chunk{
		Hash:         h,
		Type:         ChunkMethod,
		Content:      m.Source,
		Capabilities: caps,
	}
}

// ClassToChunk creates a Chunk from a ClassDigest and its source text.
func ClassToChunk(d *vm.ClassDigest, source string, caps []string) *Chunk {
	deps := make([][32]byte, len(d.MethodHashes))
	copy(deps, d.MethodHashes)
	return &Chunk{
		Hash:         d.Hash,
		Type:         ChunkClass,
		Content:      source,
		Dependencies: deps,
		Capabilities: caps,
	}
}

// ModuleToChunk creates a Chunk representing a module (namespace) that
// groups a set of class hashes.
func ModuleToChunk(namespace string, classHashes [][32]byte, caps []string) *Chunk {
	h := vm.HashClass(namespace, "", "", nil, nil, "", classHashes)
	deps := make([][32]byte, len(classHashes))
	copy(deps, classHashes)
	return &Chunk{
		Hash:         h,
		Type:         ChunkModule,
		Content:      namespace,
		Dependencies: deps,
		Capabilities: caps,
	}
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
