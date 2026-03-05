package dist

import (
	"encoding/hex"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/vm"
)

// DiskCache persists content-addressed chunks to a directory.
// Layout: <dir>/<hex[0:2]>/<hex>.chunk (git-style 2-char prefix dirs).
//
// Content-addressed chunks never need invalidation: a hash maps to exactly
// one chunk forever.
type DiskCache struct {
	dir string
}

// NewDiskCache creates a DiskCache backed by the given directory.
// The directory is created (with parents) if it does not exist.
func NewDiskCache(dir string) (*DiskCache, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("dist: create cache dir: %w", err)
	}
	return &DiskCache{dir: dir}, nil
}

// chunkPath returns the filesystem path for a given content hash.
// Uses the first byte (2 hex chars) as a prefix directory.
func (dc *DiskCache) chunkPath(h [32]byte) string {
	hexStr := hex.EncodeToString(h[:])
	return filepath.Join(dc.dir, hexStr[:2], hexStr+".chunk")
}

// Has reports whether a chunk with the given hash exists on disk.
func (dc *DiskCache) Has(h [32]byte) bool {
	_, err := os.Stat(dc.chunkPath(h))
	return err == nil
}

// Get reads and deserializes a chunk from disk.
func (dc *DiskCache) Get(h [32]byte) (*Chunk, error) {
	data, err := os.ReadFile(dc.chunkPath(h))
	if err != nil {
		return nil, fmt.Errorf("dist: cache get %x: %w", h, err)
	}
	chunk, err := UnmarshalChunk(data)
	if err != nil {
		return nil, fmt.Errorf("dist: cache get %x: %w", h, err)
	}
	return chunk, nil
}

// Put serializes a chunk to CBOR and writes it to disk atomically.
// The write uses a temp file + rename to prevent corrupt files on crash.
func (dc *DiskCache) Put(chunk *Chunk) error {
	path := dc.chunkPath(chunk.Hash)
	prefixDir := filepath.Dir(path)

	if err := os.MkdirAll(prefixDir, 0o755); err != nil {
		return fmt.Errorf("dist: cache put: mkdir: %w", err)
	}

	data, err := MarshalChunk(chunk)
	if err != nil {
		return fmt.Errorf("dist: cache put: marshal: %w", err)
	}

	// Atomic write: temp file in the same directory, then rename.
	tmp, err := os.CreateTemp(prefixDir, ".chunk-*")
	if err != nil {
		return fmt.Errorf("dist: cache put: create temp: %w", err)
	}
	tmpName := tmp.Name()

	if _, err := tmp.Write(data); err != nil {
		tmp.Close()
		os.Remove(tmpName)
		return fmt.Errorf("dist: cache put: write: %w", err)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmpName)
		return fmt.Errorf("dist: cache put: close: %w", err)
	}
	if err := os.Rename(tmpName, path); err != nil {
		os.Remove(tmpName)
		return fmt.Errorf("dist: cache put: rename: %w", err)
	}
	return nil
}

// AllHashes walks the cache directory and returns all stored chunk hashes.
func (dc *DiskCache) AllHashes() ([][32]byte, error) {
	var hashes [][32]byte

	entries, err := os.ReadDir(dc.dir)
	if err != nil {
		return nil, fmt.Errorf("dist: cache walk: %w", err)
	}

	for _, prefixEntry := range entries {
		if !prefixEntry.IsDir() || len(prefixEntry.Name()) != 2 {
			continue
		}
		prefixDir := filepath.Join(dc.dir, prefixEntry.Name())
		files, err := os.ReadDir(prefixDir)
		if err != nil {
			continue
		}
		for _, f := range files {
			name := f.Name()
			if f.IsDir() || !strings.HasSuffix(name, ".chunk") {
				continue
			}
			hexStr := strings.TrimSuffix(name, ".chunk")
			if len(hexStr) != 64 {
				continue
			}
			b, err := hex.DecodeString(hexStr)
			if err != nil || len(b) != 32 {
				continue
			}
			var h [32]byte
			copy(h[:], b)
			hashes = append(hashes, h)
		}
	}
	return hashes, nil
}

// LoadInto populates a ContentStore from the disk cache.
// Only loads chunks not already present in the store.
// Returns the number of chunks loaded.
func (dc *DiskCache) LoadInto(store *vm.ContentStore) (int, error) {
	hashes, err := dc.AllHashes()
	if err != nil {
		return 0, err
	}

	loaded := 0
	for _, h := range hashes {
		if store.HasHash(h) {
			continue
		}
		chunk, err := dc.Get(h)
		if err != nil {
			continue // skip corrupt/unreadable entries
		}
		switch chunk.Type {
		case ChunkMethod:
			m := &vm.CompiledMethod{Source: chunk.Content}
			m.SetContentHash(chunk.Hash)
			store.IndexMethod(m)
			loaded++
		case ChunkClass:
			d, decErr := DecodeClassContent(chunk.Content)
			if decErr != nil {
				// Fallback: treat Content as bare class name for backward compat
				d = &vm.ClassDigest{Name: chunk.Content}
			}
			d.Hash = chunk.Hash
			d.MethodHashes = chunk.Dependencies
			store.IndexClass(d)
			loaded++
		}
	}
	return loaded, nil
}

// SaveFrom writes ContentStore entries to disk that are not already cached.
// Returns the number of chunks written.
func (dc *DiskCache) SaveFrom(store *vm.ContentStore) (int, error) {
	written := 0

	// Save methods
	for _, h := range store.MethodHashes() {
		if dc.Has(h) {
			continue
		}
		m := store.LookupMethod(h)
		if m == nil {
			continue
		}
		chunk := MethodToChunk(m, nil)
		if err := dc.Put(chunk); err != nil {
			return written, fmt.Errorf("dist: save method %x: %w", h, err)
		}
		written++
	}

	// Save classes
	for _, h := range store.ClassHashes() {
		if dc.Has(h) {
			continue
		}
		d := store.LookupClass(h)
		if d == nil {
			continue
		}
		chunk := ClassToChunk(d, nil)
		if err := dc.Put(chunk); err != nil {
			return written, fmt.Errorf("dist: save class %x: %w", h, err)
		}
		written++
	}

	return written, nil
}
