package main

import (
	"bytes"
	"path/filepath"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// TestImageIsReproducible compiles the same lib source into two independent VMs
// and asserts their serialized images are byte-identical. Guards against
// nondeterministic image builds (maggie-k1i): map-iteration order in method
// collection / string-table registration, and cell-variable init emission for
// methods with more than one cell variable.
func TestImageIsReproducible(t *testing.T) {
	files, err := filepath.Glob(filepath.Join("..", "..", "lib", "*.mag"))
	if err != nil || len(files) == 0 {
		t.Fatalf("no lib files: %v", err)
	}

	build := func() []byte {
		v := vm.NewVM()
		v.UseGoCompiler(compiler.Compile)
		if _, err := compileAllFiles(files, v, false); err != nil {
			t.Fatalf("compile: %v", err)
		}
		data, err := v.SaveImageBytes()
		if err != nil {
			t.Fatalf("save: %v", err)
		}
		return data
	}

	a := build()
	b := build()
	if !bytes.Equal(a, b) {
		off := 0
		for off < len(a) && off < len(b) && a[off] == b[off] {
			off++
		}
		t.Fatalf("image not reproducible: %d vs %d bytes, first diff at %d", len(a), len(b), off)
	}
}
