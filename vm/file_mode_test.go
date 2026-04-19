package vm

import (
	"os"
	"path/filepath"
	"testing"
)

func TestWriteFileContentsWithMode(t *testing.T) {
	vm := NewVM()
	dir := t.TempDir()
	path := filepath.Join(dir, "key.bin")

	pathVal := vm.registry.NewStringValue(path)
	body := vm.registry.NewStringValue("hello")
	mode := FromSmallInt(0o600)

	result := sendClass(t, vm, "File", "writeFileContents:contents:mode:", pathVal, body, mode)
	_ = result

	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if got := info.Mode().Perm(); got != 0o600 {
		t.Fatalf("mode = %o, want 0600", got)
	}

	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	if string(data) != "hello" {
		t.Fatalf("content = %q", data)
	}
}
