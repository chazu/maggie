package vm

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// FileInFunc is the type for the function that compiles source text into the VM.
// It is set by the CLI layer after compiler initialization to avoid circular imports.
// Parameters: vm, source text, source path, namespace override, verbose.
// Returns: method count, error.
type FileInFunc func(v *VM, source string, sourcePath string, nsOverride string, verbose bool) (int, error)

// SetFileInFunc sets the function used by FileIn to compile source text.
func (vm *VM) SetFileInFunc(fn FileInFunc) {
	vm.fileInFunc = fn
}

// FileIn reads a .mag file and compiles it into the VM.
// Returns the number of methods compiled.
func (vm *VM) FileIn(path string) (int, error) {
	if vm.fileInFunc == nil {
		return 0, fmt.Errorf("fileIn: compiler not available (fileInFunc not set)")
	}

	content, err := os.ReadFile(path)
	if err != nil {
		return 0, fmt.Errorf("fileIn: cannot read %q: %w", path, err)
	}

	return vm.fileInFunc(vm, string(content), path, "", false)
}

// FileInSource compiles source text directly into the VM (for REPL use).
// sourcePath is used for error messages and namespace derivation.
func (vm *VM) FileInSource(source string, sourcePath string) (int, error) {
	if vm.fileInFunc == nil {
		return 0, fmt.Errorf("fileInSource: compiler not available (fileInFunc not set)")
	}

	return vm.fileInFunc(vm, source, sourcePath, "", false)
}

// FileInAll recursively loads all .mag files from a directory.
// Returns the total number of methods compiled.
func (vm *VM) FileInAll(dirPath string) (int, error) {
	if vm.fileInFunc == nil {
		return 0, fmt.Errorf("fileInAll: compiler not available (fileInFunc not set)")
	}

	dirPath, err := filepath.Abs(dirPath)
	if err != nil {
		return 0, fmt.Errorf("fileInAll: invalid path %q: %w", dirPath, err)
	}

	var files []string
	err = filepath.Walk(dirPath, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(p, ".mag") {
			files = append(files, p)
		}
		return nil
	})
	if err != nil {
		return 0, fmt.Errorf("fileInAll: walking %q: %w", dirPath, err)
	}

	totalMethods := 0
	for _, file := range files {
		n, err := vm.FileIn(file)
		if err != nil {
			return totalMethods, fmt.Errorf("fileInAll: %w", err)
		}
		totalMethods += n
	}

	return totalMethods, nil
}
