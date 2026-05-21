package vm

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
)

// ---------------------------------------------------------------------------
// VM integration
// ---------------------------------------------------------------------------

// SaveImage saves the VM state to a file.
func (vm *VM) SaveImage(path string) error {
	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("creating image file %q: %w", path, err)
	}
	defer f.Close()

	return vm.SaveImageTo(f)
}

// SaveImageAtomic saves the VM state to a file using crash-safe atomic writes.
// The write protocol is:
//  1. Write to <path>.tmp
//  2. Fsync the temp file to ensure data is on disk
//  3. If <path> exists, rename it to <path>.prev (rollback copy)
//  4. Rename <path>.tmp to <path> (atomic on POSIX)
//  5. Fsync the parent directory to ensure the rename is durable
//
// On crash during step 1-2, the original file is untouched.
// On crash during step 3-4, <path>.prev contains the previous valid image.
// After successful completion, <path>.prev is retained as a rollback copy.
func (vm *VM) SaveImageAtomic(path string) error {
	tmpPath := path + ".tmp"
	prevPath := path + ".prev"

	// Step 1: Write to temp file
	f, err := os.Create(tmpPath)
	if err != nil {
		return fmt.Errorf("creating temp image file %q: %w", tmpPath, err)
	}

	if err := vm.SaveImageTo(f); err != nil {
		f.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("writing image to temp file: %w", err)
	}

	// Step 2: Fsync to ensure data is on disk
	if err := f.Sync(); err != nil {
		f.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("fsyncing temp image file: %w", err)
	}
	f.Close()

	// Step 3: If target exists, rename to .prev for rollback
	if _, err := os.Stat(path); err == nil {
		// Remove old .prev if it exists
		os.Remove(prevPath)
		if err := os.Rename(path, prevPath); err != nil {
			os.Remove(tmpPath)
			return fmt.Errorf("renaming previous image to %q: %w", prevPath, err)
		}
	}

	// Step 4: Atomic rename of tmp to target
	if err := os.Rename(tmpPath, path); err != nil {
		return fmt.Errorf("atomic rename of temp image to %q: %w", path, err)
	}

	// Step 5: Fsync parent directory for rename durability
	dir, err := os.Open(filepath.Dir(path))
	if err == nil {
		dir.Sync()
		dir.Close()
	}

	return nil
}

// SaveImageTo saves the VM state to a writer using CBOR format.
func (vm *VM) SaveImageTo(w io.Writer) error {
	data, err := vm.SaveImageCborBytes()
	if err != nil {
		return err
	}
	_, err = w.Write(data)
	return err
}

// CollectAllObjects traverses from roots and collects all reachable objects.
// Iteration order is deterministic: globals are visited in sorted key order,
// classes in sorted full-name order, and class variables in sorted name order.
func (vm *VM) CollectAllObjects() []*Object {
	visited := make(map[uintptr]bool)
	var objects []*Object

	var visit func(v Value)
	visit = func(v Value) {
		if !v.IsObject() {
			return
		}

		ptr := uintptr(v.ObjectPtr())
		if visited[ptr] {
			return
		}
		visited[ptr] = true

		obj := ObjectFromValue(v)
		if obj == nil {
			return
		}

		objects = append(objects, obj)

		// Visit slots
		obj.ForEachSlot(func(index int, slotVal Value) {
			visit(slotVal)
		})
	}

	// Visit globals in sorted key order for deterministic output
	vm.globalsMu.RLock()
	globalNames := make([]string, 0, len(vm.globals))
	for name := range vm.globals {
		globalNames = append(globalNames, name)
	}
	globalsCopy := make(map[string]Value, len(vm.globals))
	for k, v := range vm.globals {
		globalsCopy[k] = v
	}
	vm.globalsMu.RUnlock()
	sort.Strings(globalNames)
	for _, name := range globalNames {
		visit(globalsCopy[name])
	}

	// Visit class variable values in deterministic order
	allClasses := vm.Classes.All()
	sort.Slice(allClasses, func(i, j int) bool {
		return allClasses[i].FullName() < allClasses[j].FullName()
	})
	for _, class := range allClasses {
		vars := vm.registry.GetClassVarStorage(class)
		if len(vars) > 0 {
			varNames := make([]string, 0, len(vars))
			for name := range vars {
				varNames = append(varNames, name)
			}
			sort.Strings(varNames)
			for _, name := range varNames {
				visit(vars[name])
			}
		}
	}

	return objects
}
