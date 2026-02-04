package vm

import (
	"os"
	"path/filepath"
)

// ---------------------------------------------------------------------------
// File I/O Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerFilePrimitives() {
	// Create File class
	fileClass := vm.createClass("File", vm.ObjectClass)
	vm.Globals["File"] = vm.classValue(fileClass)

	// ---------------------------------------------------------------------------
	// Reading files
	// ---------------------------------------------------------------------------

	// readFileContents: path - Read entire file contents as a string
	// Returns the file contents, or a Failure if the file can't be read
	fileClass.AddClassMethod1(vm.Selectors, "readFileContents:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("readFileContents: requires a path string")
		}

		content, err := os.ReadFile(path)
		if err != nil {
			return v.newFailureResult("Cannot read file: " + err.Error())
		}

		return v.registry.NewStringValue(string(content))
	})

	// ---------------------------------------------------------------------------
	// Writing files
	// ---------------------------------------------------------------------------

	// writeFileContents:contents: - Write string contents to a file
	// Returns Success with the path, or Failure if write fails
	fileClass.AddClassMethod2(vm.Selectors, "writeFileContents:contents:", func(vmPtr interface{}, recv Value, pathVal, contentsVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("writeFileContents:contents: requires a path string")
		}

		contents := v.valueToString(contentsVal)

		err := os.WriteFile(path, []byte(contents), 0644)
		if err != nil {
			return v.newFailureResult("Cannot write file: " + err.Error())
		}

		return v.newSuccessResult(pathVal)
	})

	// appendToFile:contents: - Append string contents to a file
	// Creates the file if it doesn't exist
	fileClass.AddClassMethod2(vm.Selectors, "appendToFile:contents:", func(vmPtr interface{}, recv Value, pathVal, contentsVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("appendToFile:contents: requires a path string")
		}

		contents := v.valueToString(contentsVal)

		f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			return v.newFailureResult("Cannot open file: " + err.Error())
		}
		defer f.Close()

		_, err = f.WriteString(contents)
		if err != nil {
			return v.newFailureResult("Cannot append to file: " + err.Error())
		}

		return v.newSuccessResult(pathVal)
	})

	// ---------------------------------------------------------------------------
	// File existence and info
	// ---------------------------------------------------------------------------

	// exists: path - Check if a file or directory exists
	// Returns true or false
	fileClass.AddClassMethod1(vm.Selectors, "exists:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return False
		}

		_, err := os.Stat(path)
		if err == nil {
			return True
		}
		return False
	})

	// isDirectory: path - Check if path is a directory
	fileClass.AddClassMethod1(vm.Selectors, "isDirectory:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return False
		}

		info, err := os.Stat(path)
		if err != nil {
			return False
		}
		if info.IsDir() {
			return True
		}
		return False
	})

	// isFile: path - Check if path is a regular file
	fileClass.AddClassMethod1(vm.Selectors, "isFile:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return False
		}

		info, err := os.Stat(path)
		if err != nil {
			return False
		}
		if info.Mode().IsRegular() {
			return True
		}
		return False
	})

	// ---------------------------------------------------------------------------
	// Directory operations
	// ---------------------------------------------------------------------------

	// listDirectory: path - List files and directories in a directory
	// Returns an array of filenames, or Failure if directory can't be read
	fileClass.AddClassMethod1(vm.Selectors, "listDirectory:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("listDirectory: requires a path string")
		}

		entries, err := os.ReadDir(path)
		if err != nil {
			return v.newFailureResult("Cannot read directory: " + err.Error())
		}

		values := make([]Value, len(entries))
		for i, entry := range entries {
			values[i] = v.registry.NewStringValue(entry.Name())
		}
		return v.NewArrayWithElements(values)
	})

	// createDirectory: path - Create a directory (and parents if needed)
	fileClass.AddClassMethod1(vm.Selectors, "createDirectory:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("createDirectory: requires a path string")
		}

		err := os.MkdirAll(path, 0755)
		if err != nil {
			return v.newFailureResult("Cannot create directory: " + err.Error())
		}

		return v.newSuccessResult(pathVal)
	})

	// ---------------------------------------------------------------------------
	// File manipulation
	// ---------------------------------------------------------------------------

	// delete: path - Delete a file or empty directory
	fileClass.AddClassMethod1(vm.Selectors, "delete:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)

		path := v.valueToString(pathVal)
		if path == "" {
			return v.newFailureResult("delete: requires a path string")
		}

		err := os.Remove(path)
		if err != nil {
			return v.newFailureResult("Cannot delete: " + err.Error())
		}

		return v.newSuccessResult(pathVal)
	})

	// rename:to: - Rename/move a file or directory
	fileClass.AddClassMethod2(vm.Selectors, "rename:to:", func(vmPtr interface{}, recv Value, oldPathVal, newPathVal Value) Value {
		v := vmPtr.(*VM)

		oldPath := v.valueToString(oldPathVal)
		newPath := v.valueToString(newPathVal)
		if oldPath == "" || newPath == "" {
			return v.newFailureResult("rename:to: requires path strings")
		}

		err := os.Rename(oldPath, newPath)
		if err != nil {
			return v.newFailureResult("Cannot rename: " + err.Error())
		}

		return v.newSuccessResult(newPathVal)
	})

	// copy:to: - Copy a file
	fileClass.AddClassMethod2(vm.Selectors, "copy:to:", func(vmPtr interface{}, recv Value, srcVal, dstVal Value) Value {
		v := vmPtr.(*VM)

		src := v.valueToString(srcVal)
		dst := v.valueToString(dstVal)
		if src == "" || dst == "" {
			return v.newFailureResult("copy:to: requires path strings")
		}

		content, err := os.ReadFile(src)
		if err != nil {
			return v.newFailureResult("Cannot read source: " + err.Error())
		}

		err = os.WriteFile(dst, content, 0644)
		if err != nil {
			return v.newFailureResult("Cannot write destination: " + err.Error())
		}

		return v.newSuccessResult(dstVal)
	})

	// ---------------------------------------------------------------------------
	// Path manipulation
	// ---------------------------------------------------------------------------

	// basename: path - Get the last element of a path
	fileClass.AddClassMethod1(vm.Selectors, "basename:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		return v.registry.NewStringValue(filepath.Base(path))
	})

	// dirname: path - Get the directory portion of a path
	fileClass.AddClassMethod1(vm.Selectors, "dirname:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		return v.registry.NewStringValue(filepath.Dir(path))
	})

	// extension: path - Get the file extension (including dot)
	fileClass.AddClassMethod1(vm.Selectors, "extension:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		return v.registry.NewStringValue(filepath.Ext(path))
	})

	// join:with: - Join path components
	fileClass.AddClassMethod2(vm.Selectors, "join:with:", func(vmPtr interface{}, recv Value, path1Val, path2Val Value) Value {
		v := vmPtr.(*VM)
		path1 := v.valueToString(path1Val)
		path2 := v.valueToString(path2Val)
		return v.registry.NewStringValue(filepath.Join(path1, path2))
	})

	// absolutePath: path - Convert to absolute path
	fileClass.AddClassMethod1(vm.Selectors, "absolutePath:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		path := v.valueToString(pathVal)
		abs, err := filepath.Abs(path)
		if err != nil {
			return pathVal // Return original on error
		}
		return v.registry.NewStringValue(abs)
	})

	// workingDirectory - Get current working directory
	fileClass.AddClassMethod0(vm.Selectors, "workingDirectory", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		wd, err := os.Getwd()
		if err != nil {
			return v.newFailureResult("Cannot get working directory: " + err.Error())
		}
		return v.registry.NewStringValue(wd)
	})

	// homeDirectory - Get user's home directory
	fileClass.AddClassMethod0(vm.Selectors, "homeDirectory", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		home, err := os.UserHomeDir()
		if err != nil {
			return v.newFailureResult("Cannot get home directory: " + err.Error())
		}
		return v.registry.NewStringValue(home)
	})
}

// valueToString converts a Value to a Go string.
// Works with String values and Symbol values.
func (vm *VM) valueToString(v Value) string {
	if IsStringValue(v) {
		return vm.registry.GetStringContent(v)
	}
	if v.IsSymbol() {
		return vm.Symbols.Name(v.SymbolID())
	}
	return ""
}

// newSuccessResult creates a Success result wrapping a value.
func (vm *VM) newSuccessResult(val Value) Value {
	r := createResult(ResultSuccess, val)
	return vm.registry.RegisterResultValue(r)
}
