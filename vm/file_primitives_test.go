package vm

import (
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"testing"
)

// fileClass returns the File class value from the VM globals.
func fileClass(vm *VM) Value {
	return vm.Globals["File"]
}

// assertSuccess checks that a value is a Success result and returns the unwrapped value.
func assertSuccess(t *testing.T, vm *VM, result Value, context string) Value {
	t.Helper()
	if !isResultValue(result) {
		t.Fatalf("%s: expected a Result value, got non-result", context)
	}
	isSuccess := vm.Send(result, "isSuccess", nil)
	if isSuccess != True {
		// Extract error message for better diagnostics
		errVal := vm.Send(result, "error", nil)
		errMsg := ""
		if IsStringValue(errVal) {
			errMsg = GetStringContent(errVal)
		}
		t.Fatalf("%s: expected Success result, got Failure: %s", context, errMsg)
	}
	return vm.Send(result, "value", nil)
}

// assertFailure checks that a value is a Failure result and returns the error value.
func assertFailure(t *testing.T, vm *VM, result Value, context string) Value {
	t.Helper()
	if !isResultValue(result) {
		t.Fatalf("%s: expected a Result value, got non-result", context)
	}
	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Fatalf("%s: expected Failure result, got Success", context)
	}
	return vm.Send(result, "error", nil)
}

// ---------------------------------------------------------------------------
// readFileContents:
// ---------------------------------------------------------------------------

func TestFileReadFileContents(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Write a test file from Go side
	testFile := filepath.Join(tmpDir, "hello.txt")
	if err := os.WriteFile(testFile, []byte("Hello, Maggie!"), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	if !IsStringValue(result) {
		t.Fatalf("readFileContents: did not return a string, got result-like value")
	}
	content := GetStringContent(result)
	if content != "Hello, Maggie!" {
		t.Errorf("readFileContents: returned %q, want %q", content, "Hello, Maggie!")
	}
}

func TestFileReadFileContentsEmpty(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Write an empty file
	testFile := filepath.Join(tmpDir, "empty.txt")
	if err := os.WriteFile(testFile, []byte{}, 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	if !IsStringValue(result) {
		t.Fatalf("readFileContents: of empty file did not return a string")
	}
	content := GetStringContent(result)
	if content != "" {
		t.Errorf("readFileContents: of empty file returned %q, want empty string", content)
	}
}

func TestFileReadFileContentsUnicode(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	unicodeContent := "Hello \u4e16\u754c! \U0001f600 \u00e9\u00e8\u00ea \u00fc\u00f6\u00e4"
	testFile := filepath.Join(tmpDir, "unicode.txt")
	if err := os.WriteFile(testFile, []byte(unicodeContent), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	if !IsStringValue(result) {
		t.Fatalf("readFileContents: of unicode file did not return a string")
	}
	content := GetStringContent(result)
	if content != unicodeContent {
		t.Errorf("readFileContents: returned %q, want %q", content, unicodeContent)
	}
}

func TestFileReadFileContentsMultiline(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	multiline := "line1\nline2\nline3\n"
	testFile := filepath.Join(tmpDir, "multiline.txt")
	if err := os.WriteFile(testFile, []byte(multiline), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	if !IsStringValue(result) {
		t.Fatalf("readFileContents: of multiline file did not return a string")
	}
	content := GetStringContent(result)
	if content != multiline {
		t.Errorf("readFileContents: returned %q, want %q", content, multiline)
	}
}

func TestFileReadFileContentsNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue("/nonexistent/path/file.txt")})
	assertFailure(t, vm, result, "readFileContents: nonexistent file")
}

func TestFileReadFileContentsEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue("")})
	assertFailure(t, vm, result, "readFileContents: empty path")
}

// ---------------------------------------------------------------------------
// writeFileContents:contents:
// ---------------------------------------------------------------------------

func TestFileWriteFileContents(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "output.txt")
	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("Written by Maggie"),
	})

	assertSuccess(t, vm, result, "writeFileContents:contents:")

	// Verify from Go side
	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read written file: %v", err)
	}
	if string(data) != "Written by Maggie" {
		t.Errorf("Written file contains %q, want %q", string(data), "Written by Maggie")
	}
}

func TestFileWriteFileContentsOverwrite(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "overwrite.txt")

	// Write initial content
	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("initial content"),
	})

	// Overwrite
	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("new content"),
	})
	assertSuccess(t, vm, result, "writeFileContents:contents: overwrite")

	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	if string(data) != "new content" {
		t.Errorf("File contains %q after overwrite, want %q", string(data), "new content")
	}
}

func TestFileWriteFileContentsEmptyContent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "empty_write.txt")
	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue(""),
	})
	assertSuccess(t, vm, result, "writeFileContents:contents: empty")

	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	if string(data) != "" {
		t.Errorf("File contains %q, want empty", string(data))
	}
}

func TestFileWriteFileContentsUnicode(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	unicodeContent := "\u00e9\u00e8\u00ea \U0001f600 \u4e16\u754c"
	testFile := filepath.Join(tmpDir, "unicode_write.txt")
	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue(unicodeContent),
	})
	assertSuccess(t, vm, result, "writeFileContents:contents: unicode")

	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	if string(data) != unicodeContent {
		t.Errorf("File contains %q, want %q", string(data), unicodeContent)
	}
}

func TestFileWriteFileContentsInvalidPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue("/nonexistent/dir/file.txt"),
		NewStringValue("data"),
	})
	assertFailure(t, vm, result, "writeFileContents:contents: invalid path")
}

func TestFileWriteFileContentsEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(""),
		NewStringValue("data"),
	})
	assertFailure(t, vm, result, "writeFileContents:contents: empty path")
}

// ---------------------------------------------------------------------------
// appendToFile:contents:
// ---------------------------------------------------------------------------

func TestFileAppendToFile(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "append.txt")

	// Write initial content
	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("Hello"),
	})

	// Append
	result := vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue(", World!"),
	})
	assertSuccess(t, vm, result, "appendToFile:contents:")

	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	if string(data) != "Hello, World!" {
		t.Errorf("File contains %q, want %q", string(data), "Hello, World!")
	}
}

func TestFileAppendToFileCreatesNew(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "new_append.txt")

	// Append to non-existent file should create it
	result := vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("created by append"),
	})
	assertSuccess(t, vm, result, "appendToFile:contents: creates new file")

	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	if string(data) != "created by append" {
		t.Errorf("File contains %q, want %q", string(data), "created by append")
	}
}

func TestFileAppendToFileMultiple(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "multi_append.txt")

	// Multiple appends
	vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("one"),
	})
	vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("two"),
	})
	vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("three"),
	})

	data, err := os.ReadFile(testFile)
	if err != nil {
		t.Fatalf("Failed to read file: %v", err)
	}
	if string(data) != "onetwothree" {
		t.Errorf("File contains %q, want %q", string(data), "onetwothree")
	}
}

func TestFileAppendToFileEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue(""),
		NewStringValue("data"),
	})
	assertFailure(t, vm, result, "appendToFile:contents: empty path")
}

func TestFileAppendToFileInvalidPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "appendToFile:contents:", []Value{
		NewStringValue("/nonexistent/dir/file.txt"),
		NewStringValue("data"),
	})
	assertFailure(t, vm, result, "appendToFile:contents: invalid path")
}

// ---------------------------------------------------------------------------
// exists:
// ---------------------------------------------------------------------------

func TestFileExists(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Create a file
	testFile := filepath.Join(tmpDir, "exists.txt")
	if err := os.WriteFile(testFile, []byte("hi"), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "exists:", []Value{NewStringValue(testFile)})
	if result != True {
		t.Errorf("exists: for existing file returned %v, want True", result)
	}
}

func TestFileExistsDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	result := vm.Send(fc, "exists:", []Value{NewStringValue(tmpDir)})
	if result != True {
		t.Errorf("exists: for existing directory returned %v, want True", result)
	}
}

func TestFileExistsNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "exists:", []Value{NewStringValue("/nonexistent/path/xyz")})
	if result != False {
		t.Errorf("exists: for nonexistent path returned %v, want False", result)
	}
}

func TestFileExistsEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "exists:", []Value{NewStringValue("")})
	if result != False {
		t.Errorf("exists: for empty path returned %v, want False", result)
	}
}

// ---------------------------------------------------------------------------
// isDirectory:
// ---------------------------------------------------------------------------

func TestFileIsDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	result := vm.Send(fc, "isDirectory:", []Value{NewStringValue(tmpDir)})
	if result != True {
		t.Errorf("isDirectory: for directory returned %v, want True", result)
	}
}

func TestFileIsDirectoryOnFile(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "afile.txt")
	if err := os.WriteFile(testFile, []byte("data"), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "isDirectory:", []Value{NewStringValue(testFile)})
	if result != False {
		t.Errorf("isDirectory: for regular file returned %v, want False", result)
	}
}

func TestFileIsDirectoryNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "isDirectory:", []Value{NewStringValue("/nonexistent/dir")})
	if result != False {
		t.Errorf("isDirectory: for nonexistent path returned %v, want False", result)
	}
}

func TestFileIsDirectoryEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "isDirectory:", []Value{NewStringValue("")})
	if result != False {
		t.Errorf("isDirectory: for empty path returned %v, want False", result)
	}
}

// ---------------------------------------------------------------------------
// isFile:
// ---------------------------------------------------------------------------

func TestFileIsFile(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "regular.txt")
	if err := os.WriteFile(testFile, []byte("data"), 0644); err != nil {
		t.Fatal(err)
	}

	result := vm.Send(fc, "isFile:", []Value{NewStringValue(testFile)})
	if result != True {
		t.Errorf("isFile: for regular file returned %v, want True", result)
	}
}

func TestFileIsFileOnDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	result := vm.Send(fc, "isFile:", []Value{NewStringValue(tmpDir)})
	if result != False {
		t.Errorf("isFile: for directory returned %v, want False", result)
	}
}

func TestFileIsFileNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "isFile:", []Value{NewStringValue("/nonexistent/file")})
	if result != False {
		t.Errorf("isFile: for nonexistent path returned %v, want False", result)
	}
}

func TestFileIsFileEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "isFile:", []Value{NewStringValue("")})
	if result != False {
		t.Errorf("isFile: for empty path returned %v, want False", result)
	}
}

// ---------------------------------------------------------------------------
// listDirectory:
// ---------------------------------------------------------------------------

func TestFileListDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Create some files and a subdirectory
	os.WriteFile(filepath.Join(tmpDir, "alpha.txt"), []byte("a"), 0644)
	os.WriteFile(filepath.Join(tmpDir, "beta.txt"), []byte("b"), 0644)
	os.Mkdir(filepath.Join(tmpDir, "subdir"), 0755)

	result := vm.Send(fc, "listDirectory:", []Value{NewStringValue(tmpDir)})

	if !result.IsObject() {
		t.Fatalf("listDirectory: did not return an object (array)")
	}

	arr := ObjectFromValue(result)
	if arr == nil {
		t.Fatal("listDirectory: returned nil object")
	}
	if arr.NumSlots() != 3 {
		t.Errorf("listDirectory: returned %d entries, want 3", arr.NumSlots())
	}

	// Collect names and sort for deterministic comparison
	names := make([]string, arr.NumSlots())
	for i := 0; i < arr.NumSlots(); i++ {
		v := arr.GetSlot(i)
		if !IsStringValue(v) {
			t.Fatalf("listDirectory: entry %d is not a string", i)
		}
		names[i] = GetStringContent(v)
	}
	sort.Strings(names)

	expected := []string{"alpha.txt", "beta.txt", "subdir"}
	for i, want := range expected {
		if names[i] != want {
			t.Errorf("listDirectory: entry %d = %q, want %q", i, names[i], want)
		}
	}
}

func TestFileListDirectoryEmpty(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	result := vm.Send(fc, "listDirectory:", []Value{NewStringValue(tmpDir)})

	if !result.IsObject() {
		t.Fatalf("listDirectory: of empty dir did not return an object")
	}

	arr := ObjectFromValue(result)
	if arr == nil {
		t.Fatal("listDirectory: returned nil object")
	}
	if arr.NumSlots() != 0 {
		t.Errorf("listDirectory: of empty dir returned %d entries, want 0", arr.NumSlots())
	}
}

func TestFileListDirectoryNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "listDirectory:", []Value{NewStringValue("/nonexistent/dir")})
	assertFailure(t, vm, result, "listDirectory: nonexistent")
}

func TestFileListDirectoryEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "listDirectory:", []Value{NewStringValue("")})
	assertFailure(t, vm, result, "listDirectory: empty path")
}

func TestFileListDirectoryOnFile(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "notadir.txt")
	os.WriteFile(testFile, []byte("data"), 0644)

	result := vm.Send(fc, "listDirectory:", []Value{NewStringValue(testFile)})
	assertFailure(t, vm, result, "listDirectory: on a file")
}

// ---------------------------------------------------------------------------
// createDirectory:
// ---------------------------------------------------------------------------

func TestFileCreateDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	newDir := filepath.Join(tmpDir, "newdir")
	result := vm.Send(fc, "createDirectory:", []Value{NewStringValue(newDir)})
	assertSuccess(t, vm, result, "createDirectory:")

	info, err := os.Stat(newDir)
	if err != nil {
		t.Fatalf("Created directory does not exist: %v", err)
	}
	if !info.IsDir() {
		t.Error("createDirectory: did not create a directory")
	}
}

func TestFileCreateDirectoryNested(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// MkdirAll should create intermediate directories
	nestedDir := filepath.Join(tmpDir, "a", "b", "c")
	result := vm.Send(fc, "createDirectory:", []Value{NewStringValue(nestedDir)})
	assertSuccess(t, vm, result, "createDirectory: nested")

	info, err := os.Stat(nestedDir)
	if err != nil {
		t.Fatalf("Nested directory does not exist: %v", err)
	}
	if !info.IsDir() {
		t.Error("createDirectory: nested did not create a directory")
	}
}

func TestFileCreateDirectoryAlreadyExists(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Creating an existing directory with MkdirAll should succeed
	result := vm.Send(fc, "createDirectory:", []Value{NewStringValue(tmpDir)})
	assertSuccess(t, vm, result, "createDirectory: already exists")
}

func TestFileCreateDirectoryEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "createDirectory:", []Value{NewStringValue("")})
	assertFailure(t, vm, result, "createDirectory: empty path")
}

// ---------------------------------------------------------------------------
// delete:
// ---------------------------------------------------------------------------

func TestFileDelete(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "todelete.txt")
	os.WriteFile(testFile, []byte("bye"), 0644)

	result := vm.Send(fc, "delete:", []Value{NewStringValue(testFile)})
	assertSuccess(t, vm, result, "delete:")

	if _, err := os.Stat(testFile); !os.IsNotExist(err) {
		t.Error("delete: did not remove the file")
	}
}

func TestFileDeleteEmptyDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	emptyDir := filepath.Join(tmpDir, "emptydir")
	os.Mkdir(emptyDir, 0755)

	result := vm.Send(fc, "delete:", []Value{NewStringValue(emptyDir)})
	assertSuccess(t, vm, result, "delete: empty directory")

	if _, err := os.Stat(emptyDir); !os.IsNotExist(err) {
		t.Error("delete: did not remove the empty directory")
	}
}

func TestFileDeleteNonEmptyDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	nonEmptyDir := filepath.Join(tmpDir, "nonempty")
	os.Mkdir(nonEmptyDir, 0755)
	os.WriteFile(filepath.Join(nonEmptyDir, "child.txt"), []byte("x"), 0644)

	// os.Remove only removes empty directories, so this should fail
	result := vm.Send(fc, "delete:", []Value{NewStringValue(nonEmptyDir)})
	assertFailure(t, vm, result, "delete: non-empty directory")
}

func TestFileDeleteNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "delete:", []Value{NewStringValue("/nonexistent/file.txt")})
	assertFailure(t, vm, result, "delete: nonexistent")
}

func TestFileDeleteEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "delete:", []Value{NewStringValue("")})
	assertFailure(t, vm, result, "delete: empty path")
}

// ---------------------------------------------------------------------------
// rename:to:
// ---------------------------------------------------------------------------

func TestFileRename(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	oldPath := filepath.Join(tmpDir, "old.txt")
	newPath := filepath.Join(tmpDir, "new.txt")
	os.WriteFile(oldPath, []byte("content"), 0644)

	result := vm.Send(fc, "rename:to:", []Value{
		NewStringValue(oldPath),
		NewStringValue(newPath),
	})
	assertSuccess(t, vm, result, "rename:to:")

	// Old file should not exist
	if _, err := os.Stat(oldPath); !os.IsNotExist(err) {
		t.Error("rename:to: did not remove old file")
	}

	// New file should exist with same content
	data, err := os.ReadFile(newPath)
	if err != nil {
		t.Fatalf("Cannot read renamed file: %v", err)
	}
	if string(data) != "content" {
		t.Errorf("Renamed file contains %q, want %q", string(data), "content")
	}
}

func TestFileRenameDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	oldDir := filepath.Join(tmpDir, "olddir")
	newDir := filepath.Join(tmpDir, "newdir")
	os.Mkdir(oldDir, 0755)
	os.WriteFile(filepath.Join(oldDir, "file.txt"), []byte("inside"), 0644)

	result := vm.Send(fc, "rename:to:", []Value{
		NewStringValue(oldDir),
		NewStringValue(newDir),
	})
	assertSuccess(t, vm, result, "rename:to: directory")

	// Verify contents moved
	data, err := os.ReadFile(filepath.Join(newDir, "file.txt"))
	if err != nil {
		t.Fatalf("Cannot read file in renamed directory: %v", err)
	}
	if string(data) != "inside" {
		t.Errorf("File in renamed dir contains %q, want %q", string(data), "inside")
	}
}

func TestFileRenameNonexistent(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	result := vm.Send(fc, "rename:to:", []Value{
		NewStringValue(filepath.Join(tmpDir, "nope.txt")),
		NewStringValue(filepath.Join(tmpDir, "dest.txt")),
	})
	assertFailure(t, vm, result, "rename:to: nonexistent source")
}

func TestFileRenameEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "rename:to:", []Value{
		NewStringValue(""),
		NewStringValue("/tmp/dest.txt"),
	})
	assertFailure(t, vm, result, "rename:to: empty old path")

	result = vm.Send(fc, "rename:to:", []Value{
		NewStringValue("/tmp/src.txt"),
		NewStringValue(""),
	})
	assertFailure(t, vm, result, "rename:to: empty new path")
}

// ---------------------------------------------------------------------------
// copy:to:
// ---------------------------------------------------------------------------

func TestFileCopy(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	srcFile := filepath.Join(tmpDir, "source.txt")
	dstFile := filepath.Join(tmpDir, "dest.txt")
	os.WriteFile(srcFile, []byte("copy me"), 0644)

	result := vm.Send(fc, "copy:to:", []Value{
		NewStringValue(srcFile),
		NewStringValue(dstFile),
	})
	assertSuccess(t, vm, result, "copy:to:")

	// Source should still exist
	srcData, err := os.ReadFile(srcFile)
	if err != nil {
		t.Fatalf("Source file was removed after copy: %v", err)
	}
	if string(srcData) != "copy me" {
		t.Errorf("Source file modified after copy: %q", string(srcData))
	}

	// Destination should have same content
	dstData, err := os.ReadFile(dstFile)
	if err != nil {
		t.Fatalf("Cannot read destination file: %v", err)
	}
	if string(dstData) != "copy me" {
		t.Errorf("Destination file contains %q, want %q", string(dstData), "copy me")
	}
}

func TestFileCopyOverwrite(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	srcFile := filepath.Join(tmpDir, "src.txt")
	dstFile := filepath.Join(tmpDir, "dst.txt")
	os.WriteFile(srcFile, []byte("new"), 0644)
	os.WriteFile(dstFile, []byte("old"), 0644)

	result := vm.Send(fc, "copy:to:", []Value{
		NewStringValue(srcFile),
		NewStringValue(dstFile),
	})
	assertSuccess(t, vm, result, "copy:to: overwrite")

	data, err := os.ReadFile(dstFile)
	if err != nil {
		t.Fatal(err)
	}
	if string(data) != "new" {
		t.Errorf("Destination after overwrite contains %q, want %q", string(data), "new")
	}
}

func TestFileCopyNonexistentSource(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	result := vm.Send(fc, "copy:to:", []Value{
		NewStringValue(filepath.Join(tmpDir, "nope.txt")),
		NewStringValue(filepath.Join(tmpDir, "dest.txt")),
	})
	assertFailure(t, vm, result, "copy:to: nonexistent source")
}

func TestFileCopyInvalidDest(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	srcFile := filepath.Join(tmpDir, "src.txt")
	os.WriteFile(srcFile, []byte("data"), 0644)

	result := vm.Send(fc, "copy:to:", []Value{
		NewStringValue(srcFile),
		NewStringValue("/nonexistent/dir/dest.txt"),
	})
	assertFailure(t, vm, result, "copy:to: invalid destination path")
}

func TestFileCopyEmptyPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "copy:to:", []Value{
		NewStringValue(""),
		NewStringValue("/tmp/dest.txt"),
	})
	assertFailure(t, vm, result, "copy:to: empty source path")

	result = vm.Send(fc, "copy:to:", []Value{
		NewStringValue("/tmp/src.txt"),
		NewStringValue(""),
	})
	assertFailure(t, vm, result, "copy:to: empty dest path")
}

// ---------------------------------------------------------------------------
// basename:
// ---------------------------------------------------------------------------

func TestFileBasename(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	tests := []struct {
		path string
		want string
	}{
		{"/foo/bar/baz.txt", "baz.txt"},
		{"/foo/bar/", "bar"},
		{"file.txt", "file.txt"},
		{"/", "/"},
		{".", "."},
	}

	for _, tc := range tests {
		result := vm.Send(fc, "basename:", []Value{NewStringValue(tc.path)})
		got := GetStringContent(result)
		if got != tc.want {
			t.Errorf("basename: %q = %q, want %q", tc.path, got, tc.want)
		}
	}
}

// ---------------------------------------------------------------------------
// dirname:
// ---------------------------------------------------------------------------

func TestFileDirname(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	tests := []struct {
		path string
		want string
	}{
		{"/foo/bar/baz.txt", "/foo/bar"},
		{"/foo/bar/", "/foo/bar"},
		{"file.txt", "."},
		{"/", "/"},
	}

	for _, tc := range tests {
		result := vm.Send(fc, "dirname:", []Value{NewStringValue(tc.path)})
		got := GetStringContent(result)
		if got != tc.want {
			t.Errorf("dirname: %q = %q, want %q", tc.path, got, tc.want)
		}
	}
}

// ---------------------------------------------------------------------------
// extension:
// ---------------------------------------------------------------------------

func TestFileExtension(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	tests := []struct {
		path string
		want string
	}{
		{"file.txt", ".txt"},
		{"archive.tar.gz", ".gz"},
		{"noext", ""},
		{"/path/to/file.mag", ".mag"},
		{".hidden", ".hidden"},
	}

	for _, tc := range tests {
		result := vm.Send(fc, "extension:", []Value{NewStringValue(tc.path)})
		got := GetStringContent(result)
		if got != tc.want {
			t.Errorf("extension: %q = %q, want %q", tc.path, got, tc.want)
		}
	}
}

// ---------------------------------------------------------------------------
// join:with:
// ---------------------------------------------------------------------------

func TestFileJoinWith(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "join:with:", []Value{
		NewStringValue("/foo/bar"),
		NewStringValue("baz.txt"),
	})
	got := GetStringContent(result)
	want := filepath.Join("/foo/bar", "baz.txt")
	if got != want {
		t.Errorf("join:with: = %q, want %q", got, want)
	}
}

func TestFileJoinWithCleanup(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	// filepath.Join cleans paths
	result := vm.Send(fc, "join:with:", []Value{
		NewStringValue("/foo/bar/"),
		NewStringValue("../baz.txt"),
	})
	got := GetStringContent(result)
	want := filepath.Join("/foo/bar/", "../baz.txt")
	if got != want {
		t.Errorf("join:with: with .. = %q, want %q", got, want)
	}
}

// ---------------------------------------------------------------------------
// absolutePath:
// ---------------------------------------------------------------------------

func TestFileAbsolutePath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "absolutePath:", []Value{NewStringValue("relative/path")})
	got := GetStringContent(result)

	// The result should be absolute
	if !filepath.IsAbs(got) {
		t.Errorf("absolutePath: %q is not absolute", got)
	}
}

func TestFileAbsolutePathAlreadyAbsolute(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	absPath := "/already/absolute"
	if runtime.GOOS == "windows" {
		absPath = "C:\\already\\absolute"
	}

	result := vm.Send(fc, "absolutePath:", []Value{NewStringValue(absPath)})
	got := GetStringContent(result)

	expected, _ := filepath.Abs(absPath)
	if got != expected {
		t.Errorf("absolutePath: %q = %q, want %q", absPath, got, expected)
	}
}

// ---------------------------------------------------------------------------
// workingDirectory
// ---------------------------------------------------------------------------

func TestFileWorkingDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "workingDirectory", nil)
	if !IsStringValue(result) {
		t.Fatalf("workingDirectory did not return a string")
	}

	got := GetStringContent(result)
	expected, err := os.Getwd()
	if err != nil {
		t.Fatalf("os.Getwd failed: %v", err)
	}

	if got != expected {
		t.Errorf("workingDirectory = %q, want %q", got, expected)
	}
}

// ---------------------------------------------------------------------------
// homeDirectory
// ---------------------------------------------------------------------------

func TestFileHomeDirectory(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "homeDirectory", nil)
	if !IsStringValue(result) {
		t.Fatalf("homeDirectory did not return a string")
	}

	got := GetStringContent(result)
	expected, err := os.UserHomeDir()
	if err != nil {
		t.Fatalf("os.UserHomeDir failed: %v", err)
	}

	if got != expected {
		t.Errorf("homeDirectory = %q, want %q", got, expected)
	}
}

// ---------------------------------------------------------------------------
// Integration / round-trip tests
// ---------------------------------------------------------------------------

func TestFileWriteThenRead(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "roundtrip.txt")
	content := "Round-trip test content with special chars: \t\n\r"

	// Write via Maggie
	writeResult := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue(content),
	})
	assertSuccess(t, vm, writeResult, "write in roundtrip")

	// Read via Maggie
	readResult := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	if !IsStringValue(readResult) {
		t.Fatalf("readFileContents: did not return a string in roundtrip")
	}

	got := GetStringContent(readResult)
	if got != content {
		t.Errorf("Round-trip content mismatch: got %q, want %q", got, content)
	}
}

func TestFileWriteDeleteVerify(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "delete_me.txt")

	// Write
	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("temporary"),
	})

	// Verify exists
	existsResult := vm.Send(fc, "exists:", []Value{NewStringValue(testFile)})
	if existsResult != True {
		t.Fatal("File should exist after write")
	}

	// Delete
	delResult := vm.Send(fc, "delete:", []Value{NewStringValue(testFile)})
	assertSuccess(t, vm, delResult, "delete in write-delete-verify")

	// Verify no longer exists
	existsResult = vm.Send(fc, "exists:", []Value{NewStringValue(testFile)})
	if existsResult != False {
		t.Error("File should not exist after delete")
	}

	// Reading deleted file should fail
	readResult := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	assertFailure(t, vm, readResult, "read deleted file")
}

func TestFileCreateDirectoryListAndDelete(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Create directory structure
	subDir := filepath.Join(tmpDir, "mydir")
	vm.Send(fc, "createDirectory:", []Value{NewStringValue(subDir)})

	// Write files inside
	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(filepath.Join(subDir, "a.txt")),
		NewStringValue("aaa"),
	})
	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(filepath.Join(subDir, "b.txt")),
		NewStringValue("bbb"),
	})

	// List the directory
	listResult := vm.Send(fc, "listDirectory:", []Value{NewStringValue(subDir)})
	arr := ObjectFromValue(listResult)
	if arr == nil {
		t.Fatal("listDirectory: returned nil")
	}
	if arr.NumSlots() != 2 {
		t.Errorf("listDirectory: returned %d entries, want 2", arr.NumSlots())
	}

	// Verify isDirectory and isFile
	isDirResult := vm.Send(fc, "isDirectory:", []Value{NewStringValue(subDir)})
	if isDirResult != True {
		t.Error("isDirectory: on created dir should be True")
	}

	isFileResult := vm.Send(fc, "isFile:", []Value{
		NewStringValue(filepath.Join(subDir, "a.txt")),
	})
	if isFileResult != True {
		t.Error("isFile: on created file should be True")
	}
}

func TestFileCopyThenVerifyContents(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	srcFile := filepath.Join(tmpDir, "original.txt")
	dstFile := filepath.Join(tmpDir, "copied.txt")

	// Write original
	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(srcFile),
		NewStringValue("original content"),
	})

	// Copy
	vm.Send(fc, "copy:to:", []Value{
		NewStringValue(srcFile),
		NewStringValue(dstFile),
	})

	// Read copy via Maggie
	readResult := vm.Send(fc, "readFileContents:", []Value{NewStringValue(dstFile)})
	if !IsStringValue(readResult) {
		t.Fatalf("readFileContents: did not return string")
	}
	if GetStringContent(readResult) != "original content" {
		t.Errorf("Copied file content = %q, want %q", GetStringContent(readResult), "original content")
	}
}

func TestFileRenameThenVerify(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	oldFile := filepath.Join(tmpDir, "before.txt")
	newFile := filepath.Join(tmpDir, "after.txt")

	vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(oldFile),
		NewStringValue("rename me"),
	})

	vm.Send(fc, "rename:to:", []Value{
		NewStringValue(oldFile),
		NewStringValue(newFile),
	})

	// Old should not exist
	existsOld := vm.Send(fc, "exists:", []Value{NewStringValue(oldFile)})
	if existsOld != False {
		t.Error("Old file should not exist after rename")
	}

	// New should exist with correct content
	readResult := vm.Send(fc, "readFileContents:", []Value{NewStringValue(newFile)})
	if !IsStringValue(readResult) {
		t.Fatalf("Cannot read renamed file")
	}
	if GetStringContent(readResult) != "rename me" {
		t.Errorf("Renamed file content = %q, want %q", GetStringContent(readResult), "rename me")
	}
}

func TestFileBinaryData(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	// Binary-like data with null bytes and high bytes
	binaryLike := "before\x00after\xff\xfe\x01"
	testFile := filepath.Join(tmpDir, "binary.dat")

	// Write from Go side (since writeFileContents goes through string conversion)
	os.WriteFile(testFile, []byte(binaryLike), 0644)

	// Read via Maggie -- Go strings can hold arbitrary bytes
	result := vm.Send(fc, "readFileContents:", []Value{NewStringValue(testFile)})
	if !IsStringValue(result) {
		t.Fatalf("readFileContents: of binary data did not return a string")
	}
	got := GetStringContent(result)
	if got != binaryLike {
		t.Errorf("Binary data round-trip failed: got %q, want %q", got, binaryLike)
	}
}

// ---------------------------------------------------------------------------
// Path manipulation edge cases
// ---------------------------------------------------------------------------

func TestFileJoinWithEmpty(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "join:with:", []Value{
		NewStringValue(""),
		NewStringValue("file.txt"),
	})
	got := GetStringContent(result)
	want := filepath.Join("", "file.txt")
	if got != want {
		t.Errorf("join:with: empty first = %q, want %q", got, want)
	}
}

func TestFileBasenameNoDir(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "basename:", []Value{NewStringValue("justfile")})
	if GetStringContent(result) != "justfile" {
		t.Errorf("basename: of bare filename = %q, want %q", GetStringContent(result), "justfile")
	}
}

func TestFileExtensionNone(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)

	result := vm.Send(fc, "extension:", []Value{NewStringValue("noextension")})
	if GetStringContent(result) != "" {
		t.Errorf("extension: of file with no ext = %q, want empty", GetStringContent(result))
	}
}

func TestFileSuccessResultContainsPath(t *testing.T) {
	vm := NewVM()
	fc := fileClass(vm)
	tmpDir := t.TempDir()

	testFile := filepath.Join(tmpDir, "success_path.txt")
	result := vm.Send(fc, "writeFileContents:contents:", []Value{
		NewStringValue(testFile),
		NewStringValue("test"),
	})

	// The Success result should wrap the path value
	val := assertSuccess(t, vm, result, "writeFileContents success value")
	if !IsStringValue(val) {
		t.Fatalf("Success value is not a string")
	}
	if GetStringContent(val) != testFile {
		t.Errorf("Success value = %q, want path %q", GetStringContent(val), testFile)
	}
}
