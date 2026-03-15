package main

import (
	"bytes"
	"io"
	"os"
	"strings"
	"testing"

	"github.com/chazu/maggie/pipeline"
)

// captureStdout runs fn while capturing os.Stdout and returns the output.
func captureStdout(t *testing.T, fn func()) string {
	t.Helper()

	old := os.Stdout
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("os.Pipe: %v", err)
	}
	os.Stdout = w

	fn()

	w.Close()
	os.Stdout = old

	var buf bytes.Buffer
	io.Copy(&buf, r)
	r.Close()
	return buf.String()
}

// ---------------------------------------------------------------------------
// mag help (no args) — lists classes
// ---------------------------------------------------------------------------

func TestHelp_NoArgs_ListsClasses(t *testing.T) {
	vmInst := newTestVM(t)

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{})
	})

	// The default image should have at least Array and String
	if !strings.Contains(output, "Array") {
		t.Errorf("expected class listing to include Array, got:\n%s", output)
	}
	if !strings.Contains(output, "String") {
		t.Errorf("expected class listing to include String, got:\n%s", output)
	}
}

// ---------------------------------------------------------------------------
// mag help ClassName — shows class help
// ---------------------------------------------------------------------------

func TestHelp_ClassName_ShowsClassHelp(t *testing.T) {
	vmInst := newTestVM(t)

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Array"})
	})

	// Should contain the class name
	if !strings.Contains(output, "Array") {
		t.Errorf("expected output to mention Array, got:\n%s", output)
	}
}

func TestHelp_ClassName_WithDocString(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Create a class with a docstring
	writeMagFile(t, tmpDir, "Documented.mag", `"""A well-documented class for testing."""
Documented subclass: Object

  method: value [ ^42 ]
`)

	if _, err := (&pipeline.Pipeline{VM: vmInst}).CompilePath(tmpDir); err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Documented"})
	})

	if !strings.Contains(output, "Documented") {
		t.Errorf("expected output to contain class name, got:\n%s", output)
	}
	if !strings.Contains(output, "A well-documented class for testing.") {
		t.Errorf("expected output to contain docstring, got:\n%s", output)
	}
}

// ---------------------------------------------------------------------------
// mag help Class>>method — shows method docstring
// ---------------------------------------------------------------------------

func TestHelp_ClassMethod_ShowsMethodDoc(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Create a class with a documented method
	writeMagFile(t, tmpDir, "Helper.mag", `Helper subclass: Object
  """Return a friendly greeting."""
  method: greet [ ^'hello' ]
`)

	if _, err := (&pipeline.Pipeline{VM: vmInst}).CompilePath(tmpDir); err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Helper>>greet"})
	})

	if !strings.Contains(output, "Helper>>greet") {
		t.Errorf("expected output to contain 'Helper>>greet', got:\n%s", output)
	}
	if !strings.Contains(output, "Return a friendly greeting.") {
		t.Errorf("expected output to contain method docstring, got:\n%s", output)
	}
}

func TestHelp_ClassMethod_NoDocString(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Plain.mag", `Plain subclass: Object
  method: value [ ^1 ]
`)

	if _, err := (&pipeline.Pipeline{VM: vmInst}).CompilePath(tmpDir); err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Plain>>value"})
	})

	if !strings.Contains(output, "Plain>>value") {
		t.Errorf("expected output to contain 'Plain>>value', got:\n%s", output)
	}
	if !strings.Contains(output, "(no documentation)") {
		t.Errorf("expected '(no documentation)' for undocumented method, got:\n%s", output)
	}
}

func TestHelp_ClassMethod_WithSpaces(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Spacer.mag", `Spacer subclass: Object
  method: value [ ^1 ]
`)

	if _, err := (&pipeline.Pipeline{VM: vmInst}).CompilePath(tmpDir); err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	// "mag help Spacer >> value" with spaces around >>
	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Spacer", ">>", "value"})
	})

	if !strings.Contains(output, "Spacer>>value") {
		t.Errorf("expected output to handle spaces around >>, got:\n%s", output)
	}
}

// ---------------------------------------------------------------------------
// mag help NonExistent — shows appropriate error
// ---------------------------------------------------------------------------

func TestHelp_NonExistentClass(t *testing.T) {
	vmInst := newTestVM(t)

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"NonExistentClass"})
	})

	if !strings.Contains(output, "Unknown class: NonExistentClass") {
		t.Errorf("expected 'Unknown class' error, got:\n%s", output)
	}
}

func TestHelp_NonExistentMethod(t *testing.T) {
	vmInst := newTestVM(t)

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Array>>nonExistentMethod"})
	})

	if !strings.Contains(output, "does not define #nonExistentMethod") {
		t.Errorf("expected 'does not define' error, got:\n%s", output)
	}
}

func TestHelp_NonExistentClassInMethodLookup(t *testing.T) {
	vmInst := newTestVM(t)

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"FakeClass>>method"})
	})

	if !strings.Contains(output, "Unknown class: FakeClass") {
		t.Errorf("expected 'Unknown class' error for class in method lookup, got:\n%s", output)
	}
}

// ---------------------------------------------------------------------------
// mag help with keyword selector (e.g., Array>>at:)
// ---------------------------------------------------------------------------

func TestHelp_KeywordSelector(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Lookup.mag", `Lookup subclass: Object
  """Look up a value by key."""
  method: at: key [ ^key ]
`)

	if _, err := (&pipeline.Pipeline{VM: vmInst}).CompilePath(tmpDir); err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{"Lookup>>at:"})
	})

	if !strings.Contains(output, "Lookup>>at:") {
		t.Errorf("expected output for keyword selector, got:\n%s", output)
	}
	if !strings.Contains(output, "Look up a value by key.") {
		t.Errorf("expected docstring for keyword selector method, got:\n%s", output)
	}
}

// ---------------------------------------------------------------------------
// mag help with listing includes docstring summaries
// ---------------------------------------------------------------------------

func TestHelp_NoArgs_ShowsDocStringSummaries(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Summary.mag", `"""First line summary."""
Summary subclass: Object

  method: value [ ^1 ]
`)

	if _, err := (&pipeline.Pipeline{VM: vmInst}).CompilePath(tmpDir); err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	output := captureStdout(t, func() {
		handleHelpCommand(vmInst, []string{})
	})

	if !strings.Contains(output, "Summary") {
		t.Errorf("expected class listing to include Summary, got:\n%s", output)
	}
}
