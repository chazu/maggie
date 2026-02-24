package vm

import (
	"runtime"
	"strings"
	"testing"
)

// epClass returns the ExternalProcess class value from the VM globals.
func epClass(vm *VM) Value {
	return vm.Globals["ExternalProcess"]
}

// ---------------------------------------------------------------------------
// Class method: run:args: (convenience)
// ---------------------------------------------------------------------------

func TestExecRunArgsSuccess(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	result := vm.Send(ec, "run:args:", []Value{
		vm.registry.NewStringValue("echo"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("hello")}),
	})

	if !IsStringValue(result) {
		t.Fatalf("run:args: expected string result, got %v", result)
	}
	out := vm.registry.GetStringContent(result)
	if !strings.Contains(out, "hello") {
		t.Errorf("run:args: expected output containing 'hello', got %q", out)
	}
}

func TestExecRunArgsFailure(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	result := vm.Send(ec, "run:args:", []Value{
		vm.registry.NewStringValue("false"),
		vm.NewArrayWithElements([]Value{}),
	})

	// Should be a Failure result
	if !isResultValue(result) {
		t.Fatalf("run:args: expected a Result for failed command, got non-result")
	}
	isFailure := vm.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Fatalf("run:args: expected Failure for non-zero exit")
	}
}

// ---------------------------------------------------------------------------
// Instance: command:args: + run + stdout + exitCode
// ---------------------------------------------------------------------------

func TestExecCommandRunStdout(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:args:", []Value{
		vm.registry.NewStringValue("echo"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("world")}),
	})

	if !isExtProcessValue(proc) {
		t.Fatalf("command:args: did not return an ExternalProcess value")
	}

	vm.Send(proc, "run", nil)

	stdout := vm.Send(proc, "stdout", nil)
	if !IsStringValue(stdout) {
		t.Fatalf("stdout did not return a string")
	}
	out := vm.registry.GetStringContent(stdout)
	if !strings.Contains(out, "world") {
		t.Errorf("expected stdout containing 'world', got %q", out)
	}

	exitCode := vm.Send(proc, "exitCode", nil)
	if !exitCode.IsSmallInt() || exitCode.SmallInt() != 0 {
		t.Errorf("expected exit code 0, got %v", exitCode)
	}

	isSuccess := vm.Send(proc, "isSuccess", nil)
	if isSuccess != True {
		t.Error("expected isSuccess to be true")
	}

	isDone := vm.Send(proc, "isDone", nil)
	if isDone != True {
		t.Error("expected isDone to be true after run")
	}
}

// ---------------------------------------------------------------------------
// Non-zero exit code
// ---------------------------------------------------------------------------

func TestExecNonZeroExit(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:", []Value{
		vm.registry.NewStringValue("false"),
	})
	vm.Send(proc, "run", nil)

	exitCode := vm.Send(proc, "exitCode", nil)
	if !exitCode.IsSmallInt() || exitCode.SmallInt() == 0 {
		t.Error("expected non-zero exit code from 'false' command")
	}

	isSuccess := vm.Send(proc, "isSuccess", nil)
	if isSuccess != False {
		t.Error("expected isSuccess to be false")
	}
}

// ---------------------------------------------------------------------------
// stderr capture
// ---------------------------------------------------------------------------

func TestExecStderrCapture(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("skipping on windows")
	}
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:args:", []Value{
		vm.registry.NewStringValue("sh"),
		vm.NewArrayWithElements([]Value{
			vm.registry.NewStringValue("-c"),
			vm.registry.NewStringValue("echo errout >&2"),
		}),
	})
	vm.Send(proc, "run", nil)

	stderr := vm.Send(proc, "stderr", nil)
	if !IsStringValue(stderr) {
		t.Fatalf("stderr did not return a string")
	}
	errOut := vm.registry.GetStringContent(stderr)
	if !strings.Contains(errOut, "errout") {
		t.Errorf("expected stderr containing 'errout', got %q", errOut)
	}
}

// ---------------------------------------------------------------------------
// Working directory
// ---------------------------------------------------------------------------

func TestExecWorkingDirectory(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("skipping on windows")
	}
	vm := NewVM()
	ec := epClass(vm)
	tmpDir := t.TempDir()

	proc := vm.Send(ec, "command:", []Value{
		vm.registry.NewStringValue("pwd"),
	})
	vm.Send(proc, "dir:", []Value{vm.registry.NewStringValue(tmpDir)})
	vm.Send(proc, "run", nil)

	stdout := vm.Send(proc, "stdout", nil)
	out := vm.registry.GetStringContent(stdout)
	if !strings.Contains(out, tmpDir) {
		t.Errorf("expected pwd output containing %q, got %q", tmpDir, out)
	}
}

// ---------------------------------------------------------------------------
// Environment variable injection
// ---------------------------------------------------------------------------

func TestExecEnvInjection(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("skipping on windows")
	}
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:args:", []Value{
		vm.registry.NewStringValue("sh"),
		vm.NewArrayWithElements([]Value{
			vm.registry.NewStringValue("-c"),
			vm.registry.NewStringValue("echo $MAGGIE_TEST_VAR"),
		}),
	})

	// Build a dictionary with one env var
	dict := vm.registry.NewDictionaryValue()
	dictObj := vm.registry.GetDictionaryObject(dict)
	key := vm.registry.NewStringValue("MAGGIE_TEST_VAR")
	val := vm.registry.NewStringValue("it_works")
	h := hashValue(vm.registry, key)
	dictObj.Keys[h] = key
	dictObj.Data[h] = val

	vm.Send(proc, "env:", []Value{dict})
	vm.Send(proc, "run", nil)

	stdout := vm.Send(proc, "stdout", nil)
	out := vm.registry.GetStringContent(stdout)
	if !strings.Contains(out, "it_works") {
		t.Errorf("expected env var in stdout, got %q", out)
	}
}

// ---------------------------------------------------------------------------
// Async start/wait
// ---------------------------------------------------------------------------

func TestExecAsyncStartWait(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:args:", []Value{
		vm.registry.NewStringValue("echo"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("async")}),
	})

	vm.Send(proc, "start", nil)
	vm.Send(proc, "wait", nil)

	isDone := vm.Send(proc, "isDone", nil)
	if isDone != True {
		t.Error("expected isDone after wait")
	}

	stdout := vm.Send(proc, "stdout", nil)
	out := vm.registry.GetStringContent(stdout)
	if !strings.Contains(out, "async") {
		t.Errorf("expected 'async' in stdout, got %q", out)
	}
}

// ---------------------------------------------------------------------------
// Timeout
// ---------------------------------------------------------------------------

func TestExecTimeout(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("skipping on windows")
	}
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:args:", []Value{
		vm.registry.NewStringValue("sleep"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("60")}),
	})

	vm.Send(proc, "runWithTimeout:", []Value{FromSmallInt(100)})

	exitCode := vm.Send(proc, "exitCode", nil)
	if !exitCode.IsSmallInt() || exitCode.SmallInt() == 0 {
		t.Error("expected non-zero exit code after timeout")
	}

	isDone := vm.Send(proc, "isDone", nil)
	if isDone != True {
		t.Error("expected isDone after timeout")
	}
}

// ---------------------------------------------------------------------------
// command accessor
// ---------------------------------------------------------------------------

func TestExecCommandAccessor(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:", []Value{
		vm.registry.NewStringValue("ls"),
	})

	cmd := vm.Send(proc, "command", nil)
	if !IsStringValue(cmd) {
		t.Fatalf("command did not return a string")
	}
	if vm.registry.GetStringContent(cmd) != "ls" {
		t.Errorf("expected command 'ls', got %q", vm.registry.GetStringContent(cmd))
	}
}

// ---------------------------------------------------------------------------
// printString
// ---------------------------------------------------------------------------

func TestExecPrintString(t *testing.T) {
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:", []Value{
		vm.registry.NewStringValue("git"),
	})

	ps := vm.Send(proc, "printString", nil)
	if !IsStringValue(ps) {
		t.Fatalf("printString did not return a string")
	}
	out := vm.registry.GetStringContent(ps)
	if !strings.Contains(out, "git") {
		t.Errorf("expected printString containing 'git', got %q", out)
	}
}

// ---------------------------------------------------------------------------
// kill async process
// ---------------------------------------------------------------------------

func TestExecKill(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("skipping on windows")
	}
	vm := NewVM()
	ec := epClass(vm)

	proc := vm.Send(ec, "command:args:", []Value{
		vm.registry.NewStringValue("sleep"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("60")}),
	})

	vm.Send(proc, "start", nil)
	vm.Send(proc, "kill", nil)
	vm.Send(proc, "wait", nil)

	isDone := vm.Send(proc, "isDone", nil)
	if isDone != True {
		t.Error("expected isDone after kill + wait")
	}
}
