package vm

import (
	"runtime"
	"strings"
	"testing"
	"time"
)

// epClass returns the ExternalProcess class value from the VM globals.
func epClass(vm *VM) Value {
	return vm.globals["ExternalProcess"]
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
	dictObj.Put(vm.registry, key, val)

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

// ---------------------------------------------------------------------------
// Regression tests: locking, wait-without-start, empty args, run:args: failure
// ---------------------------------------------------------------------------

// TestExecKillDuringRun guards the regression where run held p.mu across
// c.Run(), so kill blocked on the same lock until the child exited on its own
// (and could not have found p.cmd anyway).
func TestExecKillDuringRun(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("needs sleep")
	}
	vm := NewVM()
	proc := vm.Send(epClass(vm), "command:args:", []Value{
		vm.registry.NewStringValue("sleep"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("30")}),
	})

	runDone := make(chan struct{})
	go func() {
		defer close(runDone)
		vm.Send(proc, "run", nil)
	}()
	time.Sleep(200 * time.Millisecond) // let run start the child

	killDone := make(chan struct{})
	go func() {
		defer close(killDone)
		vm.Send(proc, "kill", nil)
	}()
	select {
	case <-killDone:
	case <-time.After(5 * time.Second):
		t.Fatal("kill blocked while run was in progress")
	}
	select {
	case <-runDone:
	case <-time.After(5 * time.Second):
		t.Fatal("run did not return after kill")
	}
	if code := vm.Send(proc, "exitCode", nil); code == FromSmallInt(0) {
		t.Error("killed process should not report exit code 0")
	}
}

// TestExecWaitWithoutStartDoesNotHang guards the regression where wait on a
// never-started process spun forever. It must signal a catchable Error.
func TestExecWaitWithoutStartDoesNotHang(t *testing.T) {
	vm := NewVM()
	proc := vm.Send(epClass(vm), "command:", []Value{vm.registry.NewStringValue("true")})

	result := make(chan any, 1)
	go func() {
		defer func() { result <- recover() }()
		vm.Send(proc, "wait", nil)
	}()
	select {
	case r := <-result:
		if _, ok := r.(SignaledException); !ok {
			t.Fatalf("wait before start: expected SignaledException, got %T: %v", r, r)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("wait on a never-started process hung")
	}
}

// TestExecAccessorsWhileRunningRaceFree guards the regression where
// stdout/stderr/exitCode/isSuccess read fields the wait goroutine writes,
// without the lock. Run under -race.
func TestExecAccessorsWhileRunningRaceFree(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("needs echo")
	}
	vm := NewVM()
	proc := vm.Send(epClass(vm), "command:args:", []Value{
		vm.registry.NewStringValue("echo"),
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("hi")}),
	})
	vm.Send(proc, "start", nil)
	deadline := time.Now().Add(5 * time.Second)
	for vm.Send(proc, "isDone", nil) != True && time.Now().Before(deadline) {
		vm.Send(proc, "stdout", nil)
		vm.Send(proc, "stderr", nil)
		vm.Send(proc, "exitCode", nil)
		vm.Send(proc, "isSuccess", nil)
	}
	vm.Send(proc, "wait", nil)
	if vm.Send(proc, "isSuccess", nil) != True {
		t.Error("echo should succeed")
	}
}

// TestExecEmptyStringArgPreserved guards the regression where
// valueToStringArray dropped empty-string elements, silently shifting argv.
func TestExecEmptyStringArgPreserved(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("needs printf")
	}
	vm := NewVM()
	s := vm.registry.NewStringValue
	proc := vm.Send(epClass(vm), "command:args:", []Value{
		s("printf"),
		vm.NewArrayWithElements([]Value{s("%s|"), s("a"), s(""), s("b")}),
	})
	vm.Send(proc, "run", nil)
	out := vm.registry.GetStringContent(vm.Send(proc, "stdout", nil))
	if out != "a||b|" {
		t.Errorf("expected empty arg to be preserved (%q), got %q", "a||b|", out)
	}
}

// TestExecRunArgsFailureMessage checks run:args: reports the exit code and
// stderr in its Failure (it used to build a details Dictionary and discard it).
func TestExecRunArgsFailureMessage(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("needs sh")
	}
	vm := NewVM()
	s := vm.registry.NewStringValue
	result := vm.Send(epClass(vm), "run:args:", []Value{
		s("sh"),
		vm.NewArrayWithElements([]Value{s("-c"), s("echo oops >&2; exit 3")}),
	})
	if vm.Send(result, "isFailure", nil) != True {
		t.Fatal("expected Failure")
	}
	msg := vm.registry.GetStringContent(vm.Send(result, "error", nil))
	if !strings.Contains(msg, "3") || !strings.Contains(msg, "oops") {
		t.Errorf("failure message should carry exit code and stderr, got %q", msg)
	}
}
