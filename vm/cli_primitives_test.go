package vm

import (
	"bytes"
	"reflect"
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Basic wrapper / marker tests
// ---------------------------------------------------------------------------

func TestCliCommandRegistration(t *testing.T) {
	vm := NewVM()
	w := &CliCommandWrapper{VM: vm}

	val := vm.vmRegisterCliCommand(w)
	if !isCliCommandValue(val) {
		t.Fatal("registerCliCommand should produce a Cli::Command value")
	}
	if got := vm.vmGetCliCommand(val); got == nil {
		t.Fatal("getCliCommand returned nil for registered command")
	}
}

func TestIsCliCommandValueFalse(t *testing.T) {
	vm := NewVM()
	if isCliCommandValue(Nil) {
		t.Error("Nil should not be a Cli::Command value")
	}
	if isCliCommandValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be a Cli::Command value")
	}
	if isCliCommandValue(vm.registry.NewStringValue("hi")) {
		t.Error("String should not be a Cli::Command value")
	}
}

func TestCliMarkerUnique(t *testing.T) {
	// Regression guard: catch accidental reassignment of cliCommandMarker.
	markers := map[string]uint32{
		"channel":       channelMarker,
		"process":       processMarker,
		"result":        resultMarker,
		"goObject":      goObjectMarker,
		"httpClient":    httpClientMarker,
		"sseConnection": sseConnectionMarker,
		"cliCommand":    cliCommandMarker,
	}
	seen := make(map[uint32]string)
	for name, m := range markers {
		if prev, exists := seen[m]; exists {
			t.Errorf("marker collision: %s and %s both use 0x%08X", name, prev, m)
		}
		seen[m] = name
	}
}

// ---------------------------------------------------------------------------
// TestCliCommandNew — class method new: produces a usable wrapper with Use set
// ---------------------------------------------------------------------------

func TestCliCommandNew(t *testing.T) {
	vm := NewVM()

	cls, ok := vm.Globals["Cli::Command"]
	if !ok {
		t.Fatal("Cli::Command should be registered in Globals")
	}

	useVal := vm.registry.NewStringValue("demo")
	cmdVal := vm.Send(cls, "new:", []Value{useVal})

	if cmdVal == Nil {
		t.Fatal("Cli::Command new: returned Nil")
	}
	if !isCliCommandValue(cmdVal) {
		t.Fatal("Cli::Command new: should return a Cli::Command value")
	}

	w := vm.vmGetCliCommand(cmdVal)
	if w == nil {
		t.Fatal("wrapper should be retrievable from registry")
	}
	if w.Cobra == nil {
		t.Fatal("wrapper.Cobra should be set")
	}
	if w.Cobra.Use != "demo" {
		t.Errorf("Use = %q, want %q", w.Cobra.Use, "demo")
	}

	// shortDoc: and longDoc: populate Short/Long on the cobra command.
	vm.Send(cmdVal, "shortDoc:", []Value{vm.registry.NewStringValue("a demo")})
	vm.Send(cmdVal, "longDoc:", []Value{vm.registry.NewStringValue("longer explanation")})
	if w.Cobra.Short != "a demo" {
		t.Errorf("Short = %q, want %q", w.Cobra.Short, "a demo")
	}
	if w.Cobra.Long != "longer explanation" {
		t.Errorf("Long = %q, want %q", w.Cobra.Long, "longer explanation")
	}
}

// ---------------------------------------------------------------------------
// TestCliRunBlockInvoked — cobra.Execute triggers the Maggie run block and the
// exit-code convention round-trips (SmallInt → exit code).
// ---------------------------------------------------------------------------

func TestCliRunBlockInvoked(t *testing.T) {
	vm := NewVM()

	cls := vm.Globals["Cli::Command"]
	cmdVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("echo")})

	// Block: [:args | 7] — returns 7, expected as the exit code.
	block := &BlockMethod{
		Arity:    1,
		NumTemps: 0,
		Bytecode: []byte{0x14, 7, 0x73}, // OpPushInt8 7, OpBlockReturn
		Literals: nil,
	}
	blockVal := vm.interpreter.createBlockValue(block, nil)
	vm.Send(cmdVal, "run:", []Value{blockVal})

	// Route args through SetArgs so os.Args stays untouched.
	emptyArgs := vm.NewArrayWithElements(nil)
	vm.Send(cmdVal, "setArgs:", []Value{emptyArgs})

	// Silence cobra's usage chatter in case of failure.
	var buf bytes.Buffer
	vm.RegisterGoType("Go::BytesBuffer", reflect.TypeOf((*bytes.Buffer)(nil)))
	bufVal, err := vm.RegisterGoObject(&buf)
	if err != nil {
		t.Fatalf("RegisterGoObject: %v", err)
	}
	vm.Send(cmdVal, "setOutput:", []Value{bufVal})

	code := vm.Send(cmdVal, "execute", nil)
	if !code.IsSmallInt() {
		t.Fatalf("execute should return SmallInt, got %v", code)
	}
	if code.SmallInt() != 7 {
		t.Errorf("exit code = %d, want 7", code.SmallInt())
	}
}

// Block that returns non-integer values should collapse to exit code 0.
func TestCliRunBlockNonIntReturnsZero(t *testing.T) {
	vm := NewVM()
	cls := vm.Globals["Cli::Command"]
	cmdVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("noop")})

	// Block: [:args | nil]
	block := &BlockMethod{
		Arity:    1,
		NumTemps: 0,
		Bytecode: []byte{0x10, 0x73}, // OpPushNil, OpBlockReturn
		Literals: nil,
	}
	blockVal := vm.interpreter.createBlockValue(block, nil)
	vm.Send(cmdVal, "run:", []Value{blockVal})
	vm.Send(cmdVal, "setArgs:", []Value{vm.NewArrayWithElements(nil)})

	code := vm.Send(cmdVal, "execute", nil)
	if !code.IsSmallInt() || code.SmallInt() != 0 {
		t.Errorf("expected exit code 0, got %v", code)
	}
}

// ---------------------------------------------------------------------------
// TestCliFlagsRoundTrip — declaring flags and reading them back returns the
// Maggie-typed value.
// ---------------------------------------------------------------------------

func TestCliFlagsRoundTrip(t *testing.T) {
	vm := NewVM()
	cls := vm.Globals["Cli::Command"]
	cmdVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("cfg")})

	// addStringFlag: host default: 'localhost' doc: 'hostname'
	vm.Send(cmdVal, "addStringFlag:default:doc:", []Value{
		vm.registry.NewStringValue("host"),
		vm.registry.NewStringValue("localhost"),
		vm.registry.NewStringValue("hostname"),
	})
	vm.Send(cmdVal, "addBoolFlag:default:doc:", []Value{
		vm.registry.NewStringValue("verbose"),
		False,
		vm.registry.NewStringValue("verbose output"),
	})
	vm.Send(cmdVal, "addIntFlag:default:doc:", []Value{
		vm.registry.NewStringValue("port"),
		FromSmallInt(8080),
		vm.registry.NewStringValue("listen port"),
	})

	// Read defaults before parsing — cobra returns default values without
	// parsing when Flags().GetX is called on a freshly-declared flag.
	host := vm.Send(cmdVal, "stringFlag:", []Value{vm.registry.NewStringValue("host")})
	if !IsStringValue(host) || vm.registry.GetStringContent(host) != "localhost" {
		t.Errorf("default host = %v, want 'localhost'", host)
	}
	verbose := vm.Send(cmdVal, "boolFlag:", []Value{vm.registry.NewStringValue("verbose")})
	if verbose != False {
		t.Errorf("default verbose = %v, want False", verbose)
	}
	port := vm.Send(cmdVal, "intFlag:", []Value{vm.registry.NewStringValue("port")})
	if !port.IsSmallInt() || port.SmallInt() != 8080 {
		t.Errorf("default port = %v, want 8080", port)
	}

	// Feed custom args and run a block that just returns 0 — we only care
	// that the flag values round-trip after parsing.
	vm.Send(cmdVal, "setArgs:", []Value{
		vm.NewArrayWithElements([]Value{
			vm.registry.NewStringValue("--host=example.com"),
			vm.registry.NewStringValue("--verbose"),
			vm.registry.NewStringValue("--port=9000"),
		}),
	})

	// Silence output
	var buf bytes.Buffer
	vm.RegisterGoType("Go::BytesBuffer", reflect.TypeOf((*bytes.Buffer)(nil)))
	bufVal, err := vm.RegisterGoObject(&buf)
	if err != nil {
		t.Fatalf("RegisterGoObject: %v", err)
	}
	vm.Send(cmdVal, "setOutput:", []Value{bufVal})

	// Block that inspects the flags and returns 0 if everything looks right.
	// We cannot easily compile an in-line Maggie block here, so instead we
	// verify parsing via the wrapper's cobra flags after Execute.
	// Register a trivial block that returns 0.
	block := &BlockMethod{
		Arity:    1,
		NumTemps: 0,
		Bytecode: []byte{0x14, 0, 0x73},
		Literals: nil,
	}
	blockVal := vm.interpreter.createBlockValue(block, nil)
	vm.Send(cmdVal, "run:", []Value{blockVal})

	code := vm.Send(cmdVal, "execute", nil)
	if !code.IsSmallInt() || code.SmallInt() != 0 {
		t.Fatalf("execute failed: code=%v, stderr=%q", code, buf.String())
	}

	// Now read flags back — cobra has parsed them during Execute.
	host = vm.Send(cmdVal, "stringFlag:", []Value{vm.registry.NewStringValue("host")})
	if !IsStringValue(host) || vm.registry.GetStringContent(host) != "example.com" {
		t.Errorf("parsed host = %v, want 'example.com'", host)
	}
	verbose = vm.Send(cmdVal, "boolFlag:", []Value{vm.registry.NewStringValue("verbose")})
	if verbose != True {
		t.Errorf("parsed verbose = %v, want True", verbose)
	}
	port = vm.Send(cmdVal, "intFlag:", []Value{vm.registry.NewStringValue("port")})
	if !port.IsSmallInt() || port.SmallInt() != 9000 {
		t.Errorf("parsed port = %v, want 9000", port)
	}
}

// ---------------------------------------------------------------------------
// TestCliSubcommandDispatch — addSubcommand: wires a child command and cobra
// routes args to the child's run block.
// ---------------------------------------------------------------------------

func TestCliSubcommandDispatch(t *testing.T) {
	vm := NewVM()
	cls := vm.Globals["Cli::Command"]

	// Parent: root
	rootVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("root")})

	// Child: greet — returns exit code 3 when invoked.
	childVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("greet")})
	childBlock := &BlockMethod{
		Arity:    1,
		NumTemps: 0,
		Bytecode: []byte{0x14, 3, 0x73}, // OpPushInt8 3, OpBlockReturn
		Literals: nil,
	}
	childBlockVal := vm.interpreter.createBlockValue(childBlock, nil)
	vm.Send(childVal, "run:", []Value{childBlockVal})

	// Wire subcommand
	vm.Send(rootVal, "addSubcommand:", []Value{childVal})

	// Invoke: root greet
	vm.Send(rootVal, "setArgs:", []Value{
		vm.NewArrayWithElements([]Value{vm.registry.NewStringValue("greet")}),
	})

	// Silence output
	var buf bytes.Buffer
	vm.RegisterGoType("Go::BytesBuffer", reflect.TypeOf((*bytes.Buffer)(nil)))
	bufVal, err := vm.RegisterGoObject(&buf)
	if err != nil {
		t.Fatalf("RegisterGoObject: %v", err)
	}
	vm.Send(rootVal, "setOutput:", []Value{bufVal})

	// Also propagate writers to the child explicitly — cobra inherits them
	// from the parent at Execute time, but setting them on the child guards
	// against future refactors.
	vm.Send(childVal, "setOutput:", []Value{bufVal})

	code := vm.Send(rootVal, "execute", nil)
	if !code.IsSmallInt() {
		t.Fatalf("execute returned non-SmallInt: %v", code)
	}
	if code.SmallInt() != 3 {
		t.Errorf("subcommand exit code = %d, want 3 (stderr=%q)", code.SmallInt(), buf.String())
	}
}

// ---------------------------------------------------------------------------
// TestCliUnhandledExceptionExitsOne — exception in run block → stderr msg +
// exit code 1.
// ---------------------------------------------------------------------------

func TestCliUnhandledExceptionExitsOne(t *testing.T) {
	vm := NewVM()
	cls := vm.Globals["Cli::Command"]
	cmdVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("boom")})

	// Build a block that raises an unhandled error by calling a non-existent
	// method on Nil — that panics with SignaledException (MessageNotUnderstood)
	// under the default error model. Easiest: just compile a block that sends
	// a selector `crash` to nil. But constructing that bytecode by hand is
	// brittle; instead use the doesNotUnderstand route by sending a bogus
	// selector to a primitive receiver inside the block.
	//
	// Simpler: synthesize a wrapper manually and invoke runCliBlock with a
	// RunBlock that panics.
	w := vm.vmGetCliCommand(cmdVal)
	if w == nil {
		t.Fatal("wrapper nil")
	}
	var buf bytes.Buffer
	w.Cobra.SetOut(&buf)
	w.Cobra.SetErr(&buf)

	// Create a block that signals a fresh Error exception.
	// Bytecode: push string literal 0, send "error:" to the Error class,
	// BlockReturn. Constructing this by hand is tedious, so instead we
	// bypass the block and exercise the recover path directly.
	w.RunBlock = Nil

	// Manually invoke runCliBlock with a panicking synthetic path by calling
	// the recover branch: we simulate a SignaledException via signalException.
	// The run block path uses evaluateBlock, which executes the block; we'll
	// rely on the behavior where a nil RunBlock gives exitCode 0 — skip that
	// scenario and instead verify the recover path through a direct call.
	func() {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("runCliBlock should have recovered, got panic: %v", r)
			}
		}()
		// Stash a block value into the wrapper that will raise when
		// evaluated: we use a tiny BlockMethod with an unknown opcode
		// guaranteed to panic.
		bad := &BlockMethod{
			Arity:    1,
			NumTemps: 0,
			Bytecode: []byte{0xFF}, // unknown opcode → interpreter panic
			Literals: nil,
		}
		w.RunBlock = vm.interpreter.createBlockValue(bad, nil)
		vm.runCliBlock(w, []string{})
	}()

	if w.exitCode != 1 {
		t.Errorf("exitCode after panic = %d, want 1", w.exitCode)
	}
	if !strings.Contains(buf.String(), "Error:") {
		t.Errorf("expected 'Error:' in stderr, got %q", buf.String())
	}
}

// ---------------------------------------------------------------------------
// TestCliSetOutputRejectsBadValue — setOutput: with a non-writer raises
// Cli::CliError rather than silently dropping writes.
// ---------------------------------------------------------------------------

func TestCliSetOutputRejectsBadValue(t *testing.T) {
	vm := NewVM()
	cls := vm.Globals["Cli::Command"]
	cmdVal := vm.Send(cls, "new:", []Value{vm.registry.NewStringValue("bad")})

	// We expect a SignaledException panic. Wrap in defer/recover.
	func() {
		defer func() {
			r := recover()
			if r == nil {
				t.Fatal("expected panic on setOutput: non-writer")
			}
			sig, ok := r.(SignaledException)
			if !ok {
				t.Fatalf("expected SignaledException, got %T: %v", r, r)
			}
			cliErr := vm.Globals["Cli::CliError"]
			if cliErrCls := vm.classFromValue(cliErr); cliErrCls != sig.Object.ExceptionClass {
				t.Errorf("exception class = %v, want Cli::CliError", sig.Object.ExceptionClass)
			}
		}()
		// Pass a string — not a GoObject io.Writer — and expect a raise.
		vm.Send(cmdVal, "setOutput:", []Value{vm.registry.NewStringValue("not a writer")})
	}()
}

