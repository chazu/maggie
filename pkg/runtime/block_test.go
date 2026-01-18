package runtime

import (
	"testing"
)

// TestBlock is a simple block implementation for testing.
// It records invocations and returns a configurable result.
type TestBlock struct {
	params       []string
	kind         BlockKind
	result       string
	invocations  [][]string // Records args from each call
	invokeFunc   func(ctx *BlockContext, args ...string) (string, error)
}

func NewTestBlock(params []string, result string) *TestBlock {
	return &TestBlock{
		params: params,
		kind:   BlockKindNative,
		result: result,
	}
}

func (b *TestBlock) NumArgs() int {
	return len(b.params)
}

func (b *TestBlock) ParamNames() []string {
	return b.params
}

func (b *TestBlock) Kind() BlockKind {
	return b.kind
}

func (b *TestBlock) Invoke(ctx *BlockContext, args ...string) (string, error) {
	b.invocations = append(b.invocations, args)
	if b.invokeFunc != nil {
		return b.invokeFunc(ctx, args...)
	}
	return b.result, nil
}

// ============================================================================
// Block Kind Tests
// ============================================================================

func TestBlockKindString(t *testing.T) {
	tests := []struct {
		kind BlockKind
		want string
	}{
		{BlockKindBash, "bash"},
		{BlockKindBytecode, "bytecode"},
		{BlockKindNative, "native"},
		{BlockKind(99), "BlockKind(99)"},
	}

	for _, tt := range tests {
		got := tt.kind.String()
		if got != tt.want {
			t.Errorf("BlockKind(%d).String() = %q, want %q", tt.kind, got, tt.want)
		}
	}
}

// ============================================================================
// VarSource Tests
// ============================================================================

func TestVarSourceString(t *testing.T) {
	tests := []struct {
		source VarSource
		want   string
	}{
		{VarSourceLocal, "local"},
		{VarSourceIVar, "ivar"},
		{VarSourceGlobal, "global"},
		{VarSourceCapture, "capture"},
		{VarSource(99), "VarSource(99)"},
	}

	for _, tt := range tests {
		got := tt.source.String()
		if got != tt.want {
			t.Errorf("VarSource(%d).String() = %q, want %q", tt.source, got, tt.want)
		}
	}
}

// ============================================================================
// CapturedVar Tests
// ============================================================================

func TestCapturedVar(t *testing.T) {
	v := NewCapturedVar("counter", "5", VarSourceLocal)

	if v.Name != "counter" {
		t.Errorf("Name = %q, want %q", v.Name, "counter")
	}
	if v.Get() != "5" {
		t.Errorf("Get() = %q, want %q", v.Get(), "5")
	}
	if v.Source != VarSourceLocal {
		t.Errorf("Source = %v, want VarSourceLocal", v.Source)
	}

	// Test Set
	v.Set("10")
	if v.Get() != "10" {
		t.Errorf("After Set(10), Get() = %q, want %q", v.Get(), "10")
	}
}

// ============================================================================
// BlockContext Tests
// ============================================================================

func TestBlockContext(t *testing.T) {
	ctx := NewBlockContext(nil, "counter_123")

	if ctx.InstanceID != "counter_123" {
		t.Errorf("InstanceID = %q, want %q", ctx.InstanceID, "counter_123")
	}

	// Test GetCaptured returns nil for unknown variable
	if got := ctx.GetCaptured("unknown"); got != nil {
		t.Errorf("GetCaptured(unknown) = %v, want nil", got)
	}

	// Test SetCaptured and GetCaptured
	v := NewCapturedVar("x", "42", VarSourceLocal)
	ctx.SetCaptured("x", v)

	got := ctx.GetCaptured("x")
	if got == nil {
		t.Fatal("GetCaptured(x) = nil, want non-nil")
	}
	if got.Get() != "42" {
		t.Errorf("GetCaptured(x).Get() = %q, want %q", got.Get(), "42")
	}
}

func TestBlockContextNestedCaptures(t *testing.T) {
	// Create parent context with captured variable
	parent := NewBlockContext(nil, "obj_1")
	parent.SetCaptured("outer", NewCapturedVar("outer", "parent_value", VarSourceLocal))

	// Create child context
	child := NewBlockContext(nil, "obj_1")
	child.ParentFrame = parent
	child.SetCaptured("inner", NewCapturedVar("inner", "child_value", VarSourceLocal))

	// Child should see its own captures
	if got := child.GetCaptured("inner"); got == nil || got.Get() != "child_value" {
		t.Errorf("child.GetCaptured(inner) failed")
	}

	// Child should see parent's captures through chain
	if got := child.GetCaptured("outer"); got == nil || got.Get() != "parent_value" {
		t.Errorf("child.GetCaptured(outer) failed, got %v", got)
	}

	// Parent should not see child's captures
	if got := parent.GetCaptured("inner"); got != nil {
		t.Errorf("parent.GetCaptured(inner) = %v, want nil", got)
	}
}

// ============================================================================
// Block Interface Tests - Zero-arg blocks
// ============================================================================

func TestZeroArgBlock(t *testing.T) {
	// Pattern: [body] - block with no parameters
	block := NewTestBlock(nil, "hello")

	if block.NumArgs() != 0 {
		t.Errorf("NumArgs() = %d, want 0", block.NumArgs())
	}
	if block.ParamNames() != nil {
		t.Errorf("ParamNames() = %v, want nil", block.ParamNames())
	}

	ctx := NewBlockContext(nil, "")
	result, err := block.Invoke(ctx)
	if err != nil {
		t.Fatalf("Invoke() error = %v", err)
	}
	if result != "hello" {
		t.Errorf("Invoke() = %q, want %q", result, "hello")
	}
}

// ============================================================================
// Block Interface Tests - Single-arg blocks
// ============================================================================

func TestSingleArgBlock(t *testing.T) {
	// Pattern: [:x | body] - block with one parameter
	block := NewTestBlock([]string{"x"}, "processed")

	if block.NumArgs() != 1 {
		t.Errorf("NumArgs() = %d, want 1", block.NumArgs())
	}
	if len(block.ParamNames()) != 1 || block.ParamNames()[0] != "x" {
		t.Errorf("ParamNames() = %v, want [x]", block.ParamNames())
	}

	ctx := NewBlockContext(nil, "")
	result, err := block.Invoke(ctx, "value1")
	if err != nil {
		t.Fatalf("Invoke() error = %v", err)
	}
	if result != "processed" {
		t.Errorf("Invoke() = %q, want %q", result, "processed")
	}

	// Verify invocation was recorded
	if len(block.invocations) != 1 {
		t.Fatalf("Expected 1 invocation, got %d", len(block.invocations))
	}
	if len(block.invocations[0]) != 1 || block.invocations[0][0] != "value1" {
		t.Errorf("Invocation args = %v, want [value1]", block.invocations[0])
	}
}

// ============================================================================
// Block Interface Tests - Multi-arg blocks
// ============================================================================

func TestMultiArgBlock(t *testing.T) {
	// Pattern: [:x :y | body] - block with multiple parameters
	block := NewTestBlock([]string{"x", "y"}, "combined")

	if block.NumArgs() != 2 {
		t.Errorf("NumArgs() = %d, want 2", block.NumArgs())
	}
	params := block.ParamNames()
	if len(params) != 2 || params[0] != "x" || params[1] != "y" {
		t.Errorf("ParamNames() = %v, want [x y]", params)
	}

	ctx := NewBlockContext(nil, "")
	result, err := block.Invoke(ctx, "a", "b")
	if err != nil {
		t.Fatalf("Invoke() error = %v", err)
	}
	if result != "combined" {
		t.Errorf("Invoke() = %q, want %q", result, "combined")
	}

	// Verify args were passed correctly
	if len(block.invocations[0]) != 2 {
		t.Fatalf("Expected 2 args, got %d", len(block.invocations[0]))
	}
	if block.invocations[0][0] != "a" || block.invocations[0][1] != "b" {
		t.Errorf("Args = %v, want [a b]", block.invocations[0])
	}
}

// ============================================================================
// Block Interface Tests - Blocks with local variables
// ============================================================================

func TestBlockWithLocalVariables(t *testing.T) {
	// Simulates: [:x | |temp| temp := x + 1. temp]
	block := &TestBlock{
		params: []string{"x"},
		kind:   BlockKindNative,
	}
	block.invokeFunc = func(ctx *BlockContext, args ...string) (string, error) {
		// Simulate local variable 'temp'
		x := args[0]
		temp := x + "_modified"
		return temp, nil
	}

	ctx := NewBlockContext(nil, "")
	result, err := block.Invoke(ctx, "input")
	if err != nil {
		t.Fatalf("Invoke() error = %v", err)
	}
	if result != "input_modified" {
		t.Errorf("Invoke() = %q, want %q", result, "input_modified")
	}
}

// ============================================================================
// Block Interface Tests - Blocks capturing instance variables
// ============================================================================

func TestBlockCapturingIVar(t *testing.T) {
	// Simulates a block that reads and modifies an ivar
	block := &TestBlock{
		params: nil,
		kind:   BlockKindNative,
	}
	block.invokeFunc = func(ctx *BlockContext, args ...string) (string, error) {
		// Get captured ivar
		counter := ctx.GetCaptured("counter")
		if counter == nil {
			return "", nil
		}
		// Read current value
		current := counter.Get()
		// Modify it (reference semantics)
		counter.Set(current + "1")
		return counter.Get(), nil
	}

	ctx := NewBlockContext(nil, "obj_123")
	ctx.SetCaptured("counter", NewCapturedVar("counter", "0", VarSourceIVar))

	// Invoke block multiple times
	for i := 0; i < 3; i++ {
		_, err := block.Invoke(ctx)
		if err != nil {
			t.Fatalf("Invoke() error = %v", err)
		}
	}

	// Verify captured variable was modified
	counter := ctx.GetCaptured("counter")
	if counter.Get() != "0111" {
		t.Errorf("After 3 invocations, counter = %q, want %q", counter.Get(), "0111")
	}
}

// ============================================================================
// Block Interface Tests - Nested blocks
// ============================================================================

func TestNestedBlocks(t *testing.T) {
	// Simulates: [:x | [:y | x + y]]
	// Outer block returns inner block that captures x
	outerBlock := &TestBlock{
		params: []string{"x"},
		kind:   BlockKindNative,
	}

	var innerBlock *TestBlock
	outerBlock.invokeFunc = func(ctx *BlockContext, args ...string) (string, error) {
		x := args[0]
		// Capture x for the inner block
		innerCtx := NewBlockContext(ctx.Runtime, ctx.InstanceID)
		innerCtx.ParentFrame = ctx
		innerCtx.SetCaptured("x", NewCapturedVar("x", x, VarSourceCapture))

		// Create inner block
		innerBlock = &TestBlock{
			params: []string{"y"},
			kind:   BlockKindNative,
		}
		innerBlock.invokeFunc = func(innerCtxArg *BlockContext, innerArgs ...string) (string, error) {
			y := innerArgs[0]
			capturedX := innerCtxArg.GetCaptured("x")
			if capturedX == nil {
				return y, nil
			}
			return capturedX.Get() + y, nil
		}

		// Return inner block ID (simulated)
		return "inner_block_id", nil
	}

	// Invoke outer block
	outerCtx := NewBlockContext(nil, "")
	_, err := outerBlock.Invoke(outerCtx, "hello_")
	if err != nil {
		t.Fatalf("Outer Invoke() error = %v", err)
	}

	// Invoke inner block with access to captured x
	innerCtx := NewBlockContext(nil, "")
	innerCtx.SetCaptured("x", NewCapturedVar("x", "hello_", VarSourceCapture))
	result, err := innerBlock.Invoke(innerCtx, "world")
	if err != nil {
		t.Fatalf("Inner Invoke() error = %v", err)
	}
	if result != "hello_world" {
		t.Errorf("Inner result = %q, want %q", result, "hello_world")
	}
}

// ============================================================================
// BashBlock Tests
// ============================================================================

func TestBashBlockInterface(t *testing.T) {
	block := &BashBlock{
		ID:     "block_123",
		Params: []string{"x", "y"},
	}

	if block.NumArgs() != 2 {
		t.Errorf("NumArgs() = %d, want 2", block.NumArgs())
	}
	if block.Kind() != BlockKindBash {
		t.Errorf("Kind() = %v, want BlockKindBash", block.Kind())
	}
	params := block.ParamNames()
	if len(params) != 2 || params[0] != "x" || params[1] != "y" {
		t.Errorf("ParamNames() = %v, want [x y]", params)
	}
}
