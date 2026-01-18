package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// DebugServer creation tests
// ---------------------------------------------------------------------------

func TestNewDebugServer(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	if ds == nil {
		t.Fatal("NewDebugServer returned nil")
	}

	if ds.vm != vm {
		t.Error("DebugServer.vm not set correctly")
	}

	if ds.active {
		t.Error("DebugServer should not be active by default")
	}

	if len(ds.breakpoints) != 0 {
		t.Error("DebugServer should have no breakpoints by default")
	}
}

func TestDebugServerActivation(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	if ds.IsActive() {
		t.Error("DebugServer should not be active initially")
	}

	ds.Activate()
	if !ds.IsActive() {
		t.Error("DebugServer should be active after Activate()")
	}

	ds.Deactivate()
	if ds.IsActive() {
		t.Error("DebugServer should not be active after Deactivate()")
	}
}

// ---------------------------------------------------------------------------
// Breakpoint management tests
// ---------------------------------------------------------------------------

func TestSetBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	// Set breakpoint on existing class (Object is bootstrapped)
	err := ds.SetBreakpoint("Object", "yourself", 1)
	if err != nil {
		t.Errorf("SetBreakpoint failed: %v", err)
	}

	// Verify breakpoint exists
	if !ds.HasBreakpoint("Object", "yourself", 1) {
		t.Error("Breakpoint should exist after SetBreakpoint")
	}
}

func TestSetBreakpointNonExistentClass(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	err := ds.SetBreakpoint("NonExistentClass", "someMethod", 1)
	if err == nil {
		t.Error("SetBreakpoint should fail for non-existent class")
	}
}

func TestRemoveBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	// Set then remove
	err := ds.SetBreakpoint("Object", "yourself", 1)
	if err != nil {
		t.Fatalf("SetBreakpoint failed: %v", err)
	}

	err = ds.RemoveBreakpoint("Object", "yourself", 1)
	if err != nil {
		t.Errorf("RemoveBreakpoint failed: %v", err)
	}

	// Verify breakpoint no longer exists
	if ds.HasBreakpoint("Object", "yourself", 1) {
		t.Error("Breakpoint should not exist after RemoveBreakpoint")
	}
}

func TestRemoveNonExistentBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	err := ds.RemoveBreakpoint("Object", "yourself", 1)
	if err == nil {
		t.Error("RemoveBreakpoint should fail for non-existent breakpoint")
	}
}

func TestListBreakpoints(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	// Initially empty
	bps := ds.ListBreakpoints()
	if len(bps) != 0 {
		t.Error("ListBreakpoints should return empty list initially")
	}

	// Add some breakpoints
	ds.SetBreakpoint("Object", "yourself", 1)
	ds.SetBreakpoint("Object", "class", 5)
	ds.SetBreakpoint("SmallInteger", "+", 10)

	bps = ds.ListBreakpoints()
	if len(bps) != 3 {
		t.Errorf("Expected 3 breakpoints, got %d", len(bps))
	}

	// Verify each has a unique ID and is active
	ids := make(map[int]bool)
	for _, bp := range bps {
		if ids[bp.ID] {
			t.Errorf("Duplicate breakpoint ID: %d", bp.ID)
		}
		ids[bp.ID] = true

		if !bp.Active {
			t.Errorf("Breakpoint should be active: %s>>%s line %d", bp.Class, bp.Method, bp.Line)
		}
	}
}

func TestClearAllBreakpoints(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	ds.SetBreakpoint("Object", "yourself", 1)
	ds.SetBreakpoint("Object", "class", 5)
	ds.SetBreakpoint("SmallInteger", "+", 10)

	ds.ClearAllBreakpoints()

	bps := ds.ListBreakpoints()
	if len(bps) != 0 {
		t.Errorf("Expected 0 breakpoints after clear, got %d", len(bps))
	}
}

func TestEnableDisableBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	ds.SetBreakpoint("Object", "yourself", 1)

	// Initially active
	if !ds.HasBreakpoint("Object", "yourself", 1) {
		t.Error("Breakpoint should be active initially")
	}

	// Disable
	err := ds.DisableBreakpoint("Object", "yourself", 1)
	if err != nil {
		t.Errorf("DisableBreakpoint failed: %v", err)
	}

	if ds.HasBreakpoint("Object", "yourself", 1) {
		t.Error("Breakpoint should not be active after disable")
	}

	// Re-enable
	err = ds.EnableBreakpoint("Object", "yourself", 1)
	if err != nil {
		t.Errorf("EnableBreakpoint failed: %v", err)
	}

	if !ds.HasBreakpoint("Object", "yourself", 1) {
		t.Error("Breakpoint should be active after enable")
	}
}

func TestDisableNonExistentBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	err := ds.DisableBreakpoint("Object", "yourself", 1)
	if err == nil {
		t.Error("DisableBreakpoint should fail for non-existent breakpoint")
	}
}

// ---------------------------------------------------------------------------
// ShouldBreak tests
// ---------------------------------------------------------------------------

func TestShouldBreakInactive(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	ds.SetBreakpoint("Object", "yourself", 1)

	// Should not break when inactive
	shouldBreak, _ := ds.ShouldBreak("Object", "yourself", 1, 0)
	if shouldBreak {
		t.Error("ShouldBreak should return false when debugger is inactive")
	}
}

func TestShouldBreakOnBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	ds.SetBreakpoint("Object", "yourself", 1)

	// Should break at breakpoint
	shouldBreak, reason := ds.ShouldBreak("Object", "yourself", 1, 0)
	if !shouldBreak {
		t.Error("ShouldBreak should return true at breakpoint")
	}
	if reason != "breakpoint" {
		t.Errorf("Expected reason 'breakpoint', got '%s'", reason)
	}
}

func TestShouldBreakNoBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	// Should not break without breakpoint
	shouldBreak, _ := ds.ShouldBreak("Object", "yourself", 1, 0)
	if shouldBreak {
		t.Error("ShouldBreak should return false without breakpoint")
	}
}

func TestShouldBreakDisabledBreakpoint(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	ds.SetBreakpoint("Object", "yourself", 1)
	ds.DisableBreakpoint("Object", "yourself", 1)

	// Should not break at disabled breakpoint
	shouldBreak, _ := ds.ShouldBreak("Object", "yourself", 1, 0)
	if shouldBreak {
		t.Error("ShouldBreak should return false at disabled breakpoint")
	}
}

// ---------------------------------------------------------------------------
// Pause state tests
// ---------------------------------------------------------------------------

func TestPauseState(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	if ds.IsPaused() {
		t.Error("Should not be paused initially")
	}

	// Trigger pause request
	ds.Pause()

	// ShouldBreak should pick up the pause request
	shouldBreak, reason := ds.ShouldBreak("Object", "test", 1, 0)
	if !shouldBreak {
		t.Error("ShouldBreak should return true after Pause()")
	}
	if reason != "user request" {
		t.Errorf("Expected reason 'user request', got '%s'", reason)
	}

	if !ds.IsPaused() {
		t.Error("Should be paused after ShouldBreak returns true")
	}
}

// ---------------------------------------------------------------------------
// Stepping tests
// ---------------------------------------------------------------------------

func TestStepInto(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	// Set up initial pause
	ds.SetBreakpoint("Object", "test", 1)
	ds.ShouldBreak("Object", "test", 1, 0)

	// Step into
	ds.StepInto()

	if ds.IsPaused() {
		t.Error("Should not be paused after StepInto")
	}

	// Should break on next line
	shouldBreak, reason := ds.ShouldBreak("Object", "test", 2, 0)
	if !shouldBreak {
		t.Error("ShouldBreak should return true after StepInto on new line")
	}
	if reason != "step" {
		t.Errorf("Expected reason 'step', got '%s'", reason)
	}
}

func TestStepOver(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	// Set up initial pause at frame 1, line 1
	ds.SetBreakpoint("Object", "test", 1)
	ds.ShouldBreak("Object", "test", 1, 1)

	// Step over from frame 1, line 1
	ds.StepOver(1, 1)

	// Should NOT break in deeper frame
	shouldBreak, _ := ds.ShouldBreak("Object", "nested", 5, 2)
	if shouldBreak {
		t.Error("StepOver should not break in deeper frame")
	}

	// Should break when back at same frame level with different line
	shouldBreak, reason := ds.ShouldBreak("Object", "test", 2, 1)
	if !shouldBreak {
		t.Error("StepOver should break at same frame level on new line")
	}
	if reason != "step" {
		t.Errorf("Expected reason 'step', got '%s'", reason)
	}
}

func TestStepOut(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)
	ds.Activate()

	// Set up initial pause at frame 2
	ds.SetBreakpoint("Object", "test", 1)
	ds.ShouldBreak("Object", "test", 1, 2)

	// Step out from frame 2
	ds.StepOut(2)

	// Should NOT break in same or deeper frame
	shouldBreak, _ := ds.ShouldBreak("Object", "test", 2, 2)
	if shouldBreak {
		t.Error("StepOut should not break in same frame")
	}

	shouldBreak, _ = ds.ShouldBreak("Object", "nested", 5, 3)
	if shouldBreak {
		t.Error("StepOut should not break in deeper frame")
	}

	// Should break when in shallower frame
	shouldBreak, reason := ds.ShouldBreak("Object", "caller", 10, 1)
	if !shouldBreak {
		t.Error("StepOut should break in shallower frame")
	}
	if reason != "step" {
		t.Errorf("Expected reason 'step', got '%s'", reason)
	}
}

// ---------------------------------------------------------------------------
// Value formatting tests
// ---------------------------------------------------------------------------

func TestFormatValue(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	tests := []struct {
		value    Value
		expected string
	}{
		{Nil, "nil"},
		{True, "true"},
		{False, "false"},
		{FromSmallInt(42), "42"},
		{FromSmallInt(-123), "-123"},
		{FromFloat64(3.14), "3.14"},
	}

	for _, tt := range tests {
		result := ds.formatValue(tt.value)
		if result != tt.expected {
			t.Errorf("formatValue(%v) = %q, want %q", tt.value, result, tt.expected)
		}
	}
}

func TestTypeOf(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	tests := []struct {
		value    Value
		expected string
	}{
		{Nil, "UndefinedObject"},
		{True, "True"},
		{False, "False"},
		{FromSmallInt(42), "SmallInteger"},
		{FromFloat64(3.14), "Float"},
	}

	for _, tt := range tests {
		result := ds.typeOf(tt.value)
		if result != tt.expected {
			t.Errorf("typeOf(%v) = %q, want %q", tt.value, result, tt.expected)
		}
	}
}

// ---------------------------------------------------------------------------
// Stack frame tests
// ---------------------------------------------------------------------------

func TestGetCallStackEmpty(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	// With nil interpreter
	frames := ds.GetCallStack(nil)
	if frames != nil {
		t.Error("GetCallStack should return nil for nil interpreter")
	}

	// With empty frame stack
	interp := NewInterpreter()
	frames = ds.GetCallStack(interp)
	if frames != nil {
		t.Error("GetCallStack should return nil for empty frame stack")
	}
}

func TestGetVariablesEmpty(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	// With nil interpreter
	vars := ds.GetVariables(nil, 0)
	if vars != nil {
		t.Error("GetVariables should return nil for nil interpreter")
	}

	// With invalid frame ID
	interp := NewInterpreter()
	vars = ds.GetVariables(interp, 100)
	if vars != nil {
		t.Error("GetVariables should return nil for invalid frame ID")
	}
}

// ---------------------------------------------------------------------------
// Event tests
// ---------------------------------------------------------------------------

func TestEvents(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	eventChan := ds.Events()
	if eventChan == nil {
		t.Error("Events() should not return nil")
	}

	// Send event and verify receipt
	ds.sendEvent(DebugEvent{
		Type:   "test",
		Reason: "test reason",
	})

	select {
	case event := <-eventChan:
		if event.Type != "test" {
			t.Errorf("Expected event type 'test', got '%s'", event.Type)
		}
		if event.Reason != "test reason" {
			t.Errorf("Expected reason 'test reason', got '%s'", event.Reason)
		}
	default:
		t.Error("Expected event on channel")
	}
}

func TestNotifyMethods(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	eventChan := ds.Events()

	loc := &SourceLocation{
		Class:  "TestClass",
		Method: "testMethod",
		Line:   42,
		Column: 1,
	}

	// Test NotifyBreakpointHit
	ds.NotifyBreakpointHit(loc)
	select {
	case event := <-eventChan:
		if event.Type != "breakpointHit" {
			t.Errorf("Expected type 'breakpointHit', got '%s'", event.Type)
		}
	default:
		t.Error("Expected breakpointHit event")
	}

	// Test NotifyStopped
	ds.NotifyStopped("step", loc)
	select {
	case event := <-eventChan:
		if event.Type != "stopped" {
			t.Errorf("Expected type 'stopped', got '%s'", event.Type)
		}
	default:
		t.Error("Expected stopped event")
	}

	// Test NotifyException
	ds.NotifyException("error message", loc)
	select {
	case event := <-eventChan:
		if event.Type != "exception" {
			t.Errorf("Expected type 'exception', got '%s'", event.Type)
		}
	default:
		t.Error("Expected exception event")
	}
}

// ---------------------------------------------------------------------------
// Deactivation tests
// ---------------------------------------------------------------------------

func TestDeactivationClearsBreakpoints(t *testing.T) {
	vm := NewVM()
	ds := NewDebugServer(vm)

	ds.SetBreakpoint("Object", "yourself", 1)
	ds.SetBreakpoint("Object", "class", 5)

	if len(ds.ListBreakpoints()) != 2 {
		t.Error("Should have 2 breakpoints before deactivation")
	}

	ds.Deactivate()

	if len(ds.ListBreakpoints()) != 0 {
		t.Error("Deactivation should clear all breakpoints")
	}
}
