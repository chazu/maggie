package vm

import (
	"strings"
	"testing"
)

func TestInspector_Nil(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	result := inspector.Inspect(Nil)

	if result.Type != "Nil" {
		t.Errorf("expected Type 'Nil', got %q", result.Type)
	}
	if result.Value != "nil" {
		t.Errorf("expected Value 'nil', got %q", result.Value)
	}
}

func TestInspector_True(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	result := inspector.Inspect(True)

	if result.Type != "True" {
		t.Errorf("expected Type 'True', got %q", result.Type)
	}
	if result.Value != "true" {
		t.Errorf("expected Value 'true', got %q", result.Value)
	}
}

func TestInspector_False(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	result := inspector.Inspect(False)

	if result.Type != "False" {
		t.Errorf("expected Type 'False', got %q", result.Type)
	}
	if result.Value != "false" {
		t.Errorf("expected Value 'false', got %q", result.Value)
	}
}

func TestInspector_SmallInt(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	tests := []struct {
		value    int64
		expected string
	}{
		{0, "0"},
		{42, "42"},
		{-17, "-17"},
		{1000000, "1000000"},
		{MaxSmallInt, "140737488355327"},
		{MinSmallInt, "-140737488355328"},
	}

	for _, tc := range tests {
		v := FromSmallInt(tc.value)
		result := inspector.Inspect(v)

		if result.Type != "SmallInt" {
			t.Errorf("for %d: expected Type 'SmallInt', got %q", tc.value, result.Type)
		}
		if result.Value != tc.expected {
			t.Errorf("for %d: expected Value %q, got %q", tc.value, tc.expected, result.Value)
		}
	}
}

func TestInspector_Float(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	tests := []struct {
		value    float64
		expected string
	}{
		{0.0, "0"},
		{3.14159, "3.14159"},
		{-2.5, "-2.5"},
		{1e10, "1e+10"},
	}

	for _, tc := range tests {
		v := FromFloat64(tc.value)
		result := inspector.Inspect(v)

		if result.Type != "Float" {
			t.Errorf("for %v: expected Type 'Float', got %q", tc.value, result.Type)
		}
		if result.Value != tc.expected {
			t.Errorf("for %v: expected Value %q, got %q", tc.value, tc.expected, result.Value)
		}
	}
}

func TestInspector_Symbol(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a symbol
	symVal := vm.Symbols.SymbolValue("testSymbol")

	result := inspector.Inspect(symVal)

	if result.Type != "Symbol" {
		t.Errorf("expected Type 'Symbol', got %q", result.Type)
	}
	if result.Value != "#testSymbol" {
		t.Errorf("expected Value '#testSymbol', got %q", result.Value)
	}
}

func TestInspector_Object(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a class with instance variables
	pointClass := vm.createClassWithIvars("Point", vm.ObjectClass, []string{"x", "y"})

	// Create an object of that class
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(10))
	obj.SetSlot(1, FromSmallInt(20))

	result := inspector.Inspect(obj.ToValue())

	if result.Type != "Object" {
		t.Errorf("expected Type 'Object', got %q", result.Type)
	}
	if result.ClassName != "Point" {
		t.Errorf("expected ClassName 'Point', got %q", result.ClassName)
	}
	if !strings.Contains(result.Value, "Point") {
		t.Errorf("expected Value to contain 'Point', got %q", result.Value)
	}

	// Check instance variables
	if len(result.InstVars) != 2 {
		t.Fatalf("expected 2 instance variables, got %d", len(result.InstVars))
	}

	if result.InstVars[0].Name != "x" {
		t.Errorf("expected first inst var name 'x', got %q", result.InstVars[0].Name)
	}
	if result.InstVars[0].Value.Value != "10" {
		t.Errorf("expected first inst var value '10', got %q", result.InstVars[0].Value.Value)
	}

	if result.InstVars[1].Name != "y" {
		t.Errorf("expected second inst var name 'y', got %q", result.InstVars[1].Name)
	}
	if result.InstVars[1].Value.Value != "20" {
		t.Errorf("expected second inst var value '20', got %q", result.InstVars[1].Value.Value)
	}
}

func TestInspector_ObjectWithNestedObjects(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create Point class
	pointClass := vm.createClassWithIvars("Point", vm.ObjectClass, []string{"x", "y"})

	// Create a Rect class with Point objects
	rectClass := vm.createClassWithIvars("Rect", vm.ObjectClass, []string{"origin", "corner"})

	// Create origin and corner points
	origin := NewObject(pointClass.VTable, 2)
	origin.SetSlot(0, FromSmallInt(0))
	origin.SetSlot(1, FromSmallInt(0))

	corner := NewObject(pointClass.VTable, 2)
	corner.SetSlot(0, FromSmallInt(100))
	corner.SetSlot(1, FromSmallInt(50))

	// Create rectangle
	rect := NewObject(rectClass.VTable, 2)
	rect.SetSlot(0, origin.ToValue())
	rect.SetSlot(1, corner.ToValue())

	result := inspector.Inspect(rect.ToValue())

	if result.ClassName != "Rect" {
		t.Errorf("expected ClassName 'Rect', got %q", result.ClassName)
	}

	// Check nested objects are inspected
	if len(result.InstVars) != 2 {
		t.Fatalf("expected 2 instance variables, got %d", len(result.InstVars))
	}

	originResult := result.InstVars[0]
	if originResult.Name != "origin" {
		t.Errorf("expected first inst var 'origin', got %q", originResult.Name)
	}
	if originResult.Value.ClassName != "Point" {
		t.Errorf("expected origin to be Point, got %q", originResult.Value.ClassName)
	}
}

func TestInspector_DepthLimit(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a self-referential structure
	nodeClass := vm.createClassWithIvars("Node", vm.ObjectClass, []string{"value", "next"})

	node1 := NewObject(nodeClass.VTable, 2)
	node2 := NewObject(nodeClass.VTable, 2)
	node3 := NewObject(nodeClass.VTable, 2)

	node1.SetSlot(0, FromSmallInt(1))
	node1.SetSlot(1, node2.ToValue())

	node2.SetSlot(0, FromSmallInt(2))
	node2.SetSlot(1, node3.ToValue())

	node3.SetSlot(0, FromSmallInt(3))
	node3.SetSlot(1, Nil)

	// With depth 1, should not fully recurse
	result := inspector.InspectDepth(node1.ToValue(), 1)

	if result.ClassName != "Node" {
		t.Errorf("expected ClassName 'Node', got %q", result.ClassName)
	}

	// First level should have inst vars
	if len(result.InstVars) != 2 {
		t.Fatalf("expected 2 instance variables at depth 1, got %d", len(result.InstVars))
	}

	// The nested node should just be a summary (no inst vars)
	nextNode := result.InstVars[1].Value
	if len(nextNode.InstVars) != 0 {
		t.Errorf("expected no nested inst vars at depth limit, got %d", len(nextNode.InstVars))
	}
}

func TestInspector_Channel(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a buffered channel
	ch := createChannel(5)
	chVal := registerChannel(ch)

	result := inspector.Inspect(chVal)

	if result.Type != "Channel" {
		t.Errorf("expected Type 'Channel', got %q", result.Type)
	}
	if result.ClassName != "Channel" {
		t.Errorf("expected ClassName 'Channel', got %q", result.ClassName)
	}
	if !strings.Contains(result.Value, "buffered") {
		t.Errorf("expected Value to mention 'buffered', got %q", result.Value)
	}
	if !strings.Contains(result.Value, "capacity: 5") {
		t.Errorf("expected Value to mention 'capacity: 5', got %q", result.Value)
	}
}

func TestInspector_UnbufferedChannel(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create an unbuffered channel
	ch := createChannel(0)
	chVal := registerChannel(ch)

	result := inspector.Inspect(chVal)

	if result.Type != "Channel" {
		t.Errorf("expected Type 'Channel', got %q", result.Type)
	}
	if !strings.Contains(result.Value, "unbuffered") {
		t.Errorf("expected Value to mention 'unbuffered', got %q", result.Value)
	}
}

func TestInspector_ClosedChannel(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create and close a channel
	ch := createChannel(3)
	ch.closed.Store(true)
	close(ch.ch)
	chVal := registerChannel(ch)

	result := inspector.Inspect(chVal)

	if result.Type != "Channel" {
		t.Errorf("expected Type 'Channel', got %q", result.Type)
	}
	if !strings.Contains(result.Value, "closed") {
		t.Errorf("expected Value to mention 'closed', got %q", result.Value)
	}
}

func TestInspector_Process(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a process
	proc := createProcess()
	procVal := registerProcess(proc)

	result := inspector.Inspect(procVal)

	if result.Type != "Process" {
		t.Errorf("expected Type 'Process', got %q", result.Type)
	}
	if result.ClassName != "Process" {
		t.Errorf("expected ClassName 'Process', got %q", result.ClassName)
	}
	if !strings.Contains(result.Value, "running") {
		t.Errorf("expected Value to mention 'running', got %q", result.Value)
	}

	// Terminate the process and check again
	proc.markDone(FromSmallInt(42), nil)

	result = inspector.Inspect(procVal)
	if !strings.Contains(result.Value, "terminated") {
		t.Errorf("expected terminated process, got %q", result.Value)
	}

	// Should show result in inst vars
	if len(result.InstVars) == 0 {
		t.Error("expected inst vars for terminated process with result")
	} else if result.InstVars[0].Name != "result" {
		t.Errorf("expected 'result' inst var, got %q", result.InstVars[0].Name)
	}
}

func TestInspector_ResultSuccess(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a Success result
	r := createResult(ResultSuccess, FromSmallInt(100))
	rVal := vm.registry.RegisterResultValue(r)

	result := inspector.Inspect(rVal)

	if result.Type != "Success" {
		t.Errorf("expected Type 'Success', got %q", result.Type)
	}
	if result.ClassName != "Success" {
		t.Errorf("expected ClassName 'Success', got %q", result.ClassName)
	}
	if !strings.Contains(result.Value, "Success") {
		t.Errorf("expected Value to contain 'Success', got %q", result.Value)
	}
	if !strings.Contains(result.Value, "100") {
		t.Errorf("expected Value to contain '100', got %q", result.Value)
	}

	// Check inst vars
	if len(result.InstVars) != 1 {
		t.Fatalf("expected 1 inst var, got %d", len(result.InstVars))
	}
	if result.InstVars[0].Name != "value" {
		t.Errorf("expected inst var 'value', got %q", result.InstVars[0].Name)
	}
}

func TestInspector_ResultFailure(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a Failure result with a symbol as error
	errorSym := vm.Symbols.SymbolValue("notFound")
	r := createResult(ResultFailure, errorSym)
	rVal := vm.registry.RegisterResultValue(r)

	result := inspector.Inspect(rVal)

	if result.Type != "Failure" {
		t.Errorf("expected Type 'Failure', got %q", result.Type)
	}
	if result.ClassName != "Failure" {
		t.Errorf("expected ClassName 'Failure', got %q", result.ClassName)
	}
	if !strings.Contains(result.Value, "Failure") {
		t.Errorf("expected Value to contain 'Failure', got %q", result.Value)
	}

	// Check inst vars
	if len(result.InstVars) != 1 {
		t.Fatalf("expected 1 inst var, got %d", len(result.InstVars))
	}
	if result.InstVars[0].Name != "error" {
		t.Errorf("expected inst var 'error', got %q", result.InstVars[0].Name)
	}
}

func TestInspectionResult_String(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Test with a simple value
	result := inspector.Inspect(FromSmallInt(42))
	str := result.String()

	if !strings.Contains(str, "SmallInt") {
		t.Errorf("String output should contain type, got: %s", str)
	}
	if !strings.Contains(str, "42") {
		t.Errorf("String output should contain value, got: %s", str)
	}
}

func TestInspectionResult_StringWithInstVars(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a Point object
	pointClass := vm.createClassWithIvars("Point", vm.ObjectClass, []string{"x", "y"})
	obj := NewObject(pointClass.VTable, 2)
	obj.SetSlot(0, FromSmallInt(10))
	obj.SetSlot(1, FromSmallInt(20))

	result := inspector.Inspect(obj.ToValue())
	str := result.String()

	if !strings.Contains(str, "Point") {
		t.Errorf("String output should contain class name, got: %s", str)
	}
	if !strings.Contains(str, "instance variables") {
		t.Errorf("String output should mention instance variables, got: %s", str)
	}
	if !strings.Contains(str, "x:") {
		t.Errorf("String output should contain 'x:', got: %s", str)
	}
	if !strings.Contains(str, "y:") {
		t.Errorf("String output should contain 'y:', got: %s", str)
	}
}

func TestInspector_NilVM(t *testing.T) {
	// Test that inspector works without a VM (for symbol lookup fallback)
	inspector := NewInspector(nil)

	result := inspector.Inspect(FromSmallInt(42))
	if result.Type != "SmallInt" {
		t.Errorf("expected Type 'SmallInt', got %q", result.Type)
	}

	result = inspector.Inspect(Nil)
	if result.Type != "Nil" {
		t.Errorf("expected Type 'Nil', got %q", result.Type)
	}
}

func TestInspector_SymbolWithoutVM(t *testing.T) {
	inspector := NewInspector(nil)

	// Create a symbol value directly
	symVal := FromSymbolID(42)

	result := inspector.Inspect(symVal)

	if result.Type != "Symbol" {
		t.Errorf("expected Type 'Symbol', got %q", result.Type)
	}
	if !strings.Contains(result.Value, "<symbol:42>") {
		t.Errorf("expected fallback symbol format, got %q", result.Value)
	}
}

func TestInspectionResult_PrettyPrint(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create nested objects
	pointClass := vm.createClassWithIvars("Point", vm.ObjectClass, []string{"x", "y"})
	rectClass := vm.createClassWithIvars("Rect", vm.ObjectClass, []string{"origin", "corner"})

	origin := NewObject(pointClass.VTable, 2)
	origin.SetSlot(0, FromSmallInt(0))
	origin.SetSlot(1, FromSmallInt(0))

	corner := NewObject(pointClass.VTable, 2)
	corner.SetSlot(0, FromSmallInt(100))
	corner.SetSlot(1, FromSmallInt(50))

	rect := NewObject(rectClass.VTable, 2)
	rect.SetSlot(0, origin.ToValue())
	rect.SetSlot(1, corner.ToValue())

	result := inspector.Inspect(rect.ToValue())
	prettyStr := result.PrettyPrint()

	// Should have proper nesting with indentation
	if !strings.Contains(prettyStr, "Rect") {
		t.Errorf("PrettyPrint should contain 'Rect', got: %s", prettyStr)
	}
	if !strings.Contains(prettyStr, "origin") {
		t.Errorf("PrettyPrint should contain 'origin', got: %s", prettyStr)
	}
	if !strings.Contains(prettyStr, "Point") {
		t.Errorf("PrettyPrint should contain nested 'Point', got: %s", prettyStr)
	}
}

func TestInspector_ObjectWithNoVTable(t *testing.T) {
	inspector := NewInspector(nil)

	// Create an object with nil vtable
	obj := &Object{}

	result := inspector.Inspect(obj.ToValue())

	if result.Type != "Object" {
		t.Errorf("expected Type 'Object', got %q", result.Type)
	}
	// Should handle nil vtable gracefully
	if result.ClassName != "Object" {
		t.Errorf("expected ClassName 'Object' for nil vtable, got %q", result.ClassName)
	}
}

func TestInspector_InheritedInstVars(t *testing.T) {
	vm := NewVM()
	inspector := NewInspector(vm)

	// Create a class hierarchy
	shapeClass := vm.createClassWithIvars("Shape", vm.ObjectClass, []string{"color"})
	rectClass := vm.createClassWithIvars("ColoredRect", shapeClass, []string{"width", "height"})

	// Create object with 3 slots (1 inherited + 2 local)
	obj := NewObject(rectClass.VTable, 3)
	obj.SetSlot(0, FromSmallInt(255))  // color
	obj.SetSlot(1, FromSmallInt(100))  // width
	obj.SetSlot(2, FromSmallInt(50))   // height

	result := inspector.Inspect(obj.ToValue())

	if result.ClassName != "ColoredRect" {
		t.Errorf("expected ClassName 'ColoredRect', got %q", result.ClassName)
	}

	if len(result.InstVars) != 3 {
		t.Fatalf("expected 3 instance variables (1 inherited + 2 local), got %d", len(result.InstVars))
	}

	// Check inherited comes first
	if result.InstVars[0].Name != "color" {
		t.Errorf("expected first inst var 'color' (inherited), got %q", result.InstVars[0].Name)
	}
	if result.InstVars[1].Name != "width" {
		t.Errorf("expected second inst var 'width', got %q", result.InstVars[1].Name)
	}
	if result.InstVars[2].Name != "height" {
		t.Errorf("expected third inst var 'height', got %q", result.InstVars[2].Name)
	}
}
