package codegen

import (
	"strings"
	"testing"
)

func TestNewCodeValidator(t *testing.T) {
	cv := NewCodeValidator("test.go")
	if cv == nil {
		t.Fatal("NewCodeValidator returned nil")
	}
	if cv.filename != "test.go" {
		t.Errorf("filename = %q, want %q", cv.filename, "test.go")
	}
}

func TestValidate_ValidCode(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

func main() {
	x := 1
	_ = x
}
`
	errors := cv.Validate(source)
	if len(errors) != 0 {
		t.Errorf("Expected no errors for valid code, got %d: %v", len(errors), errors)
	}
}

func TestValidate_SyntaxError(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

func main() {
	x :=
}
`
	errors := cv.Validate(source)
	if len(errors) == 0 {
		t.Error("Expected syntax error, got none")
	}
}

func TestValidate_TypeErrorUndefinedVariable(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

func main() {
	_ = undefinedVariable
}
`
	errors := cv.Validate(source)
	if len(errors) == 0 {
		t.Error("Expected type error for undefined variable, got none")
	}
	// Check that the error mentions the undefined variable
	found := false
	for _, err := range errors {
		if strings.Contains(err.Message, "undefined") || strings.Contains(err.Message, "undefinedVariable") {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("Expected error about undefined variable, got: %v", errors)
	}
}

func TestValidate_TypeErrorWrongType(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

func main() {
	var x int = "not an int"
	_ = x
}
`
	errors := cv.Validate(source)
	if len(errors) == 0 {
		t.Error("Expected type error for wrong type, got none")
	}
}

func TestValidate_MethodWithReceiver(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

type Counter struct {
	value int
}

func (c *Counter) Increment() {
	_ = undefinedVar
}

func main() {}
`
	errors := cv.Validate(source)
	if len(errors) == 0 {
		t.Error("Expected error in method, got none")
	}
	// Check that the error is attributed to the Increment method
	found := false
	for _, err := range errors {
		if err.Function == "Increment" {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("Expected error attributed to Increment method, got: %v", errors)
	}
}

func TestGetMethodsWithErrors(t *testing.T) {
	cv := NewCodeValidator("test.go")

	errors := []ValidationError{
		{Function: "Increment", Receiver: "*Counter", Message: "test"},
		{Function: "Decrement", Receiver: "*Counter", Message: "test"},
		{Function: "helper", Message: "test"},
		{Function: "<package>", Message: "package level"},
		{Function: "", Message: "no function"},
	}

	methods := cv.GetMethodsWithErrors(errors)

	expected := map[string]bool{
		"*Counter.Increment": true,
		"*Counter.Decrement": true,
		"helper":             true,
	}

	if len(methods) != len(expected) {
		t.Errorf("GetMethodsWithErrors returned %d methods, want %d", len(methods), len(expected))
	}

	for m := range expected {
		if !methods[m] {
			t.Errorf("Expected method %q in result", m)
		}
	}
}

func TestGetMethodSelectorsWithErrors(t *testing.T) {
	cv := NewCodeValidator("test.go")

	errors := []ValidationError{
		{Function: "SetValue", Message: "test"},
		{Function: "GetValue", Message: "test"},
		{Function: "UnknownFunction", Message: "test"},
		{Function: "<package>", Message: "package level"},
	}

	goNameToSelector := map[string]string{
		"SetValue": "setValue:",
		"GetValue": "getValue",
	}

	selectors := cv.GetMethodSelectorsWithErrors(errors, goNameToSelector)

	if !selectors["setValue:"] {
		t.Error("Expected setValue: selector in result")
	}
	if !selectors["getValue"] {
		t.Error("Expected getValue selector in result")
	}
	if !selectors["UnknownFunction"] {
		t.Error("Expected UnknownFunction (fallback) in result")
	}
	if selectors["<package>"] {
		t.Error("Did not expect <package> in result")
	}
}

func TestFormatValidationErrors_Empty(t *testing.T) {
	result := FormatValidationErrors(nil, "test.go")
	if result != "" {
		t.Errorf("Expected empty string for no errors, got %q", result)
	}
}

func TestFormatValidationErrors_WithErrors(t *testing.T) {
	errors := []ValidationError{
		{Function: "Increment", Receiver: "*Counter", Message: "undefined: x"},
		{Function: "helper", Message: "undefined: y"},
		{Function: "<package>", Message: "import error"},
		{Message: "general error"},
	}

	result := FormatValidationErrors(errors, "test.go")

	if !strings.Contains(result, "(*Counter).Increment") {
		t.Error("Expected formatted method with receiver")
	}
	if !strings.Contains(result, "helper:") {
		t.Error("Expected formatted function")
	}
	if !strings.Contains(result, "undefined: x") {
		t.Error("Expected error message")
	}
}

func TestValidate_FunctionMap(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

func foo() {
	_ = undefined1
}

func bar() {
	_ = undefined2
}

func main() {}
`
	errors := cv.Validate(source)

	// Check that errors are attributed to correct functions
	fooError := false
	barError := false
	for _, err := range errors {
		if err.Function == "foo" {
			fooError = true
		}
		if err.Function == "bar" {
			barError = true
		}
	}

	if !fooError {
		t.Error("Expected error attributed to foo")
	}
	if !barError {
		t.Error("Expected error attributed to bar")
	}
}

func TestValidate_ReceiverTypeExtraction(t *testing.T) {
	cv := NewCodeValidator("test.go")
	source := `package main

type MyType struct{}

func (m MyType) ValueReceiver() {
	_ = undefined1
}

func (m *MyType) PointerReceiver() {
	_ = undefined2
}

func main() {}
`
	errors := cv.Validate(source)

	// Check receiver extraction
	valueReceiverFound := false
	pointerReceiverFound := false
	for _, err := range errors {
		if err.Function == "ValueReceiver" && err.Receiver == "MyType" {
			valueReceiverFound = true
		}
		if err.Function == "PointerReceiver" && err.Receiver == "*MyType" {
			pointerReceiverFound = true
		}
	}

	if !valueReceiverFound {
		t.Error("Expected error with MyType receiver")
	}
	if !pointerReceiverFound {
		t.Error("Expected error with *MyType receiver")
	}
}
