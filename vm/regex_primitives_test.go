package vm

import (
	"testing"
)

// helper: compile a regex and return the unwrapped Regex value
func compileRegex(t *testing.T, v *VM, pattern string) Value {
	t.Helper()
	regexClass := v.classValue(v.Classes.Lookup("Regex"))
	patternVal := v.registry.NewStringValue(pattern)
	result := v.Send(regexClass, "compile:", []Value{patternVal})

	// result should be a Success — unwrap via "value"
	isSuccess := v.Send(result, "isSuccess", nil)
	if isSuccess != True {
		t.Fatalf("Regex compile: '%s' returned Failure", pattern)
	}
	return v.Send(result, "value", nil)
}

func TestRegexCompileSuccess(t *testing.T) {
	v := NewVM()
	regexClass := v.classValue(v.Classes.Lookup("Regex"))
	patternVal := v.registry.NewStringValue(`\d+`)
	result := v.Send(regexClass, "compile:", []Value{patternVal})
	isSuccess := v.Send(result, "isSuccess", nil)
	if isSuccess != True {
		t.Fatal("expected Success")
	}
}

func TestRegexCompileFailure(t *testing.T) {
	v := NewVM()
	regexClass := v.classValue(v.Classes.Lookup("Regex"))
	patternVal := v.registry.NewStringValue("[invalid")
	result := v.Send(regexClass, "compile:", []Value{patternVal})
	isFailure := v.Send(result, "isFailure", nil)
	if isFailure != True {
		t.Fatal("expected Failure for invalid regex")
	}
}

func TestRegexMatches(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `^\d+$`)

	strVal := v.registry.NewStringValue("123")
	result := v.Send(re, "matches:", []Value{strVal})
	if result != True {
		t.Fatal("expected '123' to match ^\\d+$")
	}

	strVal = v.registry.NewStringValue("abc")
	result = v.Send(re, "matches:", []Value{strVal})
	if result != False {
		t.Fatal("expected 'abc' to not match ^\\d+$")
	}
}

func TestRegexFindIn(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `\d+`)

	strVal := v.registry.NewStringValue("abc 123 def")
	result := v.Send(re, "findIn:", []Value{strVal})
	got := v.valueToString(result)
	if got != "123" {
		t.Fatalf("expected '123', got '%s'", got)
	}
}

func TestRegexFindInNoMatch(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `xyz`)

	strVal := v.registry.NewStringValue("abc def")
	result := v.Send(re, "findIn:", []Value{strVal})
	if result != Nil {
		t.Fatalf("expected Nil, got %v", result)
	}
}

func TestRegexFindAllIn(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `\d+`)

	strVal := v.registry.NewStringValue("a1 b22 c333")
	result := v.Send(re, "findAllIn:", []Value{strVal})
	size := v.Send(result, "size", nil)
	if size != FromSmallInt(3) {
		t.Fatalf("expected 3 matches, got %v", size)
	}
}

func TestRegexReplaceInWith(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `\d+`)

	strVal := v.registry.NewStringValue("a1b2c3")
	replVal := v.registry.NewStringValue("X")
	result := v.Send(re, "replaceIn:with:", []Value{strVal, replVal})
	got := v.valueToString(result)
	if got != "aXbXcX" {
		t.Fatalf("expected 'aXbXcX', got '%s'", got)
	}
}

func TestRegexSplit(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `,\s*`)

	strVal := v.registry.NewStringValue("a, b, c")
	result := v.Send(re, "split:", []Value{strVal})
	size := v.Send(result, "size", nil)
	if size != FromSmallInt(3) {
		t.Fatalf("expected 3 parts, got %v", size)
	}
}

func TestRegexPattern(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `hello`)

	result := v.Send(re, "pattern", nil)
	got := v.valueToString(result)
	if got != "hello" {
		t.Fatalf("expected 'hello', got '%s'", got)
	}
}

func TestRegexPrintString(t *testing.T) {
	v := NewVM()
	re := compileRegex(t, v, `\d+`)

	result := v.Send(re, "printString", nil)
	got := v.valueToString(result)
	if got != `Regex(/\d+/)` {
		t.Fatalf("expected 'Regex(/\\d+/)', got '%s'", got)
	}
}

func TestStringMatchesRegex(t *testing.T) {
	v := NewVM()
	strVal := v.registry.NewStringValue("hello123")
	patternVal := v.registry.NewStringValue(`\d+`)
	result := v.Send(strVal, "matchesRegex:", []Value{patternVal})
	if result != True {
		t.Fatal("expected 'hello123' to match \\d+")
	}

	strVal = v.registry.NewStringValue("hello")
	patternVal = v.registry.NewStringValue(`^\d+$`)
	result = v.Send(strVal, "matchesRegex:", []Value{patternVal})
	if result != False {
		t.Fatal("expected 'hello' to not match ^\\d+$")
	}
}

func TestStringSplitRegex(t *testing.T) {
	v := NewVM()
	strVal := v.registry.NewStringValue("a1b2c")
	patternVal := v.registry.NewStringValue(`\d+`)
	result := v.Send(strVal, "splitRegex:", []Value{patternVal})
	size := v.Send(result, "size", nil)
	if size != FromSmallInt(3) {
		t.Fatalf("expected 3 parts, got %v", size)
	}
}
