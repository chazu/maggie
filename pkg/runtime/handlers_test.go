package runtime

import (
	"strings"
	"testing"
)

func TestHandlerRegistry(t *testing.T) {
	hr := NewHandlerRegistry()

	called := false
	hr.Register("Counter", "increment", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		called = true
		return "42", nil
	})

	// Lookup registered handler
	h := hr.Lookup("Counter", "increment")
	if h == nil {
		t.Fatal("Lookup returned nil for registered handler")
	}

	// Call it
	result, err := h(nil, nil, "", nil)
	if err != nil {
		t.Fatalf("handler error: %v", err)
	}
	if !called {
		t.Error("handler was not called")
	}
	if result != "42" {
		t.Errorf("result = %v, want 42", result)
	}

	// Lookup unregistered handler
	h = hr.Lookup("Counter", "decrement")
	if h != nil {
		t.Error("Lookup returned non-nil for unregistered handler")
	}
}

func TestHandlerRegistryWildcard(t *testing.T) {
	hr := NewHandlerRegistry()

	hr.Register("*", "class", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return inst.Class, nil
	})

	// Should match any class
	inst := &Instance{Class: "Counter"}
	h := hr.Lookup("Counter", "class")
	if h == nil {
		t.Fatal("Lookup returned nil for wildcard handler")
	}
	result, _ := h(nil, inst, "", nil)
	if result != "Counter" {
		t.Errorf("result = %v, want Counter", result)
	}

	// Should also match other classes
	inst.Class = "Timer"
	h = hr.Lookup("Timer", "class")
	if h == nil {
		t.Fatal("Lookup returned nil for wildcard handler with different class")
	}
	result, _ = h(nil, inst, "", nil)
	if result != "Timer" {
		t.Errorf("result = %v, want Timer", result)
	}
}

func TestListHandlers(t *testing.T) {
	hr := NewHandlerRegistry()

	hr.Register("A", "foo", nil)
	hr.Register("B", "bar", nil)
	hr.Register("A", "baz", nil)

	handlers := hr.ListHandlers()
	if len(handlers) != 3 {
		t.Errorf("len(handlers) = %v, want 3", len(handlers))
	}

	// Should be sorted
	if handlers[0] != "A.baz" || handlers[1] != "A.foo" || handlers[2] != "B.bar" {
		t.Errorf("handlers = %v, want sorted", handlers)
	}
}

// Test Object handlers
func TestObjectHandlers(t *testing.T) {
	r := testRuntime(t)

	inst := &Instance{
		ID:    "counter_123",
		Class: "Counter",
		Data:  map[string]interface{}{"value": 42},
	}

	t.Run("class", func(t *testing.T) {
		result, handled, err := r.Dispatch("Counter", "class", inst, "counter_123", nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "Counter" {
			t.Errorf("result = %v, want Counter", result)
		}
	})

	t.Run("yourself", func(t *testing.T) {
		result, handled, err := r.Dispatch("Counter", "yourself", inst, "counter_123", nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "counter_123" {
			t.Errorf("result = %v, want counter_123", result)
		}
	})

	t.Run("id", func(t *testing.T) {
		result, handled, err := r.Dispatch("Counter", "id", inst, "counter_123", nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "counter_123" {
			t.Errorf("result = %v, want counter_123", result)
		}
	})

	t.Run("printString", func(t *testing.T) {
		result, handled, err := r.Dispatch("Counter", "printString", inst, "counter_123", nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "<Counter counter_123>" {
			t.Errorf("result = %v, want <Counter counter_123>", result)
		}
	})

	t.Run("asJson", func(t *testing.T) {
		result, handled, err := r.Dispatch("Counter", "asJson", inst, "counter_123", nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if !strings.Contains(result, `"class":"Counter"`) {
			t.Errorf("result = %v, expected to contain class:Counter", result)
		}
	})
}

// Test Array handlers
func TestArrayHandlers(t *testing.T) {
	r := testRuntime(t)

	// Create Array instance
	id, inst, err := r.CreateInstance("Array", map[string]interface{}{
		"items": `["a","b","c"]`,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	t.Run("size", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "size", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "3" {
			t.Errorf("result = %v, want 3", result)
		}
	})

	t.Run("at_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "at_", inst, id, []string{"1"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "b" {
			t.Errorf("result = %v, want b", result)
		}
	})

	t.Run("at_ negative index", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "at_", inst, id, []string{"-1"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "c" {
			t.Errorf("result = %v, want c", result)
		}
	})

	t.Run("first", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "first", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "a" {
			t.Errorf("result = %v, want a", result)
		}
	})

	t.Run("last", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "last", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "c" {
			t.Errorf("result = %v, want c", result)
		}
	})

	t.Run("push_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "push_", inst, id, []string{"d"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "d" {
			t.Errorf("result = %v, want d", result)
		}

		// Verify size increased
		sizeResult, _, _ := r.Dispatch("Array", "size", inst, id, nil)
		if sizeResult != "4" {
			t.Errorf("size after push = %v, want 4", sizeResult)
		}
	})

	t.Run("pop", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "pop", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "d" {
			t.Errorf("result = %v, want d", result)
		}

		// Verify size decreased
		sizeResult, _, _ := r.Dispatch("Array", "size", inst, id, nil)
		if sizeResult != "3" {
			t.Errorf("size after pop = %v, want 3", sizeResult)
		}
	})

	t.Run("at_put_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "at_put_", inst, id, []string{"1", "X"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "X" {
			t.Errorf("result = %v, want X", result)
		}

		// Verify value changed
		atResult, _, _ := r.Dispatch("Array", "at_", inst, id, []string{"1"})
		if atResult != "X" {
			t.Errorf("at:1 after put = %v, want X", atResult)
		}
	})

	t.Run("isEmpty", func(t *testing.T) {
		result, handled, err := r.Dispatch("Array", "isEmpty", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "false" {
			t.Errorf("result = %v, want false", result)
		}
	})
}

func TestArrayHandlersEmpty(t *testing.T) {
	r := testRuntime(t)

	id, inst, err := r.CreateInstance("Array", map[string]interface{}{
		"items": `[]`,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	t.Run("isEmpty on empty array", func(t *testing.T) {
		result, _, _ := r.Dispatch("Array", "isEmpty", inst, id, nil)
		if result != "true" {
			t.Errorf("result = %v, want true", result)
		}
	})

	t.Run("size on empty array", func(t *testing.T) {
		result, _, _ := r.Dispatch("Array", "size", inst, id, nil)
		if result != "0" {
			t.Errorf("result = %v, want 0", result)
		}
	})
}

// Test Dictionary handlers
func TestDictionaryHandlers(t *testing.T) {
	r := testRuntime(t)

	id, inst, err := r.CreateInstance("Dictionary", map[string]interface{}{
		"items": `{"name":"Alice","age":"30"}`,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	t.Run("size", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "size", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "2" {
			t.Errorf("result = %v, want 2", result)
		}
	})

	t.Run("at_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "at_", inst, id, []string{"name"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "Alice" {
			t.Errorf("result = %v, want Alice", result)
		}
	})

	t.Run("at_ missing key", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "at_", inst, id, []string{"missing"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "" {
			t.Errorf("result = %v, want empty string", result)
		}
	})

	t.Run("includesKey_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "includesKey_", inst, id, []string{"name"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "true" {
			t.Errorf("result = %v, want true", result)
		}
	})

	t.Run("includesKey_ missing", func(t *testing.T) {
		result, _, _ := r.Dispatch("Dictionary", "includesKey_", inst, id, []string{"missing"})
		if result != "false" {
			t.Errorf("result = %v, want false", result)
		}
	})

	t.Run("at_put_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "at_put_", inst, id, []string{"city", "NYC"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "NYC" {
			t.Errorf("result = %v, want NYC", result)
		}

		// Verify value was set
		atResult, _, _ := r.Dispatch("Dictionary", "at_", inst, id, []string{"city"})
		if atResult != "NYC" {
			t.Errorf("at:city = %v, want NYC", atResult)
		}

		// Verify size increased
		sizeResult, _, _ := r.Dispatch("Dictionary", "size", inst, id, nil)
		if sizeResult != "3" {
			t.Errorf("size = %v, want 3", sizeResult)
		}
	})

	t.Run("removeAt_", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "removeAt_", inst, id, []string{"city"})
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		if result != "NYC" {
			t.Errorf("result = %v, want NYC", result)
		}

		// Verify key was removed
		includesResult, _, _ := r.Dispatch("Dictionary", "includesKey_", inst, id, []string{"city"})
		if includesResult != "false" {
			t.Errorf("includesKey:city = %v, want false", includesResult)
		}
	})

	t.Run("keys", func(t *testing.T) {
		result, handled, err := r.Dispatch("Dictionary", "keys", inst, id, nil)
		if err != nil {
			t.Fatalf("error: %v", err)
		}
		if !handled {
			t.Error("handler not found")
		}
		// Keys are returned sorted
		if result != `["age","name"]` {
			t.Errorf("result = %v, want [\"age\",\"name\"]", result)
		}
	})

	t.Run("isEmpty", func(t *testing.T) {
		result, _, _ := r.Dispatch("Dictionary", "isEmpty", inst, id, nil)
		if result != "false" {
			t.Errorf("result = %v, want false", result)
		}
	})
}

func TestDictionaryHandlersEmpty(t *testing.T) {
	r := testRuntime(t)

	id, inst, err := r.CreateInstance("Dictionary", map[string]interface{}{
		"items": `{}`,
	})
	if err != nil {
		t.Fatalf("CreateInstance error: %v", err)
	}

	t.Run("isEmpty on empty dict", func(t *testing.T) {
		result, _, _ := r.Dispatch("Dictionary", "isEmpty", inst, id, nil)
		if result != "true" {
			t.Errorf("result = %v, want true", result)
		}
	})

	t.Run("size on empty dict", func(t *testing.T) {
		result, _, _ := r.Dispatch("Dictionary", "size", inst, id, nil)
		if result != "0" {
			t.Errorf("result = %v, want 0", result)
		}
	})
}

func TestDispatchUnhandled(t *testing.T) {
	r := testRuntime(t)

	inst := &Instance{Class: "Unknown"}
	result, handled, err := r.Dispatch("Unknown", "unknownMethod", inst, "id", nil)
	if err != nil {
		t.Fatalf("error: %v", err)
	}
	if handled {
		t.Error("handled should be false for unknown method")
	}
	if result != "" {
		t.Errorf("result = %v, want empty", result)
	}
}

// Test JSON helper functions directly
func TestJsonArrayHelpers(t *testing.T) {
	t.Run("jsonArrayLen", func(t *testing.T) {
		if jsonArrayLen(`[1,2,3]`) != 3 {
			t.Error("len of [1,2,3] should be 3")
		}
		if jsonArrayLen(`[]`) != 0 {
			t.Error("len of [] should be 0")
		}
		if jsonArrayLen(`invalid`) != 0 {
			t.Error("len of invalid should be 0")
		}
	})

	t.Run("jsonArrayAt", func(t *testing.T) {
		if jsonArrayAt(`["a","b","c"]`, 0) != "a" {
			t.Error("at 0 should be a")
		}
		if jsonArrayAt(`["a","b","c"]`, -1) != "c" {
			t.Error("at -1 should be c")
		}
		if jsonArrayAt(`["a","b","c"]`, 10) != "" {
			t.Error("at 10 should be empty")
		}
	})

	t.Run("jsonArrayPush", func(t *testing.T) {
		result := jsonArrayPush(`["a"]`, "b")
		if result != `["a","b"]` {
			t.Errorf("push b to [a] = %v, want [a,b]", result)
		}
	})

	t.Run("jsonArrayRemoveAt", func(t *testing.T) {
		result := jsonArrayRemoveAt(`["a","b","c"]`, 1)
		if result != `["a","c"]` {
			t.Errorf("remove at 1 = %v, want [a,c]", result)
		}
	})
}

func TestJsonObjectHelpers(t *testing.T) {
	t.Run("jsonObjectLen", func(t *testing.T) {
		if jsonObjectLen(`{"a":1,"b":2}`) != 2 {
			t.Error("len should be 2")
		}
		if jsonObjectLen(`{}`) != 0 {
			t.Error("len of {} should be 0")
		}
	})

	t.Run("jsonObjectAt", func(t *testing.T) {
		if jsonObjectAt(`{"name":"Alice"}`, "name") != "Alice" {
			t.Error("at name should be Alice")
		}
		if jsonObjectAt(`{"name":"Alice"}`, "missing") != "" {
			t.Error("at missing should be empty")
		}
	})

	t.Run("jsonObjectAtPut", func(t *testing.T) {
		result := jsonObjectAtPut(`{"a":1}`, "b", 2)
		if !strings.Contains(result, `"b":2`) {
			t.Errorf("put b:2 = %v, should contain b:2", result)
		}
	})

	t.Run("jsonObjectHasKey", func(t *testing.T) {
		if !jsonObjectHasKey(`{"name":"Alice"}`, "name") {
			t.Error("should have key name")
		}
		if jsonObjectHasKey(`{"name":"Alice"}`, "missing") {
			t.Error("should not have key missing")
		}
	})

	t.Run("jsonObjectRemoveKey", func(t *testing.T) {
		result := jsonObjectRemoveKey(`{"a":1,"b":2}`, "a")
		if strings.Contains(result, `"a"`) {
			t.Errorf("remove a = %v, should not contain a", result)
		}
		if !strings.Contains(result, `"b"`) {
			t.Errorf("remove a = %v, should still contain b", result)
		}
	})

	t.Run("jsonObjectKeys", func(t *testing.T) {
		keys := jsonObjectKeys(`{"b":1,"a":2}`)
		if len(keys) != 2 || keys[0] != "a" || keys[1] != "b" {
			t.Errorf("keys = %v, want [a, b] sorted", keys)
		}
	})
}
