// handlers.go provides native Go implementations for common Trashtalk methods.
// These handlers can be registered with the runtime to bypass Bash fallback.

package runtime

import (
	"encoding/json"
	"fmt"
	"sort"
	"strconv"
	"sync"
)

// HandlerFunc is the signature for native method handlers.
// It receives the runtime, instance, instance ID, and method arguments.
// Returns the result string and any error.
type HandlerFunc func(r *Runtime, inst *Instance, id string, args []string) (string, error)

// HandlerRegistry maps class/selector pairs to native handlers.
type HandlerRegistry struct {
	handlers map[string]HandlerFunc // key: "ClassName.selector"
	mu       sync.RWMutex
}

// NewHandlerRegistry creates a new handler registry.
func NewHandlerRegistry() *HandlerRegistry {
	return &HandlerRegistry{
		handlers: make(map[string]HandlerFunc),
	}
}

// Register adds a handler for a class/selector combination.
func (hr *HandlerRegistry) Register(className, selector string, handler HandlerFunc) {
	hr.mu.Lock()
	defer hr.mu.Unlock()
	key := className + "." + selector
	hr.handlers[key] = handler
}

// Lookup finds a handler for the given class and selector.
// Returns nil if no handler is registered.
func (hr *HandlerRegistry) Lookup(className, selector string) HandlerFunc {
	hr.mu.RLock()
	defer hr.mu.RUnlock()

	// Try exact match first
	key := className + "." + selector
	if h, ok := hr.handlers[key]; ok {
		return h
	}

	// Try wildcard class (for Object methods inherited by all)
	key = "*." + selector
	if h, ok := hr.handlers[key]; ok {
		return h
	}

	return nil
}

// ListHandlers returns all registered class.selector combinations.
func (hr *HandlerRegistry) ListHandlers() []string {
	hr.mu.RLock()
	defer hr.mu.RUnlock()

	keys := make([]string, 0, len(hr.handlers))
	for k := range hr.handlers {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// DefaultRegistry is the global handler registry with built-in handlers.
var DefaultRegistry = NewHandlerRegistry()

func init() {
	// Register Object handlers (wildcard class for inheritance)
	RegisterObjectHandlers(DefaultRegistry)

	// Register Array handlers
	RegisterArrayHandlers(DefaultRegistry)

	// Register Dictionary handlers
	RegisterDictionaryHandlers(DefaultRegistry)
}

// RegisterObjectHandlers registers handlers for Object methods.
// These use wildcard class (*) so they apply to all classes.
func RegisterObjectHandlers(hr *HandlerRegistry) {
	// class - return the class name
	hr.Register("*", "class", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return inst.Class, nil
	})

	// yourself - return the receiver (instance ID)
	hr.Register("*", "yourself", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return id, nil
	})

	// id - return the instance ID
	hr.Register("*", "id", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return id, nil
	})

	// printString - return "<ClassName instanceId>"
	hr.Register("*", "printString", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return fmt.Sprintf("<%s %s>", inst.Class, id), nil
	})

	// asJson - return instance data as JSON
	hr.Register("*", "asJson", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		data, err := json.Marshal(inst)
		if err != nil {
			return "", err
		}
		return string(data), nil
	})
}

// RegisterArrayHandlers registers handlers for Array methods.
func RegisterArrayHandlers(hr *HandlerRegistry) {
	// at: - get element at index
	hr.Register("Array", "at_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("at: requires 1 argument")
		}
		idx, err := strconv.Atoi(args[0])
		if err != nil {
			return "", fmt.Errorf("at: invalid index: %v", err)
		}

		items := inst.GetVarString("items")
		return jsonArrayAt(items, idx), nil
	})

	// at:put: - set element at index
	hr.Register("Array", "at_put_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 2 {
			return "", fmt.Errorf("at:put: requires 2 arguments")
		}
		idx, err := strconv.Atoi(args[0])
		if err != nil {
			return "", fmt.Errorf("at:put: invalid index: %v", err)
		}
		value := args[1]

		items := inst.GetVarString("items")
		newItems := jsonArrayAtPut(items, idx, value)
		inst.SetVar("items", newItems)
		r.MarkDirty(id)
		return value, nil
	})

	// push: - add element to end
	hr.Register("Array", "push_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("push: requires 1 argument")
		}
		value := args[0]

		items := inst.GetVarString("items")
		newItems := jsonArrayPush(items, value)
		inst.SetVar("items", newItems)
		r.MarkDirty(id)
		return value, nil
	})

	// pop - remove and return last element
	hr.Register("Array", "pop", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		last := jsonArrayLast(items)
		newItems := jsonArrayRemoveAt(items, -1)
		inst.SetVar("items", newItems)
		r.MarkDirty(id)
		return last, nil
	})

	// size - get array length
	hr.Register("Array", "size", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		return strconv.Itoa(jsonArrayLen(items)), nil
	})

	// isEmpty - check if array is empty
	hr.Register("Array", "isEmpty", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		if jsonArrayLen(items) == 0 {
			return "true", nil
		}
		return "false", nil
	})

	// first - get first element
	hr.Register("Array", "first", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		return jsonArrayFirst(items), nil
	})

	// last - get last element
	hr.Register("Array", "last", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		return jsonArrayLast(items), nil
	})

	// getItems - get raw items JSON
	hr.Register("Array", "getItems", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return inst.GetVarString("items"), nil
	})

	// setItems: - set raw items JSON
	hr.Register("Array", "setItems_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("setItems: requires 1 argument")
		}
		inst.SetVar("items", args[0])
		r.MarkDirty(id)
		return "", nil
	})
}

// RegisterDictionaryHandlers registers handlers for Dictionary methods.
func RegisterDictionaryHandlers(hr *HandlerRegistry) {
	// at: - get value at key
	hr.Register("Dictionary", "at_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("at: requires 1 argument")
		}
		key := args[0]

		items := inst.GetVarString("items")
		return jsonObjectAt(items, key), nil
	})

	// at:put: - set value at key
	hr.Register("Dictionary", "at_put_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 2 {
			return "", fmt.Errorf("at:put: requires 2 arguments")
		}
		key := args[0]
		value := args[1]

		items := inst.GetVarString("items")
		newItems := jsonObjectAtPut(items, key, value)
		inst.SetVar("items", newItems)
		r.MarkDirty(id)
		return value, nil
	})

	// includesKey: - check if key exists
	hr.Register("Dictionary", "includesKey_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("includesKey: requires 1 argument")
		}
		key := args[0]

		items := inst.GetVarString("items")
		if jsonObjectHasKey(items, key) {
			return "true", nil
		}
		return "false", nil
	})

	// removeAt: - remove key and return old value
	hr.Register("Dictionary", "removeAt_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("removeAt: requires 1 argument")
		}
		key := args[0]

		items := inst.GetVarString("items")
		oldValue := jsonObjectAt(items, key)
		newItems := jsonObjectRemoveKey(items, key)
		inst.SetVar("items", newItems)
		r.MarkDirty(id)
		return oldValue, nil
	})

	// size - get number of keys
	hr.Register("Dictionary", "size", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		return strconv.Itoa(jsonObjectLen(items)), nil
	})

	// isEmpty - check if dictionary is empty
	hr.Register("Dictionary", "isEmpty", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		if jsonObjectLen(items) == 0 {
			return "true", nil
		}
		return "false", nil
	})

	// keys - get all keys as JSON array
	hr.Register("Dictionary", "keys", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		items := inst.GetVarString("items")
		keys := jsonObjectKeys(items)
		result, err := json.Marshal(keys)
		if err != nil {
			return "[]", nil
		}
		return string(result), nil
	})

	// getItems - get raw items JSON
	hr.Register("Dictionary", "getItems", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		return inst.GetVarString("items"), nil
	})

	// setItems: - set raw items JSON
	hr.Register("Dictionary", "setItems_", func(r *Runtime, inst *Instance, id string, args []string) (string, error) {
		if len(args) < 1 {
			return "", fmt.Errorf("setItems: requires 1 argument")
		}
		inst.SetVar("items", args[0])
		r.MarkDirty(id)
		return "", nil
	})
}

// === JSON helper functions ===
// These operate on JSON strings representing arrays and objects.

func jsonArrayLen(jsonStr string) int {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return 0
	}
	return len(arr)
}

func jsonArrayAt(jsonStr string, idx int) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return ""
	}
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx < 0 || idx >= len(arr) {
		return ""
	}
	return fmt.Sprintf("%v", arr[idx])
}

func jsonArrayAtPut(jsonStr string, idx int, val interface{}) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		arr = []interface{}{}
	}
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx >= 0 && idx < len(arr) {
		arr[idx] = val
	}
	result, _ := json.Marshal(arr)
	return string(result)
}

func jsonArrayPush(jsonStr string, val interface{}) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		arr = []interface{}{}
	}
	arr = append(arr, val)
	result, _ := json.Marshal(arr)
	return string(result)
}

func jsonArrayFirst(jsonStr string) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil || len(arr) == 0 {
		return ""
	}
	return fmt.Sprintf("%v", arr[0])
}

func jsonArrayLast(jsonStr string) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil || len(arr) == 0 {
		return ""
	}
	return fmt.Sprintf("%v", arr[len(arr)-1])
}

func jsonArrayRemoveAt(jsonStr string, idx int) string {
	var arr []interface{}
	if err := json.Unmarshal([]byte(jsonStr), &arr); err != nil {
		return "[]"
	}
	// Handle negative indices
	if idx < 0 {
		idx = len(arr) + idx
	}
	if idx >= 0 && idx < len(arr) {
		arr = append(arr[:idx], arr[idx+1:]...)
	}
	result, _ := json.Marshal(arr)
	return string(result)
}

func jsonObjectLen(jsonStr string) int {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return 0
	}
	return len(m)
}

func jsonObjectAt(jsonStr string, key string) string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return ""
	}
	if v, ok := m[key]; ok {
		return fmt.Sprintf("%v", v)
	}
	return ""
}

func jsonObjectAtPut(jsonStr string, key string, val interface{}) string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		m = make(map[string]interface{})
	}
	m[key] = val
	result, _ := json.Marshal(m)
	return string(result)
}

func jsonObjectHasKey(jsonStr string, key string) bool {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return false
	}
	_, ok := m[key]
	return ok
}

func jsonObjectRemoveKey(jsonStr string, key string) string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return "{}"
	}
	delete(m, key)
	result, _ := json.Marshal(m)
	return string(result)
}

func jsonObjectKeys(jsonStr string) []string {
	var m map[string]interface{}
	if err := json.Unmarshal([]byte(jsonStr), &m); err != nil {
		return nil
	}
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// Dispatch attempts to dispatch a message using registered handlers.
// Returns result, handled (true if handler was found), and any error.
func (r *Runtime) Dispatch(className, selector string, inst *Instance, id string, args []string) (string, bool, error) {
	handler := DefaultRegistry.Lookup(className, selector)
	if handler == nil {
		return "", false, nil
	}

	result, err := handler(r, inst, id, args)
	return result, true, err
}

// DispatchWithRegistry is like Dispatch but uses a custom registry.
func (r *Runtime) DispatchWithRegistry(registry *HandlerRegistry, className, selector string, inst *Instance, id string, args []string) (string, bool, error) {
	handler := registry.Lookup(className, selector)
	if handler == nil {
		return "", false, nil
	}

	result, err := handler(r, inst, id, args)
	return result, true, err
}
