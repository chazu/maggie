package vm

import (
	"sync"

	"github.com/fxamacker/cbor/v2"
)

// SerializeHook checks if a Value belongs to a contrib plugin and serializes it.
type SerializeHook func(vm *VM, v Value) ([]byte, bool, error)

// DeserializeHook deserializes a CBOR tag belonging to a contrib plugin.
type DeserializeHook func(vm *VM, tag cbor.Tag) (Value, error)

var (
	serialHooksMu      sync.RWMutex
	serializeHooks     []SerializeHook
	deserializeHookMap = map[uint64]DeserializeHook{}
)

// RegisterSerializeHook adds a hook that is tried during serialization for
// values that don't match any built-in type.
func RegisterSerializeHook(h SerializeHook) {
	serialHooksMu.Lock()
	defer serialHooksMu.Unlock()
	serializeHooks = append(serializeHooks, h)
}

// RegisterDeserializeHook registers a hook for a specific CBOR tag number.
func RegisterDeserializeHook(tag uint64, h DeserializeHook) {
	serialHooksMu.Lock()
	defer serialHooksMu.Unlock()
	deserializeHookMap[tag] = h
}

func trySerializeHooks(vm *VM, v Value) ([]byte, bool, error) {
	serialHooksMu.RLock()
	hooks := make([]SerializeHook, len(serializeHooks))
	copy(hooks, serializeHooks)
	serialHooksMu.RUnlock()

	for _, h := range hooks {
		data, handled, err := h(vm, v)
		if handled {
			return data, true, err
		}
	}
	return nil, false, nil
}

func tryDeserializeHook(vm *VM, tag cbor.Tag) (Value, bool, error) {
	serialHooksMu.RLock()
	h, ok := deserializeHookMap[tag.Number]
	serialHooksMu.RUnlock()
	if !ok {
		return Nil, false, nil
	}
	val, err := h(vm, tag)
	return val, true, err
}
