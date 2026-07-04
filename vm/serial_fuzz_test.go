package vm

import "testing"

// DeserializeValue and DeserializeSpawnBlock decode CBOR from remote peers
// (message payloads, spawned blocks). Malformed or hostile input — including
// deeply nested or circular-looking structures — must return an error, never
// panic or hang.

func FuzzDeserializeValue(f *testing.F) {
	vm := NewVM()
	// Seed with a few round-tripped values.
	for _, v := range []Value{FromSmallInt(42), True, Nil, vm.registry.NewStringValue("hi")} {
		if b, err := vm.SerializeValue(v); err == nil {
			f.Add(b)
		}
	}
	f.Add([]byte{})
	f.Add([]byte("not cbor"))

	f.Fuzz(func(t *testing.T, data []byte) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("DeserializeValue panicked on %d bytes: %v", len(data), r)
			}
		}()
		_, _ = vm.DeserializeValue(data)
	})
}

func FuzzDeserializeSpawnBlock(f *testing.F) {
	f.Add([]byte{})
	f.Add([]byte("not cbor"))
	f.Fuzz(func(t *testing.T, data []byte) {
		defer func() {
			if r := recover(); r != nil {
				t.Fatalf("DeserializeSpawnBlock panicked on %d bytes: %v", len(data), r)
			}
		}()
		_, _ = DeserializeSpawnBlock(data)
	})
}
