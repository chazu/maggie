package vm

import (
	"github.com/chazu/goquint"
)

// registerProquintPrimitives registers the Proquint class and its primitives.
// Proquint is a utility class with only class-side methods (no instances).
func (vm *VM) registerProquintPrimitives() {
	c := vm.createClass("Proquint", vm.ObjectClass)
	vm.Globals["Proquint"] = vm.classValue(c)

	// Proquint encode: anInteger — encode a 32-bit integer as proquint string
	c.AddClassMethod1(vm.Selectors, "encode:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if !arg.IsSmallInt() {
			return Nil
		}
		n := arg.SmallInt()
		if n < 0 || n > 0xFFFFFFFF {
			return Nil
		}
		result := goquint.Encode(uint32(n))
		return v.registry.NewStringValue(result)
	})

	// Proquint encode64: anInteger — encode a 64-bit integer as proquint string
	c.AddClassMethod1(vm.Selectors, "encode64:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		if !arg.IsSmallInt() {
			return Nil
		}
		n := arg.SmallInt()
		if n < 0 {
			return Nil
		}
		result := goquint.Encode64(uint64(n))
		return v.registry.NewStringValue(result)
	})

	// Proquint decode: aString — decode a proquint string to a 32-bit integer
	c.AddClassMethod1(vm.Selectors, "decode:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(arg)
		if s == "" {
			return Nil
		}
		n, err := goquint.Decode(s)
		if err != nil {
			return Nil
		}
		return FromSmallInt(int64(n))
	})

	// Proquint decode64: aString — decode a 4-quintuplet proquint to a 64-bit integer
	c.AddClassMethod1(vm.Selectors, "decode64:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(arg)
		if s == "" {
			return Nil
		}
		n, err := goquint.Decode64(s)
		if err != nil {
			return Nil
		}
		return FromSmallInt(int64(n))
	})

	// Proquint random — generate a random 32-bit proquint
	c.AddClassMethod0(vm.Selectors, "random", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.registry.NewStringValue(goquint.Random())
	})

	// Proquint random64 — generate a random 64-bit proquint
	c.AddClassMethod0(vm.Selectors, "random64", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.registry.NewStringValue(goquint.Random64())
	})

	// Proquint encodeHex: aHexString — encode a hex string as proquint
	c.AddClassMethod1(vm.Selectors, "encodeHex:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(arg)
		if s == "" {
			return Nil
		}
		result := goquint.EncodeHex(s)
		return v.registry.NewStringValue(result)
	})

	// Proquint encodeHex64: aHexString — encode 64-bit hex as proquint
	c.AddClassMethod1(vm.Selectors, "encodeHex64:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(arg)
		if s == "" {
			return Nil
		}
		result := goquint.EncodeHex64(s)
		return v.registry.NewStringValue(result)
	})
}
