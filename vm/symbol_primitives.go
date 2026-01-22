package vm

// ---------------------------------------------------------------------------
// Symbol Primitives
// ---------------------------------------------------------------------------

func (vm *VM) registerSymbolPrimitives() {
	c := vm.SymbolClass

	// asString - convert symbol to string (returns actual string)
	c.AddMethod0(vm.Selectors, "asString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			return NewStringValue(name)
		}
		return Nil
	})

	// primAsString - same as asString, for compatibility with Symbol.mag
	c.AddMethod0(vm.Selectors, "primAsString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			return NewStringValue(name)
		}
		return Nil
	})

	// = - symbol equality (identity since symbols are interned)
	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// hash - symbol hash (use symbol ID)
	c.AddMethod0(vm.Selectors, "hash", func(_ interface{}, recv Value) Value {
		if recv.IsSymbol() {
			return FromSmallInt(int64(recv.SymbolID()))
		}
		return FromSmallInt(0)
	})

	// size - length of symbol name
	c.AddMethod0(vm.Selectors, "size", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			return FromSmallInt(int64(len(name)))
		}
		return FromSmallInt(0)
	})

	// asSymbol - return self
	c.AddMethod0(vm.Selectors, "asSymbol", func(_ interface{}, recv Value) Value {
		return recv
	})
}
