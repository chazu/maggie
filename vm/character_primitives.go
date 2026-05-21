package vm

import (
	"fmt"
	"unicode"
)

// registerCharacterPrimitives registers primitives on CharacterClass.
func (vm *VM) registerCharacterPrimitives() {
	c := vm.CharacterClass

	// --- Instance methods ---

	// value - return code point as SmallInt
	c.AddMethod0(vm.Selectors, "value", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(GetCharacterCodePoint(recv)))
	})

	// asString - single-character string
	c.AddMethod0(vm.Selectors, "asString", func(v *VM, recv Value) Value {
		return v.registry.NewStringValue(string(GetCharacterCodePoint(recv)))
	})

	// printString - $X format
	c.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
		cp := GetCharacterCodePoint(recv)
		return v.registry.NewStringValue(fmt.Sprintf("$%c", cp))
	})

	// isLetter
	c.AddMethod0(vm.Selectors, "isLetter", func(_ *VM, recv Value) Value {
		return FromBool(unicode.IsLetter(GetCharacterCodePoint(recv)))
	})

	// isDigit
	c.AddMethod0(vm.Selectors, "isDigit", func(_ *VM, recv Value) Value {
		return FromBool(unicode.IsDigit(GetCharacterCodePoint(recv)))
	})

	// isWhitespace
	c.AddMethod0(vm.Selectors, "isWhitespace", func(_ *VM, recv Value) Value {
		return FromBool(unicode.IsSpace(GetCharacterCodePoint(recv)))
	})

	// isUppercase
	c.AddMethod0(vm.Selectors, "isUppercase", func(_ *VM, recv Value) Value {
		return FromBool(unicode.IsUpper(GetCharacterCodePoint(recv)))
	})

	// isLowercase
	c.AddMethod0(vm.Selectors, "isLowercase", func(_ *VM, recv Value) Value {
		return FromBool(unicode.IsLower(GetCharacterCodePoint(recv)))
	})

	// asUppercase - return uppercase Character
	c.AddMethod0(vm.Selectors, "asUppercase", func(_ *VM, recv Value) Value {
		return FromCharacter(unicode.ToUpper(GetCharacterCodePoint(recv)))
	})

	// asLowercase - return lowercase Character
	c.AddMethod0(vm.Selectors, "asLowercase", func(_ *VM, recv Value) Value {
		return FromCharacter(unicode.ToLower(GetCharacterCodePoint(recv)))
	})

	// = other - compare code points
	c.AddMethod1(vm.Selectors, "=", func(_ *VM, recv Value, other Value) Value {
		if !IsCharacterValue(other) {
			return False
		}
		return FromBool(GetCharacterCodePoint(recv) == GetCharacterCodePoint(other))
	})

	// < other - ordering
	c.AddMethod1(vm.Selectors, "<", func(_ *VM, recv Value, other Value) Value {
		if !IsCharacterValue(other) {
			return False
		}
		return FromBool(GetCharacterCodePoint(recv) < GetCharacterCodePoint(other))
	})

	// > other - ordering
	c.AddMethod1(vm.Selectors, ">", func(_ *VM, recv Value, other Value) Value {
		if !IsCharacterValue(other) {
			return False
		}
		return FromBool(GetCharacterCodePoint(recv) > GetCharacterCodePoint(other))
	})

	// hash - code point as hash
	c.AddMethod0(vm.Selectors, "hash", func(_ *VM, recv Value) Value {
		return FromSmallInt(int64(GetCharacterCodePoint(recv)))
	})

	// --- Class methods ---

	// value: anInteger - create character from code point
	c.AddClassMethod1(vm.Selectors, "value:", func(_ *VM, recv Value, arg Value) Value {
		if !arg.IsSmallInt() {
			return Nil
		}
		return FromCharacter(rune(arg.SmallInt()))
	})

	// Character constants
	c.AddClassMethod0(vm.Selectors, "space", func(_ *VM, recv Value) Value {
		return FromCharacter(' ')
	})

	c.AddClassMethod0(vm.Selectors, "tab", func(_ *VM, recv Value) Value {
		return FromCharacter('\t')
	})

	c.AddClassMethod0(vm.Selectors, "cr", func(_ *VM, recv Value) Value {
		return FromCharacter('\r')
	})

	c.AddClassMethod0(vm.Selectors, "lf", func(_ *VM, recv Value) Value {
		return FromCharacter('\n')
	})

	c.AddClassMethod0(vm.Selectors, "newline", func(_ *VM, recv Value) Value {
		return FromCharacter('\n')
	})
}
