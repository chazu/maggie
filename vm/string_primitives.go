package vm

import (
	"fmt"
	"strconv"
	"strings"
)

// ---------------------------------------------------------------------------
// String Storage: Native Go strings wrapped for the VM
// ---------------------------------------------------------------------------

// StringObject represents a Maggie string value.
// Unlike Symbols which are interned and compared by identity,
// Strings are mutable-by-value and compared by content.
type StringObject struct {
	Content string
}

// stringIDOffset is the starting offset for string IDs.
// This separates string IDs from symbol IDs in the symbol tag space.
const stringIDOffset uint32 = 0x80000000

// IsStringValue returns true if the value is a string object (not a symbol).
// String IDs are in the range [stringIDOffset, dictionaryIDOffset).
func IsStringValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	// String IDs: 0x80000000 to 0xBFFFFFFF
	// Dictionary IDs: 0xC0000000+
	return id >= stringIDOffset && id < dictionaryIDOffset
}

// getStringLike returns the string content from a Value that is
// a String, Character, or Symbol. Returns "" for other types.
// Uses the VM's registry to look up string content.
func (vm *VM) getStringLike(v Value) string {
	if IsStringValue(v) {
		return vm.registry.GetStringContent(v)
	}
	if IsCharacterValue(v) {
		return string(GetCharacterCodePoint(v))
	}
	// Symbols are not handled here — use vm.Symbols.Name() for that
	return ""
}

// ---------------------------------------------------------------------------
// String Primitives Implementation
// ---------------------------------------------------------------------------

// registerStringPrimitivesExtended registers all string primitives.
// This is called from VM bootstrap to add string operations.
func (vm *VM) registerStringPrimitivesExtended() {
	c := vm.StringClass

	// primSize - return the length of the string
	c.AddMethod0(vm.Selectors, "primSize", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		return FromSmallInt(int64(len(s)))
	})

	// primAt: - return the Character at the given 0-based index
	c.AddMethod1(vm.Selectors, "primAt:", func(vmPtr interface{}, recv Value, index Value) Value {
		v := vmPtr.(*VM)
		if !index.IsSmallInt() {
			return Nil
		}
		s := v.registry.GetStringContent(recv)
		idx := int(index.SmallInt())
		if idx < 0 || idx >= len(s) {
			return Nil // Index out of bounds
		}
		return FromCharacter(rune(s[idx]))
	})

	// primConcat: - concatenate two strings (also accepts Characters)
	c.AddMethod1(vm.Selectors, "primConcat:", func(vmPtr interface{}, recv Value, other Value) Value {
		v := vmPtr.(*VM)
		s1 := v.registry.GetStringContent(recv)
		s2 := v.getStringLike(other)
		return v.registry.NewStringValue(s1 + s2)
	})

	// primEquals: - compare two strings for equality
	c.AddMethod1(vm.Selectors, "primEquals:", func(vmPtr interface{}, recv Value, other Value) Value {
		v := vmPtr.(*VM)
		s1 := v.registry.GetStringContent(recv)
		s2 := v.registry.GetStringContent(other)
		if s1 == s2 {
			return True
		}
		return False
	})

	// primLessThan: - lexicographic comparison
	c.AddMethod1(vm.Selectors, "primLessThan:", func(vmPtr interface{}, recv Value, other Value) Value {
		v := vmPtr.(*VM)
		s1 := v.registry.GetStringContent(recv)
		s2 := v.registry.GetStringContent(other)
		if s1 < s2 {
			return True
		}
		return False
	})

	// primIncludes: - check if string contains a character or substring
	c.AddMethod1(vm.Selectors, "primIncludes:", func(vmPtr interface{}, recv Value, char Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		ch := v.getStringLike(char)
		if len(ch) == 0 {
			return False
		}
		if strings.Contains(s, ch) {
			return True
		}
		return False
	})

	// primIndexOf: - return the 0-based index of a substring or character, or -1 if not found
	c.AddMethod1(vm.Selectors, "primIndexOf:", func(vmPtr interface{}, recv Value, char Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		ch := v.getStringLike(char)
		if len(ch) == 0 {
			return FromSmallInt(-1)
		}
		idx := strings.Index(s, ch)
		// Return 0-based index, or -1 if not found
		return FromSmallInt(int64(idx))
	})

	// primAsSymbol - convert string to symbol
	c.AddMethod0(vm.Selectors, "primAsSymbol", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		return v.Symbols.SymbolValue(s)
	})

	// primAsUppercase - convert string to uppercase
	c.AddMethod0(vm.Selectors, "primAsUppercase", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		return v.registry.NewStringValue(strings.ToUpper(s))
	})

	// primAsLowercase - convert string to lowercase
	c.AddMethod0(vm.Selectors, "primAsLowercase", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		return v.registry.NewStringValue(strings.ToLower(s))
	})

	// primCopyFrom:to: - return substring (0-based, exclusive end like Go slices)
	c.AddMethod2(vm.Selectors, "primCopyFrom:to:", func(vmPtr interface{}, recv Value, start, end Value) Value {
		v := vmPtr.(*VM)
		if !start.IsSmallInt() || !end.IsSmallInt() {
			return Nil
		}
		s := v.registry.GetStringContent(recv)
		startIdx := int(start.SmallInt())
		endIdx := int(end.SmallInt())

		if startIdx < 0 {
			startIdx = 0
		}
		if endIdx > len(s) {
			endIdx = len(s)
		}
		if startIdx >= endIdx || startIdx >= len(s) {
			return v.registry.NewStringValue("")
		}
		return v.registry.NewStringValue(s[startIdx:endIdx])
	})

	// asInteger - convert string to integer
	c.AddMethod0(vm.Selectors, "asInteger", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		n, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			return Nil
		}
		return FromSmallInt(n)
	})

	// asFloat - convert string to float
	c.AddMethod0(vm.Selectors, "asFloat", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		f, err := strconv.ParseFloat(s, 64)
		if err != nil {
			return Nil
		}
		return FromFloat64(f)
	})

	// primIsDigit - check if single-char string is a digit
	c.AddMethod0(vm.Selectors, "primIsDigit", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		if len(s) != 1 {
			return False
		}
		ch := s[0]
		if ch >= '0' && ch <= '9' {
			return True
		}
		return False
	})

	// primIsLetter - check if single-char string is a letter
	c.AddMethod0(vm.Selectors, "primIsLetter", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		if len(s) != 1 {
			return False
		}
		ch := s[0]
		if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') {
			return True
		}
		return False
	})

	// primIsWhitespace - check if single-char string is whitespace
	c.AddMethod0(vm.Selectors, "primIsWhitespace", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		if len(s) != 1 {
			return False
		}
		ch := s[0]
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			return True
		}
		return False
	})

	// primDo: - iterate over characters, yielding Character values to the block
	c.AddMethod1(vm.Selectors, "primDo:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		var last Value = Nil
		for _, ch := range s {
			charVal := FromCharacter(ch)
			last = v.Send(block, "value:", []Value{charVal})
		}
		return last
	})

	// println - print the string to stdout with newline
	c.AddMethod0(vm.Selectors, "println", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		fmt.Println(s)
		return recv
	})

	// print - print the string to stdout without newline
	c.AddMethod0(vm.Selectors, "print", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		s := v.registry.GetStringContent(recv)
		fmt.Print(s)
		return recv
	})
}
