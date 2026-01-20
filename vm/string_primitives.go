package vm

import (
	"strconv"
	"strings"
	"sync"
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

// stringRegistry stores active strings.
// This is a temporary solution until proper heap allocation is implemented.
// String IDs are encoded using the symbol tag with a special prefix range.
var stringRegistry = struct {
	sync.RWMutex
	strings map[uint32]*StringObject
	nextID  uint32
}{
	strings: make(map[uint32]*StringObject),
	nextID:  0x80000000, // Use high IDs to distinguish from regular symbols
}

// stringIDOffset is the starting offset for string IDs.
// This separates string IDs from symbol IDs in the symbol tag space.
const stringIDOffset uint32 = 0x80000000

// NewStringValue creates a Value from a Go string.
func NewStringValue(s string) Value {
	stringRegistry.Lock()
	defer stringRegistry.Unlock()

	id := stringRegistry.nextID
	stringRegistry.nextID++
	stringRegistry.strings[id] = &StringObject{Content: s}
	return FromSymbolID(id)
}

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

// GetStringContent returns the Go string content of a string Value.
// Returns empty string if v is not a string.
func GetStringContent(v Value) string {
	if !v.IsSymbol() {
		return ""
	}
	id := v.SymbolID()
	if id < stringIDOffset {
		return "" // It's a regular symbol, not a string
	}

	stringRegistry.RLock()
	defer stringRegistry.RUnlock()

	if obj, ok := stringRegistry.strings[id]; ok {
		return obj.Content
	}
	return ""
}

// GetStringObject returns the StringObject for a Value.
// Returns nil if v is not a string.
func GetStringObject(v Value) *StringObject {
	if !v.IsSymbol() {
		return nil
	}
	id := v.SymbolID()
	if id < stringIDOffset {
		return nil
	}

	stringRegistry.RLock()
	defer stringRegistry.RUnlock()

	return stringRegistry.strings[id]
}

// ---------------------------------------------------------------------------
// String Primitives Implementation
// ---------------------------------------------------------------------------

// registerStringPrimitivesExtended registers all string primitives.
// This is called from VM bootstrap to add string operations.
func (vm *VM) registerStringPrimitivesExtended() {
	c := vm.StringClass

	// primSize - return the length of the string
	c.AddMethod0(vm.Selectors, "primSize", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
		return FromSmallInt(int64(len(s)))
	})

	// primAt: - return the character at the given 0-based index
	c.AddMethod1(vm.Selectors, "primAt:", func(_ interface{}, recv Value, index Value) Value {
		if !index.IsSmallInt() {
			return Nil
		}
		s := GetStringContent(recv)
		idx := int(index.SmallInt())
		if idx < 0 || idx >= len(s) {
			return Nil // Index out of bounds
		}
		// Return a single-character string
		return NewStringValue(string(s[idx]))
	})

	// primConcat: - concatenate two strings
	c.AddMethod1(vm.Selectors, "primConcat:", func(_ interface{}, recv Value, other Value) Value {
		s1 := GetStringContent(recv)
		s2 := GetStringContent(other)
		return NewStringValue(s1 + s2)
	})

	// primEquals: - compare two strings for equality
	c.AddMethod1(vm.Selectors, "primEquals:", func(_ interface{}, recv Value, other Value) Value {
		s1 := GetStringContent(recv)
		s2 := GetStringContent(other)
		if s1 == s2 {
			return True
		}
		return False
	})

	// primLessThan: - lexicographic comparison
	c.AddMethod1(vm.Selectors, "primLessThan:", func(_ interface{}, recv Value, other Value) Value {
		s1 := GetStringContent(recv)
		s2 := GetStringContent(other)
		if s1 < s2 {
			return True
		}
		return False
	})

	// primIncludes: - check if string contains a character
	c.AddMethod1(vm.Selectors, "primIncludes:", func(_ interface{}, recv Value, char Value) Value {
		s := GetStringContent(recv)
		ch := GetStringContent(char)
		if len(ch) == 0 {
			return False
		}
		if strings.Contains(s, ch) {
			return True
		}
		return False
	})

	// primIndexOf: - return the 0-based index of a substring, or -1 if not found
	c.AddMethod1(vm.Selectors, "primIndexOf:", func(_ interface{}, recv Value, char Value) Value {
		s := GetStringContent(recv)
		ch := GetStringContent(char)
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
		s := GetStringContent(recv)
		return v.Symbols.SymbolValue(s)
	})

	// primAsUppercase - convert string to uppercase
	c.AddMethod0(vm.Selectors, "primAsUppercase", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
		return NewStringValue(strings.ToUpper(s))
	})

	// primAsLowercase - convert string to lowercase
	c.AddMethod0(vm.Selectors, "primAsLowercase", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
		return NewStringValue(strings.ToLower(s))
	})

	// primCopyFrom:to: - return substring (0-based, exclusive end like Go slices)
	c.AddMethod2(vm.Selectors, "primCopyFrom:to:", func(_ interface{}, recv Value, start, end Value) Value {
		if !start.IsSmallInt() || !end.IsSmallInt() {
			return Nil
		}
		s := GetStringContent(recv)
		startIdx := int(start.SmallInt())
		endIdx := int(end.SmallInt())

		if startIdx < 0 {
			startIdx = 0
		}
		if endIdx > len(s) {
			endIdx = len(s)
		}
		if startIdx >= endIdx || startIdx >= len(s) {
			return NewStringValue("")
		}
		return NewStringValue(s[startIdx:endIdx])
	})

	// asInteger - convert string to integer
	c.AddMethod0(vm.Selectors, "asInteger", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
		n, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			return Nil
		}
		return FromSmallInt(n)
	})

	// asFloat - convert string to float
	c.AddMethod0(vm.Selectors, "asFloat", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
		f, err := strconv.ParseFloat(s, 64)
		if err != nil {
			return Nil
		}
		return FromFloat64(f)
	})

	// primIsDigit - check if single-char string is a digit
	c.AddMethod0(vm.Selectors, "primIsDigit", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
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
	c.AddMethod0(vm.Selectors, "primIsLetter", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
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
	c.AddMethod0(vm.Selectors, "primIsWhitespace", func(_ interface{}, recv Value) Value {
		s := GetStringContent(recv)
		if len(s) != 1 {
			return False
		}
		ch := s[0]
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
			return True
		}
		return False
	})
}
