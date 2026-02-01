package vm

// ---------------------------------------------------------------------------
// SymbolDispatch: Central registry for symbol-encoded value types
// ---------------------------------------------------------------------------
//
// Maggie uses NaN-boxed symbol tags with marker bytes to encode many value
// types (channels, processes, mutexes, etc.). Three separate if/else chains
// (vtableFor, Send, ClassFor) previously duplicated the logic for mapping
// these markers to classes. SymbolDispatch centralises this into one lookup.

// SymbolTypeEntry describes how to dispatch a symbol-encoded value type.
type SymbolTypeEntry struct {
	Class     *Class                                         // static class (nil if Resolve is set)
	ClassSide bool                                           // dispatch via ClassVTable (e.g. class values)
	Resolve   func(v Value, vm *VM) (class *Class, ok bool)  // dynamic class resolution
}

// SymbolDispatch is a registry keyed by the marker byte (bits 31-24 of the symbol ID).
type SymbolDispatch struct {
	markers map[uint32]*SymbolTypeEntry // keyed by full marker value (marker << 24)
}

// NewSymbolDispatch creates an empty dispatch table.
func NewSymbolDispatch() *SymbolDispatch {
	return &SymbolDispatch{
		markers: make(map[uint32]*SymbolTypeEntry),
	}
}

// Register adds a type entry for the given marker.
// The marker should be the full marker value (e.g. 1 << 24 for channels).
func (sd *SymbolDispatch) Register(marker uint32, entry *SymbolTypeEntry) {
	sd.markers[marker] = entry
}

// ClassForSymbol resolves the class for a symbol-encoded value.
// Returns (class, isClassSide). If the value is not a registered
// symbol-encoded type, returns (nil, false) to indicate fallthrough.
func (sd *SymbolDispatch) ClassForSymbol(v Value, symbols *SymbolTable, classes *ClassTable) (*Class, bool) {
	id := v.SymbolID()

	// String range: 0x80000000 to 0xBFFFFFFF
	if id >= stringIDOffset && id < dictionaryIDOffset {
		if c := classes.Lookup("String"); c != nil {
			return c, false
		}
		return nil, false
	}

	// Dictionary range: 0xC0000000+
	if id >= dictionaryIDOffset {
		if c := classes.Lookup("Dictionary"); c != nil {
			return c, false
		}
		return nil, false
	}

	// Extract marker byte and look up in registry
	marker := id & (0xFF << 24)
	if marker == 0 {
		// No marker bits — not a registered type, fall through
		return nil, false
	}

	entry := sd.markers[marker]
	if entry == nil {
		// Unknown marker — fall through
		return nil, false
	}

	// Dynamic resolution
	if entry.Resolve != nil {
		cls, ok := entry.Resolve(v, nil)
		if ok {
			return cls, entry.ClassSide
		}
		return nil, false
	}

	return entry.Class, entry.ClassSide
}

// ClassForSymbolVM is the same as ClassForSymbol but passes the VM to resolvers.
func (sd *SymbolDispatch) ClassForSymbolVM(v Value, vm *VM) (*Class, bool) {
	id := v.SymbolID()

	// String range: 0x80000000 to 0xBFFFFFFF
	if id >= stringIDOffset && id < dictionaryIDOffset {
		return vm.StringClass, false
	}

	// Dictionary range: 0xC0000000+
	if id >= dictionaryIDOffset {
		return vm.DictionaryClass, false
	}

	// Extract marker byte and look up in registry
	marker := id & (0xFF << 24)
	if marker == 0 {
		return nil, false
	}

	entry := sd.markers[marker]
	if entry == nil {
		return nil, false
	}

	if entry.Resolve != nil {
		cls, ok := entry.Resolve(v, vm)
		if ok {
			return cls, entry.ClassSide
		}
		return nil, false
	}

	return entry.Class, entry.ClassSide
}
