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
	Class     *Class                                        // static class (nil if Resolve is set)
	ClassSide bool                                          // dispatch via ClassVTable (e.g. class values)
	Resolve   func(v Value, vm *VM) (class *Class, ok bool) // dynamic class resolution
}

// SymbolDispatch is a registry keyed by the marker byte (bits 31-24 of the symbol ID).
//
// Lookup is an indexed load into a fixed [256]*SymbolTypeEntry array — one
// load per dispatch — replacing what used to be a map lookup (and before that
// a chain of equality comparisons).
type SymbolDispatch struct {
	// table is indexed by the marker byte (id >> 24). nil entries indicate
	// "not a registered symbol-encoded type — fall through".
	table [256]*SymbolTypeEntry
}

// NewSymbolDispatch creates an empty dispatch table.
func NewSymbolDispatch() *SymbolDispatch {
	return &SymbolDispatch{}
}

// Register adds a type entry for the given marker.
// The marker should be the full marker value (e.g. 1 << 24 for channels).
// The marker byte (bits 31-24) indexes the dispatch table.
func (sd *SymbolDispatch) Register(marker uint32, entry *SymbolTypeEntry) {
	sd.table[byte(marker>>24)] = entry
}

// ClassForSymbol resolves the class for a symbol-encoded value.
// Returns (class, isClassSide). If the value is not a registered
// symbol-encoded type, returns (nil, false) to indicate fallthrough.
//
// Strings (0x80000000–0xBFFFFFFF) and dictionaries (0xC0000000+) use a
// high-bit ID-space discriminator instead of a marker byte and are handled
// as explicit cases. All other symbol-encoded types are dispatched via an
// indexed load on the marker byte (one array load).
func (sd *SymbolDispatch) ClassForSymbol(v Value, symbols *SymbolTable, classes *ClassTable) (*Class, bool) {
	id := v.SymbolID()

	// Indexed marker-byte lookup (one array load).
	entry := sd.table[byte(id>>24)]
	if entry == nil {
		return nil, false
	}

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
// Strings/dictionaries use the wider ID-space discriminator and stay as
// explicit cases; all marker-tagged kinds are an indexed load.
func (sd *SymbolDispatch) ClassForSymbolVM(v Value, vm *VM) (*Class, bool) {
	id := v.SymbolID()

	// Indexed marker-byte lookup (one array load).
	entry := sd.table[byte(id>>24)]
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

// ClassForMarkerVM resolves the class registered for a raw marker. It is used by
// kindExtension heap Values, which carry their marker in an extensionObject
// rather than piggy-backing it on a symbol id. All extension/IO markers register
// static Class entries (no Resolve), so Resolve-based entries are unsupported
// here and report "not found".
func (sd *SymbolDispatch) ClassForMarkerVM(marker uint32, vm *VM) (*Class, bool) {
	entry := sd.table[byte(marker>>24)]
	if entry == nil || entry.Resolve != nil {
		return nil, false
	}
	return entry.Class, entry.ClassSide
}
