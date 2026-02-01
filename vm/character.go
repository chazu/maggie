package vm

// ---------------------------------------------------------------------------
// Character: First-class Unicode character values
// ---------------------------------------------------------------------------
//
// Characters are encoded in the symbol tag using marker 37 << 24.
// The lower 24 bits hold the Unicode code point (max 0x10FFFF fits in 21 bits).
// No registry is needed — the code point IS the payload (like SmallInteger).

const characterMarker uint32 = 37 << 24

// FromCharacter creates a Character value from a Unicode code point.
func FromCharacter(codePoint rune) Value {
	return FromSymbolID(uint32(codePoint) | characterMarker)
}

// IsCharacterValue returns true if the value is a Character.
func IsCharacterValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & (0xFF << 24)) == characterMarker
}

// GetCharacterCodePoint extracts the Unicode code point from a Character value.
func GetCharacterCodePoint(v Value) rune {
	return rune(v.SymbolID() & 0x00FFFFFF)
}
