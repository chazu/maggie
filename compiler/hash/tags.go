package hash

// ---------------------------------------------------------------------------
// Frozen tag bytes for the hashing AST serialization format.
//
// IMPORTANT: These tags are FROZEN. Once assigned, a tag byte must never
// change meaning. Adding new tags is fine; changing existing ones breaks
// all previously computed content hashes.
// ---------------------------------------------------------------------------

// HashVersion is the version prefix for the serialization format.
// Bumping this invalidates all existing content hashes.
const HashVersion byte = 1

// AST node type tags. Each tag uniquely identifies a node kind in the
// serialized byte stream.
const (
	TagReservedZero byte = 0x00 // version prefix / reserved

	// Literal values
	TagIntLiteral    byte = 0x01
	TagFloatLiteral  byte = 0x02
	TagStringLiteral byte = 0x03
	TagSymbolLiteral byte = 0x04
	TagCharLiteral   byte = 0x05
	TagArrayLiteral  byte = 0x06
	TagBoolLiteral   byte = 0x07
	TagNilLiteral    byte = 0x08

	// Pseudo-variables
	TagSelfRef    byte = 0x09
	TagSuperRef   byte = 0x0A

	// Variable references (de Bruijn indexed)
	TagLocalVarRef    byte = 0x0B
	TagInstanceVarRef byte = 0x0C
	TagGlobalRef      byte = 0x0D

	// Reserved 0x0E-0x0F

	// Messages
	TagUnaryMessage   byte = 0x10
	TagBinaryMessage  byte = 0x11
	TagKeywordMessage byte = 0x12
	TagCascade        byte = 0x13

	// Statements / structure
	TagAssignment  byte = 0x14
	TagReturn      byte = 0x15
	TagBlock       byte = 0x16
	TagMethodDef   byte = 0x17
	TagClassDef    byte = 0x18
	TagTraitDef    byte = 0x19
	TagPrimitive   byte = 0x1A
	TagDynamicArray byte = 0x1B
	TagExprStmt    byte = 0x1C
	TagThisContext byte = 0x1D

	// Reserved 0xFE-0xFF

	// Cascade message sub-types (used within cascade serialization)
	TagCascadeUnary   byte = 0x20
	TagCascadeBinary  byte = 0x21
	TagCascadeKeyword byte = 0x22
)

// allTags lists every defined tag for uniqueness verification in tests.
var allTags = []byte{
	TagReservedZero,
	TagIntLiteral, TagFloatLiteral, TagStringLiteral, TagSymbolLiteral,
	TagCharLiteral, TagArrayLiteral, TagBoolLiteral, TagNilLiteral,
	TagSelfRef, TagSuperRef,
	TagLocalVarRef, TagInstanceVarRef, TagGlobalRef,
	TagUnaryMessage, TagBinaryMessage, TagKeywordMessage, TagCascade,
	TagAssignment, TagReturn, TagBlock, TagMethodDef,
	TagClassDef, TagTraitDef, TagPrimitive, TagDynamicArray,
	TagExprStmt, TagThisContext,
	TagCascadeUnary, TagCascadeBinary, TagCascadeKeyword,
}
