package vm

import "github.com/fxamacker/cbor/v2"

// cborImageEnvelope is the top-level tagged structure.
// Wrapped in cbor.Tag{Number: imgTagHeader, Content: cborImageEnvelope{...}}
type cborImageEnvelope struct {
	Version    uint32              `cbor:"1,keyasint"`
	Flags      uint32              `cbor:"2,keyasint"`
	Stats      cborImageStats      `cbor:"3,keyasint"`
	Strings    []string            `cbor:"4,keyasint"`
	Symbols    []string            `cbor:"5,keyasint"`              // symbol names
	Selectors  []string            `cbor:"6,keyasint"`              // selector names
	Classes    []cborClassDef      `cbor:"7,keyasint"`
	Methods    []cborMethodDef     `cbor:"8,keyasint"`
	Objects    []cborObjectDef     `cbor:"9,keyasint"`
	Globals    []cborGlobalEntry   `cbor:"10,keyasint"`
	ClassVars  []cborClassVarEntry `cbor:"11,keyasint,omitempty"`
	EntryPoint uint32              `cbor:"12,keyasint,omitempty"` // method index
}

type cborImageStats struct {
	Classes uint32 `cbor:"1,keyasint"`
	Methods uint32 `cbor:"2,keyasint"`
	Objects uint32 `cbor:"3,keyasint"`
	Globals uint32 `cbor:"4,keyasint"`
}

type cborClassDef struct {
	Name            uint32   `cbor:"1,keyasint"`           // string table index
	Namespace       int64    `cbor:"2,keyasint"`           // string index or -1
	Super           int64    `cbor:"3,keyasint"`           // class index or -1
	NumSlots        uint32   `cbor:"4,keyasint"`
	InstVars        []uint32 `cbor:"5,keyasint,omitempty"` // string indices
	InstanceMethods []uint32 `cbor:"6,keyasint,omitempty"` // method indices
	ClassMethods    []uint32 `cbor:"7,keyasint,omitempty"` // method indices
	DocString       uint32   `cbor:"8,keyasint,omitempty"` // string index
	HasDocString    bool     `cbor:"9,keyasint,omitempty"` // disambiguates 0 vs absent
}

type cborMethodDef struct {
	Selector      int32              `cbor:"1,keyasint"`
	Class         int64              `cbor:"2,keyasint"`            // class index or -1
	Name          uint32             `cbor:"3,keyasint"`            // string index
	IsClassMethod bool               `cbor:"4,keyasint,omitempty"`
	Arity         uint32             `cbor:"5,keyasint"`
	NumTemps      uint32             `cbor:"6,keyasint"`
	Literals      []cbor.RawMessage  `cbor:"7,keyasint"`           // each is an encoded image value
	Bytecode      []byte             `cbor:"8,keyasint"`
	Blocks        []cborBlockDef     `cbor:"9,keyasint,omitempty"`
	Source        uint32             `cbor:"10,keyasint,omitempty"` // string index
	HasSource     bool               `cbor:"11,keyasint,omitempty"`
	DocString     uint32             `cbor:"12,keyasint,omitempty"` // string index
	HasDocString  bool               `cbor:"13,keyasint,omitempty"`
	SourceMap     [][3]uint32        `cbor:"14,keyasint,omitempty"` // [offset, line, column]
	ContentHash   []byte             `cbor:"15,keyasint,omitempty"` // 32 bytes or empty
	TypedHash     []byte             `cbor:"16,keyasint,omitempty"` // 32 bytes or empty
}

type cborBlockDef struct {
	Arity       uint32             `cbor:"1,keyasint"`
	NumTemps    uint32             `cbor:"2,keyasint"`
	NumCaptures uint32             `cbor:"3,keyasint"`
	Literals    []cbor.RawMessage  `cbor:"4,keyasint,omitempty"`
	Bytecode    []byte             `cbor:"5,keyasint"`
	SourceMap   [][3]uint32        `cbor:"6,keyasint,omitempty"`
	Source      uint32             `cbor:"7,keyasint,omitempty"` // string index
	HasSource   bool               `cbor:"8,keyasint,omitempty"`
}

type cborObjectDef struct {
	Class int64             `cbor:"1,keyasint"`           // class index or -1
	Slots []cbor.RawMessage `cbor:"2,keyasint,omitempty"` // encoded values
}

type cborGlobalEntry struct {
	Name  uint32          `cbor:"1,keyasint"` // string index
	Value cbor.RawMessage `cbor:"2,keyasint"` // encoded value
}

type cborClassVarEntry struct {
	Class int64              `cbor:"1,keyasint"` // class index
	Vars  []cborClassVarPair `cbor:"2,keyasint"`
}

type cborClassVarPair struct {
	Name  uint32          `cbor:"1,keyasint"` // string index
	Value cbor.RawMessage `cbor:"2,keyasint"` // encoded value
}
