package vm

// CBOR tags for image format serialization (27100-27115 range).
// These are separate from the serial.go tags (27001-27015) used for
// wire-protocol value serialization.
const (
	imgTagHeader        uint64 = 27100
	imgTagClassDef      uint64 = 27104
	imgTagMethodDef     uint64 = 27105
	imgTagBlockDef      uint64 = 27106
	imgTagObjectDef     uint64 = 27107
	imgTagGlobalEntry   uint64 = 27108
	imgTagClassVarEntry uint64 = 27109
	imgTagValueRef      uint64 = 27111 // object back-reference by index
	imgTagClassRef      uint64 = 27112 // class reference by index
	imgTagMethodRef     uint64 = 27113 // method reference by index
	imgTagSymbolRef     uint64 = 27114 // symbol reference by index
)
