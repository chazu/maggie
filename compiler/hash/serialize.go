package hash

import (
	"encoding/binary"
	"math"
)

// ---------------------------------------------------------------------------
// Deterministic binary serialization of the frozen hashing AST.
//
// Encoding conventions:
//   - First byte: HashVersion (0x01)
//   - Integers: big-endian fixed-width (int64=8B, uint16=2B)
//   - Floats: IEEE 754 big-endian 8B
//   - Strings: uint32 big-endian length + UTF-8 bytes
//   - Booleans: single byte (0/1)
//   - Child nodes: serialized inline (flat)
// ---------------------------------------------------------------------------

// Serialize produces a deterministic byte serialization of an HNode tree.
// The returned bytes are suitable for hashing with SHA-256.
func Serialize(node HNode) []byte {
	s := &serializer{buf: make([]byte, 0, 256)}
	s.writeByte(HashVersion)
	s.serializeNode(node)
	return s.buf
}

type serializer struct {
	buf []byte
}

func (s *serializer) writeByte(b byte) {
	s.buf = append(s.buf, b)
}

func (s *serializer) writeUint16(v uint16) {
	var b [2]byte
	binary.BigEndian.PutUint16(b[:], v)
	s.buf = append(s.buf, b[:]...)
}

func (s *serializer) writeUint32(v uint32) {
	var b [4]byte
	binary.BigEndian.PutUint32(b[:], v)
	s.buf = append(s.buf, b[:]...)
}

func (s *serializer) writeInt64(v int64) {
	var b [8]byte
	binary.BigEndian.PutUint64(b[:], uint64(v))
	s.buf = append(s.buf, b[:]...)
}

func (s *serializer) writeFloat64(v float64) {
	var b [8]byte
	binary.BigEndian.PutUint64(b[:], math.Float64bits(v))
	s.buf = append(s.buf, b[:]...)
}

func (s *serializer) writeString(v string) {
	s.writeUint32(uint32(len(v)))
	s.buf = append(s.buf, v...)
}

func (s *serializer) writeInt(v int) {
	s.writeInt64(int64(v))
}

func (s *serializer) serializeNode(node HNode) {
	switch n := node.(type) {
	case *HIntLiteral:
		s.writeByte(TagIntLiteral)
		s.writeInt64(n.Value)

	case *HFloatLiteral:
		s.writeByte(TagFloatLiteral)
		s.writeFloat64(n.Value)

	case *HStringLiteral:
		s.writeByte(TagStringLiteral)
		s.writeString(n.Value)

	case *HSymbolLiteral:
		s.writeByte(TagSymbolLiteral)
		s.writeString(n.Value)

	case *HCharLiteral:
		s.writeByte(TagCharLiteral)
		s.writeUint32(uint32(n.Value))

	case *HBoolLiteral:
		s.writeByte(TagBoolLiteral)
		if n.Value {
			s.writeByte(1)
		} else {
			s.writeByte(0)
		}

	case *HNilLiteral:
		s.writeByte(TagNilLiteral)

	case *HSelfRef:
		s.writeByte(TagSelfRef)

	case *HSuperRef:
		s.writeByte(TagSuperRef)

	case *HThisContext:
		s.writeByte(TagThisContext)

	case *HArrayLiteral:
		s.writeByte(TagArrayLiteral)
		s.writeUint32(uint32(len(n.Elements)))
		for _, el := range n.Elements {
			s.serializeNode(el)
		}

	case *HDynamicArray:
		s.writeByte(TagDynamicArray)
		s.writeUint32(uint32(len(n.Elements)))
		for _, el := range n.Elements {
			s.serializeNode(el)
		}

	case *HLocalVarRef:
		s.writeByte(TagLocalVarRef)
		s.writeUint16(n.ScopeDepth)
		s.writeUint16(n.SlotIndex)

	case *HInstanceVarRef:
		s.writeByte(TagInstanceVarRef)
		s.writeUint16(n.Index)

	case *HGlobalRef:
		s.writeByte(TagGlobalRef)
		s.writeString(n.FQN)

	case *HUnaryMessage:
		s.writeByte(TagUnaryMessage)
		s.writeString(n.Selector)
		s.serializeNode(n.Receiver)

	case *HBinaryMessage:
		s.writeByte(TagBinaryMessage)
		s.writeString(n.Selector)
		s.serializeNode(n.Receiver)
		s.serializeNode(n.Argument)

	case *HKeywordMessage:
		s.writeByte(TagKeywordMessage)
		s.writeString(n.Selector)
		s.writeUint32(uint32(len(n.Arguments)))
		s.serializeNode(n.Receiver)
		for _, arg := range n.Arguments {
			s.serializeNode(arg)
		}

	case *HCascade:
		s.writeByte(TagCascade)
		s.serializeNode(n.Receiver)
		s.writeUint32(uint32(len(n.Messages)))
		for _, msg := range n.Messages {
			s.writeByte(msg.Type)
			s.writeString(msg.Selector)
			s.writeUint32(uint32(len(msg.Arguments)))
			for _, arg := range msg.Arguments {
				s.serializeNode(arg)
			}
		}

	case *HAssignment:
		s.writeByte(TagAssignment)
		s.serializeNode(n.Target)
		s.serializeNode(n.Value)

	case *HReturn:
		s.writeByte(TagReturn)
		s.serializeNode(n.Value)

	case *HExprStmt:
		s.writeByte(TagExprStmt)
		s.serializeNode(n.Expr)

	case *HBlock:
		s.writeByte(TagBlock)
		s.writeInt(n.Arity)
		s.writeInt(n.NumTemps)
		s.writeUint32(uint32(len(n.Statements)))
		for _, stmt := range n.Statements {
			s.serializeNode(stmt)
		}

	case *HPrimitive:
		s.writeByte(TagPrimitive)
		s.writeInt(n.Number)

	case *HMethodDef:
		s.writeByte(TagMethodDef)
		s.writeString(n.Selector)
		s.writeInt(n.Arity)
		s.writeInt(n.NumTemps)
		s.writeInt(n.Primitive)
		s.writeString(n.DocString)
		if n.Primitive > 0 {
			// Primitive methods have no body to serialize
			s.writeUint32(0)
		} else {
			s.writeUint32(uint32(len(n.Statements)))
			for _, stmt := range n.Statements {
				s.serializeNode(stmt)
			}
		}
	}
}
