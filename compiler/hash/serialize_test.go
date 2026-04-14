package hash

import (
	"encoding/binary"
	"math"
	"testing"
)

func TestSerialize_Deterministic(t *testing.T) {
	node := &hMethodDef{
		Selector: "foo:",
		Arity:    1,
		NumTemps: 2,
		Statements: []hNode{
			&hExprStmt{Expr: &hBinaryMessage{
				Receiver: &hLocalVarRef{ScopeDepth: 0, SlotIndex: 0},
				Selector: "+",
				Argument: &hIntLiteral{Value: 42},
			}},
		},
	}

	data1 := serializeHash(node)
	data2 := serializeHash(node)

	if string(data1) != string(data2) {
		t.Error("serialization is not deterministic")
	}
}

func TestSerialize_VersionPrefix(t *testing.T) {
	node := &hNilLiteral{}
	data := serializeHash(node)

	if len(data) < 1 {
		t.Fatal("empty serialization")
	}
	if data[0] != HashVersion {
		t.Errorf("version prefix: got 0x%02X, want 0x%02X", data[0], HashVersion)
	}
}

func TestSerialize_IntLiteral(t *testing.T) {
	node := &hIntLiteral{Value: 12345}
	data := serializeHash(node)

	// version(1) + tag(1) + int64(8) = 10
	if len(data) != 10 {
		t.Fatalf("length: got %d, want 10", len(data))
	}
	if data[1] != TagIntLiteral {
		t.Errorf("tag: got 0x%02X, want 0x%02X", data[1], TagIntLiteral)
	}
	v := int64(binary.BigEndian.Uint64(data[2:10]))
	if v != 12345 {
		t.Errorf("value: got %d, want 12345", v)
	}
}

func TestSerialize_FloatLiteral(t *testing.T) {
	node := &hFloatLiteral{Value: 3.14}
	data := serializeHash(node)

	// version(1) + tag(1) + float64(8) = 10
	if len(data) != 10 {
		t.Fatalf("length: got %d, want 10", len(data))
	}
	bits := binary.BigEndian.Uint64(data[2:10])
	v := math.Float64frombits(bits)
	if v != 3.14 {
		t.Errorf("value: got %f, want 3.14", v)
	}
}

func TestSerialize_StringLiteral(t *testing.T) {
	node := &hStringLiteral{Value: "hello"}
	data := serializeHash(node)

	// version(1) + tag(1) + len(4) + "hello"(5) = 11
	if len(data) != 11 {
		t.Fatalf("length: got %d, want 11", len(data))
	}
	strLen := binary.BigEndian.Uint32(data[2:6])
	if strLen != 5 {
		t.Errorf("string length: got %d, want 5", strLen)
	}
	if string(data[6:11]) != "hello" {
		t.Errorf("string value: got %q, want %q", string(data[6:11]), "hello")
	}
}

func TestSerialize_BoolLiteral(t *testing.T) {
	nodeTrue := &hBoolLiteral{Value: true}
	nodeFalse := &hBoolLiteral{Value: false}

	dataTrue := serializeHash(nodeTrue)
	dataFalse := serializeHash(nodeFalse)

	// version(1) + tag(1) + bool(1) = 3
	if len(dataTrue) != 3 || len(dataFalse) != 3 {
		t.Fatalf("lengths: true=%d false=%d, want 3", len(dataTrue), len(dataFalse))
	}
	if dataTrue[2] != 1 {
		t.Errorf("true: got %d, want 1", dataTrue[2])
	}
	if dataFalse[2] != 0 {
		t.Errorf("false: got %d, want 0", dataFalse[2])
	}
	if string(dataTrue) == string(dataFalse) {
		t.Error("true and false should serialize differently")
	}
}

func TestSerialize_LocalVarRef(t *testing.T) {
	node := &hLocalVarRef{ScopeDepth: 2, SlotIndex: 3}
	data := serializeHash(node)

	// version(1) + tag(1) + depth(2) + slot(2) = 6
	if len(data) != 6 {
		t.Fatalf("length: got %d, want 6", len(data))
	}
	depth := binary.BigEndian.Uint16(data[2:4])
	slot := binary.BigEndian.Uint16(data[4:6])
	if depth != 2 || slot != 3 {
		t.Errorf("got depth=%d slot=%d, want depth=2 slot=3", depth, slot)
	}
}

func TestSerialize_DifferentNodesDiffer(t *testing.T) {
	nodes := []hNode{
		&hIntLiteral{Value: 1},
		&hFloatLiteral{Value: 1.0},
		&hStringLiteral{Value: "1"},
		&hSymbolLiteral{Value: "foo"},
		&hNilLiteral{},
		&hSelfRef{},
		&hSuperRef{},
		&hThisContext{},
		&hLocalVarRef{ScopeDepth: 0, SlotIndex: 0},
		&hInstanceVarRef{Index: 0},
		&hGlobalRef{FQN: "Foo"},
	}

	seen := make(map[string]int)
	for i, node := range nodes {
		data := string(serializeHash(node))
		if prev, ok := seen[data]; ok {
			t.Errorf("node %d and %d produce identical serializations", prev, i)
		}
		seen[data] = i
	}
}
