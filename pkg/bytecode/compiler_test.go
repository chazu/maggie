package bytecode

import (
	"strings"
	"testing"

	"github.com/chazu/procyon/pkg/parser"
)

func TestCompileEmptyBlock(t *testing.T) {
	block := &parser.BlockExpr{
		Params:     []string{},
		Statements: []parser.Statement{},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.Code) == 0 {
		t.Error("Expected at least one instruction (return)")
	}

	// Should end with RETURN_NIL
	lastOp := Opcode(chunk.Code[len(chunk.Code)-1])
	if lastOp != OpReturnNil {
		t.Errorf("Expected RETURN_NIL, got %s", lastOp)
	}
}

func TestCompileNumberLiteral(t *testing.T) {
	tests := []struct {
		value    string
		expected Opcode
	}{
		{"0", OpConstZero},
		{"1", OpConstOne},
		{"42", OpConst},
	}

	for _, tt := range tests {
		block := &parser.BlockExpr{
			Statements: []parser.Statement{
				&parser.Return{Value: &parser.NumberLit{Value: tt.value}},
			},
		}

		ctx := &CompilerContext{}
		chunk, err := CompileBlock(block, ctx)
		if err != nil {
			t.Fatalf("CompileBlock failed for %s: %v", tt.value, err)
		}

		if len(chunk.Code) == 0 {
			t.Errorf("Expected code for %s", tt.value)
			continue
		}

		firstOp := Opcode(chunk.Code[0])
		if firstOp != tt.expected {
			t.Errorf("For %s: expected %s, got %s", tt.value, tt.expected, firstOp)
		}
	}
}

func TestCompileStringLiteral(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.StringLit{Value: "hello"}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.Constants) != 1 || chunk.Constants[0] != "hello" {
		t.Errorf("Expected constant 'hello', got %v", chunk.Constants)
	}

	if Opcode(chunk.Code[0]) != OpConst {
		t.Errorf("Expected CONST, got %s", Opcode(chunk.Code[0]))
	}
}

func TestCompileEmptyString(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.StringLit{Value: ""}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if Opcode(chunk.Code[0]) != OpConstEmpty {
		t.Errorf("Expected CONST_EMPTY, got %s", Opcode(chunk.Code[0]))
	}
}

func TestCompileBlockParams(t *testing.T) {
	block := &parser.BlockExpr{
		Params: []string{"x", "y"},
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.Identifier{Name: "x"}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if chunk.ParamCount != 2 {
		t.Errorf("Expected 2 params, got %d", chunk.ParamCount)
	}

	if len(chunk.ParamNames) != 2 || chunk.ParamNames[0] != "x" || chunk.ParamNames[1] != "y" {
		t.Errorf("Expected param names [x, y], got %v", chunk.ParamNames)
	}

	// Check that it loads param 0
	if Opcode(chunk.Code[0]) != OpLoadParam {
		t.Errorf("Expected LOAD_PARAM, got %s", Opcode(chunk.Code[0]))
	}
	if chunk.Code[1] != 0 {
		t.Errorf("Expected param slot 0, got %d", chunk.Code[1])
	}
}

func TestCompileBinaryExpr(t *testing.T) {
	tests := []struct {
		op       string
		expected Opcode
	}{
		{"+", OpAdd},
		{"-", OpSub},
		{"*", OpMul},
		{"/", OpDiv},
		{"%", OpMod},
		{",", OpConcat},
	}

	for _, tt := range tests {
		block := &parser.BlockExpr{
			Statements: []parser.Statement{
				&parser.Return{Value: &parser.BinaryExpr{
					Left:  &parser.NumberLit{Value: "1"},
					Op:    tt.op,
					Right: &parser.NumberLit{Value: "2"},
				}},
			},
		}

		ctx := &CompilerContext{}
		chunk, err := CompileBlock(block, ctx)
		if err != nil {
			t.Fatalf("CompileBlock failed for %s: %v", tt.op, err)
		}

		// Should be: CONST_ONE, CONST (2), <op>, RETURN
		found := false
		for i := 0; i < len(chunk.Code); i++ {
			if Opcode(chunk.Code[i]) == tt.expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected %s in bytecode for operator %s", tt.expected, tt.op)
		}
	}
}

func TestCompileComparisonExpr(t *testing.T) {
	tests := []struct {
		op       string
		expected Opcode
	}{
		{"==", OpEq},
		{"!=", OpNe},
		{"<", OpLt},
		{"<=", OpLe},
		{">", OpGt},
		{">=", OpGe},
	}

	for _, tt := range tests {
		block := &parser.BlockExpr{
			Statements: []parser.Statement{
				&parser.Return{Value: &parser.ComparisonExpr{
					Left:  &parser.NumberLit{Value: "1"},
					Op:    tt.op,
					Right: &parser.NumberLit{Value: "2"},
				}},
			},
		}

		ctx := &CompilerContext{}
		chunk, err := CompileBlock(block, ctx)
		if err != nil {
			t.Fatalf("CompileBlock failed for %s: %v", tt.op, err)
		}

		found := false
		for i := 0; i < len(chunk.Code); i++ {
			if Opcode(chunk.Code[i]) == tt.expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected %s in bytecode for operator %s", tt.expected, tt.op)
		}
	}
}

func TestCompileAssignment(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Assignment{
				Target: "x",
				Value:  &parser.NumberLit{Value: "42"},
			},
			&parser.Return{Value: &parser.Identifier{Name: "x"}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if chunk.LocalCount != 1 {
		t.Errorf("Expected 1 local, got %d", chunk.LocalCount)
	}

	// Check for STORE_LOCAL and LOAD_LOCAL
	hasStore := false
	hasLoad := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpStoreLocal {
			hasStore = true
		}
		if Opcode(chunk.Code[i]) == OpLoadLocal {
			hasLoad = true
		}
	}

	if !hasStore {
		t.Error("Expected STORE_LOCAL")
	}
	if !hasLoad {
		t.Error("Expected LOAD_LOCAL")
	}
}

func TestCompileLocalVarDecl(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.LocalVarDecl{Names: []string{"x", "y"}},
			&parser.Assignment{Target: "x", Value: &parser.NumberLit{Value: "1"}},
			&parser.Assignment{Target: "y", Value: &parser.NumberLit{Value: "2"}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if chunk.LocalCount != 2 {
		t.Errorf("Expected 2 locals, got %d", chunk.LocalCount)
	}
}

func TestCompileIfTrue(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.IfExpr{
				Condition: &parser.Identifier{Name: "true"},
				TrueBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "1"}},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should contain JUMP_FALSE
	hasJump := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpJumpFalse {
			hasJump = true
			break
		}
	}
	if !hasJump {
		t.Error("Expected JUMP_FALSE for ifTrue:")
	}
}

func TestCompileIfTrueIfFalse(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.IfExpr{
				Condition: &parser.Identifier{Name: "true"},
				TrueBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "1"}},
				},
				FalseBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "0"}},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should contain JUMP_FALSE and JUMP
	hasJumpFalse := false
	hasJump := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpJumpFalse {
			hasJumpFalse = true
		}
		if op == OpJump {
			hasJump = true
		}
	}
	if !hasJumpFalse {
		t.Error("Expected JUMP_FALSE")
	}
	if !hasJump {
		t.Error("Expected JUMP for skipping false block")
	}
}

func TestCompileWhileTrue(t *testing.T) {
	block := &parser.BlockExpr{
		Params: []string{"n"},
		Statements: []parser.Statement{
			&parser.WhileExpr{
				Condition: &parser.ComparisonExpr{
					Left:  &parser.Identifier{Name: "n"},
					Op:    ">",
					Right: &parser.NumberLit{Value: "0"},
				},
				Body: []parser.Statement{
					&parser.Assignment{
						Target: "n",
						Value: &parser.BinaryExpr{
							Left:  &parser.Identifier{Name: "n"},
							Op:    "-",
							Right: &parser.NumberLit{Value: "1"},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have backward jump
	jumpCount := 0
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpJump || op == OpJumpFalse {
			jumpCount++
		}
	}
	if jumpCount < 2 {
		t.Error("Expected at least 2 jumps for while loop")
	}
}

func TestCompileMessageSend(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.MessageSend{
				Receiver: &parser.Identifier{Name: "obj"},
				Selector: "getValue",
				Args:     nil,
				IsSelf:   false,
			}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have SEND opcode
	hasSend := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpSend {
			hasSend = true
			break
		}
	}
	if !hasSend {
		t.Error("Expected SEND opcode")
	}

	// Selector should be in constants
	found := false
	for _, c := range chunk.Constants {
		if c == "getValue" {
			found = true
			break
		}
	}
	if !found {
		t.Error("Expected 'getValue' in constants")
	}
}

func TestCompileSelfSend(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.MessageSend{
				Receiver: &parser.Identifier{Name: "self"},
				Selector: "getValue",
				Args:     nil,
				IsSelf:   true,
			}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have SEND_SELF opcode
	hasSendSelf := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpSendSelf {
			hasSendSelf = true
			break
		}
	}
	if !hasSendSelf {
		t.Error("Expected SEND_SELF opcode")
	}
}

func TestCompileMessageWithArgs(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.MessageSend{
				Receiver: &parser.Identifier{Name: "obj"},
				Selector: "at_put_",
				Args: []parser.Expr{
					&parser.NumberLit{Value: "1"},
					&parser.StringLit{Value: "hello"},
				},
				IsSelf: false,
			}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Check bytecode contains args before SEND
	disasm := chunk.Disassemble()
	if !strings.Contains(disasm, "argc=2") {
		t.Errorf("Expected argc=2 in disassembly, got:\n%s", disasm)
	}
}

func TestCompileCaptureLocalVar(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.Identifier{Name: "outerVar"}},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"outerVar"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.CaptureInfo) != 1 {
		t.Errorf("Expected 1 capture, got %d", len(chunk.CaptureInfo))
	}

	if len(chunk.CaptureInfo) > 0 && chunk.CaptureInfo[0].Name != "outerVar" {
		t.Errorf("Expected capture of 'outerVar', got '%s'", chunk.CaptureInfo[0].Name)
	}

	// Should use LOAD_CAPTURE
	hasLoadCapture := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpLoadCapture {
			hasLoadCapture = true
			break
		}
	}
	if !hasLoadCapture {
		t.Error("Expected LOAD_CAPTURE opcode")
	}
}

func TestCompileCaptureIVar(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.Identifier{Name: "counter"}},
		},
	}

	ctx := &CompilerContext{
		InstanceVars: []string{"counter"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.CaptureInfo) != 1 {
		t.Errorf("Expected 1 capture for ivar, got %d", len(chunk.CaptureInfo))
	}

	if len(chunk.CaptureInfo) > 0 {
		if chunk.CaptureInfo[0].Source != VarSourceIVar {
			t.Errorf("Expected VarSourceIVar, got %d", chunk.CaptureInfo[0].Source)
		}
	}
}

func TestCompileStoreCapture(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Assignment{
				Target: "outerVar",
				Value:  &parser.NumberLit{Value: "42"},
			},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"outerVar"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should use STORE_CAPTURE
	hasStoreCapture := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpStoreCapture {
			hasStoreCapture = true
			break
		}
	}
	if !hasStoreCapture {
		t.Error("Expected STORE_CAPTURE opcode")
	}
}

func TestCompileNestedBlock(t *testing.T) {
	// [ [:x | x + 1] ]
	innerBlock := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.BinaryExpr{
				Left:  &parser.Identifier{Name: "x"},
				Op:    "+",
				Right: &parser.NumberLit{Value: "1"},
			}},
		},
	}

	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: innerBlock},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have MAKE_BLOCK opcode
	hasMakeBlock := false
	for i := 0; i < len(chunk.Code); i++ {
		if Opcode(chunk.Code[i]) == OpMakeBlock {
			hasMakeBlock = true
			break
		}
	}
	if !hasMakeBlock {
		t.Error("Expected MAKE_BLOCK opcode for nested block")
	}

	// Nested chunk should be serialized in constants
	if len(chunk.Constants) == 0 {
		t.Error("Expected nested block chunk in constants")
	}

	// Try to deserialize it
	for _, c := range chunk.Constants {
		if len(c) > 4 && c[0:4] == "TTBC" {
			nested, err := Deserialize([]byte(c))
			if err != nil {
				t.Errorf("Failed to deserialize nested chunk: %v", err)
			} else {
				if nested.ParamCount != 1 {
					t.Errorf("Nested block should have 1 param, got %d", nested.ParamCount)
				}
			}
		}
	}
}

func TestCompileNestedBlockWithCapture(t *testing.T) {
	// | outer |
	// outer := 10.
	// [:x | outer + x]
	innerBlock := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.BinaryExpr{
				Left:  &parser.Identifier{Name: "outer"},
				Op:    "+",
				Right: &parser.Identifier{Name: "x"},
			}},
		},
	}

	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.LocalVarDecl{Names: []string{"outer"}},
			&parser.Assignment{Target: "outer", Value: &parser.NumberLit{Value: "10"}},
			&parser.Return{Value: innerBlock},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// The nested block needs to capture 'outer'
	// Find and check the nested chunk
	for _, c := range chunk.Constants {
		if len(c) > 4 && c[0:4] == "TTBC" {
			nested, err := Deserialize([]byte(c))
			if err != nil {
				continue
			}
			// Inner block should capture 'outer'
			if len(nested.CaptureInfo) > 0 {
				found := false
				for _, cap := range nested.CaptureInfo {
					if cap.Name == "outer" {
						found = true
						break
					}
				}
				if !found {
					t.Error("Inner block should capture 'outer'")
				}
			}
		}
	}
}

func TestCompileSpecialIdentifiers(t *testing.T) {
	tests := []struct {
		name     string
		expected Opcode
	}{
		{"nil", OpConstNil},
		{"true", OpConstTrue},
		{"false", OpConstFalse},
	}

	for _, tt := range tests {
		block := &parser.BlockExpr{
			Statements: []parser.Statement{
				&parser.Return{Value: &parser.Identifier{Name: tt.name}},
			},
		}

		ctx := &CompilerContext{}
		chunk, err := CompileBlock(block, ctx)
		if err != nil {
			t.Fatalf("CompileBlock failed for %s: %v", tt.name, err)
		}

		if Opcode(chunk.Code[0]) != tt.expected {
			t.Errorf("For %s: expected %s, got %s", tt.name, tt.expected, Opcode(chunk.Code[0]))
		}
	}
}

func TestCompileDisassemblyRoundTrip(t *testing.T) {
	// Compile a non-trivial block
	block := &parser.BlockExpr{
		Params: []string{"n"},
		Statements: []parser.Statement{
			&parser.LocalVarDecl{Names: []string{"result"}},
			&parser.Assignment{Target: "result", Value: &parser.NumberLit{Value: "0"}},
			&parser.WhileExpr{
				Condition: &parser.ComparisonExpr{
					Left:  &parser.Identifier{Name: "n"},
					Op:    ">",
					Right: &parser.NumberLit{Value: "0"},
				},
				Body: []parser.Statement{
					&parser.Assignment{
						Target: "result",
						Value: &parser.BinaryExpr{
							Left:  &parser.Identifier{Name: "result"},
							Op:    "+",
							Right: &parser.Identifier{Name: "n"},
						},
					},
					&parser.Assignment{
						Target: "n",
						Value: &parser.BinaryExpr{
							Left:  &parser.Identifier{Name: "n"},
							Op:    "-",
							Right: &parser.NumberLit{Value: "1"},
						},
					},
				},
			},
			&parser.Return{Value: &parser.Identifier{Name: "result"}},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Serialize
	data, err := chunk.Serialize()
	if err != nil {
		t.Fatalf("Serialize failed: %v", err)
	}

	// Deserialize
	restored, err := Deserialize(data)
	if err != nil {
		t.Fatalf("Deserialize failed: %v", err)
	}

	// Compare essential fields
	if restored.ParamCount != chunk.ParamCount {
		t.Errorf("ParamCount mismatch: %d vs %d", restored.ParamCount, chunk.ParamCount)
	}
	if restored.LocalCount != chunk.LocalCount {
		t.Errorf("LocalCount mismatch: %d vs %d", restored.LocalCount, chunk.LocalCount)
	}
	if len(restored.Code) != len(chunk.Code) {
		t.Errorf("Code length mismatch: %d vs %d", len(restored.Code), len(chunk.Code))
	}

	// Compare code bytes directly
	for i := 0; i < len(chunk.Code) && i < len(restored.Code); i++ {
		if chunk.Code[i] != restored.Code[i] {
			t.Errorf("Code mismatch at byte %d: %02x vs %02x", i, chunk.Code[i], restored.Code[i])
			break
		}
	}

	// Compare constants
	if len(restored.Constants) != len(chunk.Constants) {
		t.Errorf("Constants count mismatch: %d vs %d", len(restored.Constants), len(chunk.Constants))
	}
	for i := 0; i < len(chunk.Constants) && i < len(restored.Constants); i++ {
		if chunk.Constants[i] != restored.Constants[i] {
			t.Errorf("Constant %d mismatch: %q vs %q", i, chunk.Constants[i], restored.Constants[i])
		}
	}

	// Verify restored chunk is executable (InstructionCount doesn't panic)
	count := restored.InstructionCount()
	if count == 0 {
		t.Error("Restored chunk should have instructions")
	}
}

func TestCompileJSONPrimitive(t *testing.T) {
	tests := []struct {
		operation string
		expected  Opcode
	}{
		{"arrayLength", OpArrayLen},
		{"arrayFirst", OpArrayFirst},
		{"arrayLast", OpArrayLast},
		{"objectKeys", OpObjectKeys},
	}

	for _, tt := range tests {
		block := &parser.BlockExpr{
			Params: []string{"arr"},
			Statements: []parser.Statement{
				&parser.Return{Value: &parser.JSONPrimitiveExpr{
					Receiver:  &parser.Identifier{Name: "arr"},
					Operation: tt.operation,
					Args:      nil,
				}},
			},
		}

		ctx := &CompilerContext{}
		chunk, err := CompileBlock(block, ctx)
		if err != nil {
			t.Fatalf("CompileBlock failed for %s: %v", tt.operation, err)
		}

		found := false
		for i := 0; i < len(chunk.Code); i++ {
			if Opcode(chunk.Code[i]) == tt.expected {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected %s for operation %s", tt.expected, tt.operation)
		}
	}
}

func TestCompilerErrors(t *testing.T) {
	// Test unsupported binary operator
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.BinaryExpr{
				Left:  &parser.NumberLit{Value: "1"},
				Op:    "???",
				Right: &parser.NumberLit{Value: "2"},
			}},
		},
	}

	ctx := &CompilerContext{}
	_, err := CompileBlock(block, ctx)
	if err == nil {
		t.Error("Expected error for unsupported operator")
	}
}

func TestAnalyzeCapturesEmpty(t *testing.T) {
	block := &parser.BlockExpr{
		Params:     []string{},
		Statements: []parser.Statement{},
	}

	c := &Compiler{
		chunk:       NewChunk(),
		context:     &CompilerContext{},
		constantMap: make(map[string]uint16),
	}

	captures := c.analyzeCaptures(block)
	if len(captures) != 0 {
		t.Errorf("Expected no captures, got %d", len(captures))
	}
}

func TestAnalyzeCapturesParamsNotCaptured(t *testing.T) {
	// Block params should not be captured
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.Identifier{Name: "x"}},
		},
	}

	c := &Compiler{
		chunk:       NewChunk(),
		context:     &CompilerContext{},
		constantMap: make(map[string]uint16),
	}

	captures := c.analyzeCaptures(block)
	if len(captures) != 0 {
		t.Errorf("Block params should not be captured, got %d captures", len(captures))
	}
}

func TestAnalyzeCapturesMethodParams(t *testing.T) {
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{Value: &parser.Identifier{Name: "methodArg"}},
		},
	}

	c := &Compiler{
		chunk: NewChunk(),
		context: &CompilerContext{
			MethodParams: []string{"methodArg"},
		},
		constantMap: make(map[string]uint16),
	}

	captures := c.analyzeCaptures(block)
	if len(captures) != 1 {
		t.Errorf("Expected 1 capture for method param, got %d", len(captures))
	}
	if len(captures) > 0 && captures[0].Source != VarSourceParam {
		t.Errorf("Expected VarSourceParam, got %v", captures[0].Source)
	}
}

// ============ Additional Coverage Tests ============

// TestCompileIfNilExpr tests ifNil:/ifNotNil: compilation
func TestCompileIfNilExpr(t *testing.T) {
	// Test: x ifNil: [0] ifNotNil: [x]
	// This is represented as a MessageSend with ifNil: and block args
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.MessageSend{
					Receiver: &parser.Identifier{Name: "x"},
					Selector: "ifNil:ifNotNil:",
					Args: []parser.Expr{
						&parser.BlockExpr{
							Statements: []parser.Statement{
								&parser.Return{Value: &parser.NumberLit{Value: "0"}},
							},
						},
						&parser.BlockExpr{
							Params: []string{"val"},
							Statements: []parser.Statement{
								&parser.Return{Value: &parser.Identifier{Name: "val"}},
							},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have generated some code
	if len(chunk.Code) < 5 {
		t.Errorf("Expected significant code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileIteration tests do: and collect: iteration compilation
func TestCompileIteration(t *testing.T) {
	// Test: array do: [:each | each + 1]
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.ExprStmt{
				Expr: &parser.MessageSend{
					Receiver: &parser.Identifier{Name: "array"},
					Selector: "do:",
					Args: []parser.Expr{
						&parser.BlockExpr{
							Params: []string{"each"},
							Statements: []parser.Statement{
								&parser.ExprStmt{
									Expr: &parser.BinaryExpr{
										Op:    "+",
										Left:  &parser.Identifier{Name: "each"},
										Right: &parser.NumberLit{Value: "1"},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"array"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have code for iteration setup
	if len(chunk.Code) < 5 {
		t.Errorf("Expected iteration code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileCollectIteration tests collect: iteration with value return
func TestCompileCollectIteration(t *testing.T) {
	// Test: array collect: [:each | each * 2]
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.MessageSend{
					Receiver: &parser.Identifier{Name: "array"},
					Selector: "collect:",
					Args: []parser.Expr{
						&parser.BlockExpr{
							Params: []string{"each"},
							Statements: []parser.Statement{
								&parser.Return{
									Value: &parser.BinaryExpr{
										Op:    "*",
										Left:  &parser.Identifier{Name: "each"},
										Right: &parser.NumberLit{Value: "2"},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"array"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.Code) < 5 {
		t.Errorf("Expected collect code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileIfExpressionValue tests if expressions that return values using IfExpr
func TestCompileIfExpressionValue(t *testing.T) {
	// Test: (x > 0) ifTrue: [1] ifFalse: [0]
	// Use IfExpr directly as the parser would produce
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.IfExpr{
					Condition: &parser.ComparisonExpr{
						Op:    ">",
						Left:  &parser.Identifier{Name: "x"},
						Right: &parser.NumberLit{Value: "0"},
					},
					TrueBlock: []parser.Statement{
						&parser.Return{Value: &parser.NumberLit{Value: "1"}},
					},
					FalseBlock: []parser.Statement{
						&parser.Return{Value: &parser.NumberLit{Value: "0"}},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have conditional jump code
	hasJump := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op.IsJump() {
			hasJump = true
			break
		}
		i += op.OperandLen()
	}
	if !hasJump {
		t.Error("Expected jump instruction for if expression")
	}
}

// TestCompileClassSend tests class method sends
func TestCompileClassSend(t *testing.T) {
	// Test: Counter new
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.MessageSend{
					Receiver: &parser.Identifier{Name: "Counter"},
					Selector: "new",
					Args:     []parser.Expr{},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have SEND_CLASS or similar
	if len(chunk.Code) < 3 {
		t.Errorf("Expected class send code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileSelectIteration tests select: iteration
func TestCompileSelectIteration(t *testing.T) {
	// Test: array select: [:each | each > 0]
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.MessageSend{
					Receiver: &parser.Identifier{Name: "array"},
					Selector: "select:",
					Args: []parser.Expr{
						&parser.BlockExpr{
							Params: []string{"each"},
							Statements: []parser.Statement{
								&parser.Return{
									Value: &parser.ComparisonExpr{
										Op:    ">",
										Left:  &parser.Identifier{Name: "each"},
										Right: &parser.NumberLit{Value: "0"},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"array"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.Code) < 5 {
		t.Errorf("Expected select code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileInjectInto tests inject:into: iteration
func TestCompileInjectInto(t *testing.T) {
	// Test: array inject: 0 into: [:sum :each | sum + each]
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.MessageSend{
					Receiver: &parser.Identifier{Name: "array"},
					Selector: "inject:into:",
					Args: []parser.Expr{
						&parser.NumberLit{Value: "0"},
						&parser.BlockExpr{
							Params: []string{"sum", "each"},
							Statements: []parser.Statement{
								&parser.Return{
									Value: &parser.BinaryExpr{
										Op:    "+",
										Left:  &parser.Identifier{Name: "sum"},
										Right: &parser.Identifier{Name: "each"},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"array"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.Code) < 5 {
		t.Errorf("Expected inject:into: code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileTimesRepeat tests timesRepeat: iteration
func TestCompileTimesRepeat(t *testing.T) {
	// Test: 5 timesRepeat: [x := x + 1]
	block := &parser.BlockExpr{
		Statements: []parser.Statement{
			&parser.ExprStmt{
				Expr: &parser.MessageSend{
					Receiver: &parser.NumberLit{Value: "5"},
					Selector: "timesRepeat:",
					Args: []parser.Expr{
						&parser.BlockExpr{
							Statements: []parser.Statement{
								&parser.Assignment{
									Target: "x",
									Value: &parser.BinaryExpr{
										Op:    "+",
										Left:  &parser.Identifier{Name: "x"},
										Right: &parser.NumberLit{Value: "1"},
									},
								},
							},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{
		MethodLocals: []string{"x"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	if len(chunk.Code) < 5 {
		t.Errorf("Expected timesRepeat code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileIfNilOnly tests ifNil: only
func TestCompileIfNilOnly(t *testing.T) {
	// Test: value ifNil: [^ 0]
	block := &parser.BlockExpr{
		Params: []string{"value"},
		Statements: []parser.Statement{
			&parser.IfNilExpr{
				Subject:  &parser.Identifier{Name: "value"},
				NilBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "0"}},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have jump instructions
	hasJump := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpJumpNotNil {
			hasJump = true
			break
		}
		i += op.OperandLen()
	}
	if !hasJump {
		t.Error("Expected JumpNotNil instruction for ifNil:")
	}
}

// TestCompileIfNotNilOnly tests ifNotNil: only
func TestCompileIfNotNilOnly(t *testing.T) {
	// Test: value ifNotNil: [:v | ^ v]
	block := &parser.BlockExpr{
		Params: []string{"value"},
		Statements: []parser.Statement{
			&parser.IfNilExpr{
				Subject: &parser.Identifier{Name: "value"},
				NotNilBlock: []parser.Statement{
					&parser.Return{Value: &parser.Identifier{Name: "v"}},
				},
				BindingVar: "v",
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have OpJumpNil and OpDup/OpStoreLocal for binding
	hasJumpNil := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpJumpNil {
			hasJumpNil = true
			break
		}
		i += op.OperandLen()
	}
	if !hasJumpNil {
		t.Error("Expected JumpNil instruction for ifNotNil:")
	}
}

// TestCompileIfNilIfNotNil tests both ifNil: and ifNotNil:
func TestCompileIfNilIfNotNil(t *testing.T) {
	// Test: value ifNil: [^ 0] ifNotNil: [:v | ^ v + 1]
	block := &parser.BlockExpr{
		Params: []string{"value"},
		Statements: []parser.Statement{
			&parser.IfNilExpr{
				Subject: &parser.Identifier{Name: "value"},
				NilBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "0"}},
				},
				NotNilBlock: []parser.Statement{
					&parser.Return{
						Value: &parser.BinaryExpr{
							Op:    "+",
							Left:  &parser.Identifier{Name: "v"},
							Right: &parser.NumberLit{Value: "1"},
						},
					},
				},
				BindingVar: "v",
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have both OpJumpNotNil and OpJump
	hasJumpNotNil := false
	hasJump := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpJumpNotNil {
			hasJumpNotNil = true
		}
		if op == OpJump {
			hasJump = true
		}
		i += op.OperandLen()
	}
	if !hasJumpNotNil || !hasJump {
		t.Errorf("Expected JumpNotNil and Jump for ifNil:ifNotNil:, got JumpNotNil=%v Jump=%v", hasJumpNotNil, hasJump)
	}
}

// TestCompileIterationDo tests do: iteration
func TestCompileIterationDo(t *testing.T) {
	// Test: items do: [:item | @ self process: item]
	block := &parser.BlockExpr{
		Params: []string{"items"},
		Statements: []parser.Statement{
			&parser.IterationExpr{
				Collection: &parser.Identifier{Name: "items"},
				IterVar:    "item",
				Kind:       "do",
				Body: []parser.Statement{
					&parser.ExprStmt{
						Expr: &parser.MessageSend{
							Receiver: &parser.Identifier{Name: "self"},
							Selector: "process:",
							Args:     []parser.Expr{&parser.Identifier{Name: "item"}},
							IsSelf:   true,
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have loop instructions
	if len(chunk.Code) < 10 {
		t.Errorf("Expected iteration code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileIterationSelect tests select: iteration (as statement)
func TestCompileIterationSelect(t *testing.T) {
	// Test: items select: [:item | item > 0] (as a statement)
	block := &parser.BlockExpr{
		Params: []string{"items"},
		Statements: []parser.Statement{
			&parser.IterationExpr{
				Collection: &parser.Identifier{Name: "items"},
				IterVar:    "item",
				Kind:       "select",
				Body: []parser.Statement{
					&parser.Return{
						Value: &parser.ComparisonExpr{
							Op:    ">",
							Left:  &parser.Identifier{Name: "item"},
							Right: &parser.NumberLit{Value: "0"},
						},
					},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have iteration code
	if len(chunk.Code) < 10 {
		t.Errorf("Expected select iteration code, got %d bytes", len(chunk.Code))
	}
}

// TestCompileClassPrimitive tests class primitive expressions
func TestCompileClassPrimitive(t *testing.T) {
	// Test: String#isEmpty(str)
	block := &parser.BlockExpr{
		Params: []string{"str"},
		Statements: []parser.Statement{
			&parser.Return{
				Value: &parser.ClassPrimitiveExpr{
					ClassName: "String",
					Operation: "isEmpty",
					Args:      []parser.Expr{&parser.Identifier{Name: "str"}},
				},
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have SEND_CLASS opcode
	hasClassSend := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpSendClass {
			hasClassSend = true
			break
		}
		i += op.OperandLen()
	}
	if !hasClassSend {
		t.Error("Expected OpSendClass for class primitive")
	}
}

// TestAddSourceLocation tests source location tracking
func TestAddSourceLocation(t *testing.T) {
	chunk := NewChunk()

	// Emit some code
	chunk.Code = append(chunk.Code, byte(OpConst))
	chunk.Code = append(chunk.Code, 0, 0)

	// Add source location (offset, line, column)
	chunk.AddSourceLocation(0, 10, 5)

	// Check that we can retrieve it
	line, column := chunk.GetSourceLocation(0)
	if line != 10 {
		t.Errorf("Expected line 10, got %d", line)
	}
	if column != 5 {
		t.Errorf("Expected column 5, got %d", column)
	}
}

// TestCompilerContextInstanceVars tests instance variable handling
// In this design, instance variables accessed from blocks are treated as captures
func TestCompilerContextInstanceVars(t *testing.T) {
	// Test: value := x + 1 (where value is an instance variable)
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.Assignment{
				Target: "value",
				Value: &parser.BinaryExpr{
					Op:    "+",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "1"},
				},
			},
		},
	}

	ctx := &CompilerContext{
		InstanceVars: []string{"value", "count"},
	}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Instance variables are captured when accessed from blocks
	// Verify capture info was set up correctly
	if len(chunk.CaptureInfo) == 0 {
		t.Error("Expected instance variable to be captured")
	}

	foundValueCapture := false
	for _, cap := range chunk.CaptureInfo {
		if cap.Name == "value" && cap.Source == VarSourceIVar {
			foundValueCapture = true
			break
		}
	}
	if !foundValueCapture {
		t.Error("Expected 'value' to be captured as an ivar")
	}

	// Should have capture store operation (ivars are accessed as captures in blocks)
	hasStoreCapture := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpStoreCapture {
			hasStoreCapture = true
			break
		}
		i += op.OperandLen()
	}
	if !hasStoreCapture {
		t.Error("Expected OpStoreCapture for instance variable assignment from block")
	}
}

// TestPatchJumpTo tests patching jumps to specific offsets
func TestPatchJumpTo(t *testing.T) {
	chunk := NewChunk()

	// Emit a jump instruction
	chunk.Code = append(chunk.Code, byte(OpJump))
	placeholderOffset := len(chunk.Code)
	chunk.Code = append(chunk.Code, 0, 0) // placeholder for delta

	// Emit some filler code
	chunk.Code = append(chunk.Code, byte(OpConst), 0, 0)
	chunk.Code = append(chunk.Code, byte(OpConst), 0, 0)

	targetOffset := len(chunk.Code)

	// Patch the jump
	chunk.PatchJumpTo(placeholderOffset, targetOffset)

	// PatchJumpTo stores delta = target - (placeholderOffset + 2)
	// delta = 9 - 3 = 6
	expectedDelta := targetOffset - (placeholderOffset + 2)

	// Delta is stored big-endian
	patchedDelta := int16(uint16(chunk.Code[placeholderOffset])<<8 | uint16(chunk.Code[placeholderOffset+1]))
	if int(patchedDelta) != expectedDelta {
		t.Errorf("Expected delta %d, got %d", expectedDelta, patchedDelta)
	}
}

// TestCompileIfTrueOnly tests ifTrue: without ifFalse:
func TestCompileIfTrueOnly(t *testing.T) {
	// Test: x > 0 ifTrue: [^ 1]
	block := &parser.BlockExpr{
		Params: []string{"x"},
		Statements: []parser.Statement{
			&parser.IfExpr{
				Condition: &parser.ComparisonExpr{
					Op:    ">",
					Left:  &parser.Identifier{Name: "x"},
					Right: &parser.NumberLit{Value: "0"},
				},
				TrueBlock: []parser.Statement{
					&parser.Return{Value: &parser.NumberLit{Value: "1"}},
				},
				FalseBlock: nil, // No else branch
			},
		},
	}

	ctx := &CompilerContext{}
	chunk, err := CompileBlock(block, ctx)
	if err != nil {
		t.Fatalf("CompileBlock failed: %v", err)
	}

	// Should have conditional jump
	hasJumpFalse := false
	for i := 0; i < len(chunk.Code); i++ {
		op := Opcode(chunk.Code[i])
		if op == OpJumpFalse {
			hasJumpFalse = true
			break
		}
		i += op.OperandLen()
	}
	if !hasJumpFalse {
		t.Error("Expected JumpFalse instruction for ifTrue: only")
	}
}
