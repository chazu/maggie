package compiler

import (
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Control-flow inlining
// ---------------------------------------------------------------------------
//
// The standard Smalltalk-80 conditional and loop selectors compile to jump
// bytecode instead of block closures + full dispatch when their block
// arguments are literal, parameterless, temp-less blocks. Anything else —
// blocks held in variables, blocks with temps, super sends — falls back to a
// real message send, so user-visible send semantics are preserved exactly
// where they can be observed.
//
// OpJumpTrue/OpJumpFalse pop their condition and trap non-boolean values with
// a catchable mustBeBoolean error (see Interpreter.signalMustBeBoolean), the
// bytecode analog of the doesNotUnderstand a real ifTrue:/and: send raises.
//
// Inlined bodies compile in the enclosing method frame: no BlockMethod is
// created, variable references resolve exactly as method-body references
// (including the cell protocol for captured variables), and a ^ inside an
// inlined block becomes a plain method return — no NLR machinery.

// inlinableBlock returns the literal block if expr can be compiled inline in
// the enclosing frame: no parameters (the inlined selectors pass none) and no
// block-local temps (inlining those would need shadowing-aware slot
// allocation; they fall back to a closure instead).
func inlinableBlock(expr Expr) (*Block, bool) {
	b, ok := expr.(*Block)
	if !ok || len(b.Parameters) != 0 || len(b.Temps) != 0 {
		return nil, false
	}
	return b, true
}

// compileInlineBody compiles a block body in the CURRENT frame, leaving the
// body's value on the stack (nil for an empty body). No frame push, no
// OpBlockReturn — the inline analog of compileBlock's body handling.
func (c *Compiler) compileInlineBody(b *Block) {
	if len(b.Statements) == 0 {
		c.frame.builder.Emit(vm.OpPushNil)
		return
	}
	c.compileStatements(b.Statements)
}

// inlineParts describes how an inlined send decomposes: exprs are evaluated
// in the enclosing scope as ordinary expressions (the boolean receiver of
// if/and/or forms); blocks have their bodies compiled directly in the
// enclosing scope. This is THE shared predicate: findCellVariables must treat
// a send as scope-transparent exactly when codegen will inline it — if the
// analysis skipped a block codegen turns into a closure, an assignment could
// silently bypass its cell.
type inlineParts struct {
	exprs  []Expr
	blocks []*Block
}

// keywordInlineParts reports whether tryInlineControlFlow will inline msg,
// and if so which subexpressions stay expressions vs compile inline.
func keywordInlineParts(msg *KeywordMessage) (inlineParts, bool) {
	if _, isSuper := msg.Receiver.(*Super); isSuper {
		return inlineParts{}, false
	}
	switch msg.Selector {
	case "ifTrue:", "ifFalse:", "and:", "or:":
		b, ok := inlinableBlock(msg.Arguments[0])
		if !ok {
			return inlineParts{}, false
		}
		return inlineParts{exprs: []Expr{msg.Receiver}, blocks: []*Block{b}}, true
	case "ifTrue:ifFalse:", "ifFalse:ifTrue:":
		b1, ok1 := inlinableBlock(msg.Arguments[0])
		b2, ok2 := inlinableBlock(msg.Arguments[1])
		if !ok1 || !ok2 {
			return inlineParts{}, false
		}
		return inlineParts{exprs: []Expr{msg.Receiver}, blocks: []*Block{b1, b2}}, true
	case "whileTrue:", "whileFalse:":
		cond, body, ok := inlinableWhile(msg.Receiver, msg.Arguments[0])
		if !ok {
			return inlineParts{}, false
		}
		return inlineParts{blocks: []*Block{cond, body}}, true
	}
	return inlineParts{}, false
}

// unaryInlineParts reports whether tryInlineUnaryControlFlow will inline msg.
func unaryInlineParts(msg *UnaryMessage) (*Block, bool) {
	if msg.Selector != "whileTrue" && msg.Selector != "whileFalse" {
		return nil, false
	}
	cond, ok := inlinableBlock(msg.Receiver)
	if !ok || len(cond.Statements) == 0 {
		return nil, false
	}
	return cond, true
}

// tryInlineControlFlow inlines the standard control-flow keyword selectors.
// Returns true if it emitted code; false means the caller must fall back to
// a real message send. The inline/fallback decision is keywordInlineParts;
// this function only emits.
func (c *Compiler) tryInlineControlFlow(msg *KeywordMessage) bool {
	if _, ok := keywordInlineParts(msg); !ok {
		return false
	}

	switch msg.Selector {
	case "ifTrue:":
		b, ok := inlinableBlock(msg.Arguments[0])
		if !ok {
			return false
		}
		c.inlineConditional(msg.Receiver, b, nil, vm.OpJumpFalse)
		return true

	case "ifFalse:":
		b, ok := inlinableBlock(msg.Arguments[0])
		if !ok {
			return false
		}
		c.inlineConditional(msg.Receiver, b, nil, vm.OpJumpTrue)
		return true

	case "ifTrue:ifFalse:":
		tb, ok1 := inlinableBlock(msg.Arguments[0])
		fb, ok2 := inlinableBlock(msg.Arguments[1])
		if !ok1 || !ok2 {
			return false
		}
		c.inlineConditional(msg.Receiver, tb, fb, vm.OpJumpFalse)
		return true

	case "ifFalse:ifTrue:":
		fb, ok1 := inlinableBlock(msg.Arguments[0])
		tb, ok2 := inlinableBlock(msg.Arguments[1])
		if !ok1 || !ok2 {
			return false
		}
		c.inlineConditional(msg.Receiver, fb, tb, vm.OpJumpTrue)
		return true

	case "and:":
		b, ok := inlinableBlock(msg.Arguments[0])
		if !ok {
			return false
		}
		c.inlineAndOr(msg.Receiver, b, vm.OpJumpFalse)
		return true

	case "or:":
		b, ok := inlinableBlock(msg.Arguments[0])
		if !ok {
			return false
		}
		c.inlineAndOr(msg.Receiver, b, vm.OpJumpTrue)
		return true

	case "whileTrue:":
		cond, body, ok := inlinableWhile(msg.Receiver, msg.Arguments[0])
		if !ok {
			return false
		}
		c.inlineWhile(cond, body, vm.OpJumpFalse)
		return true

	case "whileFalse:":
		cond, body, ok := inlinableWhile(msg.Receiver, msg.Arguments[0])
		if !ok {
			return false
		}
		c.inlineWhile(cond, body, vm.OpJumpTrue)
		return true
	}
	return false
}

// tryInlineUnaryControlFlow inlines [cond] whileTrue / [cond] whileFalse.
func (c *Compiler) tryInlineUnaryControlFlow(msg *UnaryMessage) bool {
	cond, ok := unaryInlineParts(msg)
	if !ok {
		return false
	}
	exitOp := vm.OpJumpFalse
	if msg.Selector == "whileFalse" {
		exitOp = vm.OpJumpTrue
	}
	c.inlineWhile(cond, nil, exitOp)
	return true
}

// inlinableWhile validates a whileTrue:/whileFalse: send for inlining: both
// the receiver and the argument must be inlinable literal blocks, and the
// condition must be non-empty ([] whileTrue: … falls back to the primitive,
// which treats a nil condition as loop exit).
func inlinableWhile(receiver, arg Expr) (cond, body *Block, ok bool) {
	cond, ok1 := inlinableBlock(receiver)
	body, ok2 := inlinableBlock(arg)
	if !ok1 || !ok2 || len(cond.Statements) == 0 {
		return nil, nil, false
	}
	return cond, body, true
}

// inlineConditional emits:
//
//	<receiver>
//	skipOp    -> Lelse     (traps non-boolean)
//	<taken body>           (value of the expression)
//	Jump      -> Lend
//	Lelse: <other body>    (or PushNil for one-armed forms)
//	Lend:
//
// skipOp jumps AWAY from the taken branch: OpJumpFalse for ifTrue:-first
// forms, OpJumpTrue for ifFalse:-first forms.
func (c *Compiler) inlineConditional(receiver Expr, taken, other *Block, skipOp vm.Opcode) {
	b := c.frame.builder
	c.compileExpr(receiver)

	elseLabel := b.NewLabel()
	endLabel := b.NewLabel()
	b.EmitJump(skipOp, elseLabel)
	c.compileInlineBody(taken)
	b.EmitJump(vm.OpJump, endLabel)
	b.Mark(elseLabel)
	if other != nil {
		c.compileInlineBody(other)
	} else {
		b.Emit(vm.OpPushNil)
	}
	b.Mark(endLabel)
}

// inlineAndOr emits short-circuit and:/or::
//
//	<receiver>
//	shortOp   -> Lshort    (traps non-boolean)
//	<block body>           (value of the expression)
//	Jump      -> Lend
//	Lshort: PushFalse      (and:) / PushTrue (or:)
//	Lend:
func (c *Compiler) inlineAndOr(receiver Expr, blk *Block, shortOp vm.Opcode) {
	b := c.frame.builder
	c.compileExpr(receiver)

	shortLabel := b.NewLabel()
	endLabel := b.NewLabel()
	b.EmitJump(shortOp, shortLabel)
	c.compileInlineBody(blk)
	b.EmitJump(vm.OpJump, endLabel)
	b.Mark(shortLabel)
	if shortOp == vm.OpJumpFalse {
		b.Emit(vm.OpPushFalse)
	} else {
		b.Emit(vm.OpPushTrue)
	}
	b.Mark(endLabel)
}

// inlineWhile emits a whileTrue:/whileFalse: loop (body nil for the unary
// forms). The loop's value is nil, per Smalltalk-80.
//
//	Lloop: <cond body>
//	exitOp    -> Lend      (traps non-boolean)
//	<body>  Pop
//	Jump      -> Lloop
//	Lend:  PushNil
func (c *Compiler) inlineWhile(cond, body *Block, exitOp vm.Opcode) {
	b := c.frame.builder

	loopLabel := b.NewLabel()
	endLabel := b.NewLabel()
	b.Mark(loopLabel) // resolved label: EmitJump computes a backward offset
	c.compileInlineBody(cond)
	b.EmitJump(exitOp, endLabel)
	if body != nil {
		c.compileInlineBody(body)
		b.Emit(vm.OpPOP)
	}
	b.EmitJump(vm.OpJump, loopLabel)
	b.Mark(endLabel)
	b.Emit(vm.OpPushNil)
}
