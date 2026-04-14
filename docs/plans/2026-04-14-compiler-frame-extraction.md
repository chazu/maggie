# Compiler Frame Extraction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract the per-method/per-block mutable compilation state from the Compiler struct into a `compilationFrame` struct, turning the manual 15-field save/restore in `compileBlock()` into a frame stack push/pop.

**Architecture:** The `Compiler` struct is split into two concerns: (1) immutable shared state (selectors, symbols, registry, classTable, namespace, imports) and (2) per-scope mutable state (builder, literals, temps, args, captures, etc.) held in a `compilationFrame`. The Compiler maintains a stack of frames. `compileBlock()` pushes a new frame; when done it pops back. All compilation methods access the current frame via `c.frame`.

**Tech Stack:** Go, existing compiler test suite (parser_test, codegen_test, integration_test, fuzz_test)

---

### File Structure

- **Modify:** `compiler/codegen.go` — extract `compilationFrame` struct, add frame stack to `Compiler`, update all methods to use `c.frame.*`
- **No new files** — the frame struct is small and belongs in codegen.go alongside the Compiler

### Field Partitioning

**Stays on Compiler** (shared across all scopes):
- `selectors`, `symbols`, `registry` — VM tables
- `instVars` — set once per class, shared by all methods/blocks
- `blocks` — all blocks accumulate into the method's list
- `cellVars` — computed once per method, shared with all nested blocks
- `errors` — accumulated across the whole compilation
- `methodSelector` — for tail-call detection, set once per method
- `namespace`, `imports`, `classTable` — FQN resolution context

**Moves to compilationFrame** (per-scope, saved/restored):
- `builder` — bytecode builder
- `literals`, `literalMap` — literal pool
- `temps`, `args` — variable name → slot mappings
- `numArgs`, `numTemps` — slot counts
- `inBlock` — whether we're in a block
- `outerTemps`, `outerArgs` — home frame variable access
- `enclosingBlockVars` — vars available for capture
- `capturedVars` — captured var name → capture index
- `blockNestingDepth` — nesting level
- `cellInitialized` — tracks cell creation per scope
- `sourceMap` — bytecode → source position

---

### Task 1: Establish baseline

**Files:**
- Read: `compiler/codegen.go`, `compiler/codegen_test.go`

- [ ] **Step 1: Run the full test suite to confirm green baseline**

Run: `go test ./compiler/ -count=1 -timeout 60s -v 2>&1 | tail -5`
Expected: PASS

- [ ] **Step 2: Run the full project tests**

Run: `go test ./... -count=1 -timeout 300s 2>&1 | tail -16`
Expected: All PASS

---

### Task 2: Define compilationFrame and add frame stack

**Files:**
- Modify: `compiler/codegen.go`

- [ ] **Step 1: Add the compilationFrame struct and update Compiler**

Insert the `compilationFrame` struct definition before the `Compiler` struct. Move the 15 per-scope fields from `Compiler` into it. Add a `frame *compilationFrame` and `frameStack []*compilationFrame` to `Compiler`.

Replace the Compiler struct definition (lines 14-57) with:

```go
// compilationFrame holds the mutable state for a single compilation scope
// (method or block). The Compiler maintains a stack of these.
type compilationFrame struct {
	builder     *vm.BytecodeBuilder
	literals    []vm.Value
	literalMap  map[interface{}]int
	temps       map[string]int
	args        map[string]int
	numArgs     int
	numTemps    int
	inBlock     bool
	outerTemps  map[string]int
	outerArgs   map[string]int
	enclosingBlockVars map[string]int
	capturedVars       map[string]int
	blockNestingDepth  int
	cellInitialized    map[string]bool
	sourceMap          []vm.SourceLoc
}

// Compiler compiles AST nodes to bytecode.
type Compiler struct {
	selectors *vm.SelectorTable
	symbols   *vm.SymbolTable
	registry  *vm.ObjectRegistry

	// Current compilation frame (per-scope mutable state)
	frame      *compilationFrame
	frameStack []*compilationFrame

	// Shared across all scopes within a method compilation
	instVars       map[string]int
	blocks         []*vm.BlockMethod
	cellVars       map[string]bool
	errors         []string
	methodSelector string

	// FQN resolution context
	namespace  string
	imports    []string
	classTable *vm.ClassTable
}
```

- [ ] **Step 2: Add pushFrame and popFrame methods**

Add these methods after `NewCompiler`:

```go
// pushFrame saves the current frame and activates a new one.
func (c *Compiler) pushFrame(f *compilationFrame) {
	if c.frame != nil {
		c.frameStack = append(c.frameStack, c.frame)
	}
	c.frame = f
}

// popFrame restores the previous frame.
func (c *Compiler) popFrame() {
	n := len(c.frameStack)
	c.frame = c.frameStack[n-1]
	c.frameStack = c.frameStack[:n-1]
}
```

- [ ] **Step 3: Compilation should fail — don't run tests yet**

This step just sets up the types. All method bodies still reference `c.builder`, `c.temps`, etc. instead of `c.frame.builder`, `c.frame.temps`, etc. We'll fix that in the next task.

- [ ] **Step 4: Commit the struct definition only (won't compile yet)**

Don't commit — we'll commit after the full migration compiles.

---

### Task 3: Migrate all field accesses to use c.frame

**Files:**
- Modify: `compiler/codegen.go`

- [ ] **Step 1: Update CompileMethod to push a frame**

Replace the field initialization at the top of `CompileMethod()` (lines 145-154) with:

```go
	c.pushFrame(&compilationFrame{
		builder:    vm.NewBytecodeBuilder(),
		literalMap: make(map[interface{}]int),
		temps:      make(map[string]int),
		args:       make(map[string]int),
		numArgs:    len(method.Parameters),
		numTemps:   len(method.Parameters) + len(method.Temps),
	})
	defer c.popFrame()
	c.blocks = nil
	c.methodSelector = method.Selector
```

- [ ] **Step 2: Update CompileExpression similarly**

Replace the field initialization at the top of `CompileExpression()` with the same pattern (push a frame, defer pop).

- [ ] **Step 3: Mechanically replace all c.fieldName with c.frame.fieldName**

For each of these fields that moved to the frame, do a global find-replace in codegen.go:
- `c.builder` → `c.frame.builder`
- `c.literals` → `c.frame.literals` (but NOT `c.literalMap` references in `addLiteral`)
- `c.literalMap` → `c.frame.literalMap`
- `c.temps` → `c.frame.temps`
- `c.args` → `c.frame.args`
- `c.numArgs` → `c.frame.numArgs`
- `c.numTemps` → `c.frame.numTemps`
- `c.inBlock` → `c.frame.inBlock`
- `c.outerTemps` → `c.frame.outerTemps`
- `c.outerArgs` → `c.frame.outerArgs`
- `c.enclosingBlockVars` → `c.frame.enclosingBlockVars`
- `c.capturedVars` → `c.frame.capturedVars`
- `c.blockNestingDepth` → `c.frame.blockNestingDepth`
- `c.cellInitialized` → `c.frame.cellInitialized`
- `c.sourceMap` → `c.frame.sourceMap`

**Be careful NOT to replace:**
- `c.selectors`, `c.symbols`, `c.registry` — stays on Compiler
- `c.instVars`, `c.blocks`, `c.cellVars`, `c.errors` — stays on Compiler
- `c.methodSelector`, `c.namespace`, `c.imports`, `c.classTable` — stays on Compiler
- `c.errorf`, `c.checkSlotIndex`, `c.errorAt` — methods on Compiler
- Any field inside a saved-state variable (e.g., `oldBuilder` in compileBlock)

- [ ] **Step 4: Verify compilation**

Run: `go build ./compiler/`
Expected: Compiles successfully

- [ ] **Step 5: Run tests**

Run: `go test ./compiler/ -count=1 -timeout 60s`
Expected: PASS

---

### Task 4: Simplify compileBlock with pushFrame/popFrame

**Files:**
- Modify: `compiler/codegen.go`

- [ ] **Step 1: Replace the save/restore pattern in compileBlock**

The current `compileBlock()` has 15 lines of `oldX := c.frame.X` saves and 15 lines of `c.frame.X = oldX` restores. Replace the entire function body with the frame-based approach.

The key insight: instead of saving 15 fields individually, we just `pushFrame()` with a new frame, do the work, then `popFrame()`. But we need to read from the old frame to build the new one (enclosing vars, etc.), so we capture `outerFrame := c.frame` first.

Replace `compileBlock()` with:

```go
func (c *Compiler) compileBlock(block *Block) {
	outerFrame := c.frame

	// Build map of enclosing scope variables that this block can capture
	newNestingDepth := outerFrame.blockNestingDepth + 1
	newEnclosingBlockVars := make(map[string]int)
	if outerFrame.enclosingBlockVars != nil {
		for k, v := range outerFrame.enclosingBlockVars {
			newEnclosingBlockVars[k] = v
		}
	}
	if newNestingDepth == 1 {
		for k, v := range outerFrame.temps {
			newEnclosingBlockVars[k] = v
		}
		for k, v := range outerFrame.args {
			newEnclosingBlockVars[k] = v
		}
	} else if outerFrame.inBlock {
		for k, v := range outerFrame.temps {
			newEnclosingBlockVars[k] = v
		}
		for k, v := range outerFrame.args {
			newEnclosingBlockVars[k] = v
		}
	}

	// Identify which variables this block needs to capture
	varsToCapture := c.findCapturedVariables(block, newEnclosingBlockVars)
	numCaptures := len(varsToCapture)
	if numCaptures > 0xFF {
		c.errorf("block capture overflow: block captures %d variables, exceeds maximum of 255", numCaptures)
	}

	// Build capturedVars map for the inner block
	newCapturedVars := make(map[string]int)
	for i, varName := range varsToCapture {
		newCapturedVars[varName] = i
	}

	// Push new frame for the block
	c.pushFrame(&compilationFrame{
		builder:            vm.NewBytecodeBuilder(),
		literalMap:         make(map[interface{}]int),
		temps:              make(map[string]int),
		args:               make(map[string]int),
		numArgs:            len(block.Parameters),
		numTemps:           len(block.Parameters) + len(block.Temps),
		inBlock:            true,
		outerTemps:         make(map[string]int),
		outerArgs:          make(map[string]int),
		enclosingBlockVars: newEnclosingBlockVars,
		capturedVars:       newCapturedVars,
		blockNestingDepth:  newNestingDepth,
		cellInitialized:    make(map[string]bool),
	})

	// Parameters
	for i, param := range block.Parameters {
		c.frame.args[param] = i
	}

	// Temps
	for i, temp := range block.Temps {
		c.frame.temps[temp] = c.frame.numArgs + i
	}

	// Pre-initialize cell variables
	for name, idx := range c.frame.temps {
		if c.cellVars[name] {
			c.checkSlotIndex(idx, "block temp", name)
			c.frame.builder.Emit(vm.OpPushNil)
			c.frame.builder.Emit(vm.OpMakeCell)
			c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.frame.builder.Emit(vm.OpPOP)
			c.frame.cellInitialized[name] = true
		}
	}
	for name, idx := range c.frame.args {
		if c.cellVars[name] {
			c.checkSlotIndex(idx, "block arg", name)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
			c.frame.builder.Emit(vm.OpMakeCell)
			c.frame.builder.EmitByte(vm.OpStoreTemp, byte(idx))
			c.frame.builder.Emit(vm.OpPOP)
			c.frame.cellInitialized[name] = true
		}
	}

	// Compile block body
	c.compileStatements(block.Statements)

	if len(block.Statements) == 0 {
		c.frame.builder.Emit(vm.OpPushNil)
	}
	c.frame.builder.Emit(vm.OpBlockReturn)

	// Create BlockMethod with its own literals
	blockMethod := &vm.BlockMethod{
		Arity:       c.frame.numArgs,
		NumTemps:    c.frame.numTemps,
		NumCaptures: numCaptures,
		Bytecode:    c.frame.builder.Bytes(),
		Literals:    c.frame.literals,
		SourceMap:   c.frame.sourceMap,
	}

	// Pop back to outer frame
	c.popFrame()

	// Emit capture instructions in the outer frame
	for _, varName := range varsToCapture {
		if idx, ok := outerFrame.enclosingBlockVars[varName]; ok {
			if outerFrame.capturedVars != nil {
				if captIdx, ok := outerFrame.capturedVars[varName]; ok {
					c.checkSlotIndex(captIdx, "captured variable", varName)
					c.frame.builder.EmitByte(vm.OpPushCaptured, byte(captIdx))
					continue
				}
			}
			c.checkSlotIndex(idx, "block variable", varName)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.frame.temps[varName]; ok {
			c.checkSlotIndex(idx, "temp", varName)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.frame.args[varName]; ok {
			c.checkSlotIndex(idx, "argument", varName)
			c.frame.builder.EmitByte(vm.OpPushTemp, byte(idx))
		} else if idx, ok := c.frame.outerTemps[varName]; ok {
			c.checkSlotIndex(idx, "outer temp", varName)
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
		} else if idx, ok := c.frame.outerArgs[varName]; ok {
			c.checkSlotIndex(idx, "outer arg", varName)
			c.frame.builder.EmitByte(vm.OpPushHomeTemp, byte(idx))
		}
	}

	// Add block to list
	blockIdx := len(c.blocks)
	if blockIdx > 0xFFFF {
		c.errorf("block index overflow: method has more than 65535 blocks")
	}
	c.blocks = append(c.blocks, blockMethod)

	// Emit create block instruction
	c.frame.builder.EmitCreateBlock(uint16(blockIdx), uint8(numCaptures))
}
```

- [ ] **Step 2: Run tests**

Run: `go test ./compiler/ -count=1 -timeout 60s`
Expected: PASS

- [ ] **Step 3: Run full test suite**

Run: `go test ./... -count=1 -timeout 300s`
Expected: All PASS

- [ ] **Step 4: Commit**

```bash
git add compiler/codegen.go
git commit -m "refactor: extract compilationFrame from Compiler struct

Replace 15-field manual save/restore in compileBlock() with a frame
stack (pushFrame/popFrame). The Compiler struct now has two clear
concerns: shared state (selectors, symbols, registry, classTable)
and per-scope state (compilationFrame with builder, temps, literals,
captures, etc.)."
```

---

### Task 5: Final verification

**Files:**
- Read: `compiler/codegen.go`

- [ ] **Step 1: Run fuzz tests briefly**

Run: `go test ./compiler/ -fuzz=Fuzz -fuzztime=10s 2>&1 | tail -3`
Expected: No crashes

- [ ] **Step 2: Run bootstrap (compiles all lib/*.mag files)**

Run: `go run ./cmd/bootstrap/ 2>&1 | tail -5`
Expected: Successful image generation

- [ ] **Step 3: Run full test suite one final time**

Run: `go test ./... -count=1 -timeout 300s`
Expected: All PASS

- [ ] **Step 4: Verify line count improvement**

Run: `wc -l compiler/codegen.go`
Expected: ~30 fewer lines (15 saves + 15 restores removed, pushFrame/popFrame/struct added)
