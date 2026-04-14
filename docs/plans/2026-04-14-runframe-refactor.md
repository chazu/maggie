# runFrame() Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Break the 786-line `runFrame()` switch statement into logically grouped handler methods without changing behavior or hurting performance.

**Architecture:** Extract the 6 complex opcode groups (globals, class vars, sends, tail sends, returns, block creation) into private methods on `Interpreter`. Keep simple 1-3 line opcodes inline in the switch for performance (avoiding function call overhead on the hottest path). Each handler method receives `frame`, `bc`, `literals`, and `isBlock` as parameters — the same locals available in the switch body.

**Tech Stack:** Go, existing vm package test suite and benchmarks

---

### Strategy

The switch in `runFrame()` has two categories:

1. **Simple opcodes (1-5 lines):** OpNOP, OpPOP, OpDUP, OpPushNil/True/False/Self, OpPushInt8/Int32, OpPushLiteral, OpPushFloat, OpPushTemp/StoreTemp, OpPushHomeTemp/StoreHomeTemp, OpPushIvar/StoreIvar, OpPushCaptured/StoreCaptured, OpMakeCell/CellGet/CellSet, OpCaptureTemp/CaptureIvar, all jump ops, OpReturnSelf/Nil, OpBlockReturn, OpCreateArray/Dict/Object. These stay inline.

2. **Complex opcodes (10+ lines):** OpPushGlobal (52 lines), OpStoreGlobal (33 lines), OpPushClassVar (24 lines), OpStoreClassVar (18 lines), OpTailSend (70 lines), OpSendSuper (24 lines), OpReturnTop (21 lines), OpCreateBlock (34 lines). These get extracted.

3. **Optimized sends (OpSendPlus through OpSendClass):** 19 cases, all identical 5-line pattern. Extract a single helper that handles the common unwinding check.

The extracted methods return a `Value` and a `bool` indicating whether `runFrame()` should return (for unwinding/returns). This avoids needing to return multiple signals.

### File Structure

- **Modify:** `vm/interpreter.go` — slim down `runFrame()`, add handler methods
- **Create:** `vm/interpreter_ops.go` — extracted handler methods (globals, class vars, sends, blocks, returns)
- **No changes to:** `vm/aot.go` (AOT compiler has its own code generation; it doesn't call runFrame handlers)

---

### Task 1: Establish baseline — run tests and benchmarks

**Files:**
- Read: `vm/interpreter.go`, `vm/benchmark_test.go`

- [ ] **Step 1: Run the full test suite to confirm green baseline**

Run: `go test ./vm/ -count=1 -timeout 120s`
Expected: PASS

- [ ] **Step 2: Run hot-path benchmarks and save baseline**

Run: `go test -bench=BenchmarkHotPath -run='^$' -count=5 -benchtime=100ms ./vm/ > /tmp/runframe-baseline.txt`
Expected: Benchmark output saved

- [ ] **Step 3: Run the interpreter-specific tests**

Run: `go test ./vm/ -run='TestInterpreter' -v -count=1 2>&1 | tail -5`
Expected: All pass

---

### Task 2: Create `vm/interpreter_ops.go` with global variable handlers

**Files:**
- Create: `vm/interpreter_ops.go`

- [ ] **Step 1: Create the new file with the global variable handler methods**

Create `vm/interpreter_ops.go` with this content:

```go
package vm

import "encoding/binary"

// ---------------------------------------------------------------------------
// Extracted opcode handlers for runFrame()
//
// Each handler receives the current frame state and operates on the
// interpreter's stack. Handlers that may cause runFrame() to return
// use a (Value, bool) return signature where bool=true means "return now".
// ---------------------------------------------------------------------------

// execPushGlobal handles OpPushGlobal: push a global variable onto the stack.
func (i *Interpreter) execPushGlobal(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) >= len(literals) {
		i.push(Nil)
		return
	}
	// Get global name from literal
	globalName := ""
	lit := literals[idx]
	if lit.IsSymbol() && i.Symbols != nil {
		globalName = i.Symbols.Name(lit.SymbolID())
	} else if IsStringValue(lit) {
		globalName = i.vm.registry.GetStringContent(lit)
	}
	if globalName == "" {
		i.push(Nil)
		return
	}
	if i.hidden != nil && i.hidden[globalName] {
		i.push(Nil)
		return
	}
	if i.localWrites != nil {
		if val, ok := i.localWrites[globalName]; ok {
			i.push(val)
			return
		}
	}
	if i.vm != nil {
		i.vm.globalsMu.RLock()
	}
	val, ok := i.Globals[globalName]
	if i.vm != nil {
		i.vm.globalsMu.RUnlock()
	}
	if ok {
		i.push(val)
	} else {
		i.push(Nil)
	}
}

// execStoreGlobal handles OpStoreGlobal: store TOS into a global variable.
func (i *Interpreter) execStoreGlobal(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) >= len(literals) {
		return
	}
	// Get global name from literal
	globalName := ""
	lit := literals[idx]
	if lit.IsSymbol() && i.Symbols != nil {
		globalName = i.Symbols.Name(lit.SymbolID())
	} else if IsStringValue(lit) {
		globalName = i.vm.registry.GetStringContent(lit)
	}
	if globalName == "" {
		return
	}
	if i.hidden != nil && i.hidden[globalName] {
		// silently deny write to restricted name
		return
	}
	if i.forked {
		// Forked process: write to process-local overlay
		if i.localWrites == nil {
			i.localWrites = make(map[string]Value)
		}
		i.localWrites[globalName] = i.top()
	} else {
		// Main interpreter: write directly to shared Globals
		if i.vm != nil {
			i.vm.globalsMu.Lock()
		}
		i.Globals[globalName] = i.top()
		if i.vm != nil {
			i.vm.globalsMu.Unlock()
		}
	}
}

// execPushClassVar handles OpPushClassVar: push a class variable onto the stack.
func (i *Interpreter) execPushClassVar(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) >= len(literals) {
		i.push(Nil)
		return
	}
	varName := ""
	lit := literals[idx]
	if lit.IsSymbol() && i.Symbols != nil {
		varName = i.Symbols.Name(lit.SymbolID())
	}
	if varName == "" {
		i.push(Nil)
		return
	}
	class := frame.Method.Class()
	if class != nil {
		i.push(class.GetClassVar(i.vm.registry, varName))
	} else {
		i.push(Nil)
	}
}

// execStoreClassVar handles OpStoreClassVar: store TOS into a class variable.
func (i *Interpreter) execStoreClassVar(frame *CallFrame, bc []byte, literals []Value) {
	idx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	if int(idx) >= len(literals) {
		return
	}
	varName := ""
	lit := literals[idx]
	if lit.IsSymbol() && i.Symbols != nil {
		varName = i.Symbols.Name(lit.SymbolID())
	}
	if varName == "" {
		return
	}
	class := frame.Method.Class()
	if class != nil {
		class.SetClassVar(i.vm.registry, varName, i.top())
	}
}
```

- [ ] **Step 2: Wire up the new handlers in runFrame()**

In `vm/interpreter.go`, replace the `case OpPushGlobal:` through end of `case OpStoreClassVar:` blocks (lines 697-873) with:

```go
		case OpPushGlobal:
			i.execPushGlobal(frame, bc, literals)

		case OpStoreGlobal:
			i.execStoreGlobal(frame, bc, literals)

		// ... (OpPushCaptured through OpCellSet remain inline — they're short) ...

		case OpPushClassVar:
			i.execPushClassVar(frame, bc, literals)

		case OpStoreClassVar:
			i.execStoreClassVar(frame, bc, literals)
```

Note: The cases between OpStoreGlobal and OpPushClassVar (OpPushCaptured, OpStoreCaptured, OpMakeCell, OpCellGet, OpCellSet) stay inline — they're all short.

- [ ] **Step 3: Run tests to verify no regression**

Run: `go test ./vm/ -count=1 -timeout 120s`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add vm/interpreter_ops.go vm/interpreter.go
git commit -m "refactor: extract global and class var opcode handlers from runFrame()"
```

---

### Task 3: Extract send operation handlers

**Files:**
- Modify: `vm/interpreter_ops.go`
- Modify: `vm/interpreter.go`

- [ ] **Step 1: Add send handlers to `interpreter_ops.go`**

Append to `vm/interpreter_ops.go`:

```go
// execTailSend handles OpTailSend: tail-call optimized message send.
// Returns (result, shouldContinue, shouldReturn).
// shouldContinue=true means the for loop should continue (tail call optimized).
// shouldReturn=true means runFrame() should return Nil (unwinding).
// Otherwise, push result onto stack.
func (i *Interpreter) execTailSend(frame *CallFrame, bc []byte, isBlock bool) (Value, bool, bool) {
	sel := int(binary.LittleEndian.Uint16(bc[frame.IP:]))
	frame.IP += 2
	argc := int(bc[frame.IP])
	frame.IP++

	// Copy args into a stack-local buffer to avoid allocation.
	var tailBuf [16]Value
	var tailArgs []Value
	if argc <= len(tailBuf) {
		copy(tailBuf[:argc], i.peekN(argc))
		tailArgs = tailBuf[:argc]
		i.dropN(argc)
	} else {
		tailArgs = i.popN(argc)
	}
	tailRcvr := i.pop()

	// Only optimize if:
	// 1. We're in a method frame (not a block)
	// 2. The receiver is self (same as current frame's receiver)
	// 3. The target method is the same CompiledMethod as the current one
	if !isBlock && tailRcvr == frame.Receiver {
		vt := i.vtableFor(tailRcvr)
		if vt != nil {
			tailMethod := vt.Lookup(sel)
			if cm, ok := tailMethod.(*CompiledMethod); ok && cm == frame.Method {
				// Self-recursive tail call: reuse the current frame
				i.sp = frame.BP
				for _, arg := range tailArgs {
					i.push(arg)
				}
				for j := len(tailArgs); j < cm.NumTemps; j++ {
					i.push(Nil)
				}
				frame.IP = 0
				return Nil, true, false // continue the loop
			}
		}
	}

	// Fallback: non-self-recursive — do a normal send
	i.push(tailRcvr)
	for _, arg := range tailArgs {
		i.push(arg)
	}
	tailResult := i.send(sel, argc)
	if i.unwinding {
		i.popFrame()
		return Nil, false, true // return from runFrame
	}
	return tailResult, false, false // push result
}

// execSendSuper handles OpSendSuper: send to superclass implementation.
// Returns (result, shouldReturn). shouldReturn=true means runFrame() should return Nil.
func (i *Interpreter) execSendSuper(frame *CallFrame, bc []byte, isBlock bool) (Value, bool) {
	sel := int(binary.LittleEndian.Uint16(bc[frame.IP:]))
	frame.IP += 2
	argc := int(bc[frame.IP])
	frame.IP++
	var method *CompiledMethod
	if isBlock {
		if frame.HomeFrame >= 0 && frame.HomeFrame < len(i.frames) && i.frames[frame.HomeFrame] != nil {
			method = i.frames[frame.HomeFrame].Method
		}
	} else {
		method = frame.Method
	}
	if method != nil {
		result := i.sendSuper(sel, argc, method)
		if i.unwinding {
			i.popFrame()
			return Nil, true
		}
		return result, false
	}
	return Nil, false
}
```

- [ ] **Step 2: Wire up send handlers in runFrame()**

In `vm/interpreter.go`, replace the `case OpTailSend:` block (lines 888-957) with:

```go
		case OpTailSend:
			result, doContinue, doReturn := i.execTailSend(frame, bc, isBlock)
			if doContinue {
				continue
			}
			if doReturn {
				return Nil
			}
			i.push(result)
```

Replace the `case OpSendSuper:` block (lines 959-982) with:

```go
		case OpSendSuper:
			result, doReturn := i.execSendSuper(frame, bc, isBlock)
			if doReturn {
				return Nil
			}
			i.push(result)
```

- [ ] **Step 3: Run tests**

Run: `go test ./vm/ -count=1 -timeout 120s`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add vm/interpreter_ops.go vm/interpreter.go
git commit -m "refactor: extract tail send and super send handlers from runFrame()"
```

---

### Task 4: Extract optimized send helper

**Files:**
- Modify: `vm/interpreter_ops.go`
- Modify: `vm/interpreter.go`

- [ ] **Step 1: Add the optimized binary send helper to `interpreter_ops.go`**

Append to `vm/interpreter_ops.go`:

```go
// execOptimizedBinarySend handles all OpSend{Plus,Minus,...} opcodes.
// Returns (result, shouldReturn). shouldReturn=true means runFrame() should return Nil.
func (i *Interpreter) execOptimizedBinarySend(primFn func(a, b Value) Value) (Value, bool) {
	b := i.pop()
	a := i.pop()
	result := primFn(a, b)
	if i.unwinding {
		i.popFrame()
		return Nil, true
	}
	return result, false
}

// execOptimizedUnarySend handles OpSend{Size,Value,New,Class}.
// Returns (result, shouldReturn).
func (i *Interpreter) execOptimizedUnarySend(primFn func(rcvr Value) Value) (Value, bool) {
	rcvr := i.pop()
	result := primFn(rcvr)
	if i.unwinding {
		i.popFrame()
		return Nil, true
	}
	return result, false
}
```

- [ ] **Step 2: Wire up optimized sends in runFrame()**

Replace the 11 arithmetic + 2 collection binary send cases (OpSendPlus through OpSendNE, OpSendAt, OpSendAtPut) and the 4 unary send cases (OpSendSize, OpSendValue, OpSendNew, OpSendClass) with the helper. Each case becomes 5 lines:

```go
		case OpSendPlus:
			if result, doReturn := i.execOptimizedBinarySend(i.primitivePlus); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendMinus:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveMinus); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendTimes:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveTimes); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendDiv:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveDiv); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendMod:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveMod); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendLT:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveLT); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendGT:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveGT); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendLE:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveLE); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendGE:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveGE); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendEQ:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveEQ); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendNE:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveNE); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendAt:
			if result, doReturn := i.execOptimizedBinarySend(i.primitiveAt); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendAtPut:
			val := i.pop()
			idx := i.pop()
			rcvr := i.pop()
			result := i.primitiveAtPut(rcvr, idx, val)
			if i.unwinding {
				i.popFrame()
				return Nil
			}
			i.push(result)

		case OpSendSize:
			if result, doReturn := i.execOptimizedUnarySend(i.primitiveSize); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendValue:
			if result, doReturn := i.execOptimizedUnarySend(i.primitiveValue); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendValue1:
			arg := i.pop()
			rcvr := i.pop()
			result := i.primitiveValue1(rcvr, arg)
			if i.unwinding {
				i.popFrame()
				return Nil
			}
			i.push(result)

		case OpSendValue2:
			arg2 := i.pop()
			arg1 := i.pop()
			rcvr := i.pop()
			result := i.primitiveValue2(rcvr, arg1, arg2)
			if i.unwinding {
				i.popFrame()
				return Nil
			}
			i.push(result)

		case OpSendNew:
			if result, doReturn := i.execOptimizedUnarySend(i.primitiveNew); doReturn {
				return Nil
			} else {
				i.push(result)
			}

		case OpSendClass:
			if result, doReturn := i.execOptimizedUnarySend(i.primitiveClass); doReturn {
				return Nil
			} else {
				i.push(result)
			}
```

Note: OpSendAtPut (3 args), OpSendValue1 (2 args), OpSendValue2 (3 args) don't fit the binary/unary helper pattern — keep them inline since they're already short.

- [ ] **Step 3: Run tests**

Run: `go test ./vm/ -count=1 -timeout 120s`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add vm/interpreter_ops.go vm/interpreter.go
git commit -m "refactor: extract optimized send opcode pattern into helpers"
```

---

### Task 5: Extract return and block creation handlers

**Files:**
- Modify: `vm/interpreter_ops.go`
- Modify: `vm/interpreter.go`

- [ ] **Step 1: Add return and block handlers to `interpreter_ops.go`**

Append to `vm/interpreter_ops.go`:

```go
// execReturnTop handles OpReturnTop: return TOS, with NLR support for blocks.
// Always causes runFrame() to return. Returns the value to return.
func (i *Interpreter) execReturnTop(frame *CallFrame, isBlock bool) Value {
	result := i.pop()
	if isBlock {
		if frame.HomeFrame == -1 {
			// Detached block: treat ^ as local return
			i.popFrame()
			return result
		}
		// Non-local return: set unwinding flag
		i.popFrame()
		i.unwinding = true
		i.unwindValue = result
		i.unwindTarget = frame.HomeFrame
		return Nil
	}
	// Method: local return
	i.popFrame()
	return result
}

// execCreateBlock handles OpCreateBlock: create a block closure.
func (i *Interpreter) execCreateBlock(frame *CallFrame, bc []byte, isBlock bool) {
	methodIdx := binary.LittleEndian.Uint16(bc[frame.IP:])
	frame.IP += 2
	nCaptures := int(bc[frame.IP])
	frame.IP++

	var block *BlockMethod
	if isBlock {
		if frame.HomeMethod != nil && int(methodIdx) < len(frame.HomeMethod.Blocks) {
			block = frame.HomeMethod.Blocks[methodIdx]
		} else if frame.HomeFrame >= 0 && frame.HomeFrame < len(i.frames) && i.frames[frame.HomeFrame] != nil {
			homeMethod := i.frames[frame.HomeFrame].Method
			if homeMethod != nil && int(methodIdx) < len(homeMethod.Blocks) {
				block = homeMethod.Blocks[methodIdx]
			}
		}
	} else {
		block = frame.Method.GetBlock(int(methodIdx))
	}

	captures := i.popN(nCaptures)

	if block != nil {
		blockVal := i.createBlockValue(block, captures)
		i.push(blockVal)
	} else {
		i.push(Nil)
	}
}
```

- [ ] **Step 2: Wire up in runFrame()**

Replace `case OpReturnTop:` block (lines 1212-1232) with:

```go
		case OpReturnTop:
			return i.execReturnTop(frame, isBlock)
```

Replace `case OpCreateBlock:` block (lines 1250-1283) with:

```go
		case OpCreateBlock:
			i.execCreateBlock(frame, bc, isBlock)
```

- [ ] **Step 3: Run tests**

Run: `go test ./vm/ -count=1 -timeout 120s`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add vm/interpreter_ops.go vm/interpreter.go
git commit -m "refactor: extract return and block creation handlers from runFrame()"
```

---

### Task 6: Verify performance — run benchmarks and compare

**Files:**
- Read: `/tmp/runframe-baseline.txt`

- [ ] **Step 1: Run benchmarks again**

Run: `go test -bench=BenchmarkHotPath -run='^$' -count=5 -benchtime=100ms ./vm/ > /tmp/runframe-after.txt`

- [ ] **Step 2: Compare with baseline using benchstat**

Run: `benchstat /tmp/runframe-baseline.txt /tmp/runframe-after.txt`

Expected: No statistically significant regressions. Small regressions (<5%) in `execPushGlobal`/`execTailSend` are acceptable due to function call overhead; the Go compiler may inline them anyway.

If a regression >10% is found: consider reverting the specific extraction and keeping that case inline. The most performance-sensitive opcodes are `OpSendPlus` through `OpSendNE` (arithmetic hot path) — these use the helper pattern which should be inlineable.

- [ ] **Step 3: Run the full test suite one final time**

Run: `go test ./vm/ -count=1 -timeout 120s && go test ./... -count=1 -timeout 300s`
Expected: All PASS

- [ ] **Step 4: Commit the plan file**

```bash
git add docs/plans/2026-04-14-runframe-refactor.md docs/plans/2026-04-14-top-10-improvements.md
git commit -m "docs: add runFrame refactor plan and top-10 improvements list"
```
