# JIT Compilation Implementation Plan for Maggie

## Executive Summary

Based on traditional Smalltalk JIT implementations (Cog VM, VisualWorks), we'll implement a **tiered adaptive compilation** system:

1. **Inline Caching** - Monomorphic → Polymorphic → Megamorphic (90%/9%/1% of call sites)
2. **Method & Block Profiling** - Track invocations, compile after threshold
3. **Adaptive AOT** - Compile hot methods AND blocks using existing AOT compiler
4. **Deferred Loading** - Persist compiled code for next startup (like Sista snapshots)

This follows proven Smalltalk VM architecture adapted for Go's constraints.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Maggie VM Execution                       │
├─────────────────────────────────────────────────────────────┤
│  Tier 0: Interpreter + Inline Cache                         │
│    - All methods/blocks start here                           │
│    - Monomorphic IC on first call (cache class+method)       │
│    - Upgrade to PIC after 2nd different class                │
│    - Profiling counters increment                            │
├─────────────────────────────────────────────────────────────┤
│  Tier 1: Polymorphic Inline Cache (PIC)                      │
│    - Up to 6 cached (class, method) pairs per call site      │
│    - Falls back to megamorphic (full lookup) if exceeded     │
│    - ~10% speedup over monomorphic-only                      │
├─────────────────────────────────────────────────────────────┤
│  Tier 2: AOT Compiled (hot methods & blocks)                 │
│    - Methods/blocks exceeding invocation threshold           │
│    - Generated Go code via existing AOT compiler             │
│    - Written to file, loaded on next startup                 │
└─────────────────────────────────────────────────────────────┘
```

## Phase 1: Inline Caching (2-3 days)

### Goal
Implement monomorphic and polymorphic inline caches following Cog VM's proven approach where ~90% of call sites are monomorphic.

### Key Insight from Smalltalk
- Monomorphic IC: Cache single (class, method) pair at call site
- Polymorphic IC (PIC): Cache up to 6 pairs when site sees multiple types
- Megamorphic: Fall back to full vtable lookup (rare, ~1% of sites)

### Files to Create/Modify
- `vm/inline_cache.go` (new) - IC data structures
- `vm/interpreter.go` - Instrument send opcodes
- `vm/inline_cache_test.go` (new) - Tests and benchmarks

### Implementation

```go
// vm/inline_cache.go

// CacheState represents the current state of an inline cache
type CacheState uint8

const (
    CacheEmpty      CacheState = iota // No cached lookup yet
    CacheMonomorphic                   // Single (class, method) cached
    CachePolymorphic                   // 2-6 entries in PIC
    CacheMegamorphic                   // Too many types, use full lookup
)

const MaxPICEntries = 6 // Cog uses 6

// InlineCacheEntry holds a single cached method lookup
type InlineCacheEntry struct {
    Class  *Class
    Method Method
}

// InlineCache represents the cache state for a single call site
type InlineCache struct {
    State   CacheState
    Entries [MaxPICEntries]InlineCacheEntry
    Count   int    // Number of valid entries (1 for mono, 2-6 for poly)
    Hits    uint64 // Cache hits (for profiling)
    Misses  uint64 // Cache misses
}

// Lookup checks the cache for a method, returns nil on miss
func (ic *InlineCache) Lookup(class *Class) Method {
    switch ic.State {
    case CacheMonomorphic:
        if ic.Entries[0].Class == class {
            ic.Hits++
            return ic.Entries[0].Method
        }
    case CachePolymorphic:
        for i := 0; i < ic.Count; i++ {
            if ic.Entries[i].Class == class {
                ic.Hits++
                return ic.Entries[i].Method
            }
        }
    case CacheMegamorphic:
        // Always miss, use full lookup
    }
    ic.Misses++
    return nil
}

// Update records a new (class, method) pair, potentially upgrading state
func (ic *InlineCache) Update(class *Class, method Method) {
    switch ic.State {
    case CacheEmpty:
        ic.State = CacheMonomorphic
        ic.Entries[0] = InlineCacheEntry{class, method}
        ic.Count = 1

    case CacheMonomorphic:
        if ic.Entries[0].Class == class {
            return // Already cached
        }
        // Upgrade to polymorphic
        ic.State = CachePolymorphic
        ic.Entries[1] = InlineCacheEntry{class, method}
        ic.Count = 2

    case CachePolymorphic:
        // Check if already present
        for i := 0; i < ic.Count; i++ {
            if ic.Entries[i].Class == class {
                return
            }
        }
        if ic.Count < MaxPICEntries {
            ic.Entries[ic.Count] = InlineCacheEntry{class, method}
            ic.Count++
        } else {
            // Too many types, go megamorphic
            ic.State = CacheMegamorphic
        }
    }
}

// InlineCacheTable manages caches for all call sites in a method
type InlineCacheTable struct {
    caches map[int]*InlineCache // bytecode PC -> cache
}

func NewInlineCacheTable() *InlineCacheTable {
    return &InlineCacheTable{
        caches: make(map[int]*InlineCache),
    }
}

func (t *InlineCacheTable) GetOrCreate(pc int) *InlineCache {
    if ic := t.caches[pc]; ic != nil {
        return ic
    }
    ic := &InlineCache{State: CacheEmpty}
    t.caches[pc] = ic
    return ic
}
```

### Interpreter Integration

```go
// In interpreter.go, modify send()

func (i *Interpreter) send(selector int, argc int) Value {
    frame := i.frames[i.fp]
    pc := frame.IP - 4 // PC of the send instruction (before operands)

    // Pop args and get receiver
    args := i.popN(argc)
    rcvr := i.pop()
    class := i.classFor(rcvr)

    // Check inline cache
    var method Method
    ic := frame.Method.InlineCaches.GetOrCreate(pc)

    if cachedMethod := ic.Lookup(class); cachedMethod != nil {
        method = cachedMethod // Cache hit!
    } else {
        // Cache miss - do full lookup
        method = class.VTable.Lookup(selector)
        if method != nil {
            ic.Update(class, method)
        }
    }

    // ... rest of dispatch logic ...
}
```

### Verification
- Measure cache hit rates on compiler self-compile (expect ~90% monomorphic)
- Benchmark dispatch with/without IC
- Test polymorphic sites (e.g., `printOn:` called on multiple types)

---

## Phase 2: Method & Block Profiling (2 days)

### Goal
Track invocation counts for methods AND blocks to identify hot code for compilation.

### Key Insight from Smalltalk
- Cog compiles methods after just 2 invocations
- Blocks are critical - they're loop bodies, conditionals, callbacks
- Profile at method/block level, not call-site level

### Files to Create/Modify
- `vm/profiler.go` (new) - Profiling infrastructure
- `vm/interpreter.go` - Instrument execution
- `vm/compiled_method.go` - Add invocation counter field

### Implementation

```go
// vm/profiler.go

type MethodProfile struct {
    InvocationCount uint64
    IsHot           bool      // Exceeded threshold
    CompiledAt      time.Time // When AOT was generated (if ever)
}

type BlockProfile struct {
    InvocationCount uint64
    IsHot           bool
    OwningMethod    *CompiledMethod
    BlockIndex      int
}

type Profiler struct {
    methodProfiles sync.Map // *CompiledMethod -> *MethodProfile
    blockProfiles  sync.Map // *BlockMethod -> *BlockProfile

    // Configuration (following Cog's low threshold approach)
    MethodHotThreshold uint64 // Default: 100 (Cog uses 2, but we have compilation overhead)
    BlockHotThreshold  uint64 // Default: 500 (blocks often in tight loops)

    // Callback when something becomes hot
    OnHot func(code interface{}, profile interface{})
}

func (p *Profiler) RecordMethodInvocation(method *CompiledMethod) {
    val, _ := p.methodProfiles.LoadOrStore(method, &MethodProfile{})
    profile := val.(*MethodProfile)

    count := atomic.AddUint64(&profile.InvocationCount, 1)

    if !profile.IsHot && count >= p.MethodHotThreshold {
        profile.IsHot = true
        if p.OnHot != nil {
            p.OnHot(method, profile)
        }
    }
}

func (p *Profiler) RecordBlockInvocation(block *BlockMethod, owner *CompiledMethod, idx int) {
    val, _ := p.blockProfiles.LoadOrStore(block, &BlockProfile{
        OwningMethod: owner,
        BlockIndex:   idx,
    })
    profile := val.(*BlockProfile)

    count := atomic.AddUint64(&profile.InvocationCount, 1)

    if !profile.IsHot && count >= p.BlockHotThreshold {
        profile.IsHot = true
        if p.OnHot != nil {
            p.OnHot(block, profile)
        }
    }
}
```

### Verification
- Profile compiler self-compile, verify hot methods detected
- Check that loop blocks (whileTrue:) are marked hot
- Measure profiling overhead (<5%)

---

## Phase 3: Adaptive AOT Compilation (3-4 days)

### Goal
Automatically compile hot methods and blocks using existing AOT compiler.

### Key Insight from Smalltalk
- Sista persists optimized code to snapshots (similar to our file-based approach)
- Blocks need special handling for captures and non-local returns
- Compilation happens in background, doesn't block execution

### Files to Create/Modify
- `vm/jit.go` (new) - JIT controller
- `vm/aot.go` - Add block compilation support
- `vm/vm.go` - Integration

### Implementation

```go
// vm/jit.go

type JITCompiler struct {
    vm       *VM
    aot      *AOTCompiler
    profiler *Profiler

    // Compilation queue
    pending chan interface{} // *CompiledMethod or *BlockMethod

    // Output - hot code registry
    hotMethods map[string]string // "Class>>method" -> Go source
    hotBlocks  map[string]string // "Class>>method[N]" -> Go source

    // Configuration
    OutputDir string // Where to write compiled code
}

func NewJITCompiler(vm *VM) *JITCompiler {
    jit := &JITCompiler{
        vm:         vm,
        aot:        NewAOTCompiler(vm.Selectors, vm.Symbols),
        pending:    make(chan interface{}, 100),
        hotMethods: make(map[string]string),
        hotBlocks:  make(map[string]string),
        OutputDir:  "generated/aot",
    }

    // Start background compilation worker
    go jit.compilationWorker()

    return jit
}

func (jit *JITCompiler) compilationWorker() {
    for code := range jit.pending {
        switch c := code.(type) {
        case *CompiledMethod:
            jit.compileMethod(c)
        case *BlockMethod:
            jit.compileBlock(c)
        }
    }
}

func (jit *JITCompiler) compileMethod(method *CompiledMethod) {
    className := method.Class().Name
    methodName := method.Name()
    key := fmt.Sprintf("%s>>%s", className, methodName)

    goCode := jit.aot.CompileMethod(method, className, methodName)
    jit.hotMethods[key] = goCode

    log.Printf("JIT: Compiled hot method %s", key)
}

func (jit *JITCompiler) compileBlock(block *BlockMethod) {
    // Block compilation - need to handle captures
    // This extends the AOT compiler
    // ...
}

func (jit *JITCompiler) OnHotCode(code interface{}, profile interface{}) {
    select {
    case jit.pending <- code:
    default:
        // Queue full, skip this one
    }
}

// WriteCompiledCode writes all hot code to files for next startup
func (jit *JITCompiler) WriteCompiledCode() error {
    // Write Go source files that can be compiled into the binary
    // Similar to Sista persisting to snapshots
    // ...
}
```

### Block Compilation

Extend `vm/aot.go` to handle blocks:

```go
// CompileBlock generates Go code for a hot block
func (c *AOTCompiler) CompileBlock(block *BlockMethod, className, methodName string, blockIndex int) string {
    // Similar to CompileMethod but:
    // 1. Takes captures as parameter
    // 2. Handles OpPushCaptured/OpStoreCaptured
    // 3. Handles non-local returns (OpBlockReturn)

    funcName := fmt.Sprintf("aot_%s_%s_block%d",
        c.sanitizeName(className),
        c.sanitizeName(methodName),
        blockIndex)

    // Generate function with captures parameter
    c.writeLine("func %s(vm *VM, self Value, args []Value, captures []Value) Value {", funcName)
    // ... bytecode translation ...
}
```

### Verification
- Enable JIT, run compiler self-compile
- Verify hot methods written to files
- Test that block compilation handles captures correctly

---

## Phase 4: Compiled Code Loading (2 days)

### Goal
Load previously compiled code on startup (like Sista loading from snapshots).

### Implementation

```go
// vm/jit_loader.go

// LoadCompiledMethods loads AOT-compiled methods from generated files
func (vm *VM) LoadCompiledMethods(dir string) error {
    // Option A: Generated Go files compiled into binary
    // - Requires rebuild, but zero runtime overhead

    // Option B: Go plugin loading
    // - No rebuild needed, but complex

    // For MVP, use Option A with code generation
    return nil
}

// GenerateAOTPackage creates a Go package with all hot methods
func (jit *JITCompiler) GenerateAOTPackage(packagePath string) error {
    var sb strings.Builder

    sb.WriteString("// Code generated by Maggie JIT. DO NOT EDIT.\n")
    sb.WriteString("package aot\n\n")
    sb.WriteString("import . \"github.com/chazu/maggie/vm\"\n\n")

    // Write all hot methods
    for key, code := range jit.hotMethods {
        sb.WriteString(fmt.Sprintf("// %s\n", key))
        sb.WriteString(code)
        sb.WriteString("\n")
    }

    // Write dispatch table
    sb.WriteString("func RegisterAll(vm *VM) {\n")
    sb.WriteString("\ttable := make(AOTDispatchTable)\n")
    for key := range jit.hotMethods {
        // Parse key to get class and method names
        // ... generate table entries ...
    }
    sb.WriteString("\tvm.RegisterAOTMethods(table)\n")
    sb.WriteString("}\n")

    return os.WriteFile(packagePath, []byte(sb.String()), 0644)
}
```

### Verification
- Generate AOT package from hot methods
- Rebuild with package, verify methods load
- Benchmark with/without precompiled methods

---

## File Summary

| File | Purpose | Phase |
|------|---------|-------|
| `vm/inline_cache.go` | Mono/Poly/Mega inline caches | 1 |
| `vm/inline_cache_test.go` | IC tests and benchmarks | 1 |
| `vm/profiler.go` | Method/block invocation tracking | 2 |
| `vm/jit.go` | JIT controller, background compilation | 3 |
| `vm/jit_loader.go` | Load compiled code on startup | 4 |
| `vm/aot.go` | Extend for block compilation | 3 |
| `vm/interpreter.go` | Instrument for IC and profiling | 1-2 |
| `vm/compiled_method.go` | Add InlineCaches field | 1 |

---

## Expected Performance Gains

| Phase | Technique | Expected Speedup | Based On |
|-------|-----------|------------------|----------|
| 1 | Inline Caching | 1.5-2x dispatch | Cog: 90% monomorphic |
| 1 | Polymorphic IC | +10% over mono | Cog benchmarks |
| 2 | Profiling | ~0% (overhead) | - |
| 3 | Hot Method AOT | 5-10x on hot code | Our AOT benchmarks |
| 3 | Hot Block AOT | 5-10x on loops | Critical for perf |
| 4 | Preloaded AOT | Instant warm start | Like Sista snapshots |

---

## Implementation Order

1. **Phase 1a: Monomorphic IC** - Single entry cache, quick win
2. **Phase 1b: Polymorphic IC** - Extend to 6 entries
3. **Phase 2: Profiling** - Count invocations, detect hot code
4. **Phase 3a: Method AOT** - Compile hot methods
5. **Phase 3b: Block AOT** - Compile hot blocks (loops!)
6. **Phase 4: Persistence** - Write/load compiled code

---

## Beads Issues to Create

1. `Implement monomorphic inline cache` - Phase 1a
2. `Implement polymorphic inline cache (PIC)` - Phase 1b
3. `Add method/block profiling` - Phase 2
4. `Create JIT controller for hot method compilation` - Phase 3a
5. `Add block compilation to AOT` - Phase 3b
6. `Implement compiled code persistence` - Phase 4
7. `Epic: JIT compilation system` - Parent issue
