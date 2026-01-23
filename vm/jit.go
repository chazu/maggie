package vm

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
)

// JITCompiler manages adaptive compilation of hot methods and blocks.
// It connects the profiler (which detects hot code) to the AOT compiler
// (which generates Go code), following Cog VM's approach of compiling
// frequently-executed code.
type JITCompiler struct {
	vm       *VM
	aot      *AOTCompiler
	profiler *Profiler

	// Compilation queue for background processing
	pending chan jitWorkItem
	done    chan struct{}

	// Compiled code registry
	mu           sync.RWMutex
	hotMethods   map[string]string // "Class>>method" -> Go source
	hotBlocks    map[string]string // "Class>>method[N]" -> Go source
	compiledKeys map[string]bool   // Track what's been compiled

	// Statistics
	methodsCompiled uint64
	blocksCompiled  uint64
	compilationTime uint64 // nanoseconds

	// Configuration
	OutputDir      string // Where to write compiled code (empty = don't write)
	Enabled        bool   // Master switch for JIT
	CompileBlocks  bool   // Whether to compile hot blocks (not just methods)
	LogCompilation bool   // Log when methods are compiled
}

// jitWorkItem represents a unit of compilation work.
type jitWorkItem struct {
	method     *CompiledMethod
	block      *BlockMethod
	blockIndex int
}

// NewJITCompiler creates a new JIT compiler connected to a VM.
func NewJITCompiler(vm *VM) *JITCompiler {
	jit := &JITCompiler{
		vm:           vm,
		aot:          NewAOTCompiler(vm.Selectors, vm.Symbols),
		profiler:     vm.GetProfiler(),
		pending:      make(chan jitWorkItem, 100),
		done:         make(chan struct{}),
		hotMethods:   make(map[string]string),
		hotBlocks:    make(map[string]string),
		compiledKeys: make(map[string]bool),
		Enabled:      true,
		CompileBlocks: false, // Blocks need more work for captures
		LogCompilation: false,
	}

	// Connect to profiler
	if jit.profiler != nil {
		jit.profiler.OnHot = jit.onHotCode
	}

	// Start background compilation worker
	go jit.compilationWorker()

	return jit
}

// onHotCode is called by the profiler when code becomes hot.
func (jit *JITCompiler) onHotCode(code interface{}, profile interface{}) {
	if !jit.Enabled {
		return
	}

	switch c := code.(type) {
	case *CompiledMethod:
		jit.queueMethod(c)
	case *BlockMethod:
		if jit.CompileBlocks {
			if bp, ok := profile.(*BlockProfile); ok {
				jit.queueBlock(c, bp.OwningMethod, bp.BlockIndex)
			}
		}
	}
}

// queueMethod adds a method to the compilation queue.
func (jit *JITCompiler) queueMethod(method *CompiledMethod) {
	if method == nil || method.Class() == nil {
		return
	}

	key := jit.methodKey(method)

	// Check if already compiled
	jit.mu.RLock()
	compiled := jit.compiledKeys[key]
	jit.mu.RUnlock()

	if compiled {
		return
	}

	select {
	case jit.pending <- jitWorkItem{method: method}:
	default:
		// Queue full, skip this one
	}
}

// queueBlock adds a block to the compilation queue.
func (jit *JITCompiler) queueBlock(block *BlockMethod, owner *CompiledMethod, blockIndex int) {
	if block == nil {
		return
	}

	key := jit.blockKey(owner, blockIndex)

	// Check if already compiled
	jit.mu.RLock()
	compiled := jit.compiledKeys[key]
	jit.mu.RUnlock()

	if compiled {
		return
	}

	select {
	case jit.pending <- jitWorkItem{block: block, method: owner, blockIndex: blockIndex}:
	default:
		// Queue full, skip this one
	}
}

// compilationWorker processes the compilation queue in the background.
func (jit *JITCompiler) compilationWorker() {
	for {
		select {
		case work := <-jit.pending:
			if work.method != nil && work.block == nil {
				jit.compileMethod(work.method)
			} else if work.block != nil {
				jit.compileBlock(work.block, work.method, work.blockIndex)
			}
		case <-jit.done:
			return
		}
	}
}

// compileMethod compiles a hot method.
func (jit *JITCompiler) compileMethod(method *CompiledMethod) {
	if method.Class() == nil {
		return
	}

	className := method.Class().Name
	methodName := method.Name()
	key := jit.methodKey(method)

	// Mark as compiled (even before we're done, to prevent duplicates)
	jit.mu.Lock()
	if jit.compiledKeys[key] {
		jit.mu.Unlock()
		return
	}
	jit.compiledKeys[key] = true
	jit.mu.Unlock()

	// Generate Go code
	goCode := jit.aot.CompileMethod(method, className, methodName)

	// Store compiled code
	jit.mu.Lock()
	jit.hotMethods[key] = goCode
	jit.mu.Unlock()

	atomic.AddUint64(&jit.methodsCompiled, 1)

	if jit.LogCompilation {
		log.Printf("JIT: Compiled hot method %s", key)
	}

	// Write to file if output directory is set
	if jit.OutputDir != "" {
		jit.writeMethodToFile(className, methodName, goCode)
	}
}

// compileBlock compiles a hot block.
func (jit *JITCompiler) compileBlock(block *BlockMethod, owner *CompiledMethod, blockIndex int) {
	if owner == nil || owner.Class() == nil {
		return
	}

	key := jit.blockKey(owner, blockIndex)
	className := owner.Class().Name
	methodName := owner.Name()

	// Mark as compiled
	jit.mu.Lock()
	if jit.compiledKeys[key] {
		jit.mu.Unlock()
		return
	}
	jit.compiledKeys[key] = true
	jit.mu.Unlock()

	// Generate Go code for the block
	goCode := jit.aot.CompileBlock(block, className, methodName, blockIndex)

	// Store compiled code
	jit.mu.Lock()
	jit.hotBlocks[key] = goCode
	jit.mu.Unlock()

	atomic.AddUint64(&jit.blocksCompiled, 1)

	if jit.LogCompilation {
		log.Printf("JIT: Compiled hot block %s", key)
	}

	// Write to file if output directory is set
	if jit.OutputDir != "" {
		jit.writeBlockToFile(className, methodName, blockIndex, goCode)
	}
}

// writeBlockToFile writes compiled block code to a file.
func (jit *JITCompiler) writeBlockToFile(className, methodName string, blockIndex int, goCode string) {
	if jit.OutputDir == "" {
		return
	}

	// Create output directory if needed
	if err := os.MkdirAll(jit.OutputDir, 0755); err != nil {
		if jit.LogCompilation {
			log.Printf("JIT: Failed to create output dir: %v", err)
		}
		return
	}

	// Sanitize filename
	filename := fmt.Sprintf("%s_%s_block%d.go",
		jit.aot.sanitizeName(className),
		jit.aot.sanitizeName(methodName),
		blockIndex)
	filepath := filepath.Join(jit.OutputDir, filename)

	// Write file
	content := jit.wrapBlockAsGoFile(className, methodName, blockIndex, goCode)
	if err := os.WriteFile(filepath, []byte(content), 0644); err != nil {
		if jit.LogCompilation {
			log.Printf("JIT: Failed to write file: %v", err)
		}
	}
}

// wrapBlockAsGoFile wraps compiled block code as a complete Go source file.
func (jit *JITCompiler) wrapBlockAsGoFile(className, methodName string, blockIndex int, goCode string) string {
	var sb strings.Builder
	sb.WriteString("// Code generated by Maggie JIT. DO NOT EDIT.\n")
	sb.WriteString(fmt.Sprintf("// Hot block: %s>>%s[%d]\n\n", className, methodName, blockIndex))
	sb.WriteString("package aot\n\n")
	sb.WriteString("import (\n")
	sb.WriteString("\t\"math\"\n")
	sb.WriteString("\t. \"github.com/chazu/maggie/vm\"\n")
	sb.WriteString(")\n\n")
	sb.WriteString("var _ = math.Float64frombits // silence unused import\n\n")
	sb.WriteString(goCode)
	return sb.String()
}

// methodKey returns a unique key for a method.
func (jit *JITCompiler) methodKey(method *CompiledMethod) string {
	if method.Class() == nil {
		return fmt.Sprintf("<detached>>>%s", method.Name())
	}
	return fmt.Sprintf("%s>>%s", method.Class().Name, method.Name())
}

// blockKey returns a unique key for a block.
func (jit *JITCompiler) blockKey(owner *CompiledMethod, blockIndex int) string {
	if owner == nil || owner.Class() == nil {
		return fmt.Sprintf("<detached>>>[%d]", blockIndex)
	}
	return fmt.Sprintf("%s>>%s[%d]", owner.Class().Name, owner.Name(), blockIndex)
}

// writeMethodToFile writes compiled code to a file.
func (jit *JITCompiler) writeMethodToFile(className, methodName, goCode string) {
	if jit.OutputDir == "" {
		return
	}

	// Create output directory if needed
	if err := os.MkdirAll(jit.OutputDir, 0755); err != nil {
		if jit.LogCompilation {
			log.Printf("JIT: Failed to create output dir: %v", err)
		}
		return
	}

	// Sanitize filename
	filename := jit.aot.sanitizeName(className) + "_" + jit.aot.sanitizeName(methodName) + ".go"
	filepath := filepath.Join(jit.OutputDir, filename)

	// Write file
	content := jit.wrapAsGoFile(className, methodName, goCode)
	if err := os.WriteFile(filepath, []byte(content), 0644); err != nil {
		if jit.LogCompilation {
			log.Printf("JIT: Failed to write file: %v", err)
		}
	}
}

// wrapAsGoFile wraps compiled code as a complete Go source file.
func (jit *JITCompiler) wrapAsGoFile(className, methodName, goCode string) string {
	var sb strings.Builder
	sb.WriteString("// Code generated by Maggie JIT. DO NOT EDIT.\n")
	sb.WriteString(fmt.Sprintf("// Hot method: %s>>%s\n\n", className, methodName))
	sb.WriteString("package aot\n\n")
	sb.WriteString("import (\n")
	sb.WriteString("\t\"math\"\n")
	sb.WriteString("\t. \"github.com/chazu/maggie/vm\"\n")
	sb.WriteString(")\n\n")
	sb.WriteString("var _ = math.Float64frombits // silence unused import\n\n")
	sb.WriteString(goCode)
	return sb.String()
}

// JITStats holds JIT compiler statistics.
type JITStats struct {
	MethodsCompiled uint64
	BlocksCompiled  uint64
	HotMethodCount  int
	HotBlockCount   int
	QueueLength     int
}

// Stats returns JIT compiler statistics.
func (jit *JITCompiler) Stats() JITStats {
	jit.mu.RLock()
	defer jit.mu.RUnlock()

	return JITStats{
		MethodsCompiled: atomic.LoadUint64(&jit.methodsCompiled),
		BlocksCompiled:  atomic.LoadUint64(&jit.blocksCompiled),
		HotMethodCount:  len(jit.hotMethods),
		HotBlockCount:   len(jit.hotBlocks),
		QueueLength:     len(jit.pending),
	}
}

// GetCompiledMethod returns the compiled Go code for a method, or empty string if not compiled.
func (jit *JITCompiler) GetCompiledMethod(className, methodName string) string {
	key := fmt.Sprintf("%s>>%s", className, methodName)
	jit.mu.RLock()
	defer jit.mu.RUnlock()
	return jit.hotMethods[key]
}

// GetCompiledBlock returns the compiled Go code for a block, or empty string if not compiled.
func (jit *JITCompiler) GetCompiledBlock(className, methodName string, blockIndex int) string {
	key := fmt.Sprintf("%s>>%s[%d]", className, methodName, blockIndex)
	jit.mu.RLock()
	defer jit.mu.RUnlock()
	return jit.hotBlocks[key]
}

// CompiledMethods returns all compiled method keys.
func (jit *JITCompiler) CompiledMethods() []string {
	jit.mu.RLock()
	defer jit.mu.RUnlock()

	keys := make([]string, 0, len(jit.hotMethods))
	for key := range jit.hotMethods {
		keys = append(keys, key)
	}
	return keys
}

// CompiledBlocks returns all compiled block keys.
func (jit *JITCompiler) CompiledBlocks() []string {
	jit.mu.RLock()
	defer jit.mu.RUnlock()

	keys := make([]string, 0, len(jit.hotBlocks))
	for key := range jit.hotBlocks {
		keys = append(keys, key)
	}
	return keys
}

// GenerateAOTPackage generates a complete Go package with all hot methods and blocks.
func (jit *JITCompiler) GenerateAOTPackage(packageName string) string {
	jit.mu.RLock()
	defer jit.mu.RUnlock()

	var sb strings.Builder

	sb.WriteString("// Code generated by Maggie JIT. DO NOT EDIT.\n")
	sb.WriteString("// This package contains AOT-compiled hot methods and blocks.\n\n")
	sb.WriteString(fmt.Sprintf("package %s\n\n", packageName))
	sb.WriteString("import (\n")
	sb.WriteString("\t\"math\"\n")
	sb.WriteString("\t. \"github.com/chazu/maggie/vm\"\n")
	sb.WriteString(")\n\n")
	sb.WriteString("var _ = math.Float64frombits // silence unused import\n\n")

	// Write all compiled methods
	if len(jit.hotMethods) > 0 {
		sb.WriteString("// ============ Compiled Methods ============\n\n")
		for key, code := range jit.hotMethods {
			sb.WriteString(fmt.Sprintf("// %s\n", key))
			sb.WriteString(code)
			sb.WriteString("\n")
		}
	}

	// Write all compiled blocks
	if len(jit.hotBlocks) > 0 {
		sb.WriteString("// ============ Compiled Blocks ============\n\n")
		for key, code := range jit.hotBlocks {
			sb.WriteString(fmt.Sprintf("// %s\n", key))
			sb.WriteString(code)
			sb.WriteString("\n")
		}
	}

	// Generate registration function
	sb.WriteString("// RegisterAll registers all AOT-compiled methods with the VM.\n")
	sb.WriteString("func RegisterAll(vm *VM) {\n")
	sb.WriteString("\ttable := make(AOTDispatchTable)\n")

	for key := range jit.hotMethods {
		parts := strings.SplitN(key, ">>", 2)
		if len(parts) == 2 {
			className := parts[0]
			methodName := parts[1]
			funcName := jit.aot.sanitizeName(className) + "_" + jit.aot.sanitizeName(methodName)
			sb.WriteString(fmt.Sprintf("\ttable[AOTDispatchKey{%q, %q}] = aot_%s\n",
				className, methodName, funcName))
		}
	}

	sb.WriteString("\tvm.RegisterAOTMethods(table)\n")

	// Note: Block registration would require a separate dispatch table
	// since blocks have a different signature (with captures and homeReturn)
	if len(jit.hotBlocks) > 0 {
		sb.WriteString(fmt.Sprintf("\t// %d compiled blocks available (registration TBD)\n", len(jit.hotBlocks)))
	}

	sb.WriteString("}\n")

	return sb.String()
}

// WriteAOTPackage writes the AOT package to a file.
func (jit *JITCompiler) WriteAOTPackage(filepath, packageName string) error {
	content := jit.GenerateAOTPackage(packageName)
	return os.WriteFile(filepath, []byte(content), 0644)
}

// Stop stops the background compilation worker.
func (jit *JITCompiler) Stop() {
	close(jit.done)
}

// Reset clears all compiled code and resets statistics.
func (jit *JITCompiler) Reset() {
	jit.mu.Lock()
	defer jit.mu.Unlock()

	jit.hotMethods = make(map[string]string)
	jit.hotBlocks = make(map[string]string)
	jit.compiledKeys = make(map[string]bool)
	atomic.StoreUint64(&jit.methodsCompiled, 0)
	atomic.StoreUint64(&jit.blocksCompiled, 0)
}

// sanitizeName is exposed for testing (delegates to AOT compiler).
func (jit *JITCompiler) sanitizeName(name string) string {
	return jit.aot.sanitizeName(name)
}
