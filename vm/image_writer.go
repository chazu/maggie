package vm

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"unsafe"

	"github.com/fxamacker/cbor/v2"
)

// ---------------------------------------------------------------------------
// VM integration
// ---------------------------------------------------------------------------

// SaveImage saves the VM state to a file.
func (vm *VM) SaveImage(path string) error {
	f, err := os.Create(path)
	if err != nil {
		return fmt.Errorf("creating image file %q: %w", path, err)
	}
	defer f.Close()

	return vm.SaveImageTo(f)
}

// SaveImageAtomic saves the VM state to a file using crash-safe atomic writes.
// The write protocol is:
//  1. Write to <path>.tmp
//  2. Fsync the temp file to ensure data is on disk
//  3. If <path> exists, rename it to <path>.prev (rollback copy)
//  4. Rename <path>.tmp to <path> (atomic on POSIX)
//  5. Fsync the parent directory to ensure the rename is durable
//
// On crash during step 1-2, the original file is untouched.
// On crash during step 3-4, <path>.prev contains the previous valid image.
// After successful completion, <path>.prev is retained as a rollback copy.
func (vm *VM) SaveImageAtomic(path string) error {
	tmpPath := path + ".tmp"
	prevPath := path + ".prev"

	// Step 1: Write to temp file
	f, err := os.Create(tmpPath)
	if err != nil {
		return fmt.Errorf("creating temp image file %q: %w", tmpPath, err)
	}

	if err := vm.SaveImageTo(f); err != nil {
		f.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("writing image to temp file: %w", err)
	}

	// Step 2: Fsync to ensure data is on disk
	if err := f.Sync(); err != nil {
		f.Close()
		os.Remove(tmpPath)
		return fmt.Errorf("fsyncing temp image file: %w", err)
	}
	f.Close()

	// Step 3: If target exists, rename to .prev for rollback
	if _, err := os.Stat(path); err == nil {
		// Remove old .prev if it exists
		os.Remove(prevPath)
		if err := os.Rename(path, prevPath); err != nil {
			os.Remove(tmpPath)
			return fmt.Errorf("renaming previous image to %q: %w", prevPath, err)
		}
	}

	// Step 4: Atomic rename of tmp to target
	if err := os.Rename(tmpPath, path); err != nil {
		return fmt.Errorf("atomic rename of temp image to %q: %w", path, err)
	}

	// Step 5: Fsync parent directory for rename durability
	dir, err := os.Open(filepath.Dir(path))
	if err == nil {
		dir.Sync()
		dir.Close()
	}

	return nil
}

// SaveImageTo saves the VM state to a writer.
func (vm *VM) SaveImageTo(w io.Writer) error {
	data, err := vm.SaveImageBytes()
	if err != nil {
		return err
	}
	_, err = w.Write(data)
	return err
}

// SaveImageBytes serializes the VM state to CBOR bytes.
//
// It holds gcRunMu for the whole operation so a background collection cannot
// run concurrently. CollectAllObjects builds a pointer-keyed object table and
// the encode phase later looks objects up by pointer; a concurrent sweep that
// unpinned/freed an object between those phases would yield "object pointer not
// registered". gcRunMu is the collector's own "one collection at a time" lock,
// so taking it here fully serializes save against sweeps. (This does not make
// every non-safepoint heap walker safe — RPC/HTTP paths are tracked separately
// as the STW-soundness follow-up — but it closes the whole-VM image walk.)
func (vm *VM) SaveImageBytes() ([]byte, error) {
	vm.gcRunMu.Lock()
	defer vm.gcRunMu.Unlock()
	writer := NewImageWriter()
	writer.collectFromVM(vm)
	return writer.WriteImage()
}

// CollectAllObjects traverses from roots and collects all reachable objects.
// Iteration order is deterministic: globals are visited in sorted key order,
// classes in sorted full-name order, and class variables in sorted name order.
func (vm *VM) CollectAllObjects() []*Object {
	visited := make(map[uintptr]bool)
	var objects []*Object

	var visit func(v Value)
	visit = func(v Value) {
		if !v.IsObject() {
			return
		}

		ptr := uintptr(v.ObjectPtr())
		if visited[ptr] {
			return
		}
		visited[ptr] = true

		obj := ObjectFromValue(v)
		if obj == nil {
			return
		}

		objects = append(objects, obj)

		// Visit slots
		obj.ForEachSlot(func(index int, slotVal Value) {
			visit(slotVal)
		})
	}

	// Visit globals in sorted key order for deterministic output
	vm.globalsMu.RLock()
	globalNames := make([]string, 0, len(vm.globals))
	for name := range vm.globals {
		globalNames = append(globalNames, name)
	}
	globalsCopy := make(map[string]Value, len(vm.globals))
	for k, v := range vm.globals {
		globalsCopy[k] = v
	}
	vm.globalsMu.RUnlock()
	sort.Strings(globalNames)
	for _, name := range globalNames {
		visit(globalsCopy[name])
	}

	// Visit class variable values in deterministic order
	allClasses := vm.Classes.All()
	sort.Slice(allClasses, func(i, j int) bool {
		return allClasses[i].FullName() < allClasses[j].FullName()
	})
	for _, class := range allClasses {
		vars := vm.registry.GetClassVarStorage(class)
		if len(vars) > 0 {
			varNames := make([]string, 0, len(vars))
			for name := range vars {
				varNames = append(varNames, name)
			}
			sort.Strings(varNames)
			for _, name := range varNames {
				visit(vars[name])
			}
		}
	}

	return objects
}

// ---------------------------------------------------------------------------
// encodeImageValue: encode a single Value to CBOR bytes for image storage
// ---------------------------------------------------------------------------

// encodeImageValue encodes a Value into CBOR bytes for image serialization.
// Uses the ImageEncoder for index lookups (objects, symbols, classes).
// String literals are encoded as inline CBOR text strings.
func encodeImageValue(enc *ImageEncoder, v Value) (cbor.RawMessage, error) {
	switch {
	case v == Nil:
		return cborSerialEncMode.Marshal(nil)

	case v == True:
		return cborSerialEncMode.Marshal(true)

	case v == False:
		return cborSerialEncMode.Marshal(false)

	case v.IsSmallInt():
		return cborSerialEncMode.Marshal(v.SmallInt())

	case v.IsFloat():
		return cborSerialEncMode.Marshal(v.Float64())

	case IsStringValue(v):
		// String values must be checked BEFORE IsSymbol since both use the symbol tag.
		// Encode as inline CBOR text string.
		content := enc.registry.GetStringContent(v)
		return cborSerialEncMode.Marshal(content)

	case IsCharacterValue(v):
		// Character values must be checked BEFORE IsSymbol.
		cp := GetCharacterCodePoint(v)
		return cborSerialEncMode.Marshal(cbor.Tag{
			Number:  cborTagCharacter,
			Content: uint32(cp),
		})

	case IsBigIntValue(v):
		// BigInt must be checked BEFORE IsSymbol.
		obj := enc.registry.GetBigInt(v)
		if obj == nil {
			return nil, fmt.Errorf("encodeImageValue: BigInt value has nil object")
		}
		bi := obj.Value
		tag := uint64(cborTagBigIntPositive)
		b := bi.Bytes() // absolute value as big-endian bytes
		if bi.Sign() < 0 {
			tag = cborTagBigIntNegative
		}
		return cborSerialEncMode.Marshal(cbor.Tag{
			Number:  tag,
			Content: b,
		})

	case isClassValue(v):
		cls := enc.registry.GetClassFromValue(v)
		if cls == nil {
			return nil, fmt.Errorf("encodeImageValue: class value has nil class")
		}
		idx, ok := enc.LookupClass(cls)
		if !ok {
			return nil, fmt.Errorf("encodeImageValue: class %q not registered", cls.Name)
		}
		return cborSerialEncMode.Marshal(cbor.Tag{
			Number:  imgTagClassRef,
			Content: idx,
		})

	case v.IsSymbol():
		// Real interned symbol only — strings, characters, classes, BigInts
		// are matched in earlier cases above.
		symID := v.SymbolID()
		idx, ok := enc.LookupSymbol(symID)
		if !ok {
			return nil, fmt.Errorf("encodeImageValue: symbol ID %d not registered", symID)
		}
		return cborSerialEncMode.Marshal(cbor.Tag{
			Number:  imgTagSymbolRef,
			Content: idx,
		})

	case v.IsObject():
		ptr := uintptr(v.ObjectPtr())
		idx, ok := enc.LookupObject(ptr)
		if !ok {
			return nil, fmt.Errorf("encodeImageValue: object pointer not registered")
		}
		return cborSerialEncMode.Marshal(cbor.Tag{
			Number:  imgTagValueRef,
			Content: idx,
		})

	default:
		return nil, fmt.Errorf("encodeImageValue: unsupported value type (bits: %016x)", uint64(v))
	}
}

// ---------------------------------------------------------------------------
// ImageWriter: Serializes VM state to a CBOR image
// ---------------------------------------------------------------------------

// ImageWriter serializes VM state to a CBOR image.
type ImageWriter struct {
	encoder *ImageEncoder

	// Collected data
	strings   []string
	symbols   []uint32
	selectors []int
	classes   []*Class
	methods   []*CompiledMethod
	objects   []*Object

	// Class variable data (collected from ObjectRegistry)
	classVarData map[*Class]map[string]Value

	// Globals snapshot (taken under lock)
	globals     map[string]Value
	globalNames []string // sorted keys

	// Resolved name tables for the envelope
	symbolNames   []string
	selectorNames []string

	flags      uint32
	entryPoint uint32

	// VM reference for value encoding during WriteImage
	vm *VM
}

// NewImageWriter creates a new CBOR image writer.
func NewImageWriter() *ImageWriter {
	return &ImageWriter{
		encoder: NewImageEncoder(),
		flags:   0,
	}
}

// ---------------------------------------------------------------------------
// Collection phase
// ---------------------------------------------------------------------------

func (w *ImageWriter) collectFromVM(vm *VM) {
	w.vm = vm
	w.encoder.registry = vm.registry

	// Collect strings from symbols
	allSymbols := vm.Symbols.All()
	for _, name := range allSymbols {
		w.registerString(name)
	}

	// Collect strings from selectors
	allSelectors := vm.Selectors.All()
	for _, name := range allSelectors {
		w.registerString(name)
	}

	// Snapshot globals under lock
	vm.globalsMu.RLock()
	w.globals = make(map[string]Value, len(vm.globals))
	w.globalNames = make([]string, 0, len(vm.globals))
	for name, val := range vm.globals {
		w.globals[name] = val
		w.globalNames = append(w.globalNames, name)
	}
	vm.globalsMu.RUnlock()
	sort.Strings(w.globalNames)

	// Register global name strings
	for _, name := range w.globalNames {
		w.registerString(name)
	}

	// Collect symbols in order
	for i := 0; i < len(allSymbols); i++ {
		w.registerSymbol(uint32(i))
	}

	// Collect selectors in order
	for i := 0; i < len(allSelectors); i++ {
		w.registerSelector(i)
	}

	// Collect classes (sorted by full name for deterministic output)
	allClasses := vm.Classes.All()
	sort.Slice(allClasses, func(i, j int) bool {
		return allClasses[i].FullName() < allClasses[j].FullName()
	})
	for _, class := range allClasses {
		w.collectClass(class)
	}

	// Collect class variable names and cache
	w.classVarData = make(map[*Class]map[string]Value)
	for _, class := range w.classes {
		vars := vm.registry.GetClassVarStorage(class)
		if len(vars) > 0 {
			w.classVarData[class] = vars
			varNames := make([]string, 0, len(vars))
			for name := range vars {
				varNames = append(varNames, name)
			}
			sort.Strings(varNames)
			for _, name := range varNames {
				w.registerString(name)
			}
		}
	}

	// Collect objects
	w.objects = vm.CollectAllObjects()
	for _, obj := range w.objects {
		w.encoder.RegisterObject(uintptr(unsafe.Pointer(obj)))
	}

	// Build resolved name tables
	w.symbolNames = make([]string, len(allSymbols))
	copy(w.symbolNames, allSymbols)

	w.selectorNames = make([]string, len(allSelectors))
	copy(w.selectorNames, allSelectors)
}

func (w *ImageWriter) collectClass(c *Class) {
	w.registerString(c.Name)
	if c.Namespace != "" {
		w.registerString(c.Namespace)
	}
	for _, ivar := range c.InstVars {
		w.registerString(ivar)
	}
	if c.DocString != "" {
		w.registerString(c.DocString)
	}

	w.encoder.RegisterClass(c)
	w.classes = append(w.classes, c)

	if c.VTable != nil {
		for _, method := range c.VTable.methods {
			if cm, ok := method.(*CompiledMethod); ok {
				w.collectMethod(cm)
			}
		}
	}
	if c.ClassVTable != nil {
		for _, method := range c.ClassVTable.methods {
			if cm, ok := method.(*CompiledMethod); ok {
				w.collectMethod(cm)
			}
		}
	}
}

func (w *ImageWriter) collectMethod(m *CompiledMethod) {
	if _, ok := w.encoder.LookupMethod(m); ok {
		return
	}

	w.registerString(m.name)
	if m.Source != "" {
		w.registerString(m.Source)
	}
	if m.docString != "" {
		w.registerString(m.docString)
	}

	w.encoder.RegisterMethod(m)
	w.methods = append(w.methods, m)

	for _, lit := range m.Literals {
		if IsStringValue(lit) {
			w.registerString(w.encoder.registry.GetStringContent(lit))
		} else if lit.IsSymbol() {
			w.registerSymbol(lit.SymbolID())
		}
	}

	for _, block := range m.Blocks {
		w.collectBlock(block)
	}
}

func (w *ImageWriter) collectBlock(b *BlockMethod) {
	if b.Source != "" {
		w.registerString(b.Source)
	}
	for _, lit := range b.Literals {
		if IsStringValue(lit) {
			w.registerString(w.encoder.registry.GetStringContent(lit))
		} else if lit.IsSymbol() {
			w.registerSymbol(lit.SymbolID())
		}
	}
}

func (w *ImageWriter) registerString(s string) uint32 {
	idx := w.encoder.RegisterString(s)
	if int(idx) >= len(w.strings) {
		w.strings = append(w.strings, s)
	}
	return idx
}

func (w *ImageWriter) registerSymbol(symID uint32) uint32 {
	idx := w.encoder.RegisterSymbol(symID)
	if int(idx) >= len(w.symbols) {
		w.symbols = append(w.symbols, symID)
	}
	return idx
}

func (w *ImageWriter) registerSelector(selID int) int {
	for i, id := range w.selectors {
		if id == selID {
			return i
		}
	}
	idx := len(w.selectors)
	w.selectors = append(w.selectors, selID)
	return idx
}

// ---------------------------------------------------------------------------
// Sorting
// ---------------------------------------------------------------------------

func (w *ImageWriter) sortClassesByDependency() []*Class {
	sorted := make([]*Class, len(w.classes))
	copy(sorted, w.classes)

	sort.SliceStable(sorted, func(i, j int) bool {
		di, dj := sorted[i].Depth(), sorted[j].Depth()
		if di != dj {
			return di < dj
		}
		return sorted[i].FullName() < sorted[j].FullName()
	})

	return sorted
}

func (w *ImageWriter) updateClassIndicesForSortedOrder(sortedClasses []*Class) {
	w.encoder.classIndex = make(map[*Class]uint32)
	w.encoder.nextClass = 0
	for i, c := range sortedClasses {
		w.encoder.classIndex[c] = uint32(i)
	}
	w.encoder.nextClass = uint32(len(sortedClasses))
}

// ---------------------------------------------------------------------------
// WriteImage: build the envelope and marshal to CBOR
// ---------------------------------------------------------------------------

// WriteImage builds the CBOR image envelope and marshals it.
func (w *ImageWriter) WriteImage() ([]byte, error) {
	// Sort classes by dependency
	sortedClasses := w.sortClassesByDependency()
	w.updateClassIndicesForSortedOrder(sortedClasses)

	// Build class defs
	classDefs := make([]classDef, len(sortedClasses))
	for i, c := range sortedClasses {
		cd, err := w.buildClassDef(c)
		if err != nil {
			return nil, fmt.Errorf("building class %q: %w", c.Name, err)
		}
		classDefs[i] = cd
	}

	// Build method defs
	methodDefs := make([]methodDef, len(w.methods))
	for i, m := range w.methods {
		md, err := w.buildMethodDef(m)
		if err != nil {
			return nil, fmt.Errorf("building method %q: %w", m.name, err)
		}
		methodDefs[i] = md
	}

	// Build object defs
	objectDefs := make([]objectDef, len(w.objects))
	for i, obj := range w.objects {
		od, err := w.buildObjectDef(obj)
		if err != nil {
			return nil, fmt.Errorf("building object %d: %w", i, err)
		}
		objectDefs[i] = od
	}

	// Build globals
	globalEntries := make([]globalEntry, 0, len(w.globalNames))
	for _, name := range w.globalNames {
		val := w.globals[name]
		encoded, err := encodeImageValue(w.encoder, val)
		if err != nil {
			return nil, fmt.Errorf("encoding global %q: %w", name, err)
		}
		nameIdx, _ := w.encoder.LookupString(name)
		globalEntries = append(globalEntries, globalEntry{
			Name:  nameIdx,
			Value: encoded,
		})
	}

	// Build class vars
	classVarEntries, err := w.buildClassVarEntries(sortedClasses)
	if err != nil {
		return nil, fmt.Errorf("building class vars: %w", err)
	}

	envelope := imageEnvelope{
		Version: 1,
		Flags:   w.flags,
		Stats: imageStats{
			Classes: uint32(len(sortedClasses)),
			Methods: uint32(len(w.methods)),
			Objects: uint32(len(w.objects)),
			Globals: uint32(len(globalEntries)),
		},
		Strings:    w.strings,
		Symbols:    w.symbolNames,
		Selectors:  w.selectorNames,
		Classes:    classDefs,
		Methods:    methodDefs,
		Objects:    objectDefs,
		Globals:    globalEntries,
		ClassVars:  classVarEntries,
		EntryPoint: w.entryPoint,
	}

	return cborSerialEncMode.Marshal(cbor.Tag{
		Number:  imgTagHeader,
		Content: envelope,
	})
}

// ---------------------------------------------------------------------------
// Builders for individual sections
// ---------------------------------------------------------------------------

func (w *ImageWriter) buildClassDef(c *Class) (classDef, error) {
	nameIdx, _ := w.encoder.LookupString(c.Name)

	var nsIdx int64 = -1
	if c.Namespace != "" {
		idx, _ := w.encoder.LookupString(c.Namespace)
		nsIdx = int64(idx)
	}

	var superIdx int64 = -1
	if c.Superclass != nil {
		if idx, ok := w.encoder.LookupClass(c.Superclass); ok {
			superIdx = int64(idx)
		}
	}

	// Instance variable name indices
	instVars := make([]uint32, len(c.InstVars))
	for i, ivar := range c.InstVars {
		instVars[i], _ = w.encoder.LookupString(ivar)
	}

	// Instance method indices
	var instanceMethods []uint32
	if c.VTable != nil {
		for _, m := range c.VTable.methods {
			if cm, ok := m.(*CompiledMethod); ok {
				if idx, ok := w.encoder.LookupMethod(cm); ok {
					instanceMethods = append(instanceMethods, idx)
				}
			}
		}
	}

	// Class method indices
	var classMethods []uint32
	if c.ClassVTable != nil {
		for _, m := range c.ClassVTable.methods {
			if cm, ok := m.(*CompiledMethod); ok {
				if idx, ok := w.encoder.LookupMethod(cm); ok {
					classMethods = append(classMethods, idx)
				}
			}
		}
	}

	cd := classDef{
		Name:            nameIdx,
		Namespace:       nsIdx,
		Super:           superIdx,
		NumSlots:        uint32(c.NumSlots),
		InstVars:        instVars,
		InstanceMethods: instanceMethods,
		ClassMethods:    classMethods,
	}

	if c.DocString != "" {
		docIdx, _ := w.encoder.LookupString(c.DocString)
		cd.DocString = docIdx
		cd.HasDocString = true
	}

	return cd, nil
}

func (w *ImageWriter) buildMethodDef(m *CompiledMethod) (methodDef, error) {
	nameIdx, _ := w.encoder.LookupString(m.name)

	var classIdx int64 = -1
	if m.class != nil {
		if idx, ok := w.encoder.LookupClass(m.class); ok {
			classIdx = int64(idx)
		}
	}

	// Encode literals
	literals := make([]cbor.RawMessage, len(m.Literals))
	for i, lit := range m.Literals {
		encoded, err := encodeImageValue(w.encoder, lit)
		if err != nil {
			return methodDef{}, fmt.Errorf("literal %d: %w", i, err)
		}
		literals[i] = encoded
	}

	// Build blocks
	blocks := make([]blockDef, len(m.Blocks))
	for i, b := range m.Blocks {
		bd, err := w.buildBlockDef(b)
		if err != nil {
			return methodDef{}, fmt.Errorf("block %d: %w", i, err)
		}
		blocks[i] = bd
	}

	// Source map
	var sourceMap [][3]uint32
	if len(m.SourceMap) > 0 {
		sourceMap = make([][3]uint32, len(m.SourceMap))
		for i, loc := range m.SourceMap {
			sourceMap[i] = [3]uint32{uint32(loc.Offset), uint32(loc.Line), uint32(loc.Column)}
		}
	}

	md := methodDef{
		Selector:      int32(m.selector),
		Class:         classIdx,
		Name:          nameIdx,
		IsClassMethod: m.IsClassMethod,
		Arity:         uint32(m.Arity),
		NumTemps:      uint32(m.NumTemps),
		Literals:      literals,
		Bytecode:      m.Bytecode,
		Blocks:        blocks,
		SourceMap:     sourceMap,
	}

	if m.Source != "" {
		srcIdx, _ := w.encoder.LookupString(m.Source)
		md.Source = srcIdx
		md.HasSource = true
	}

	if m.docString != "" {
		docIdx, _ := w.encoder.LookupString(m.docString)
		md.DocString = docIdx
		md.HasDocString = true
	}

	// Content hash (omit if all zeros)
	var zeroHash [32]byte
	if m.ContentHash != zeroHash {
		md.ContentHash = m.ContentHash[:]
	}
	if m.TypedHash != zeroHash {
		md.TypedHash = m.TypedHash[:]
	}

	return md, nil
}

func (w *ImageWriter) buildBlockDef(b *BlockMethod) (blockDef, error) {
	literals := make([]cbor.RawMessage, len(b.Literals))
	for i, lit := range b.Literals {
		encoded, err := encodeImageValue(w.encoder, lit)
		if err != nil {
			return blockDef{}, fmt.Errorf("literal %d: %w", i, err)
		}
		literals[i] = encoded
	}

	var sourceMap [][3]uint32
	if len(b.SourceMap) > 0 {
		sourceMap = make([][3]uint32, len(b.SourceMap))
		for i, loc := range b.SourceMap {
			sourceMap[i] = [3]uint32{uint32(loc.Offset), uint32(loc.Line), uint32(loc.Column)}
		}
	}

	bd := blockDef{
		Arity:       uint32(b.Arity),
		NumTemps:    uint32(b.NumTemps),
		NumCaptures: uint32(b.NumCaptures),
		Literals:    literals,
		Bytecode:    b.Bytecode,
		SourceMap:   sourceMap,
	}

	if b.Source != "" {
		srcIdx, _ := w.encoder.LookupString(b.Source)
		bd.Source = srcIdx
		bd.HasSource = true
	}

	return bd, nil
}

func (w *ImageWriter) buildObjectDef(obj *Object) (objectDef, error) {
	var classIdx int64 = -1
	if obj.vtable != nil && obj.vtable.class != nil {
		if idx, ok := w.encoder.LookupClass(obj.vtable.class); ok {
			classIdx = int64(idx)
		}
	}

	numSlots := obj.NumSlots()
	slots := make([]cbor.RawMessage, 0, numSlots)
	var encErr error
	obj.ForEachSlot(func(index int, value Value) {
		if encErr != nil {
			return
		}
		encoded, err := encodeImageValue(w.encoder, value)
		if err != nil {
			encErr = fmt.Errorf("slot %d: %w", index, err)
			return
		}
		slots = append(slots, encoded)
	})
	if encErr != nil {
		return objectDef{}, encErr
	}

	return objectDef{
		Class: classIdx,
		Slots: slots,
	}, nil
}

func (w *ImageWriter) buildClassVarEntries(sortedClasses []*Class) ([]classVarEntry, error) {
	// Collect classes that have class vars, sorted by name for determinism
	classesWithVars := make([]*Class, 0)
	for _, c := range sortedClasses {
		if vars := w.classVarData[c]; len(vars) > 0 {
			classesWithVars = append(classesWithVars, c)
		}
	}
	sort.Slice(classesWithVars, func(i, j int) bool {
		return classesWithVars[i].Name < classesWithVars[j].Name
	})

	entries := make([]classVarEntry, 0, len(classesWithVars))
	for _, c := range classesWithVars {
		vars := w.classVarData[c]
		classIdx, _ := w.encoder.LookupClass(c)

		// Sort var names
		varNames := make([]string, 0, len(vars))
		for name := range vars {
			varNames = append(varNames, name)
		}
		sort.Strings(varNames)

		pairs := make([]classVarPair, len(varNames))
		for i, name := range varNames {
			nameIdx, _ := w.encoder.LookupString(name)
			encoded, err := encodeImageValue(w.encoder, vars[name])
			if err != nil {
				return nil, fmt.Errorf("class %q var %q: %w", c.Name, name, err)
			}
			pairs[i] = classVarPair{
				Name:  nameIdx,
				Value: encoded,
			}
		}

		entries = append(entries, classVarEntry{
			Class: int64(classIdx),
			Vars:  pairs,
		})
	}

	return entries, nil
}
