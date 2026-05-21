package vm

import (
	"fmt"
	"os"
	"sort"
	"unsafe"

	"github.com/fxamacker/cbor/v2"
)

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
// CborImageWriter: Serializes VM state to a CBOR image
// ---------------------------------------------------------------------------

// CborImageWriter serializes VM state to a CBOR image.
type CborImageWriter struct {
	encoder *ImageEncoder

	// Collected data (same structure as ImageWriter)
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

	// VM reference for value encoding during WriteCborImage
	vm *VM
}

// NewCborImageWriter creates a new CBOR image writer.
func NewCborImageWriter() *CborImageWriter {
	return &CborImageWriter{
		encoder: NewImageEncoder(),
		flags:   ImageFlagNone,
	}
}

// ---------------------------------------------------------------------------
// Collection phase (mirrors ImageWriter.collectFromVM)
// ---------------------------------------------------------------------------

func (w *CborImageWriter) collectFromVM(vm *VM) {
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

func (w *CborImageWriter) collectClass(c *Class) {
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

func (w *CborImageWriter) collectMethod(m *CompiledMethod) {
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

func (w *CborImageWriter) collectBlock(b *BlockMethod) {
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

func (w *CborImageWriter) registerString(s string) uint32 {
	idx := w.encoder.RegisterString(s)
	if int(idx) >= len(w.strings) {
		w.strings = append(w.strings, s)
	}
	return idx
}

func (w *CborImageWriter) registerSymbol(symID uint32) uint32 {
	idx := w.encoder.RegisterSymbol(symID)
	if int(idx) >= len(w.symbols) {
		w.symbols = append(w.symbols, symID)
	}
	return idx
}

func (w *CborImageWriter) registerSelector(selID int) int {
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
// Sorting (identical to ImageWriter)
// ---------------------------------------------------------------------------

func (w *CborImageWriter) sortClassesByDependency() []*Class {
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

func (w *CborImageWriter) updateClassIndicesForSortedOrder(sortedClasses []*Class) {
	w.encoder.classIndex = make(map[*Class]uint32)
	w.encoder.nextClass = 0
	for i, c := range sortedClasses {
		w.encoder.classIndex[c] = uint32(i)
	}
	w.encoder.nextClass = uint32(len(sortedClasses))
}

// ---------------------------------------------------------------------------
// WriteCborImage: build the envelope and marshal to CBOR
// ---------------------------------------------------------------------------

// WriteCborImage builds the CBOR image envelope and marshals it.
func (w *CborImageWriter) WriteCborImage() ([]byte, error) {
	// Sort classes by dependency
	sortedClasses := w.sortClassesByDependency()
	w.updateClassIndicesForSortedOrder(sortedClasses)

	// Build class defs
	classDefs := make([]cborClassDef, len(sortedClasses))
	for i, c := range sortedClasses {
		cd, err := w.buildClassDef(c)
		if err != nil {
			return nil, fmt.Errorf("building class %q: %w", c.Name, err)
		}
		classDefs[i] = cd
	}

	// Build method defs
	methodDefs := make([]cborMethodDef, len(w.methods))
	for i, m := range w.methods {
		md, err := w.buildMethodDef(m)
		if err != nil {
			return nil, fmt.Errorf("building method %q: %w", m.name, err)
		}
		methodDefs[i] = md
	}

	// Build object defs
	objectDefs := make([]cborObjectDef, len(w.objects))
	for i, obj := range w.objects {
		od, err := w.buildObjectDef(obj)
		if err != nil {
			return nil, fmt.Errorf("building object %d: %w", i, err)
		}
		objectDefs[i] = od
	}

	// Build globals
	globalEntries := make([]cborGlobalEntry, 0, len(w.globalNames))
	for _, name := range w.globalNames {
		val := w.globals[name]
		encoded, err := encodeImageValue(w.encoder, val)
		if err != nil {
			return nil, fmt.Errorf("encoding global %q: %w", name, err)
		}
		nameIdx, _ := w.encoder.LookupString(name)
		globalEntries = append(globalEntries, cborGlobalEntry{
			Name:  nameIdx,
			Value: encoded,
		})
	}

	// Build class vars
	classVarEntries, err := w.buildClassVarEntries(sortedClasses)
	if err != nil {
		return nil, fmt.Errorf("building class vars: %w", err)
	}

	envelope := cborImageEnvelope{
		Version: 1,
		Flags:   w.flags,
		Stats: cborImageStats{
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

func (w *CborImageWriter) buildClassDef(c *Class) (cborClassDef, error) {
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

	cd := cborClassDef{
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

func (w *CborImageWriter) buildMethodDef(m *CompiledMethod) (cborMethodDef, error) {
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
			return cborMethodDef{}, fmt.Errorf("literal %d: %w", i, err)
		}
		literals[i] = encoded
	}

	// Build blocks
	blocks := make([]cborBlockDef, len(m.Blocks))
	for i, b := range m.Blocks {
		bd, err := w.buildBlockDef(b)
		if err != nil {
			return cborMethodDef{}, fmt.Errorf("block %d: %w", i, err)
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

	md := cborMethodDef{
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

func (w *CborImageWriter) buildBlockDef(b *BlockMethod) (cborBlockDef, error) {
	literals := make([]cbor.RawMessage, len(b.Literals))
	for i, lit := range b.Literals {
		encoded, err := encodeImageValue(w.encoder, lit)
		if err != nil {
			return cborBlockDef{}, fmt.Errorf("literal %d: %w", i, err)
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

	bd := cborBlockDef{
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

func (w *CborImageWriter) buildObjectDef(obj *Object) (cborObjectDef, error) {
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
		return cborObjectDef{}, encErr
	}

	return cborObjectDef{
		Class: classIdx,
		Slots: slots,
	}, nil
}

func (w *CborImageWriter) buildClassVarEntries(sortedClasses []*Class) ([]cborClassVarEntry, error) {
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

	entries := make([]cborClassVarEntry, 0, len(classesWithVars))
	for _, c := range classesWithVars {
		vars := w.classVarData[c]
		classIdx, _ := w.encoder.LookupClass(c)

		// Sort var names
		varNames := make([]string, 0, len(vars))
		for name := range vars {
			varNames = append(varNames, name)
		}
		sort.Strings(varNames)

		pairs := make([]cborClassVarPair, len(varNames))
		for i, name := range varNames {
			nameIdx, _ := w.encoder.LookupString(name)
			encoded, err := encodeImageValue(w.encoder, vars[name])
			if err != nil {
				return nil, fmt.Errorf("class %q var %q: %w", c.Name, name, err)
			}
			pairs[i] = cborClassVarPair{
				Name:  nameIdx,
				Value: encoded,
			}
		}

		entries = append(entries, cborClassVarEntry{
			Class: int64(classIdx),
			Vars:  pairs,
		})
	}

	return entries, nil
}

// ---------------------------------------------------------------------------
// VM entry points
// ---------------------------------------------------------------------------

// SaveImageCbor saves the VM state to a CBOR image file.
func (vm *VM) SaveImageCbor(path string) error {
	data, err := vm.SaveImageCborBytes()
	if err != nil {
		return err
	}
	return os.WriteFile(path, data, 0644)
}

// SaveImageCborBytes serializes the VM state to CBOR bytes.
func (vm *VM) SaveImageCborBytes() ([]byte, error) {
	writer := NewCborImageWriter()
	writer.collectFromVM(vm)
	return writer.WriteCborImage()
}
