package vm

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	"cuelang.org/go/cue/load"
)

// ---------------------------------------------------------------------------
// CueContext: wraps a CUE evaluation context
// ---------------------------------------------------------------------------

type CueContextObject struct {
	ctx *cue.Context
}

func cueContextToValue(id uint32) Value {
	return FromSymbolID(id | cueContextMarker)
}

func isCueContextValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == cueContextMarker
}

func cueContextIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^uint32(0xFF<<24)
}

func (vm *VM) vmGetCueContext(v Value) *CueContextObject {
	if !isCueContextValue(v) {
		return nil
	}
	return vm.registry.GetCueContext(cueContextIDFromValue(v))
}

func (vm *VM) vmRegisterCueContext(c *CueContextObject) Value {
	id := vm.registry.RegisterCueContext(c)
	return cueContextToValue(id)
}

// ---------------------------------------------------------------------------
// CueValue: wraps a CUE value
// ---------------------------------------------------------------------------

type CueValueObject struct {
	val cue.Value
}

func cueValueToValue(id uint32) Value {
	return FromSymbolID(id | cueValueMarker)
}

func isCueValueValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == cueValueMarker
}

func cueValueIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^uint32(0xFF<<24)
}

func (vm *VM) vmGetCueValue(v Value) *CueValueObject {
	if !isCueValueValue(v) {
		return nil
	}
	return vm.registry.GetCueValue(cueValueIDFromValue(v))
}

func (vm *VM) vmRegisterCueValue(c *CueValueObject) Value {
	id := vm.registry.RegisterCueValue(c)
	return cueValueToValue(id)
}

// ---------------------------------------------------------------------------
// CUE Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerCuePrimitives() {
	cueContextClass := vm.createClass("CueContext", vm.ObjectClass)
	cueValueClass := vm.createClass("CueValue", vm.ObjectClass)

	vm.Globals["CueContext"] = vm.classValue(cueContextClass)
	vm.Globals["CueValue"] = vm.classValue(cueValueClass)

	vm.symbolDispatch.Register(cueContextMarker, &SymbolTypeEntry{Class: cueContextClass})
	vm.symbolDispatch.Register(cueValueMarker, &SymbolTypeEntry{Class: cueValueClass})

	// -----------------------------------------------------------------------
	// CueContext class methods
	// -----------------------------------------------------------------------

	// CueContext new — create a new CUE evaluation context
	cueContextClass.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		ctx := cuecontext.New()
		obj := &CueContextObject{ctx: ctx}
		return v.vmRegisterCueContext(obj)
	})

	// CueContext>>primCompileString: — compile CUE source from a string, returns CueValue
	cueContextClass.AddMethod1(vm.Selectors, "primCompileString:", func(vmPtr interface{}, recv Value, srcVal Value) Value {
		v := vmPtr.(*VM)
		cctx := v.vmGetCueContext(recv)
		if cctx == nil {
			return v.newFailureResult("invalid CueContext")
		}
		src := v.valueToString(srcVal)
		cueVal := cctx.ctx.CompileString(src)
		if err := cueVal.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: cueVal}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// CueContext>>loadFile: — load CUE from a single file
	cueContextClass.AddMethod1(vm.Selectors, "primLoadFile:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		cctx := v.vmGetCueContext(recv)
		if cctx == nil {
			return v.newFailureResult("invalid CueContext")
		}
		path := v.valueToString(pathVal)
		data, err := os.ReadFile(path)
		if err != nil {
			return v.newFailureResult(fmt.Sprintf("cannot read file: %s", err))
		}
		cueVal := cctx.ctx.CompileBytes(data, cue.Filename(path))
		if err := cueVal.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: cueVal}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// CueContext>>loadDir: — load CUE from a directory (module-aware with cue.mod/ traversal)
	cueContextClass.AddMethod1(vm.Selectors, "primLoadDir:", func(vmPtr interface{}, recv Value, dirVal Value) Value {
		v := vmPtr.(*VM)
		cctx := v.vmGetCueContext(recv)
		if cctx == nil {
			return v.newFailureResult("invalid CueContext")
		}
		dir := v.valueToString(dirVal)
		absDir, err := filepath.Abs(dir)
		if err != nil {
			return v.newFailureResult(fmt.Sprintf("cannot resolve path: %s", err))
		}
		cfg := &load.Config{Dir: absDir}
		insts := load.Instances([]string{"."}, cfg)
		if len(insts) == 0 {
			return v.newFailureResult("no CUE instances found in directory")
		}
		inst := insts[0]
		if inst.Err != nil {
			return v.newFailureResult(fmt.Sprintf("load error: %s", inst.Err))
		}
		cueVal := cctx.ctx.BuildInstance(inst)
		if err := cueVal.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: cueVal}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// CueContext>>validate:against: — validate data CUE string against schema CUE string
	cueContextClass.AddMethod2(vm.Selectors, "primValidate:against:", func(vmPtr interface{}, recv Value, dataVal, schemaVal Value) Value {
		v := vmPtr.(*VM)
		cctx := v.vmGetCueContext(recv)
		if cctx == nil {
			return v.newFailureResult("invalid CueContext")
		}
		dataSrc := v.valueToString(dataVal)
		schemaSrc := v.valueToString(schemaVal)

		dataValue := cctx.ctx.CompileString(dataSrc)
		if err := dataValue.Err(); err != nil {
			return v.newFailureResult(fmt.Sprintf("data compile error: %s", err))
		}
		schemaValue := cctx.ctx.CompileString(schemaSrc)
		if err := schemaValue.Err(); err != nil {
			return v.newFailureResult(fmt.Sprintf("schema compile error: %s", err))
		}

		unified := schemaValue.Unify(dataValue)
		if err := unified.Validate(); err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(True)
	})

	// CueContext>>inject:into: — inject hidden fields (as JSON string) into CUE source
	cueContextClass.AddMethod2(vm.Selectors, "primInject:into:", func(vmPtr interface{}, recv Value, fieldsVal, srcVal Value) Value {
		v := vmPtr.(*VM)
		cctx := v.vmGetCueContext(recv)
		if cctx == nil {
			return v.newFailureResult("invalid CueContext")
		}

		// fieldsVal is a Dictionary, srcVal is CUE source string
		src := v.valueToString(srcVal)
		baseVal := cctx.ctx.CompileString(src)
		if err := baseVal.Err(); err != nil {
			return v.newFailureResult(fmt.Sprintf("compile error: %s", err))
		}

		// Build injection: iterate the dictionary and fill paths
		dict := v.registry.GetDictionaryObject(fieldsVal)
		if dict == nil {
			return v.newFailureResult("first argument must be a Dictionary")
		}

		result := baseVal
		for hash, keyVal := range dict.Keys {
			fieldName := v.valueToString(keyVal)
			fieldValue := dict.Data[hash]
			goVal := v.cueExportValue(fieldValue)
			var p cue.Path
			if len(fieldName) > 0 && fieldName[0] == '_' {
				// Hidden fields need Hid selector with anonymous package
				p = cue.MakePath(cue.Hid(fieldName, "_"))
			} else {
				p = cue.ParsePath(fieldName)
			}
			result = result.FillPath(p, goVal)
		}
		if err := result.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: result}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// -----------------------------------------------------------------------
	// CueValue instance methods
	// -----------------------------------------------------------------------

	// CueValue>>validate — validate a CUE value, returns Result
	cueValueClass.AddMethod0(vm.Selectors, "primValidate", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue")
		}
		if err := cv.val.Validate(); err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(True)
	})

	// CueValue>>unify: — unify with another CueValue
	cueValueClass.AddMethod1(vm.Selectors, "primUnify:", func(vmPtr interface{}, recv Value, otherVal Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue receiver")
		}
		other := v.vmGetCueValue(otherVal)
		if other == nil {
			return v.newFailureResult("argument must be a CueValue")
		}
		unified := cv.val.Unify(other.val)
		if err := unified.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: unified}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// CueValue>>lookup: — lookup a field by path string
	cueValueClass.AddMethod1(vm.Selectors, "primLookup:", func(vmPtr interface{}, recv Value, pathVal Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue")
		}
		pathStr := v.valueToString(pathVal)
		result := cv.val.LookupPath(cue.ParsePath(pathStr))
		if !result.Exists() {
			return v.newFailureResult(fmt.Sprintf("path not found: %s", pathStr))
		}
		if err := result.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: result}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// CueValue>>fillPath:with: — fill a path with a value, returns new CueValue
	cueValueClass.AddMethod2(vm.Selectors, "primFillPath:with:", func(vmPtr interface{}, recv Value, pathVal, valueVal Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue")
		}
		pathStr := v.valueToString(pathVal)
		goVal := v.cueExportValue(valueVal)
		var p cue.Path
		if len(pathStr) > 0 && pathStr[0] == '_' {
			p = cue.MakePath(cue.Hid(pathStr, "_"))
		} else {
			p = cue.ParsePath(pathStr)
		}
		result := cv.val.FillPath(p, goVal)
		if err := result.Err(); err != nil {
			return v.newFailureResult(err.Error())
		}
		obj := &CueValueObject{val: result}
		return v.newSuccessResult(v.vmRegisterCueValue(obj))
	})

	// CueValue>>fields — return a Dictionary of field name -> CueValue
	cueValueClass.AddMethod0(vm.Selectors, "primFields", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue")
		}
		iter, err := cv.val.Fields()
		if err != nil {
			return v.newFailureResult(err.Error())
		}
		dict := v.registry.NewDictionaryValue()
		for iter.Next() {
			key := v.registry.NewStringValue(iter.Selector().String())
			fieldObj := &CueValueObject{val: iter.Value()}
			val := v.vmRegisterCueValue(fieldObj)
			v.DictionaryAtPut(dict, key, val)
		}
		return v.newSuccessResult(dict)
	})

	// CueValue>>toMaggie — convert CUE value to native Maggie value
	cueValueClass.AddMethod0(vm.Selectors, "primToMaggie", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue")
		}
		result := v.cueToMaggie(cv.val)
		return v.newSuccessResult(result)
	})

	// CueValue>>toJSON — serialize CUE value to JSON string
	cueValueClass.AddMethod0(vm.Selectors, "primToJSON", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.newFailureResult("invalid CueValue")
		}
		data, err := cv.val.MarshalJSON()
		if err != nil {
			return v.newFailureResult(err.Error())
		}
		return v.newSuccessResult(v.registry.NewStringValue(string(data)))
	})

	// CueValue>>kind — return CUE kind as a symbol string
	cueValueClass.AddMethod0(vm.Selectors, "primKind", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return Nil
		}
		k := cv.val.IncompleteKind()
		return v.registry.NewStringValue(k.String())
	})

	// CueValue>>isConcrete — check if the value is fully resolved
	cueValueClass.AddMethod0(vm.Selectors, "primIsConcrete", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return False
		}
		if cv.val.IsConcrete() {
			return True
		}
		return False
	})

	// CueValue>>exists — check if the value exists (non-bottom)
	cueValueClass.AddMethod0(vm.Selectors, "primExists", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return False
		}
		if cv.val.Exists() {
			return True
		}
		return False
	})

	// CueValue>>error — return error string if value has error, nil otherwise
	cueValueClass.AddMethod0(vm.Selectors, "primError", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return Nil
		}
		if err := cv.val.Err(); err != nil {
			return v.registry.NewStringValue(err.Error())
		}
		return Nil
	})

	// CueValue>>matchesObject: — unify this CUE value against an object's projection
	// Returns true if the unification succeeds (no bottom), false otherwise
	cueValueClass.AddMethod1(vm.Selectors, "primMatchesObject:", func(vmPtr interface{}, recv Value, objVal Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return False
		}
		projection := v.objectAsCueValue(objVal)
		unified := cv.val.Unify(projection.val)
		if unified.Err() != nil {
			return False
		}
		return True
	})

	// CueValue>>primSubsumes: — does self subsume other? (self is more general)
	// e.g., `int` subsumes `42`, `>0` subsumes `5`
	cueValueClass.AddMethod1(vm.Selectors, "primSubsumes:", func(vmPtr interface{}, recv Value, otherVal Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return False
		}
		other := v.vmGetCueValue(otherVal)
		if other == nil {
			// Try projecting a non-CueValue argument into CUE
			other = v.objectAsCueValue(otherVal)
		}
		if cv.val.Subsume(other.val, cue.Final()) == nil {
			return True
		}
		return False
	})

	// CueValue>>primSubsumedBy: — is self subsumed by other? (self is more specific)
	cueValueClass.AddMethod1(vm.Selectors, "primSubsumedBy:", func(vmPtr interface{}, recv Value, otherVal Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return False
		}
		other := v.vmGetCueValue(otherVal)
		if other == nil {
			other = v.objectAsCueValue(otherVal)
		}
		if other.val.Subsume(cv.val, cue.Final()) == nil {
			return True
		}
		return False
	})

	// CueValue>>printString — string representation for display
	cueValueClass.AddMethod0(vm.Selectors, "primPrintString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		cv := v.vmGetCueValue(recv)
		if cv == nil {
			return v.registry.NewStringValue("a CueValue (invalid)")
		}
		data, err := json.MarshalIndent(cueToInterface(cv.val), "", "  ")
		if err != nil {
			return v.registry.NewStringValue(fmt.Sprintf("a CueValue (%s)", cv.val.IncompleteKind()))
		}
		return v.registry.NewStringValue(string(data))
	})
}

// ---------------------------------------------------------------------------
// Object → CUE projection
// ---------------------------------------------------------------------------

// objectAsCueValue projects a Maggie object's instance variables into a CUE struct.
// The resulting CUE value has field names matching the ivar names and values
// converted via cueExportValue. Non-object values (SmallInt, String, etc.) are
// exported as simple CUE scalars.
func (vm *VM) objectAsCueValue(v Value) *CueValueObject {
	ctx := cuecontext.New()

	// For non-object values, encode directly as a scalar
	obj := ObjectFromValue(v)
	if obj == nil {
		goVal := vm.cueExportValue(v)
		cueVal := ctx.Encode(goVal)
		return &CueValueObject{val: cueVal}
	}

	// Build a map from ivar names to exported values
	cls := vm.ClassFor(v)
	if cls == nil {
		cueVal := ctx.Encode(nil)
		return &CueValueObject{val: cueVal}
	}

	ivarNames := cls.AllInstVarNames()
	m := make(map[string]interface{}, len(ivarNames))
	for i, name := range ivarNames {
		m[name] = vm.cueExportValue(obj.GetSlot(i))
	}

	cueVal := ctx.Encode(m)
	return &CueValueObject{val: cueVal}
}

// ---------------------------------------------------------------------------
// Conversion helpers
// ---------------------------------------------------------------------------

// cueToMaggie converts a CUE value to a Maggie value.
func (vm *VM) cueToMaggie(v cue.Value) Value {
	switch v.IncompleteKind() {
	case cue.NullKind:
		return Nil
	case cue.BoolKind:
		b, err := v.Bool()
		if err != nil {
			return Nil
		}
		if b {
			return True
		}
		return False
	case cue.IntKind:
		n, err := v.Int64()
		if err != nil {
			return Nil
		}
		return FromSmallInt(n)
	case cue.FloatKind, cue.NumberKind:
		f, err := v.Float64()
		if err != nil {
			return Nil
		}
		return FromFloat64(f)
	case cue.StringKind:
		s, err := v.String()
		if err != nil {
			return Nil
		}
		return vm.registry.NewStringValue(s)
	case cue.ListKind:
		iter, err := v.List()
		if err != nil {
			return Nil
		}
		var elements []Value
		for iter.Next() {
			elements = append(elements, vm.cueToMaggie(iter.Value()))
		}
		return vm.NewArrayWithElements(elements)
	case cue.StructKind:
		dict := vm.registry.NewDictionaryValue()
		iter, err := v.Fields()
		if err != nil {
			return Nil
		}
		for iter.Next() {
			key := vm.registry.NewStringValue(iter.Selector().String())
			val := vm.cueToMaggie(iter.Value())
			vm.DictionaryAtPut(dict, key, val)
		}
		return dict
	default:
		// For incomplete/constraint types, wrap as CueValue
		obj := &CueValueObject{val: v}
		return vm.vmRegisterCueValue(obj)
	}
}

// cueExportValue converts a Maggie value to a Go interface{} for CUE FillPath.
func (vm *VM) cueExportValue(v Value) interface{} {
	if v == Nil {
		return nil
	}
	if v == True {
		return true
	}
	if v == False {
		return false
	}
	if v.IsSmallInt() {
		return v.SmallInt()
	}
	if v.IsFloat() {
		return v.Float64()
	}
	if IsStringValue(v) {
		return vm.registry.GetStringContent(v)
	}
	// Check if it's a CueValue — extract the raw cue.Value
	cv := vm.vmGetCueValue(v)
	if cv != nil {
		return cv.val
	}
	// Dictionary -> map
	dict := vm.registry.GetDictionaryObject(v)
	if dict != nil {
		m := make(map[string]interface{})
		for hash, keyVal := range dict.Keys {
			key := vm.valueToString(keyVal)
			m[key] = vm.cueExportValue(dict.Data[hash])
		}
		return m
	}
	// Object -> map (named ivars) or slice (array-like)
	if v.IsObject() {
		obj := ObjectFromValue(v)
		if obj != nil {
			cls := vm.ClassFor(v)
			ivarNames := cls.AllInstVarNames()
			if len(ivarNames) > 0 {
				// Object with named instance variables -> map
				m := make(map[string]interface{}, len(ivarNames))
				for i, name := range ivarNames {
					m[name] = vm.cueExportValue(obj.GetSlot(i))
				}
				return m
			}
			// Array-like object (no ivars) -> slice
			n := obj.NumSlots()
			s := make([]interface{}, n)
			for i := 0; i < n; i++ {
				s[i] = vm.cueExportValue(obj.GetSlot(i))
			}
			return s
		}
	}
	return nil
}

// cueToInterface converts a CUE value to a Go interface{} for JSON marshaling.
func cueToInterface(v cue.Value) interface{} {
	switch v.IncompleteKind() {
	case cue.NullKind:
		return nil
	case cue.BoolKind:
		b, _ := v.Bool()
		return b
	case cue.IntKind:
		n, _ := v.Int64()
		return n
	case cue.FloatKind, cue.NumberKind:
		f, _ := v.Float64()
		return f
	case cue.StringKind:
		s, _ := v.String()
		return s
	case cue.ListKind:
		iter, err := v.List()
		if err != nil {
			return nil
		}
		var result []interface{}
		for iter.Next() {
			result = append(result, cueToInterface(iter.Value()))
		}
		return result
	case cue.StructKind:
		m := make(map[string]interface{})
		iter, err := v.Fields()
		if err != nil {
			return m
		}
		for iter.Next() {
			m[iter.Selector().String()] = cueToInterface(iter.Value())
		}
		return m
	default:
		return fmt.Sprintf("<%s>", v.IncompleteKind())
	}
}
