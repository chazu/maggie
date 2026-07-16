package cue

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	cuelang "cuelang.org/go/cue"
	"cuelang.org/go/cue/cuecontext"
	"cuelang.org/go/cue/load"

	vm "github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// CueContext: wraps a CUE evaluation context
// ---------------------------------------------------------------------------

type CueContextObject struct {
	ctx *cuelang.Context
}

func isCueContextValue(v vm.Value) bool {
	return vm.IsExtensionValue(v, vm.CueContextMarker)
}

func vmGetCueContext(vmPtr *vm.VM, v vm.Value) *CueContextObject {
	if o := vm.ExtensionObject(v, vm.CueContextMarker); o != nil {
		return o.(*CueContextObject)
	}
	return nil
}

func vmRegisterCueContext(vmPtr *vm.VM, c *CueContextObject) vm.Value {
	return vm.NewExtensionValue(vm.CueContextMarker, c)
}

// ---------------------------------------------------------------------------
// CueValue: wraps a CUE value
// ---------------------------------------------------------------------------

type CueValueObject struct {
	val cuelang.Value
}

func isCueValueValue(v vm.Value) bool {
	return vm.IsExtensionValue(v, vm.CueValueMarker)
}

func vmGetCueValue(vmPtr *vm.VM, v vm.Value) *CueValueObject {
	if o := vm.ExtensionObject(v, vm.CueValueMarker); o != nil {
		return o.(*CueValueObject)
	}
	return nil
}

func vmRegisterCueValue(vmPtr *vm.VM, c *CueValueObject) vm.Value {
	return vm.NewExtensionValue(vm.CueValueMarker, c)
}

// ---------------------------------------------------------------------------
// CUE Primitives Registration
// ---------------------------------------------------------------------------

func RegisterCuePrimitives(v *vm.VM) {
	cueContextClass := v.CreateClass("CueContext", v.ObjectClass)
	cueValueClass := v.CreateClass("CueValue", v.ObjectClass)

	v.SetGlobal("CueContext", v.ClassValue(cueContextClass))
	v.SetGlobal("CueValue", v.ClassValue(cueValueClass))

	v.RegisterSymbolDispatchEntry(vm.CueContextMarker, &vm.SymbolTypeEntry{Class: cueContextClass})
	v.RegisterSymbolDispatchEntry(vm.CueValueMarker, &vm.SymbolTypeEntry{Class: cueValueClass})

	// -----------------------------------------------------------------------
	// CueContext class methods
	// -----------------------------------------------------------------------

	// CueContext new — create a new CUE evaluation context
	cueContextClass.AddClassMethod0(v.Selectors, "new", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		ctx := cuecontext.New()
		obj := &CueContextObject{ctx: ctx}
		return vmRegisterCueContext(vmPtr, obj)
	})

	// CueContext>>primCompileString: — compile CUE source from a string, returns CueValue
	cueContextClass.AddMethod1(v.Selectors, "primCompileString:", func(vmPtr *vm.VM, recv vm.Value, srcVal vm.Value) vm.Value {
		cctx := vmGetCueContext(vmPtr, recv)
		if cctx == nil {
			return vmPtr.NewFailureResult("invalid CueContext")
		}
		src := vmPtr.ValueToString(srcVal)
		cueVal := cctx.ctx.CompileString(src)
		if err := cueVal.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: cueVal}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// CueContext>>loadFile: — load CUE from a single file
	cueContextClass.AddMethod1(v.Selectors, "primLoadFile:", func(vmPtr *vm.VM, recv vm.Value, pathVal vm.Value) vm.Value {
		cctx := vmGetCueContext(vmPtr, recv)
		if cctx == nil {
			return vmPtr.NewFailureResult("invalid CueContext")
		}
		path := vmPtr.ValueToString(pathVal)
		data, err := os.ReadFile(path)
		if err != nil {
			return vmPtr.NewFailureResult(fmt.Sprintf("cannot read file: %s", err))
		}
		cueVal := cctx.ctx.CompileBytes(data, cuelang.Filename(path))
		if err := cueVal.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: cueVal}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// CueContext>>loadDir: — load CUE from a directory (module-aware with cue.mod/ traversal)
	cueContextClass.AddMethod1(v.Selectors, "primLoadDir:", func(vmPtr *vm.VM, recv vm.Value, dirVal vm.Value) vm.Value {
		cctx := vmGetCueContext(vmPtr, recv)
		if cctx == nil {
			return vmPtr.NewFailureResult("invalid CueContext")
		}
		dir := vmPtr.ValueToString(dirVal)
		absDir, err := filepath.Abs(dir)
		if err != nil {
			return vmPtr.NewFailureResult(fmt.Sprintf("cannot resolve path: %s", err))
		}
		cfg := &load.Config{Dir: absDir}
		insts := load.Instances([]string{"."}, cfg)
		if len(insts) == 0 {
			return vmPtr.NewFailureResult("no CUE instances found in directory")
		}
		inst := insts[0]
		if inst.Err != nil {
			return vmPtr.NewFailureResult(fmt.Sprintf("load error: %s", inst.Err))
		}
		cueVal := cctx.ctx.BuildInstance(inst)
		if err := cueVal.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: cueVal}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// CueContext>>validate:against: — validate data CUE string against schema CUE string
	cueContextClass.AddMethod2(v.Selectors, "primValidate:against:", func(vmPtr *vm.VM, recv vm.Value, dataVal, schemaVal vm.Value) vm.Value {
		cctx := vmGetCueContext(vmPtr, recv)
		if cctx == nil {
			return vmPtr.NewFailureResult("invalid CueContext")
		}
		dataSrc := vmPtr.ValueToString(dataVal)
		schemaSrc := vmPtr.ValueToString(schemaVal)

		dataValue := cctx.ctx.CompileString(dataSrc)
		if err := dataValue.Err(); err != nil {
			return vmPtr.NewFailureResult(fmt.Sprintf("data compile error: %s", err))
		}
		schemaValue := cctx.ctx.CompileString(schemaSrc)
		if err := schemaValue.Err(); err != nil {
			return vmPtr.NewFailureResult(fmt.Sprintf("schema compile error: %s", err))
		}

		unified := schemaValue.Unify(dataValue)
		if err := unified.Validate(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		return vmPtr.NewSuccessResult(vm.True)
	})

	// CueContext>>inject:into: — inject hidden fields (as JSON string) into CUE source
	cueContextClass.AddMethod2(v.Selectors, "primInject:into:", func(vmPtr *vm.VM, recv vm.Value, fieldsVal, srcVal vm.Value) vm.Value {
		cctx := vmGetCueContext(vmPtr, recv)
		if cctx == nil {
			return vmPtr.NewFailureResult("invalid CueContext")
		}

		// fieldsVal is a Dictionary, srcVal is CUE source string
		src := vmPtr.ValueToString(srcVal)
		baseVal := cctx.ctx.CompileString(src)
		if err := baseVal.Err(); err != nil {
			return vmPtr.NewFailureResult(fmt.Sprintf("compile error: %s", err))
		}

		// Build injection: iterate the dictionary and fill paths
		dict := vmPtr.Registry().GetDictionaryObject(fieldsVal)
		if dict == nil {
			return vmPtr.NewFailureResult("first argument must be a Dictionary")
		}

		result := baseVal
		for _, entry := range dict.Entries() {
			fieldName := vmPtr.ValueToString(entry.Key)
			fieldValue := entry.Value
			goVal := cueExportValue(vmPtr, fieldValue)
			var p cuelang.Path
			if len(fieldName) > 0 && fieldName[0] == '_' {
				// Hidden fields need Hid selector with anonymous package
				p = cuelang.MakePath(cuelang.Hid(fieldName, "_"))
			} else {
				p = cuelang.ParsePath(fieldName)
			}
			result = result.FillPath(p, goVal)
		}
		if err := result.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: result}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// -----------------------------------------------------------------------
	// CueValue instance methods
	// -----------------------------------------------------------------------

	// CueValue>>validate — validate a CUE value, returns Result
	cueValueClass.AddMethod0(v.Selectors, "primValidate", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue")
		}
		if err := cv.val.Validate(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		return vmPtr.NewSuccessResult(vm.True)
	})

	// CueValue>>unify: — unify with another CueValue
	cueValueClass.AddMethod1(v.Selectors, "primUnify:", func(vmPtr *vm.VM, recv vm.Value, otherVal vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue receiver")
		}
		other := vmGetCueValue(vmPtr, otherVal)
		if other == nil {
			return vmPtr.NewFailureResult("argument must be a CueValue")
		}
		unified := cv.val.Unify(other.val)
		if err := unified.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: unified}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// CueValue>>lookup: — lookup a field by path string
	cueValueClass.AddMethod1(v.Selectors, "primLookup:", func(vmPtr *vm.VM, recv vm.Value, pathVal vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue")
		}
		pathStr := vmPtr.ValueToString(pathVal)
		result := cv.val.LookupPath(cuelang.ParsePath(pathStr))
		if !result.Exists() {
			return vmPtr.NewFailureResult(fmt.Sprintf("path not found: %s", pathStr))
		}
		if err := result.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: result}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// CueValue>>fillPath:with: — fill a path with a value, returns new CueValue
	cueValueClass.AddMethod2(v.Selectors, "primFillPath:with:", func(vmPtr *vm.VM, recv vm.Value, pathVal, valueVal vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue")
		}
		pathStr := vmPtr.ValueToString(pathVal)
		goVal := cueExportValue(vmPtr, valueVal)
		var p cuelang.Path
		if len(pathStr) > 0 && pathStr[0] == '_' {
			p = cuelang.MakePath(cuelang.Hid(pathStr, "_"))
		} else {
			p = cuelang.ParsePath(pathStr)
		}
		result := cv.val.FillPath(p, goVal)
		if err := result.Err(); err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		obj := &CueValueObject{val: result}
		return vmPtr.NewSuccessResult(vmRegisterCueValue(vmPtr, obj))
	})

	// CueValue>>fields — return a Dictionary of field name -> CueValue
	cueValueClass.AddMethod0(v.Selectors, "primFields", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue")
		}
		iter, err := cv.val.Fields()
		if err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		dict := vmPtr.Registry().NewDictionaryValue()
		for iter.Next() {
			key := vmPtr.Registry().NewStringValue(iter.Selector().String())
			fieldObj := &CueValueObject{val: iter.Value()}
			val := vmRegisterCueValue(vmPtr, fieldObj)
			vmPtr.DictionaryAtPut(dict, key, val)
		}
		return vmPtr.NewSuccessResult(dict)
	})

	// CueValue>>toMaggie — convert CUE value to native Maggie value
	cueValueClass.AddMethod0(v.Selectors, "primToMaggie", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue")
		}
		result := cueToMaggie(vmPtr, cv.val)
		return vmPtr.NewSuccessResult(result)
	})

	// CueValue>>toJSON — serialize CUE value to JSON string
	cueValueClass.AddMethod0(v.Selectors, "primToJSON", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.NewFailureResult("invalid CueValue")
		}
		data, err := cv.val.MarshalJSON()
		if err != nil {
			return vmPtr.NewFailureResult(err.Error())
		}
		return vmPtr.NewSuccessResult(vmPtr.Registry().NewStringValue(string(data)))
	})

	// CueValue>>kind — return CUE kind as a symbol string
	cueValueClass.AddMethod0(v.Selectors, "primKind", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.Nil
		}
		k := cv.val.IncompleteKind()
		return vmPtr.Registry().NewStringValue(k.String())
	})

	// CueValue>>isConcrete — check if the value is fully resolved
	cueValueClass.AddMethod0(v.Selectors, "primIsConcrete", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.False
		}
		if cv.val.IsConcrete() {
			return vm.True
		}
		return vm.False
	})

	// CueValue>>exists — check if the value exists (non-bottom)
	cueValueClass.AddMethod0(v.Selectors, "primExists", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.False
		}
		if cv.val.Exists() {
			return vm.True
		}
		return vm.False
	})

	// CueValue>>error — return error string if value has error, nil otherwise
	cueValueClass.AddMethod0(v.Selectors, "primError", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.Nil
		}
		if err := cv.val.Err(); err != nil {
			return vmPtr.Registry().NewStringValue(err.Error())
		}
		return vm.Nil
	})

	// CueValue>>matchesObject: — unify this CUE value against an object's projection
	// Returns true if the unification succeeds (no bottom), false otherwise
	cueValueClass.AddMethod1(v.Selectors, "primMatchesObject:", func(vmPtr *vm.VM, recv vm.Value, objVal vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.False
		}
		projection := objectAsCueValue(vmPtr, objVal)
		unified := cv.val.Unify(projection.val)
		if unified.Err() != nil {
			return vm.False
		}
		return vm.True
	})

	// CueValue>>primSubsumes: — does self subsume other? (self is more general)
	// e.g., `int` subsumes `42`, `>0` subsumes `5`
	cueValueClass.AddMethod1(v.Selectors, "primSubsumes:", func(vmPtr *vm.VM, recv vm.Value, otherVal vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.False
		}
		other := vmGetCueValue(vmPtr, otherVal)
		if other == nil {
			// Try projecting a non-CueValue argument into CUE
			other = objectAsCueValue(vmPtr, otherVal)
		}
		if cv.val.Subsume(other.val, cuelang.Final()) == nil {
			return vm.True
		}
		return vm.False
	})

	// CueValue>>primSubsumedBy: — is self subsumed by other? (self is more specific)
	cueValueClass.AddMethod1(v.Selectors, "primSubsumedBy:", func(vmPtr *vm.VM, recv vm.Value, otherVal vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vm.False
		}
		other := vmGetCueValue(vmPtr, otherVal)
		if other == nil {
			other = objectAsCueValue(vmPtr, otherVal)
		}
		if other.val.Subsume(cv.val, cuelang.Final()) == nil {
			return vm.True
		}
		return vm.False
	})

	// CueValue>>printString — string representation for display
	cueValueClass.AddMethod0(v.Selectors, "primPrintString", func(vmPtr *vm.VM, recv vm.Value) vm.Value {
		cv := vmGetCueValue(vmPtr, recv)
		if cv == nil {
			return vmPtr.Registry().NewStringValue("a CueValue (invalid)")
		}
		data, err := json.MarshalIndent(cueToInterface(cv.val), "", "  ")
		if err != nil {
			return vmPtr.Registry().NewStringValue(fmt.Sprintf("a CueValue (%s)", cv.val.IncompleteKind()))
		}
		return vmPtr.Registry().NewStringValue(string(data))
	})
}

// ---------------------------------------------------------------------------
// Object -> CUE projection
// ---------------------------------------------------------------------------

// objectAsCueValue projects a Maggie object's instance variables into a CUE struct.
func objectAsCueValue(vmPtr *vm.VM, v vm.Value) *CueValueObject {
	ctx := cuecontext.New()

	// For non-object values, encode directly as a scalar
	obj := vm.ObjectFromValue(v)
	if obj == nil {
		goVal := cueExportValue(vmPtr, v)
		cueVal := ctx.Encode(goVal)
		return &CueValueObject{val: cueVal}
	}

	// Build a map from ivar names to exported values
	cls := vmPtr.ClassFor(v)
	if cls == nil {
		cueVal := ctx.Encode(nil)
		return &CueValueObject{val: cueVal}
	}

	ivarNames := cls.AllInstVarNames()
	m := make(map[string]interface{}, len(ivarNames))
	for i, name := range ivarNames {
		m[name] = cueExportValue(vmPtr, obj.GetSlot(i))
	}

	cueVal := ctx.Encode(m)
	return &CueValueObject{val: cueVal}
}

// ---------------------------------------------------------------------------
// Conversion helpers
// ---------------------------------------------------------------------------

// cueToMaggie converts a CUE value to a Maggie value.
func cueToMaggie(vmPtr *vm.VM, v cuelang.Value) vm.Value {
	switch v.IncompleteKind() {
	case cuelang.NullKind:
		return vm.Nil
	case cuelang.BoolKind:
		b, err := v.Bool()
		if err != nil {
			return vm.Nil
		}
		if b {
			return vm.True
		}
		return vm.False
	case cuelang.IntKind:
		n, err := v.Int64()
		if err != nil {
			return vm.Nil
		}
		return vm.FromSmallInt(n)
	case cuelang.FloatKind, cuelang.NumberKind:
		f, err := v.Float64()
		if err != nil {
			return vm.Nil
		}
		return vm.FromFloat64(f)
	case cuelang.StringKind:
		s, err := v.String()
		if err != nil {
			return vm.Nil
		}
		return vmPtr.Registry().NewStringValue(s)
	case cuelang.ListKind:
		iter, err := v.List()
		if err != nil {
			return vm.Nil
		}
		var elements []vm.Value
		for iter.Next() {
			elements = append(elements, cueToMaggie(vmPtr, iter.Value()))
		}
		return vmPtr.NewArrayWithElements(elements)
	case cuelang.StructKind:
		dict := vmPtr.Registry().NewDictionaryValue()
		iter, err := v.Fields()
		if err != nil {
			return vm.Nil
		}
		for iter.Next() {
			key := vmPtr.Registry().NewStringValue(iter.Selector().String())
			val := cueToMaggie(vmPtr, iter.Value())
			vmPtr.DictionaryAtPut(dict, key, val)
		}
		return dict
	default:
		// For incomplete/constraint types, wrap as CueValue
		obj := &CueValueObject{val: v}
		return vmRegisterCueValue(vmPtr, obj)
	}
}

// cueExportValue converts a Maggie value to a Go interface{} for CUE FillPath.
func cueExportValue(vmPtr *vm.VM, v vm.Value) interface{} {
	if v == vm.Nil {
		return nil
	}
	if v == vm.True {
		return true
	}
	if v == vm.False {
		return false
	}
	if v.IsSmallInt() {
		return v.SmallInt()
	}
	if v.IsFloat() {
		return v.Float64()
	}
	if vm.IsStringValue(v) {
		return vmPtr.Registry().GetStringContent(v)
	}
	// Check if it's a CueValue — extract the raw cue.Value
	cv := vmGetCueValue(vmPtr, v)
	if cv != nil {
		return cv.val
	}
	// Dictionary -> map
	dict := vmPtr.Registry().GetDictionaryObject(v)
	if dict != nil {
		m := make(map[string]interface{})
		for _, entry := range dict.Entries() {
			key := vmPtr.ValueToString(entry.Key)
			m[key] = cueExportValue(vmPtr, entry.Value)
		}
		return m
	}
	// Object -> map (named ivars) or slice (array-like)
	if v.IsObject() {
		obj := vm.ObjectFromValue(v)
		if obj != nil {
			cls := vmPtr.ClassFor(v)
			ivarNames := cls.AllInstVarNames()
			if len(ivarNames) > 0 {
				// Object with named instance variables -> map
				m := make(map[string]interface{}, len(ivarNames))
				for i, name := range ivarNames {
					m[name] = cueExportValue(vmPtr, obj.GetSlot(i))
				}
				return m
			}
			// Array-like object (no ivars) -> slice
			n := obj.NumSlots()
			s := make([]interface{}, n)
			for i := 0; i < n; i++ {
				s[i] = cueExportValue(vmPtr, obj.GetSlot(i))
			}
			return s
		}
	}
	return nil
}

// cueToInterface converts a CUE value to a Go interface{} for JSON marshaling.
func cueToInterface(v cuelang.Value) interface{} {
	switch v.IncompleteKind() {
	case cuelang.NullKind:
		return nil
	case cuelang.BoolKind:
		b, _ := v.Bool()
		return b
	case cuelang.IntKind:
		n, _ := v.Int64()
		return n
	case cuelang.FloatKind, cuelang.NumberKind:
		f, _ := v.Float64()
		return f
	case cuelang.StringKind:
		s, _ := v.String()
		return s
	case cuelang.ListKind:
		iter, err := v.List()
		if err != nil {
			return nil
		}
		var result []interface{}
		for iter.Next() {
			result = append(result, cueToInterface(iter.Value()))
		}
		return result
	case cuelang.StructKind:
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
