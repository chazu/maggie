package gowrap

import (
	"fmt"
	"go/constant"
	"go/types"

	"golang.org/x/tools/go/packages"
)

// IntrospectPackage loads a Go package by import path and returns its API model.
// The includeFilter, if non-nil, restricts which exported names are included.
func IntrospectPackage(importPath string, includeFilter map[string]bool) (*PackageModel, error) {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedTypes | packages.NeedSyntax,
	}

	pkgs, err := packages.Load(cfg, importPath)
	if err != nil {
		return nil, fmt.Errorf("loading %s: %w", importPath, err)
	}
	if len(pkgs) == 0 {
		return nil, fmt.Errorf("no packages found for %s", importPath)
	}
	if len(pkgs[0].Errors) > 0 {
		return nil, fmt.Errorf("package errors: %v", pkgs[0].Errors)
	}

	pkg := pkgs[0]
	if pkg.Types == nil {
		return nil, fmt.Errorf("type information not available for %s", importPath)
	}

	model := &PackageModel{
		ImportPath: importPath,
		Name:       pkg.Name,
	}

	scope := pkg.Types.Scope()

	for _, name := range scope.Names() {
		if includeFilter != nil && !includeFilter[name] {
			continue
		}

		obj := scope.Lookup(name)
		if !obj.Exported() {
			continue
		}

		switch o := obj.(type) {
		case *types.Func:
			model.Functions = append(model.Functions, extractFunction(o))

		case *types.TypeName:
			tm := extractType(o, pkg.Types)
			if tm != nil {
				model.Types = append(model.Types, *tm)
			}

		case *types.Const:
			model.Constants = append(model.Constants, extractConstant(o))
		}
	}

	return model, nil
}

func extractFunction(fn *types.Func) FunctionModel {
	sig := fn.Type().(*types.Signature)
	return functionModelFromSig(fn.Name(), sig, false, "")
}

func extractType(tn *types.TypeName, pkg *types.Package) *TypeModel {
	named, ok := tn.Type().(*types.Named)
	if !ok {
		return nil
	}

	tm := &TypeModel{
		Name:   tn.Name(),
		GoType: tn.Type(),
	}

	// Check if underlying type is a struct
	if st, ok := named.Underlying().(*types.Struct); ok {
		tm.IsStruct = true
		for i := 0; i < st.NumFields(); i++ {
			f := st.Field(i)
			if f.Exported() {
				tm.Fields = append(tm.Fields, FieldModel{
					Name:    f.Name(),
					GoType:  f.Type(),
					TypeStr: types.TypeString(f.Type(), qualifier(pkg)),
				})
			}
		}
	}

	// Collect pointer-receiver methods
	ptrType := types.NewPointer(named)
	mset := types.NewMethodSet(ptrType)
	for i := 0; i < mset.Len(); i++ {
		sel := mset.At(i)
		fn, ok := sel.Obj().(*types.Func)
		if !ok || !fn.Exported() {
			continue
		}
		// Only include methods directly defined on this type (not inherited)
		if sel.Index() != nil && len(sel.Index()) > 1 {
			continue
		}
		sig := fn.Type().(*types.Signature)
		tm.Methods = append(tm.Methods, functionModelFromSig(fn.Name(), sig, true, "*"+tn.Name()))
	}

	return tm
}

func extractConstant(c *types.Const) ConstantModel {
	val := c.Val()
	valStr := ""
	if val.Kind() == constant.String {
		valStr = constant.StringVal(val)
	} else {
		valStr = val.ExactString()
	}
	return ConstantModel{
		Name:    c.Name(),
		TypeStr: c.Type().String(),
		Value:   valStr,
	}
}

func functionModelFromSig(name string, sig *types.Signature, isMethod bool, recvType string) FunctionModel {
	fm := FunctionModel{
		Name:     name,
		IsMethod: isMethod,
		RecvType: recvType,
	}

	params := sig.Params()
	for i := 0; i < params.Len(); i++ {
		p := params.At(i)
		fm.Params = append(fm.Params, ParamModel{
			Name:    p.Name(),
			GoType:  p.Type(),
			TypeStr: p.Type().String(),
		})
	}

	results := sig.Results()
	for i := 0; i < results.Len(); i++ {
		r := results.At(i)
		fm.Results = append(fm.Results, ParamModel{
			Name:    r.Name(),
			GoType:  r.Type(),
			TypeStr: r.Type().String(),
		})
	}

	// Check if last result is error
	if results.Len() > 0 {
		lastResult := results.At(results.Len() - 1)
		if isErrorType(lastResult.Type()) {
			fm.ReturnsErr = true
		}
	}

	return fm
}

func isErrorType(t types.Type) bool {
	// Check if the type implements the error interface
	iface, ok := t.Underlying().(*types.Interface)
	if !ok {
		// Check if it's the named "error" type
		if named, ok := t.(*types.Named); ok {
			return named.Obj().Name() == "error" && named.Obj().Pkg() == nil
		}
		return false
	}
	// error interface has a single method Error() string
	if iface.NumMethods() == 1 {
		m := iface.Method(0)
		return m.Name() == "Error"
	}
	return false
}

func qualifier(pkg *types.Package) types.Qualifier {
	return func(other *types.Package) string {
		if other == pkg {
			return ""
		}
		return other.Name()
	}
}
