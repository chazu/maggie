// Package codegen generates Go code from Trashtalk AST.
// This file contains shared runtime plugin generation that links against libtrashtalk.dylib.
package codegen

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/dave/jennifer/jen"
)

// GenerateSharedPlugin produces Go source code for a plugin that links against libtrashtalk.
// The output registers itself with the shared runtime at load time via init().
func GenerateSharedPlugin(class *ast.Class) *Result {
	return GenerateSharedPluginWithOptions(class, GenerateOptions{})
}

// GenerateSharedPluginWithOptions produces shared plugin code with configurable options.
func GenerateSharedPluginWithOptions(class *ast.Class, opts GenerateOptions) *Result {
	// For primitiveClass, we generate a plugin using built-in implementations from primitiveRegistry.
	// Each method is treated as primitive and looked up in hasPrimitiveImpl().

	g := &generator{
		class:          class,
		warnings:       []string{},
		skipped:        []SkippedMethod{},
		instanceVars:   map[string]bool{},
		jsonVars:       map[string]bool{},
		skippedMethods: map[string]string{},
	}

	// Build instance var lookup and track JSON-typed vars
	for _, iv := range class.InstanceVars {
		g.instanceVars[iv.Name] = true
		// Determine if this var uses json.RawMessage (needs special handling)
		// String and number types use native Go types, everything else uses json.RawMessage
		isNativeType := iv.Default.Type == "number"
		if iv.Default.Type == "string" {
			// Check if the string value is actually JSON (object or array)
			val := strings.TrimSpace(iv.Default.Value)
			isJSONValue := (strings.HasPrefix(val, "{") && strings.HasSuffix(val, "}")) ||
				(strings.HasPrefix(val, "[") && strings.HasSuffix(val, "]"))
			isNativeType = !isJSONValue
		}
		if !isNativeType {
			// Fallback: check if default value looks numeric (for backwards compatibility)
			isNativeType = isNumericString(iv.Default.Value)
		}
		if !isNativeType {
			g.jsonVars[iv.Name] = true
		}
	}

	// First pass: generate code
	result := g.generateSharedPlugin()
	if result.Code == "" {
		return result
	}

	// Skip validation if requested
	if opts.SkipValidation {
		return result
	}

	// Validate generated Go code in-memory
	validator := NewCodeValidator(class.Name + ".go")
	validationErrors := validator.Validate(result.Code)

	if len(validationErrors) == 0 {
		return result
	}

	// Build goName -> selector mapping for error attribution
	goNameToSelector := make(map[string]string)
	for _, m := range class.Methods {
		goName := selectorToGoName(m.Selector)
		goNameToSelector[goName] = m.Selector
	}

	// Find selectors with errors
	badSelectors := validator.GetMethodSelectorsWithErrors(validationErrors, goNameToSelector)

	if len(badSelectors) == 0 {
		// Errors not attributable to specific methods (package-level issues)
		for _, ve := range validationErrors {
			result.Warnings = append(result.Warnings, fmt.Sprintf("Go validation: %s", ve.Message))
		}
		return result
	}

	// Add bad selectors to skippedMethods for regeneration
	validationSkipped := []SkippedMethod{}
	validationWarnings := []string{}

	for selector := range badSelectors {
		g.skippedMethods[selector] = "Go validation failed"
		validationSkipped = append(validationSkipped, SkippedMethod{
			Selector: selector,
			Reason:   "Go validation failed",
		})
		for _, ve := range validationErrors {
			if goNameToSelector[ve.Function] == selector {
				validationWarnings = append(validationWarnings, fmt.Sprintf("%s: %s", selector, ve.Message))
				break
			}
		}
	}

	// Reset state before regeneration
	g.skipped = []SkippedMethod{}
	g.warnings = []string{}

	// Regenerate without problematic methods
	result2 := g.generateSharedPlugin()

	// Combine: validation skipped methods + compilation skipped methods
	result2.SkippedMethods = append(validationSkipped, result2.SkippedMethods...)
	result2.Warnings = append(validationWarnings, result2.Warnings...)

	return result2
}

func (g *generator) generateSharedPlugin() *Result {
	// First pass: identify which methods will be skipped
	g.preIdentifySkippedMethods()

	// Compile methods first so we know what wrappers we'll generate
	compiled := g.compileMethods()

	// Split into class and instance methods
	var instanceMethods, classMethods []*compiledMethod
	for _, m := range compiled {
		if m.isClass {
			classMethods = append(classMethods, m)
		} else {
			instanceMethods = append(instanceMethods, m)
		}
	}

	f := jen.NewFile("main")

	// CGO preamble with libtrashtalk linking AND extern declarations for method wrappers
	// Note: LDFLAGS uses simple -L path. Runtime library path should be set via
	// DYLD_LIBRARY_PATH or by installing libtrashtalk.dylib to a standard location.
	preamble := g.buildCGOPreamble(instanceMethods, classMethods)
	f.CgoPreamble(preamble)

	// Import "C" for c-shared exports
	f.ImportAlias("C", "")

	// Add standard imports
	f.Anon("github.com/mattn/go-sqlite3")

	// ErrUnknownSelector
	f.Var().Id("ErrUnknownSelector").Op("=").Qual("errors", "New").Call(jen.Lit("unknown selector"))
	f.Line()

	// Struct definition (same as binary mode)
	g.generateStruct(f)
	f.Line()

	// Helper functions
	g.generateSharedPluginHelpers(f)
	f.Line()

	// Type conversion helpers (toInt, toBool, etc.)
	g.generateTypeHelpers(f)
	f.Line()

	// JSON primitive helpers
	g.generateJSONHelpers(f)
	f.Line()

	// gRPC helper functions for GrpcClient class (optimized for shared runtime)
	if g.class.Name == "GrpcClient" {
		g.generateGrpcHelpersShared(f)
	}

	// Generate method dispatch tables (internal, for runtime callbacks)
	g.generateSharedDispatch(f, instanceMethods)
	f.Line()
	g.generateSharedClassDispatch(f, classMethods)
	f.Line()

	// Generate CGO-exported method wrappers
	g.generateSharedMethodExports(f, instanceMethods, classMethods)
	f.Line()

	// Generate runtime-compatible method wrappers for method registration
	g.generateRuntimeMethodWrappers(f, instanceMethods, classMethods)
	f.Line()

	// Generate init() with method table (after wrappers so we can reference them)
	g.generateSharedInitWithMethods(f, instanceMethods, classMethods)
	f.Line()

	// Generate method implementations
	for _, m := range compiled {
		g.generateMethod(f, m)
	}

	// Empty main (required for c-shared but unused)
	f.Func().Id("main").Params().Block()

	// Render output
	var buf bytes.Buffer
	if err := f.Render(&buf); err != nil {
		return &Result{
			Code:           "",
			Warnings:       append(g.warnings, "render error: "+err.Error()),
			SkippedMethods: g.skipped,
		}
	}

	return &Result{
		Code:           buf.String(),
		Warnings:       g.warnings,
		SkippedMethods: g.skipped,
	}
}

// generateSharedInit generates the init() function that registers the class with the shared runtime
// DEPRECATED: Use generateSharedInitWithMethods instead
func (g *generator) generateSharedInit(f *jen.File) {
	g.generateSharedInitWithMethods(f, nil, nil)
}

// generateSharedInitWithMethods generates init() with method table registration
func (g *generator) generateSharedInitWithMethods(f *jen.File, instanceMethods, classMethods []*compiledMethod) {
	className := g.class.Name
	qualifiedName := g.class.QualifiedName()
	superclass := g.class.Parent
	cClassName := strings.ReplaceAll(className, "::", "__")

	// Build instance variable names array
	varNames := make([]string, 0, len(g.class.InstanceVars))
	for _, iv := range g.class.InstanceVars {
		varNames = append(varNames, iv.Name)
	}

	// Build init function body statements
	initStmts := []jen.Code{
		// Convert class name to C string
		jen.Id("className").Op(":=").Qual("C", "CString").Call(jen.Lit(qualifiedName)),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("className"))),

		// Convert superclass to C string (nil if none)
		jen.Id("superclass").Op(":=").Func().Params().Op("*").Qual("C", "char").Block(
			jen.If(jen.Lit(superclass).Op("==").Lit("")).Block(
				jen.Return(jen.Nil()),
			),
			jen.Return(jen.Qual("C", "CString").Call(jen.Lit(superclass))),
		).Call(),

		// Build instance vars array
		g.generateInstanceVarsArray(varNames),
	}

	// Build method table if methods provided
	if len(instanceMethods) > 0 || len(classMethods) > 0 {
		// Allocate instance method entries in C memory
		if len(instanceMethods) > 0 {
			instMethodStmts := []jen.Code{
				// Allocate C memory for method entries
				jen.Id("instMethodsPtr").Op(":=").Parens(jen.Op("*").Qual("C", "TTMethodEntry")).Parens(
					jen.Qual("C", "malloc").Call(
						jen.Qual("C", "size_t").Call(
							jen.Qual("unsafe", "Sizeof").Call(jen.Qual("C", "TTMethodEntry").Values()).Op("*").Lit(len(instanceMethods)),
						),
					),
				),
				// Create a Go slice view for easier population
				jen.Id("instMethods").Op(":=").Qual("unsafe", "Slice").Call(
					jen.Id("instMethodsPtr"),
					jen.Lit(len(instanceMethods)),
				),
			}
			for i, m := range instanceMethods {
				wrapperName := fmt.Sprintf("__%s_method_%s", cClassName, m.goName)
				instMethodStmts = append(instMethodStmts,
					jen.Id("instMethods").Index(jen.Lit(i)).Op("=").Qual("C", "TTMethodEntry").Values(jen.Dict{
						jen.Id("selector"): jen.Qual("C", "CString").Call(jen.Lit(m.selector)),
						jen.Id("impl"):     jen.Parens(jen.Qual("C", "TTMethodFunc")).Parens(jen.Qual("C", wrapperName)),
						jen.Id("numArgs"):  jen.Qual("C", "int").Call(jen.Lit(len(m.args))),
						jen.Id("flags"):    jen.Qual("C", "TT_METHOD_NATIVE"),
					}),
				)
			}
			initStmts = append(initStmts, instMethodStmts...)
		} else {
			initStmts = append(initStmts, jen.Var().Id("instMethodsPtr").Op("*").Qual("C", "TTMethodEntry"))
		}

		// Allocate class method entries in C memory
		if len(classMethods) > 0 {
			classMethodStmts := []jen.Code{
				// Allocate C memory for method entries
				jen.Id("classMethodsPtr").Op(":=").Parens(jen.Op("*").Qual("C", "TTMethodEntry")).Parens(
					jen.Qual("C", "malloc").Call(
						jen.Qual("C", "size_t").Call(
							jen.Qual("unsafe", "Sizeof").Call(jen.Qual("C", "TTMethodEntry").Values()).Op("*").Lit(len(classMethods)),
						),
					),
				),
				// Create a Go slice view for easier population
				jen.Id("classMethods").Op(":=").Qual("unsafe", "Slice").Call(
					jen.Id("classMethodsPtr"),
					jen.Lit(len(classMethods)),
				),
			}
			for i, m := range classMethods {
				wrapperName := fmt.Sprintf("__%s_classmethod_%s", cClassName, m.goName)
				classMethodStmts = append(classMethodStmts,
					jen.Id("classMethods").Index(jen.Lit(i)).Op("=").Qual("C", "TTMethodEntry").Values(jen.Dict{
						jen.Id("selector"): jen.Qual("C", "CString").Call(jen.Lit(m.selector)),
						jen.Id("impl"):     jen.Parens(jen.Qual("C", "TTMethodFunc")).Parens(jen.Qual("C", wrapperName)),
						jen.Id("numArgs"):  jen.Qual("C", "int").Call(jen.Lit(len(m.args))),
						jen.Id("flags"):    jen.Qual("C", "TT_METHOD_NATIVE").Op("|").Qual("C", "TT_METHOD_CLASS_METHOD"),
					}),
				)
			}
			initStmts = append(initStmts, classMethodStmts...)
		} else {
			initStmts = append(initStmts, jen.Var().Id("classMethodsPtr").Op("*").Qual("C", "TTMethodEntry"))
		}

		// Build method table struct
		initStmts = append(initStmts,
			jen.Id("methods").Op(":=").Op("&").Qual("C", "TTMethodTable").Values(jen.Dict{
				jen.Id("instanceMethods"):    jen.Id("instMethodsPtr"),
				jen.Id("numInstanceMethods"): jen.Qual("C", "int").Call(jen.Lit(len(instanceMethods))),
				jen.Id("classMethods"):       jen.Id("classMethodsPtr"),
				jen.Id("numClassMethods"):    jen.Qual("C", "int").Call(jen.Lit(len(classMethods))),
			}),
		)

		// Register with method table
		initStmts = append(initStmts,
			jen.Qual("C", "TT_RegisterClass").Call(
				jen.Id("className"),
				jen.Id("superclass"),
				jen.Id("instanceVars"),
				jen.Qual("C", "int").Call(jen.Lit(len(varNames))),
				jen.Id("methods"),
			),
		)
	} else {
		// No methods - register with nil
		initStmts = append(initStmts,
			jen.Qual("C", "TT_RegisterClass").Call(
				jen.Id("className"),
				jen.Id("superclass"),
				jen.Id("instanceVars"),
				jen.Qual("C", "int").Call(jen.Lit(len(varNames))),
				jen.Nil(),
			),
		)
	}

	f.Comment("// init registers this class with the shared runtime")
	f.Func().Id("init").Params().Block(initStmts...)
	f.Line()

	// Also add a GetClassName export for backward compatibility
	f.Comment("//export GetClassName")
	f.Func().Id("GetClassName").Params().Op("*").Qual("C", "char").Block(
		jen.Return(jen.Qual("C", "CString").Call(jen.Lit(className))),
	)
}

// methodTablePointer generates the pointer to a method entry array
func (g *generator) methodTablePointer(varName string, length int) *jen.Statement {
	if length == 0 {
		return jen.Nil()
	}
	return jen.Op("&").Id(varName).Index(jen.Lit(0))
}

// buildCGOPreamble generates the CGO preamble with extern declarations for method wrappers
func (g *generator) buildCGOPreamble(instanceMethods, classMethods []*compiledMethod) string {
	className := g.class.Name
	cClassName := strings.ReplaceAll(className, "::", "__")

	var sb strings.Builder

	// Standard includes and flags
	sb.WriteString(`
#cgo CFLAGS: -I${SRCDIR}/../../include
#cgo LDFLAGS: -L${SRCDIR}/../../lib -ltrashtalk
#include <libtrashtalk.h>
#include <stdlib.h>
`)

	// Add extern declarations for method wrappers if we have any
	if len(instanceMethods) > 0 || len(classMethods) > 0 {
		sb.WriteString("\n// Extern declarations for method wrappers (defined via //export)\n")

		for _, m := range instanceMethods {
			wrapperName := fmt.Sprintf("__%s_method_%s", cClassName, m.goName)
			sb.WriteString(fmt.Sprintf("extern TTValue %s(TTInstance* self, TTValue* args, int numArgs);\n", wrapperName))
		}

		for _, m := range classMethods {
			wrapperName := fmt.Sprintf("__%s_classmethod_%s", cClassName, m.goName)
			sb.WriteString(fmt.Sprintf("extern TTValue %s(TTInstance* self, TTValue* args, int numArgs);\n", wrapperName))
		}
	}

	return sb.String()
}

// generateInstanceVarsArray generates code to create a C array of instance var names
func (g *generator) generateInstanceVarsArray(varNames []string) *jen.Statement {
	if len(varNames) == 0 {
		return jen.Id("instanceVars").Op(":=").Parens(jen.Op("**").Qual("C", "char")).Parens(jen.Nil())
	}

	// Build the block statements
	blockStmts := []jen.Code{
		jen.Id("vars").Op(":=").Make(jen.Index().Op("*").Qual("C", "char"), jen.Lit(len(varNames))),
	}
	for i, name := range varNames {
		blockStmts = append(blockStmts,
			jen.Id("vars").Index(jen.Lit(i)).Op("=").Qual("C", "CString").Call(jen.Lit(name)),
		)
	}
	blockStmts = append(blockStmts, jen.Return(jen.Op("&").Id("vars").Index(jen.Lit(0))))

	// Build array of C strings
	return jen.Id("instanceVars").Op(":=").Func().Params().Op("**").Qual("C", "char").Block(
		blockStmts...,
	).Call()
}

// generateSharedPluginHelpers generates helper functions that use the shared runtime
func (g *generator) generateSharedPluginHelpers(f *jen.File) {
	className := g.class.Name

	// openDB - delegates to shared runtime's database
	f.Func().Id("openDB").Params().Parens(jen.List(jen.Op("*").Qual("database/sql", "DB"), jen.Error())).Block(
		jen.Id("dbPath").Op(":=").Qual("os", "Getenv").Call(jen.Lit("SQLITE_JSON_DB")),
		jen.If(jen.Id("dbPath").Op("==").Lit("")).Block(
			jen.List(jen.Id("home"), jen.Id("_")).Op(":=").Qual("os", "UserHomeDir").Call(),
			jen.Id("dbPath").Op("=").Qual("path/filepath", "Join").Call(jen.Id("home"), jen.Lit(".trashtalk"), jen.Lit("instances.db")),
		),
		jen.Return(jen.Qual("database/sql", "Open").Call(jen.Lit("sqlite3"), jen.Id("dbPath"))),
	)
	f.Line()

	// loadInstance
	f.Func().Id("loadInstance").Params(
		jen.Id("db").Op("*").Qual("database/sql", "DB"),
		jen.Id("id").String(),
	).Parens(jen.List(jen.Op("*").Id(className), jen.Error())).Block(
		jen.Var().Id("data").String(),
		jen.Err().Op(":=").Id("db").Dot("QueryRow").Call(jen.Lit("SELECT data FROM instances WHERE id = ?"), jen.Id("id")).Dot("Scan").Call(jen.Op("&").Id("data")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Err()),
		),
		jen.Var().Id("instance").Id(className),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(jen.Index().Byte().Parens(jen.Id("data")), jen.Op("&").Id("instance")).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Err()),
		),
		jen.Return(jen.Op("&").Id("instance"), jen.Nil()),
	)
	f.Line()

	// saveInstance
	f.Func().Id("saveInstance").Params(
		jen.Id("db").Op("*").Qual("database/sql", "DB"),
		jen.Id("id").String(),
		jen.Id("instance").Op("*").Id(className),
	).Error().Block(
		jen.List(jen.Id("data"), jen.Err()).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("instance")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Err()),
		),
		jen.List(jen.Id("_"), jen.Err()).Op("=").Id("db").Dot("Exec").Call(
			jen.Lit("INSERT OR REPLACE INTO instances (id, data) VALUES (?, json(?))"),
			jen.Id("id"),
			jen.String().Parens(jen.Id("data")),
		),
		jen.Return(jen.Err()),
	)
	f.Line()

	// sendMessage - uses shared runtime's TT_Send
	f.Func().Id("sendMessage").Params(
		jen.Id("receiver").Interface(),
		jen.Id("selector").String(),
		jen.Id("args").Op("...").Interface(),
	).String().Block(
		// Convert receiver to string
		jen.Id("receiverStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("receiver")),

		// Convert to C strings
		jen.Id("cReceiver").Op(":=").Qual("C", "CString").Call(jen.Id("receiverStr")),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cReceiver"))),
		jen.Id("cSelector").Op(":=").Qual("C", "CString").Call(jen.Id("selector")),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cSelector"))),

		// Build args array
		jen.Id("cArgs").Op(":=").Make(jen.Index().Qual("C", "TTValue"), jen.Len(jen.Id("args"))),
		jen.For(jen.List(jen.Id("i"), jen.Id("arg")).Op(":=").Range().Id("args")).Block(
			jen.Id("argStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arg")),
			jen.Id("cStr").Op(":=").Qual("C", "CString").Call(jen.Id("argStr")),
			jen.Id("cArgs").Index(jen.Id("i")).Op("=").Qual("C", "TT_MakeString").Call(jen.Id("cStr")),
		),

		// Call TT_Send
		jen.Var().Id("argsPtr").Op("*").Qual("C", "TTValue"),
		jen.If(jen.Len(jen.Id("cArgs")).Op(">").Lit(0)).Block(
			jen.Id("argsPtr").Op("=").Op("&").Id("cArgs").Index(jen.Lit(0)),
		),
		jen.Id("result").Op(":=").Qual("C", "TT_Send").Call(
			jen.Id("cReceiver"),
			jen.Id("cSelector"),
			jen.Id("argsPtr"),
			jen.Qual("C", "int").Call(jen.Len(jen.Id("args"))),
		),

		// Convert result to Go string
		jen.Id("cResult").Op(":=").Qual("C", "TT_ValueAsString").Call(jen.Id("result")),
		jen.If(jen.Id("cResult").Op("==").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cResult"))),
		jen.Return(jen.Qual("C", "GoString").Call(jen.Id("cResult"))),
	)
	f.Line()

	// sendMessageDirect - uses shared runtime's TT_SendDirect with instance pointer
	f.Comment("// sendMessageDirect sends a message using direct instance pointer (faster than sendMessage)")
	f.Func().Id("sendMessageDirect").Params(
		jen.Id("inst").Op("*").Qual("C", "TTInstance"),
		jen.Id("selector").String(),
		jen.Id("args").Op("...").Interface(),
	).String().Block(
		// Convert to C strings
		jen.Id("cSelector").Op(":=").Qual("C", "CString").Call(jen.Id("selector")),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cSelector"))),

		// Build args array
		jen.Id("cArgs").Op(":=").Make(jen.Index().Qual("C", "TTValue"), jen.Len(jen.Id("args"))),
		jen.For(jen.List(jen.Id("i"), jen.Id("arg")).Op(":=").Range().Id("args")).Block(
			jen.Id("argStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arg")),
			jen.Id("cStr").Op(":=").Qual("C", "CString").Call(jen.Id("argStr")),
			jen.Id("cArgs").Index(jen.Id("i")).Op("=").Qual("C", "TT_MakeString").Call(jen.Id("cStr")),
		),

		// Call TT_SendDirect
		jen.Var().Id("argsPtr").Op("*").Qual("C", "TTValue"),
		jen.If(jen.Len(jen.Id("cArgs")).Op(">").Lit(0)).Block(
			jen.Id("argsPtr").Op("=").Op("&").Id("cArgs").Index(jen.Lit(0)),
		),
		jen.Id("result").Op(":=").Qual("C", "TT_SendDirect").Call(
			jen.Id("inst"),
			jen.Id("cSelector"),
			jen.Id("argsPtr"),
			jen.Qual("C", "int").Call(jen.Len(jen.Id("args"))),
		),

		// Convert result to Go string
		jen.Id("cResult").Op(":=").Qual("C", "TT_ValueAsString").Call(jen.Id("result")),
		jen.If(jen.Id("cResult").Op("==").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cResult"))),
		jen.Return(jen.Qual("C", "GoString").Call(jen.Id("cResult"))),
	)
	f.Line()

	// lookupInstance - gets a TTInstance pointer from an ID
	f.Comment("// lookupInstance retrieves an instance pointer from the shared runtime")
	f.Func().Id("lookupInstance").Params(
		jen.Id("instanceID").String(),
	).Op("*").Qual("C", "TTInstance").Block(
		jen.Id("cID").Op(":=").Qual("C", "CString").Call(jen.Id("instanceID")),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cID"))),
		jen.Return(jen.Qual("C", "TT_Lookup").Call(jen.Id("cID"))),
	)
	f.Line()

	// lookupBlock - gets a TTBlock pointer from a block ID
	f.Comment("// lookupBlock retrieves a block pointer from the shared runtime")
	f.Func().Id("lookupBlock").Params(
		jen.Id("blockID").String(),
	).Op("*").Qual("C", "TTBlock").Block(
		jen.Id("cID").Op(":=").Qual("C", "CString").Call(jen.Id("blockID")),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cID"))),
		jen.Return(jen.Qual("C", "TT_LookupBlock").Call(jen.Id("cID"))),
	)
	f.Line()

	// invokeBlockDirect - uses shared runtime's TT_InvokeBlockDirect with block pointer
	f.Comment("// invokeBlockDirect invokes a block using direct pointer (faster than invokeBlock)")
	f.Func().Id("invokeBlockDirect").Params(
		jen.Id("block").Op("*").Qual("C", "TTBlock"),
		jen.Id("args").Op("...").Interface(),
	).String().Block(
		jen.If(jen.Id("block").Op("==").Nil()).Block(
			jen.Return(jen.Lit("")),
		),

		// Build args array
		jen.Id("cArgs").Op(":=").Make(jen.Index().Qual("C", "TTValue"), jen.Len(jen.Id("args"))),
		jen.For(jen.List(jen.Id("i"), jen.Id("arg")).Op(":=").Range().Id("args")).Block(
			jen.Id("argStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arg")),
			jen.Id("cStr").Op(":=").Qual("C", "CString").Call(jen.Id("argStr")),
			jen.Id("cArgs").Index(jen.Id("i")).Op("=").Qual("C", "TT_MakeString").Call(jen.Id("cStr")),
		),

		// Call TT_InvokeBlockDirect
		jen.Var().Id("argsPtr").Op("*").Qual("C", "TTValue"),
		jen.If(jen.Len(jen.Id("cArgs")).Op(">").Lit(0)).Block(
			jen.Id("argsPtr").Op("=").Op("&").Id("cArgs").Index(jen.Lit(0)),
		),
		jen.Id("result").Op(":=").Qual("C", "TT_InvokeBlockDirect").Call(
			jen.Id("block"),
			jen.Id("argsPtr"),
			jen.Qual("C", "int").Call(jen.Len(jen.Id("args"))),
		),

		// Convert result to Go string
		jen.Id("cResult").Op(":=").Qual("C", "TT_ValueAsString").Call(jen.Id("result")),
		jen.If(jen.Id("cResult").Op("==").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cResult"))),
		jen.Return(jen.Qual("C", "GoString").Call(jen.Id("cResult"))),
	)
	f.Line()

	// invokeBlock - uses shared runtime's TT_InvokeBlock
	f.Comment("// invokeBlock calls a Trashtalk block through the shared runtime")
	f.Func().Id("invokeBlock").Params(
		jen.Id("blockID").String(),
		jen.Id("args").Op("...").Interface(),
	).String().Block(
		// Convert block ID to C string
		jen.Id("cBlockID").Op(":=").Qual("C", "CString").Call(jen.Id("blockID")),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cBlockID"))),

		// Build args array
		jen.Id("cArgs").Op(":=").Make(jen.Index().Qual("C", "TTValue"), jen.Len(jen.Id("args"))),
		jen.For(jen.List(jen.Id("i"), jen.Id("arg")).Op(":=").Range().Id("args")).Block(
			jen.Id("argStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arg")),
			jen.Id("cStr").Op(":=").Qual("C", "CString").Call(jen.Id("argStr")),
			jen.Id("cArgs").Index(jen.Id("i")).Op("=").Qual("C", "TT_MakeString").Call(jen.Id("cStr")),
		),

		// Call TT_InvokeBlock
		jen.Var().Id("argsPtr").Op("*").Qual("C", "TTValue"),
		jen.If(jen.Len(jen.Id("cArgs")).Op(">").Lit(0)).Block(
			jen.Id("argsPtr").Op("=").Op("&").Id("cArgs").Index(jen.Lit(0)),
		),
		jen.Id("result").Op(":=").Qual("C", "TT_InvokeBlock").Call(
			jen.Id("cBlockID"),
			jen.Id("argsPtr"),
			jen.Qual("C", "int").Call(jen.Len(jen.Id("args"))),
		),

		// Convert result to Go string
		jen.Id("cResult").Op(":=").Qual("C", "TT_ValueAsString").Call(jen.Id("result")),
		jen.If(jen.Id("cResult").Op("==").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Defer().Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cResult"))),
		jen.Return(jen.Qual("C", "GoString").Call(jen.Id("cResult"))),
	)
}

// generateSharedDispatch generates the internal dispatch switch for instance methods
func (g *generator) generateSharedDispatch(f *jen.File, methods []*compiledMethod) {
	className := g.class.Name

	cases := []jen.Code{}
	for _, m := range methods {
		methodName := m.goName
		if g.instanceVars[m.selector] {
			methodName = "Get" + methodName
		}

		var callExpr *jen.Statement
		if len(m.args) > 0 {
			argCheck := jen.If(jen.Len(jen.Id("args")).Op("<").Lit(len(m.args))).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit(m.selector+" requires "+fmt.Sprintf("%d", len(m.args))+" argument"))),
			)

			callArgs := []jen.Code{}
			for i := range m.args {
				callArgs = append(callArgs, jen.Id("args").Index(jen.Lit(i)))
			}
			callExpr = jen.Id("c").Dot(methodName).Call(callArgs...)

			if m.returnsErr {
				cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
					argCheck,
					jen.Return(callExpr),
				))
			} else {
				cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
					argCheck,
					jen.Return(callExpr, jen.Nil()),
				))
			}
		} else {
			callExpr = jen.Id("c").Dot(methodName).Call()
			if m.hasReturn {
				if m.returnsErr {
					cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
						jen.Return(callExpr),
					))
				} else {
					cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
						jen.Return(callExpr, jen.Nil()),
					))
				}
			} else {
				cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
					callExpr,
					jen.Return(jen.Lit(""), jen.Nil()),
				))
			}
		}
	}

	cases = append(cases, jen.Default().Block(
		jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("%w: %s"), jen.Id("ErrUnknownSelector"), jen.Id("selector"))),
	))

	f.Func().Id("dispatch").Params(
		jen.Id("c").Op("*").Id(className),
		jen.Id("selector").String(),
		jen.Id("args").Index().String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.Switch(jen.Id("selector")).Block(cases...),
	)
}

// generateSharedClassDispatch generates the dispatch switch for class methods
func (g *generator) generateSharedClassDispatch(f *jen.File, methods []*compiledMethod) {
	cases := []jen.Code{}
	for _, m := range methods {
		var callExpr *jen.Statement
		if len(m.args) > 0 {
			argCheck := jen.If(jen.Len(jen.Id("args")).Op("<").Lit(len(m.args))).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit(m.selector+" requires "+fmt.Sprintf("%d", len(m.args))+" argument"))),
			)

			callArgs := []jen.Code{}
			for i := range m.args {
				callArgs = append(callArgs, jen.Id("args").Index(jen.Lit(i)))
			}
			callExpr = jen.Id(m.goName).Call(callArgs...)

			if m.returnsErr {
				cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
					argCheck,
					jen.Return(callExpr),
				))
			} else {
				cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
					argCheck,
					jen.Return(callExpr, jen.Nil()),
				))
			}
		} else {
			callExpr = jen.Id(m.goName).Call()
			if m.hasReturn {
				if m.returnsErr {
					cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
						jen.Return(callExpr),
					))
				} else {
					cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
						jen.Return(callExpr, jen.Nil()),
					))
				}
			} else {
				cases = append(cases, jen.Case(jen.Lit(m.selector)).Block(
					callExpr,
					jen.Return(jen.Lit(""), jen.Nil()),
				))
			}
		}
	}

	cases = append(cases, jen.Default().Block(
		jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("%w: %s"), jen.Id("ErrUnknownSelector"), jen.Id("selector"))),
	))

	f.Func().Id("dispatchClass").Params(
		jen.Id("selector").String(),
		jen.Id("args").Index().String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.Switch(jen.Id("selector")).Block(cases...),
	)
}

// generateSharedMethodExports generates CGO-exported wrappers for methods
// These allow the shared runtime to call into this plugin's methods
func (g *generator) generateSharedMethodExports(f *jen.File, instanceMethods, classMethods []*compiledMethod) {
	className := g.class.Name
	// Make a valid C identifier from the class name
	cClassName := strings.ReplaceAll(className, "::", "__")

	// Generate a single Dispatch export that the runtime can call
	// This is simpler than exporting every method individually
	f.Comment("//export " + cClassName + "_Dispatch")
	f.Func().Id(cClassName + "_Dispatch").Params(
		jen.Id("instanceJSON").Op("*").Qual("C", "char"),
		jen.Id("selector").Op("*").Qual("C", "char"),
		jen.Id("argsJSON").Op("*").Qual("C", "char"),
	).Op("*").Qual("C", "char").Block(
		jen.Id("instanceStr").Op(":=").Qual("C", "GoString").Call(jen.Id("instanceJSON")),
		jen.Id("selectorStr").Op(":=").Qual("C", "GoString").Call(jen.Id("selector")),
		jen.Id("argsStr").Op(":=").Qual("C", "GoString").Call(jen.Id("argsJSON")),
		jen.Line(),
		jen.Id("result").Op(":=").Id("dispatchInternal").Call(
			jen.Id("instanceStr"),
			jen.Id("selectorStr"),
			jen.Id("argsStr"),
		),
		jen.Return(jen.Qual("C", "CString").Call(jen.Id("result"))),
	)
	f.Line()

	// Generate dispatchInternal that handles both class and instance methods
	f.Func().Id("dispatchInternal").Params(
		jen.Id("instanceJSON").String(),
		jen.Id("selector").String(),
		jen.Id("argsJSON").String(),
	).String().Block(
		jen.Var().Id("args").Index().String(),
		jen.Qual("encoding/json", "Unmarshal").Call(jen.Index().Byte().Parens(jen.Id("argsJSON")), jen.Op("&").Id("args")),
		jen.Line(),
		// Check if this is a class method call
		jen.If(jen.Id("instanceJSON").Op("==").Lit("").Op("||").Id("instanceJSON").Op("==").Lit(className).Op("||").Id("instanceJSON").Op("==").Lit(g.class.QualifiedName())).Block(
			jen.List(jen.Id("result"), jen.Err()).Op(":=").Id("dispatchClass").Call(jen.Id("selector"), jen.Id("args")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.If(jen.Qual("errors", "Is").Call(jen.Err(), jen.Id("ErrUnknownSelector"))).Block(
					jen.Return(jen.Lit(`{"exit_code":200}`)),
				),
				jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit(`{"exit_code":1,"error":%q}`), jen.Err().Dot("Error").Call())),
			),
			jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit(`{"result":%q,"exit_code":0}`), jen.Id("result"))),
		),
		jen.Line(),
		// Instance method - parse instance JSON
		jen.Var().Id("instance").Id(className),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(jen.Index().Byte().Parens(jen.Id("instanceJSON")), jen.Op("&").Id("instance")).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit(`{"exit_code":1,"error":%q}`), jen.Err().Dot("Error").Call())),
		),
		jen.Line(),
		// Dispatch to instance method
		jen.List(jen.Id("result"), jen.Err()).Op(":=").Id("dispatch").Call(jen.Op("&").Id("instance"), jen.Id("selector"), jen.Id("args")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.If(jen.Qual("errors", "Is").Call(jen.Err(), jen.Id("ErrUnknownSelector"))).Block(
				jen.Return(jen.Lit(`{"exit_code":200}`)),
			),
			jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit(`{"exit_code":1,"error":%q}`), jen.Err().Dot("Error").Call())),
		),
		jen.Line(),
		// Return updated instance + result
		jen.List(jen.Id("updatedJSON"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Op("&").Id("instance")),
		jen.Return(
			jen.Qual("fmt", "Sprintf").Call(jen.Lit(`{"instance":%q,"result":%q,"exit_code":0}`), jen.String().Parens(jen.Id("updatedJSON")), jen.Id("result")),
		),
	)
}

// generateRuntimeMethodWrappers generates C-compatible wrapper functions for each method
// These wrappers have the TTMethodFunc signature so they can be registered with the runtime
func (g *generator) generateRuntimeMethodWrappers(f *jen.File, instanceMethods, classMethods []*compiledMethod) {
	className := g.class.Name
	cClassName := strings.ReplaceAll(className, "::", "__")

	f.Line()
	f.Comment("// Runtime method wrappers - C-compatible functions for method registration")
	f.Line()

	// Generate wrappers for instance methods
	for _, m := range instanceMethods {
		wrapperName := fmt.Sprintf("__%s_method_%s", cClassName, m.goName)
		selector := m.selector

		f.Comment(fmt.Sprintf("//export %s", wrapperName))
		f.Func().Id(wrapperName).Params(
			jen.Id("self").Op("*").Qual("C", "TTInstance"),
			jen.Id("args").Op("*").Qual("C", "TTValue"),
			jen.Id("numArgs").Qual("C", "int"),
		).Qual("C", "TTValue").Block(
			// Serialize instance to JSON
			jen.Id("cJSON").Op(":=").Qual("C", "TT_Serialize").Call(jen.Id("self")),
			jen.Id("instanceJSON").Op(":=").Qual("C", "GoString").Call(jen.Id("cJSON")),
			jen.Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cJSON"))),
			jen.Line(),
			// Convert args to string slice
			jen.Id("goArgs").Op(":=").Make(jen.Index().String(), jen.Int().Parens(jen.Id("numArgs"))),
			jen.If(jen.Id("args").Op("!=").Nil().Op("&&").Id("numArgs").Op(">").Lit(0)).Block(
				jen.Id("cArgs").Op(":=").Qual("unsafe", "Slice").Call(jen.Id("args"), jen.Int().Parens(jen.Id("numArgs"))),
				jen.For(jen.List(jen.Id("i"), jen.Id("cArg")).Op(":=").Range().Id("cArgs")).Block(
					jen.Id("cStr").Op(":=").Qual("C", "TT_ValueAsString").Call(jen.Id("cArg")),
					jen.If(jen.Id("cStr").Op("!=").Nil()).Block(
						jen.Id("goArgs").Index(jen.Id("i")).Op("=").Qual("C", "GoString").Call(jen.Id("cStr")),
						jen.Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cStr"))),
					),
				),
			),
			jen.Line(),
			// Call dispatch with the selector
			jen.Id("argsJSON").Op(",").Id("_").Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("goArgs")),
			jen.Id("resultJSON").Op(":=").Id("dispatchInternal").Call(jen.Id("instanceJSON"), jen.Lit(selector), jen.String().Parens(jen.Id("argsJSON"))),
			jen.Line(),
			// Parse result and update instance in runtime
			jen.Var().Id("result").Struct(
				jen.Id("Instance").String().Tag(map[string]string{"json": "instance"}),
				jen.Id("Result").String().Tag(map[string]string{"json": "result"}),
				jen.Id("ExitCode").Int().Tag(map[string]string{"json": "exit_code"}),
			),
			jen.Qual("encoding/json", "Unmarshal").Call(jen.Index().Byte().Parens(jen.Id("resultJSON")), jen.Op("&").Id("result")),
			jen.Line(),
			// Update instance state if modified
			jen.If(jen.Id("result").Dot("Instance").Op("!=").Lit("").Op("&&").Id("self").Op("!=").Nil()).Block(
				jen.Id("cNewJSON").Op(":=").Qual("C", "CString").Call(jen.Id("result").Dot("Instance")),
				jen.Qual("C", "TT_Deserialize").Call(jen.Id("cNewJSON")),
				jen.Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cNewJSON"))),
			),
			jen.Line(),
			// Return result
			jen.If(jen.Id("result").Dot("ExitCode").Op("!=").Lit(0)).Block(
				jen.Return(jen.Qual("C", "TT_MakeNil").Call()),
			),
			jen.Id("cResult").Op(":=").Qual("C", "CString").Call(jen.Id("result").Dot("Result")),
			jen.Return(jen.Qual("C", "TT_MakeString").Call(jen.Id("cResult"))),
		)
		f.Line()
	}

	// Generate wrappers for class methods
	for _, m := range classMethods {
		wrapperName := fmt.Sprintf("__%s_classmethod_%s", cClassName, m.goName)
		selector := m.selector

		f.Comment(fmt.Sprintf("//export %s", wrapperName))
		f.Func().Id(wrapperName).Params(
			jen.Id("self").Op("*").Qual("C", "TTInstance"),
			jen.Id("args").Op("*").Qual("C", "TTValue"),
			jen.Id("numArgs").Qual("C", "int"),
		).Qual("C", "TTValue").Block(
			// Convert args to string slice
			jen.Id("goArgs").Op(":=").Make(jen.Index().String(), jen.Int().Parens(jen.Id("numArgs"))),
			jen.If(jen.Id("args").Op("!=").Nil().Op("&&").Id("numArgs").Op(">").Lit(0)).Block(
				jen.Id("cArgs").Op(":=").Qual("unsafe", "Slice").Call(jen.Id("args"), jen.Int().Parens(jen.Id("numArgs"))),
				jen.For(jen.List(jen.Id("i"), jen.Id("cArg")).Op(":=").Range().Id("cArgs")).Block(
					jen.Id("cStr").Op(":=").Qual("C", "TT_ValueAsString").Call(jen.Id("cArg")),
					jen.If(jen.Id("cStr").Op("!=").Nil()).Block(
						jen.Id("goArgs").Index(jen.Id("i")).Op("=").Qual("C", "GoString").Call(jen.Id("cStr")),
						jen.Qual("C", "free").Call(jen.Qual("unsafe", "Pointer").Call(jen.Id("cStr"))),
					),
				),
			),
			jen.Line(),
			// Call dispatch with the selector (empty instance for class method)
			jen.Id("argsJSON").Op(",").Id("_").Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("goArgs")),
			jen.Id("resultJSON").Op(":=").Id("dispatchInternal").Call(jen.Lit(""), jen.Lit(selector), jen.String().Parens(jen.Id("argsJSON"))),
			jen.Line(),
			// Parse result
			jen.Var().Id("result").Struct(
				jen.Id("Result").String().Tag(map[string]string{"json": "result"}),
				jen.Id("ExitCode").Int().Tag(map[string]string{"json": "exit_code"}),
			),
			jen.Qual("encoding/json", "Unmarshal").Call(jen.Index().Byte().Parens(jen.Id("resultJSON")), jen.Op("&").Id("result")),
			jen.Line(),
			// Return result
			jen.If(jen.Id("result").Dot("ExitCode").Op("!=").Lit(0)).Block(
				jen.Return(jen.Qual("C", "TT_MakeNil").Call()),
			),
			jen.Id("cResult").Op(":=").Qual("C", "CString").Call(jen.Id("result").Dot("Result")),
			jen.Return(jen.Qual("C", "TT_MakeString").Call(jen.Id("cResult"))),
		)
		f.Line()
	}
}

// generateGrpcHelpersShared generates gRPC helpers optimized for shared runtime
// This version uses lookupBlock/invokeBlockDirect for zero-overhead streaming callbacks
func (g *generator) generateGrpcHelpersShared(f *jen.File) {
	f.Line()
	f.Comment("// gRPC helper functions for GrpcClient (shared runtime optimized)")
	f.Line()

	// getConnection - lazy connection creation
	f.Comment("// getConnection returns an existing connection or creates a new one")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("getConnection").Params().Parens(jen.List(
		jen.Op("*").Qual("google.golang.org/grpc", "ClientConn"),
		jen.Error(),
	)).Block(
		jen.If(jen.Id("c").Dot("conn").Op("!=").Nil()).Block(
			jen.Return(jen.Id("c").Dot("conn"), jen.Nil()),
		),
		jen.Var().Id("opts").Index().Qual("google.golang.org/grpc", "DialOption"),
		jen.If(jen.String().Call(jen.Id("c").Dot("UsePlaintext")).Op("==").Lit("yes")).Block(
			jen.Id("opts").Op("=").Append(jen.Id("opts"), jen.Qual("google.golang.org/grpc", "WithTransportCredentials").Call(
				jen.Qual("google.golang.org/grpc/credentials/insecure", "NewCredentials").Call(),
			)),
		),
		jen.List(jen.Id("conn"), jen.Err()).Op(":=").Qual("google.golang.org/grpc", "NewClient").Call(
			jen.String().Call(jen.Id("c").Dot("Address")),
			jen.Id("opts").Op("..."),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Err()),
		),
		jen.If(jen.String().Call(jen.Id("c").Dot("PoolConnections")).Op("==").Lit("yes")).Block(
			jen.Id("c").Dot("conn").Op("=").Id("conn"),
		),
		jen.Return(jen.Id("conn"), jen.Nil()),
	)
	f.Line()

	// closeConnection
	f.Comment("// closeConnection closes the pooled connection if any")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("closeConnection").Params().Block(
		jen.If(jen.Id("c").Dot("conn").Op("!=").Nil()).Block(
			jen.Id("c").Dot("conn").Dot("Close").Call(),
			jen.Id("c").Dot("conn").Op("=").Nil(),
		),
	)
	f.Line()

	// loadProtoFile
	f.Comment("// loadProtoFile parses a proto file and caches the descriptors")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("loadProtoFile").Params().Error().Block(
		jen.If(jen.Len(jen.Id("c").Dot("fileDescs")).Op(">").Lit(0)).Block(
			jen.Return(jen.Nil()),
		),
		jen.If(jen.String().Call(jen.Id("c").Dot("ProtoFile")).Op("==").Lit("")).Block(
			jen.Return(jen.Qual("fmt", "Errorf").Call(jen.Lit("no proto file specified"))),
		),
		jen.Id("parser").Op(":=").Qual("github.com/jhump/protoreflect/desc/protoparse", "Parser").Values(jen.Dict{
			jen.Id("ImportPaths"): jen.Index().String().Values(
				jen.Qual("path/filepath", "Dir").Call(jen.String().Call(jen.Id("c").Dot("ProtoFile"))),
				jen.Lit("."),
			),
		}),
		jen.List(jen.Id("fds"), jen.Err()).Op(":=").Id("parser").Dot("ParseFiles").Call(
			jen.Qual("path/filepath", "Base").Call(jen.String().Call(jen.Id("c").Dot("ProtoFile"))),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to parse proto file %s: %w"), jen.String().Call(jen.Id("c").Dot("ProtoFile")), jen.Err())),
		),
		jen.Id("c").Dot("fileDescs").Op("=").Id("fds"),
		jen.Return(jen.Nil()),
	)
	f.Line()

	// findMethodInProto
	f.Comment("// findMethodInProto finds a method descriptor from parsed proto files")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("findMethodInProto").Params(
		jen.Id("fullMethod").String(),
	).Parens(jen.List(jen.Op("*").Qual("github.com/jhump/protoreflect/desc", "MethodDescriptor"), jen.Error())).Block(
		jen.If(jen.Err().Op(":=").Id("c").Dot("loadProtoFile").Call().Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Err()),
		),
		jen.Id("parts").Op(":=").Qual("strings", "Split").Call(jen.Id("fullMethod"), jen.Lit("/")),
		jen.If(jen.Len(jen.Id("parts")).Op("!=").Lit(2)).Block(
			jen.Return(jen.Nil(), jen.Qual("fmt", "Errorf").Call(jen.Lit("invalid method format: %s"), jen.Id("fullMethod"))),
		),
		jen.List(jen.Id("svcName"), jen.Id("mtdName")).Op(":=").List(jen.Id("parts").Index(jen.Lit(0)), jen.Id("parts").Index(jen.Lit(1))),
		jen.For(jen.List(jen.Id("_"), jen.Id("fd")).Op(":=").Range().Id("c").Dot("fileDescs")).Block(
			jen.For(jen.List(jen.Id("_"), jen.Id("svc")).Op(":=").Range().Id("fd").Dot("GetServices").Call()).Block(
				jen.If(jen.Id("svc").Dot("GetFullyQualifiedName").Call().Op("==").Id("svcName")).Block(
					jen.For(jen.List(jen.Id("_"), jen.Id("mtd")).Op(":=").Range().Id("svc").Dot("GetMethods").Call()).Block(
						jen.If(jen.Id("mtd").Dot("GetName").Call().Op("==").Id("mtdName")).Block(
							jen.Return(jen.Id("mtd"), jen.Nil()),
						),
					),
				),
			),
		),
		jen.Return(jen.Nil(), jen.Qual("fmt", "Errorf").Call(jen.Lit("method not found: %s"), jen.Id("fullMethod"))),
	)
	f.Line()

	// resolveMethod
	f.Comment("// resolveMethod resolves method to descriptor and creates stub")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("resolveMethod").Params(
		jen.Id("method").String(),
	).Parens(jen.List(
		jen.Op("*").Qual("google.golang.org/grpc", "ClientConn"),
		jen.Qual("context", "Context"),
		jen.Op("*").Qual("github.com/jhump/protoreflect/desc", "MethodDescriptor"),
		jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub"),
		jen.Func().Params(),
		jen.Error(),
	)).Block(
		jen.List(jen.Id("conn"), jen.Err()).Op(":=").Id("c").Dot("getConnection").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Err()),
		),
		jen.Id("cleanup").Op(":=").Func().Params().Block(
			jen.If(jen.String().Call(jen.Id("c").Dot("PoolConnections")).Op("!=").Lit("yes")).Block(
				jen.Id("conn").Dot("Close").Call(),
			),
		),
		jen.Id("ctx").Op(":=").Qual("context", "Background").Call(),
		jen.Var().Id("mtdDesc").Op("*").Qual("github.com/jhump/protoreflect/desc", "MethodDescriptor"),
		jen.If(jen.String().Call(jen.Id("c").Dot("UseReflection")).Op("==").Lit("yes")).Block(
			jen.Id("refClient").Op(":=").Qual("github.com/jhump/protoreflect/grpcreflect", "NewClientAuto").Call(jen.Id("ctx"), jen.Id("conn")),
			jen.Defer().Id("refClient").Dot("Reset").Call(),
			jen.Id("parts").Op(":=").Qual("strings", "Split").Call(jen.Id("method"), jen.Lit("/")),
			jen.If(jen.Len(jen.Id("parts")).Op("!=").Lit(2)).Block(
				jen.Id("cleanup").Call(),
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Qual("fmt", "Errorf").Call(jen.Lit("invalid method format"))),
			),
			jen.List(jen.Id("svcDesc"), jen.Err()).Op(":=").Id("refClient").Dot("ResolveService").Call(jen.Id("parts").Index(jen.Lit(0))),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Id("cleanup").Call(),
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Err()),
			),
			jen.Id("mtdDesc").Op("=").Id("svcDesc").Dot("FindMethodByName").Call(jen.Id("parts").Index(jen.Lit(1))),
			jen.If(jen.Id("mtdDesc").Op("==").Nil()).Block(
				jen.Id("cleanup").Call(),
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Qual("fmt", "Errorf").Call(jen.Lit("method not found: %s"), jen.Id("method"))),
			),
		).Else().Block(
			jen.List(jen.Id("mtdDesc"), jen.Err()).Op("=").Id("c").Dot("findMethodInProto").Call(jen.Id("method")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Id("cleanup").Call(),
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Err()),
			),
		),
		jen.Id("stub").Op(":=").Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "NewStub").Call(jen.Id("conn")),
		jen.Return(jen.Id("conn"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Nil()),
	)
	f.Line()

	// grpcCall - unary call
	f.Comment("// grpcCall makes a unary gRPC call")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("grpcCall").Params(
		jen.Id("method").String(),
		jen.Id("jsonPayload").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(jen.Id("mtdDesc").Dot("GetInputType").Call()),
		jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("jsonPayload"))).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to parse request: %w"), jen.Err())),
		),
		jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpc").Call(jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("reqMsg")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Return(jen.String().Parens(jen.Id("respJSON")), jen.Nil()),
	)
	f.Line()

	// serverStream - OPTIMIZED with direct block invocation
	f.Comment("// serverStream makes a server streaming gRPC call with native block callback")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("serverStream").Params(
		jen.Id("method").String(),
		jen.Id("jsonPayload").String(),
		jen.Id("handlerBlockID").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.If(jen.Op("!").Id("mtdDesc").Dot("IsServerStreaming").Call()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("method is not server streaming"))),
		),
		jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(jen.Id("mtdDesc").Dot("GetInputType").Call()),
		jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("jsonPayload"))).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to parse request: %w"), jen.Err())),
		),
		jen.List(jen.Id("stream"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpcServerStream").Call(jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("reqMsg")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to start stream: %w"), jen.Err())),
		),
		jen.Comment("// Look up block ONCE before loop for zero-overhead invocation"),
		jen.Id("block").Op(":=").Id("lookupBlock").Call(jen.Id("handlerBlockID")),
		jen.Id("count").Op(":=").Lit(0),
		jen.For().Block(
			jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stream").Dot("RecvMsg").Call(),
			jen.If(jen.Err().Op("==").Qual("io", "EOF")).Block(jen.Break()),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("stream error: %w"), jen.Err())),
			),
			jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(jen.Continue()),
			jen.Comment("// Direct block invocation - no IPC, no bash"),
			jen.Id("invokeBlockDirect").Call(jen.Id("block"), jen.String().Parens(jen.Id("respJSON"))),
			jen.Id("count").Op("++"),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d"), jen.Id("count")), jen.Nil()),
	)
	f.Line()

	// clientStream - OPTIMIZED with direct block invocation
	f.Comment("// clientStream makes a client streaming gRPC call with native block callback")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("clientStream").Params(
		jen.Id("method").String(),
		jen.Id("handlerBlockID").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.If(jen.Op("!").Id("mtdDesc").Dot("IsClientStreaming").Call()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("method is not client streaming"))),
		),
		jen.List(jen.Id("stream"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpcClientStream").Call(jen.Id("ctx"), jen.Id("mtdDesc")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to start stream: %w"), jen.Err())),
		),
		jen.Comment("// Look up block ONCE before loop for zero-overhead invocation"),
		jen.Id("block").Op(":=").Id("lookupBlock").Call(jen.Id("handlerBlockID")),
		jen.For().Block(
			jen.Comment("// Direct block invocation - no IPC, no bash"),
			jen.Id("msgJSON").Op(":=").Id("invokeBlockDirect").Call(jen.Id("block")),
			jen.If(jen.Id("msgJSON").Op("==").Lit("")).Block(jen.Break()),
			jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(jen.Id("mtdDesc").Dot("GetInputType").Call()),
			jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("msgJSON"))).Op(";").Err().Op("!=").Nil()).Block(jen.Continue()),
			jen.If(jen.Err().Op(":=").Id("stream").Dot("SendMsg").Call(jen.Id("reqMsg")).Op(";").Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("send error: %w"), jen.Err())),
			),
		),
		jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stream").Dot("CloseAndReceive").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("close error: %w"), jen.Err())),
		),
		jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Return(jen.String().Parens(jen.Id("respJSON")), jen.Nil()),
	)
	f.Line()

	// bidiStream - OPTIMIZED with direct block invocation
	f.Comment("// bidiStream makes a bidirectional streaming gRPC call with native block callback")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("bidiStream").Params(
		jen.Id("method").String(),
		jen.Id("handlerBlockID").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.If(jen.Op("!").Id("mtdDesc").Dot("IsClientStreaming").Call().Op("||").Op("!").Id("mtdDesc").Dot("IsServerStreaming").Call()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("method is not bidirectional streaming"))),
		),
		jen.List(jen.Id("stream"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpcBidiStream").Call(jen.Id("ctx"), jen.Id("mtdDesc")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to start stream: %w"), jen.Err())),
		),
		jen.Comment("// Look up block ONCE before loop for zero-overhead invocation"),
		jen.Id("block").Op(":=").Id("lookupBlock").Call(jen.Id("handlerBlockID")),
		jen.Id("count").Op(":=").Lit(0),
		jen.Id("done").Op(":=").Make(jen.Chan().Struct()),
		jen.Comment("// Goroutine to receive responses"),
		jen.Go().Func().Params().Block(
			jen.Defer().Close(jen.Id("done")),
			jen.For().Block(
				jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stream").Dot("RecvMsg").Call(),
				jen.If(jen.Err().Op("==").Qual("io", "EOF")).Block(jen.Return()),
				jen.If(jen.Err().Op("!=").Nil()).Block(jen.Return()),
				jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
				jen.If(jen.Err().Op("!=").Nil()).Block(jen.Continue()),
				jen.Comment("// Direct block invocation - no IPC, no bash"),
				jen.Id("reply").Op(":=").Id("invokeBlockDirect").Call(jen.Id("block"), jen.String().Parens(jen.Id("respJSON"))),
				jen.If(jen.Id("reply").Op("!=").Lit("")).Block(
					jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(jen.Id("mtdDesc").Dot("GetInputType").Call()),
					jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("reply"))).Op(";").Err().Op("==").Nil()).Block(
						jen.Id("stream").Dot("SendMsg").Call(jen.Id("reqMsg")),
					),
				),
				jen.Id("count").Op("++"),
			),
		).Call(),
		jen.Op("<-").Id("done"),
		jen.Id("stream").Dot("CloseSend").Call(),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d"), jen.Id("count")), jen.Nil()),
	)
}
