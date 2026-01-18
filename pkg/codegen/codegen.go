// Package codegen generates Go code from Trashtalk AST.
package codegen

import (
	"encoding/hex"
	"fmt"
	"strings"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/bytecode"
	"github.com/chazu/procyon/pkg/parser"
	"github.com/dave/jennifer/jen"
)

// Result contains the generated code and any warnings.
type Result struct {
	Code           string
	Warnings       []string
	SkippedMethods []SkippedMethod
}

// SkippedMethod records a method that couldn't be compiled.
type SkippedMethod struct {
	Selector string
	Reason   string
}

// GenerateOptions controls code generation behavior.
type GenerateOptions struct {
	// SkipValidation disables Go type-checking of generated code.
	// When false (default), generated code is validated and methods
	// that produce invalid Go are automatically skipped with warnings.
	SkipValidation bool
}

// isNumericString checks if a string represents a numeric value (integer).
// Used for backwards-compatible type inference of instance variable defaults.
func isNumericString(s string) bool {
	if len(s) == 0 {
		return false
	}
	for i, c := range s {
		if !((c >= '0' && c <= '9') || (i == 0 && c == '-')) {
			return false
		}
	}
	return true
}

type generator struct {
	class        *ast.Class
	warnings     []string
	skipped      []SkippedMethod
	instanceVars    map[string]bool
	jsonVars        map[string]bool   // vars with JSON default values (use json.RawMessage)
	skippedMethods  map[string]string // methods that will fall back to bash (selector -> reason)

	// Bytecode compilation for blocks
	compiledBlocks   map[string]*bytecode.Chunk // Block ID -> compiled bytecode
	blockCounter     int                        // Counter for generating unique block IDs
}

type compiledMethod struct {
	selector    string
	goName      string
	args        []string
	body        *parser.MethodBody
	hasReturn   bool
	isClass     bool
	returnsErr  bool
	primitive   bool                   // True if this is a primitive method with native impl
	renamedVars map[string]string      // Original name -> safe Go name
}

func (g *generator) generateStruct(f *jen.File) {
	fields := []jen.Code{
		jen.Id("Class").String().Tag(map[string]string{"json": "class"}),
		jen.Id("CreatedAt").String().Tag(map[string]string{"json": "created_at"}),
		jen.Id("Vars").Index().String().Tag(map[string]string{"json": "_vars"}),
	}

	for _, iv := range g.class.InstanceVars {
		goName := capitalize(iv.Name)
		goType := g.inferType(iv)
		fields = append(fields, jen.Id(goName).Add(goType).Tag(map[string]string{"json": iv.Name}))
	}

	// Add gRPC internal fields for GrpcClient (not serialized to JSON)
	if g.class.Name == "GrpcClient" {
		fields = append(fields, jen.Id("conn").Op("*").Qual("google.golang.org/grpc", "ClientConn").Tag(map[string]string{"json": "-"}))
		// Cached file descriptors for proto file mode
		fields = append(fields, jen.Id("fileDescs").Index().Op("*").Qual("github.com/jhump/protoreflect/desc", "FileDescriptor").Tag(map[string]string{"json": "-"}))
	}

	f.Type().Id(g.class.Name).Struct(fields...)
}

func (g *generator) inferType(iv ast.InstanceVar) *jen.Statement {
	// Use the type from the AST if available
	switch iv.Default.Type {
	case "number":
		return jen.Int()
	case "string":
		// Check if the string value is actually JSON (object or array)
		// This handles cases like items:'{}' or elements:'[]' where the
		// syntax uses a quoted string but the value is meant to be JSON
		val := strings.TrimSpace(iv.Default.Value)
		if (strings.HasPrefix(val, "{") && strings.HasSuffix(val, "}")) ||
			(strings.HasPrefix(val, "[") && strings.HasSuffix(val, "]")) {
			// This is a JSON object or array, use RawMessage to preserve structure
			return jen.Qual("encoding/json", "RawMessage")
		}
		return jen.String()
	}

	// Fallback: check if default value looks numeric (for backwards compatibility)
	if isNumericString(iv.Default.Value) {
		return jen.Int()
	}

	// For all other instance variables, use json.RawMessage.
	// This safely handles values that could be:
	// - JSON arrays: stored as ["x", "y"]
	// - JSON objects: stored as {"key": "value"}
	// - null values
	// This is necessary because instance variable values in SQLite are stored
	// as JSON, and at compile time we don't know what type of value will be set.
	return jen.Qual("encoding/json", "RawMessage")
}

// isJSONArrayType checks if an instance variable has a JSON array type
// Always returns false since we use string type for all instance variables
func (g *generator) isJSONArrayType(name string) bool {
	// With string-typed instance variables, we always use JSON string operations
	return false
}

// isJSONObjectType checks if an instance variable has a JSON object type
// Always returns false since we use string type for all instance variables
func (g *generator) isJSONObjectType(name string) bool {
	// With string-typed instance variables, we always use JSON string operations
	return false
}

// exprResultsInArray checks if an expression results in a native []interface{}
// This handles chained operations like: items arrayPush: x arrayPush: y
func (g *generator) exprResultsInArray(expr parser.Expr) bool {
	switch e := expr.(type) {
	case *parser.Identifier:
		return g.isJSONArrayType(e.Name)
	case *parser.JSONPrimitiveExpr:
		// If receiver results in array and operation preserves array type
		if g.exprResultsInArray(e.Receiver) {
			switch e.Operation {
			case "arrayPush", "arrayAtPut", "arrayRemoveAt":
				return true
			}
		}
	}
	return false
}

// exprResultsInObject checks if an expression results in a native map[string]interface{}
// This handles chained operations like: data objectAt: k1 put: v1 objectAt: k2 put: v2
func (g *generator) exprResultsInObject(expr parser.Expr) bool {
	switch e := expr.(type) {
	case *parser.Identifier:
		return g.isJSONObjectType(e.Name)
	case *parser.JSONPrimitiveExpr:
		// If receiver results in object and operation preserves object type
		if g.exprResultsInObject(e.Receiver) {
			switch e.Operation {
			case "objectAtPut", "objectRemoveKey":
				return true
			}
		}
	}
	return false
}

// generateTypeHelpers generates helper functions for type conversion in iteration blocks
func (g *generator) generateTypeHelpers(f *jen.File) {
	// toInt converts interface{} to int for arithmetic operations
	f.Comment("// toInt converts interface{} to int for arithmetic in iteration blocks")
	f.Func().Id("toInt").Params(jen.Id("v").Interface()).Int().Block(
		jen.Switch(jen.Id("x").Op(":=").Id("v").Assert(jen.Type())).Block(
			jen.Case(jen.Int()).Block(jen.Return(jen.Id("x"))),
			jen.Case(jen.Int64()).Block(jen.Return(jen.Int().Parens(jen.Id("x")))),
			jen.Case(jen.Float64()).Block(jen.Return(jen.Int().Parens(jen.Id("x")))),
			jen.Case(jen.String()).Block(
				jen.List(jen.Id("n"), jen.Id("_")).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("x")),
				jen.Return(jen.Id("n")),
			),
			jen.Default().Block(jen.Return(jen.Lit(0))),
		),
	)
	f.Line()

	// toBool converts interface{} to bool for predicates
	f.Comment("// toBool converts interface{} to bool for predicates in iteration blocks")
	f.Func().Id("toBool").Params(jen.Id("v").Interface()).Bool().Block(
		jen.Switch(jen.Id("x").Op(":=").Id("v").Assert(jen.Type())).Block(
			jen.Case(jen.Bool()).Block(jen.Return(jen.Id("x"))),
			jen.Case(jen.Int()).Block(jen.Return(jen.Id("x").Op("!=").Lit(0))),
			jen.Case(jen.String()).Block(jen.Return(jen.Id("x").Op("!=").Lit(""))),
			jen.Default().Block(jen.Return(jen.Id("v").Op("!=").Nil())),
		),
	)
	f.Line()
}

// generateBashBlockInvoker generates the bash-shelling version of invokeBlock
// This is only used for standalone plugin mode, NOT shared mode
func (g *generator) generateBashBlockInvoker(f *jen.File) {
	// invokeBlock calls a Trashtalk block through the Bash runtime (Phase 2)
	// Returns just string - errors are silently ignored to match bash behavior and simplify usage in expressions
	f.Comment("// invokeBlock calls a Trashtalk block through the Bash runtime")
	f.Comment("// blockID is the instance ID of the Block object")
	f.Comment("// args are the values to pass to the block")
	f.Func().Id("invokeBlock").Params(
		jen.Id("blockID").String(),
		jen.Id("args").Op("...").Interface(),
	).String().Block(
		// Build command based on arg count
		jen.Var().Id("cmdStr").String(),
		jen.Switch(jen.Len(jen.Id("args"))).Block(
			jen.Case(jen.Lit(0)).Block(
				jen.Id("cmdStr").Op("=").Qual("fmt", "Sprintf").Call(
					jen.Lit("source ~/.trashtalk/lib/trash.bash && @ %q value"),
					jen.Id("blockID"),
				),
			),
			jen.Case(jen.Lit(1)).Block(
				jen.Id("cmdStr").Op("=").Qual("fmt", "Sprintf").Call(
					jen.Lit("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q"),
					jen.Id("blockID"),
					jen.Qual("fmt", "Sprint").Call(jen.Id("args").Index(jen.Lit(0))),
				),
			),
			jen.Case(jen.Lit(2)).Block(
				jen.Id("cmdStr").Op("=").Qual("fmt", "Sprintf").Call(
					jen.Lit("source ~/.trashtalk/lib/trash.bash && @ %q valueWith: %q and: %q"),
					jen.Id("blockID"),
					jen.Qual("fmt", "Sprint").Call(jen.Id("args").Index(jen.Lit(0))),
					jen.Qual("fmt", "Sprint").Call(jen.Id("args").Index(jen.Lit(1))),
				),
			),
			jen.Default().Block(
				jen.Return(jen.Lit("")),
			),
		),
		jen.Line(),
		jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("cmdStr")),
		jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
		jen.Return(jen.Qual("strings", "TrimSpace").Call(jen.String().Parens(jen.Id("output")))),
	)
	f.Line()
}

// getLocalVars extracts local variable names from a compiled method
func (g *generator) getLocalVars(m *compiledMethod) []string {
	if m.body == nil {
		return nil
	}
	return m.body.LocalVars
}

// getInstanceVarNames returns the names of all instance variables for the class
func (g *generator) getInstanceVarNames() []string {
	var names []string
	for _, iv := range g.class.InstanceVars {
		names = append(names, iv.Name)
	}
	return names
}

// compileBlockToBytecode converts a parser.BlockExpr to bytecode
func (g *generator) compileBlockToBytecode(block *parser.BlockExpr, m *compiledMethod) (*bytecode.Chunk, error) {
	ctx := &bytecode.CompilerContext{
		MethodParams: m.args,
		MethodLocals: g.getLocalVars(m),
		InstanceVars: g.getInstanceVarNames(),
		InstanceID:   "", // Will be filled at runtime
	}

	return bytecode.CompileBlock(block, ctx)
}

// generateBlockCreation generates Go code that creates and executes a bytecode block.
// If bytecode compilation fails, it returns nil and callers should fall back to bash.
func (g *generator) generateBlockCreation(block *parser.BlockExpr, m *compiledMethod) *jen.Statement {
	chunk, err := g.compileBlockToBytecode(block, m)
	if err != nil {
		// Bytecode compilation failed - caller should fall back to bash
		g.warnings = append(g.warnings, fmt.Sprintf("block compilation failed: %v", err))
		return nil
	}

	// Serialize the chunk
	chunkBytes, err := chunk.Serialize()
	if err != nil {
		g.warnings = append(g.warnings, fmt.Sprintf("chunk serialization failed: %v", err))
		return nil
	}

	// Encode as hex string for embedding in Go source
	chunkHex := hex.EncodeToString(chunkBytes)

	// Store compiled block for potential reuse
	blockID := fmt.Sprintf("block_%d", g.blockCounter)
	g.blockCounter++
	g.compiledBlocks[blockID] = chunk

	// Generate a closure that deserializes and executes the bytecode
	// The closure captures the hex-encoded bytecode and creates a VM to run it
	// Note: We use ...interface{} for variadic args and convert to strings inside
	return jen.Func().Params(
		jen.Id("args").Op("...").Interface(),
	).String().Block(
		// Convert args to strings
		jen.Id("strArgs").Op(":=").Make(jen.Index().String(), jen.Len(jen.Id("args"))),
		jen.For(jen.List(jen.Id("i"), jen.Id("arg")).Op(":=").Range().Id("args")).Block(
			jen.Id("strArgs").Index(jen.Id("i")).Op("=").Qual("fmt", "Sprint").Call(jen.Id("arg")),
		),
		// Decode the hex-encoded bytecode
		jen.List(jen.Id("chunkData"), jen.Id("_")).Op(":=").Qual("encoding/hex", "DecodeString").Call(
			jen.Lit(chunkHex),
		),
		// Deserialize to chunk
		jen.List(jen.Id("chunk"), jen.Id("err")).Op(":=").Qual(
			"github.com/chazu/procyon/pkg/bytecode", "Deserialize",
		).Call(jen.Id("chunkData")),
		jen.If(jen.Id("err").Op("!=").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		// Create VM and execute
		jen.Id("vm").Op(":=").Qual(
			"github.com/chazu/procyon/pkg/bytecode", "NewVM",
		).Call(jen.Id("chunk"), jen.Nil(), jen.Nil()),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Id("vm").Dot("Execute").Call(jen.Id("strArgs").Op("...")),
		jen.Return(jen.Id("result")),
	)
}

// preIdentifySkippedMethods runs through all methods to identify which will be skipped.
// This is needed so that @ self calls can use sendMessage for skipped methods.
// The map stores selector -> reason for skipping.
func (g *generator) preIdentifySkippedMethods() {
	// Check if this is a primitiveClass - all methods use built-in implementations
	isPrimitiveClass := g.class.IsPrimitiveClass()

	for _, m := range g.class.Methods {
		var reason string

		// bashOnly pragma
		if m.HasPragma("bashOnly") {
			reason = "bashOnly pragma"
		}

		// For primitiveClass, skip only if no native impl exists
		if isPrimitiveClass {
			if !hasPrimitiveImpl(g.class.Name, m.Selector) {
				if reason == "" {
					reason = "primitiveClass method without native implementation"
				}
			}
			if reason != "" {
				g.skippedMethods[m.Selector] = reason
			}
			continue
		}

		// Raw methods (unless primitive or has procyon pragma)
		if m.Raw && !m.Primitive && !m.HasPragma("procyonOnly") && !m.HasPragma("procyonNative") {
			if reason == "" {
				reason = "raw method"
			}
		}

		// Primitive methods without native impl
		if m.Primitive && !hasPrimitiveImpl(g.class.Name, m.Selector) {
			if reason == "" {
				reason = "primitive method without native implementation"
			}
		}

		// Check for bash-specific function calls
		if reason == "" {
			for _, tok := range m.Body.Tokens {
				if tok.Type == "IDENTIFIER" {
					switch tok.Value {
					case "_ivar", "_ivar_set", "_throw", "_on_error", "_ensure", "_pop_handler":
						reason = "uses bash-specific functions"
						break
					}
				}
				if reason != "" {
					break
				}
			}
		}

		// Try to parse - if unsupported, will skip
		if reason == "" && !m.Raw && !m.Primitive {
			result := parser.ParseMethod(m.Body.Tokens)
			if result.Unsupported {
				reason = "unsupported construct"
			}
		}

		if reason != "" {
			g.skippedMethods[m.Selector] = reason
		}
	}
}

func (g *generator) compileMethods() []*compiledMethod {
	var compiled []*compiledMethod

	// Check if this is a primitiveClass - all methods use built-in implementations
	isPrimitiveClass := g.class.IsPrimitiveClass()

	for _, m := range g.class.Methods {
		// Skip methods marked for skipping in preIdentifySkippedMethods
		// Add to g.skipped with the reason that was tracked
		if reason, skipped := g.skippedMethods[m.Selector]; skipped {
			g.skipped = append(g.skipped, SkippedMethod{
				Selector: m.Selector,
				Reason:   reason,
			})
			continue
		}

		// For primitiveClass, treat ALL methods as primitive - use built-in implementations
		if isPrimitiveClass {
			if hasPrimitiveImpl(g.class.Name, m.Selector) {
				compiled = append(compiled, &compiledMethod{
					selector:    m.Selector,
					goName:      selectorToGoName(m.Selector),
					args:        m.Args,
					body:        nil, // No parsed body - native impl provided
					hasReturn:   true,
					isClass:     m.Kind == "class",
					returnsErr:  true,
					primitive:   true,
					renamedVars: make(map[string]string),
				})
			} else {
				// No native impl registered for this primitive class method
				g.warnings = append(g.warnings,
					fmt.Sprintf("primitiveClass %s method %s has no native implementation, will fall back to bash",
						g.class.Name, m.Selector))
				g.skipped = append(g.skipped, SkippedMethod{
					Selector: m.Selector,
					Reason:   "primitiveClass method without native implementation",
				})
			}
			continue
		}

		// Skip raw methods unless:
		// - procyonOnly pragma: Bash gets error stub, Procyon provides impl
		// - procyonNative pragma: Bash uses rawMethod body, Procyon provides native impl
		// - primitive: method has native Procyon implementation
		if m.Raw && !m.Primitive && !m.HasPragma("procyonOnly") && !m.HasPragma("procyonNative") {
			g.skipped = append(g.skipped, SkippedMethod{
				Selector: m.Selector,
				Reason:   "raw method",
			})
			continue
		}

		// Handle primitive methods - these have native Procyon implementations
		// The bash fallback code in the body is ignored; Procyon provides the native impl
		if m.Primitive {
			if hasPrimitiveImpl(g.class.Name, m.Selector) {
				compiled = append(compiled, &compiledMethod{
					selector:    m.Selector,
					goName:      selectorToGoName(m.Selector),
					args:        m.Args,
					body:        nil, // No parsed body - native impl provided
					hasReturn:   true,
					isClass:     m.Kind == "class",
					returnsErr:  true,
					primitive:   true,
					renamedVars: make(map[string]string),
				})
			} else {
				// No native impl registered - warn but still fall back to bash
				g.warnings = append(g.warnings,
					fmt.Sprintf("primitive method %s.%s has no native implementation, using bash fallback",
						g.class.Name, m.Selector))
				g.skipped = append(g.skipped, SkippedMethod{
					Selector: m.Selector,
					Reason:   "primitive method without native implementation",
				})
			}
			continue
		}

		// For GrpcClient procyonNative methods, skip body parsing entirely -
		// these raw methods contain Bash code that won't parse, but
		// generateGrpcClientMethod() will provide native implementations
		if g.class.Name == "GrpcClient" && m.HasPragma("procyonNative") {
			compiled = append(compiled, &compiledMethod{
				selector:    m.Selector,
				goName:      selectorToGoName(m.Selector),
				args:        m.Args,
				body:        nil, // No parsed body - native impl provided
				hasReturn:   true,
				isClass:     m.Kind == "class",
				returnsErr:  true,
				renamedVars: make(map[string]string),
			})
			continue
		}

		// Check for bash-specific function calls before parsing
		hasBashRuntimeCall := false
		for _, tok := range m.Body.Tokens {
			if tok.Type == "IDENTIFIER" {
				switch tok.Value {
				case "_ivar", "_ivar_set", "_throw", "_on_error", "_ensure", "_pop_handler":
					g.skipped = append(g.skipped, SkippedMethod{
						Selector: m.Selector,
						Reason:   "uses bash runtime function: " + tok.Value,
					})
					hasBashRuntimeCall = true
					break
				}
			}
		}
		if hasBashRuntimeCall {
			continue
		}

		// Parse the method body
		result := parser.ParseMethod(m.Body.Tokens)
		if result.Unsupported {
			g.skipped = append(g.skipped, SkippedMethod{
				Selector: m.Selector,
				Reason:   result.Reason,
			})
			continue
		}

		// Check if method has return (recursively check inside if blocks too)
		hasReturn := hasReturnInStatements(result.Body.Statements)

		// Check if any args require error handling (string to int conversion)
		returnsErr := len(m.Args) > 0

		compiled = append(compiled, &compiledMethod{
			selector:    m.Selector,
			goName:      selectorToGoName(m.Selector),
			args:        m.Args,
			body:        result.Body,
			hasReturn:   hasReturn,
			isClass:     m.Kind == "class",
			returnsErr:  returnsErr,
			renamedVars: make(map[string]string),
		})
	}

	return compiled
}

func (g *generator) generateMethod(f *jen.File, m *compiledMethod) {
	className := g.class.Name

	// Special handling for Environment class - generate SQLite-based storage methods
	if g.class.Name == "Environment" && m.isClass {
		g.generateEnvironmentMethod(f, m)
		return
	}

	// Special handling for GrpcClient class - wire methods to gRPC helpers
	if g.class.Name == "GrpcClient" && !m.isClass {
		if g.generateGrpcClientMethod(f, m) {
			return
		}
	}

	// Handle primitive methods - these have native Procyon implementations
	if m.primitive {
		if g.generatePrimitiveMethod(f, m) {
			return
		}
		// Primitive method without native implementation - skip (will fall back to bash at runtime)
		return
	}

	// Check if method name collides with an instance variable (Go doesn't allow this)
	// If it's a simple getter (no args, returns the ivar), rename to Get<Name>
	methodName := m.goName
	if !m.isClass && g.instanceVars[strings.ToLower(m.selector)] {
		// Method name matches an ivar - rename to avoid Go collision
		methodName = "Get" + methodName
	}

	// Build parameter list (sanitize Go keywords)
	params := []jen.Code{}
	for _, arg := range m.args {
		safeName := safeGoName(arg)
		if safeName != arg {
			m.renamedVars[arg] = safeName
		}
		params = append(params, jen.Id(safeName).String())
	}

	// Determine return type
	var returnType *jen.Statement
	if m.returnsErr {
		returnType = jen.Parens(jen.List(jen.String(), jen.Error()))
	} else if m.hasReturn {
		returnType = jen.String()
	}

	// Generate body
	body := g.generateMethodBody(m)

	if m.isClass {
		// Class methods are package-level functions (no receiver)
		if returnType != nil {
			f.Func().Id(methodName).Params(params...).Add(returnType).Block(body...)
		} else {
			f.Func().Id(methodName).Params(params...).Block(body...)
		}
	} else {
		// Instance methods have receiver
		if returnType != nil {
			f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(methodName).Params(params...).Add(returnType).Block(body...)
		} else {
			f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(methodName).Params(params...).Block(body...)
		}
	}
	f.Line()
}

func (g *generator) generateMethodBody(m *compiledMethod) []jen.Code {
	var stmts []jen.Code

	// Parameters come in as strings from dispatcher and are used as strings
	// Numeric conversions happen at point of use in expressions

	// Local variables - rename if they conflict with Go builtins
	// Use interface{} for dynamic typing (Trashtalk is dynamically typed)
	for _, v := range m.body.LocalVars {
		safeName := safeGoName(v)
		if safeName != v {
			m.renamedVars[v] = safeName
		}
		stmts = append(stmts, jen.Var().Id(safeName).Interface())
	}

	// Statements
	for _, stmt := range m.body.Statements {
		stmts = append(stmts, g.generateStatement(stmt, m)...)
	}

	// Add implicit return for methods that don't have explicit return
	if m.returnsErr && !m.hasReturn {
		stmts = append(stmts, jen.Return(jen.Lit(""), jen.Nil()))
	}

	return stmts
}

func (g *generator) generateStatement(stmt parser.Statement, m *compiledMethod) []jen.Code {
	switch s := stmt.(type) {
	case *parser.Assignment:
		target := s.Target
		// Check if it's an instance variable
		if g.instanceVars[target] {
			isNumericIvar := !g.jsonVars[target] // Numeric ivars are NOT in jsonVars
			var expr *jen.Statement
			switch v := s.Value.(type) {
			case *parser.Identifier:
				// Check if value is a method arg - use original string param
				isMethodArg := false
				for _, arg := range m.args {
					if arg == v.Name {
						isMethodArg = true
						break
					}
				}
				if isMethodArg {
					// Use renamed parameter name if it conflicted with Go keyword
					paramName := v.Name
					if renamed, ok := m.renamedVars[v.Name]; ok {
						paramName = renamed
					}
					if isNumericIvar {
						// Numeric ivar needs int conversion from string param
						expr = jen.Id("toInt").Call(jen.Id(paramName))
					} else {
						expr = jen.Id(paramName)
					}
				} else if g.instanceVars[v.Name] {
					// Assigning one ivar to another
					if isNumericIvar && !g.jsonVars[v.Name] {
						// Both are numeric - direct assignment
						expr = g.generateExpr(s.Value, m)
					} else if isNumericIvar {
						// Target is numeric, source is string - convert
						expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
					} else {
						// Target is string-typed
						expr = g.generateExpr(s.Value, m)
					}
				} else {
					// Local variable
					if isNumericIvar {
						// Need to convert interface{} to int
						expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
					} else {
						// Need to convert to string
						expr = jen.Id("_toStr").Call(g.generateExpr(s.Value, m))
					}
				}
			case *parser.BinaryExpr:
				if v.Op == "," {
					// String concatenation - already returns string
					if isNumericIvar {
						// Can't assign string to numeric - shouldn't happen
						expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
					} else {
						expr = g.generateExpr(s.Value, m)
					}
				} else {
					// Arithmetic expression - result is int
					if isNumericIvar {
						// Numeric ivar - assign int directly
						expr = g.generateExpr(s.Value, m)
					} else {
						// String ivar - convert int to string
						expr = jen.Qual("strconv", "Itoa").Call(g.generateExpr(s.Value, m))
					}
				}
			case *parser.StringLit:
				// String literal
				if isNumericIvar {
					expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
				} else {
					expr = g.generateExpr(s.Value, m)
				}
			case *parser.JSONPrimitiveExpr:
				// JSON primitives return strings
				if isNumericIvar {
					expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
				} else {
					expr = g.generateExpr(s.Value, m)
				}
			case *parser.MessageSend:
				// Message sends return strings
				if isNumericIvar {
					expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
				} else {
					expr = g.generateExpr(s.Value, m)
				}
			default:
				// Default
				if isNumericIvar {
					expr = jen.Id("toInt").Call(g.generateExpr(s.Value, m))
				} else {
					expr = jen.Id("_toStr").Call(g.generateExpr(s.Value, m))
				}
			}
			// JSON vars need to be wrapped in json.RawMessage
			if g.jsonVars[target] {
				expr = jen.Qual("encoding/json", "RawMessage").Parens(expr)
			}
			return []jen.Code{jen.Id("c").Dot(capitalize(target)).Op("=").Add(expr)}
		}
		// Check if target is a local variable declared in the method
		isLocalVar := false
		if m.body != nil {
			for _, local := range m.body.LocalVars {
				if local == target {
					isLocalVar = true
					break
				}
			}
		}
		// Check if it's a renamed variable (also a local)
		if _, ok := m.renamedVars[target]; ok {
			isLocalVar = true
		}
		// Check for inherited instance variable assignment
		if !isLocalVar && !m.isClass && g.class.Parent != "" && g.class.Parent != "Object" {
			// Use setInstanceVar for inherited ivar
			expr := g.generateExpr(s.Value, m)
			return []jen.Code{jen.Id("setInstanceVar").Call(
				jen.Id("instanceID"),
				jen.Lit(target),
				jen.Id("_toStr").Call(expr),
			)}
		}
		// For local variables
		expr := g.generateExpr(s.Value, m)
		// Check if target was renamed to avoid Go builtin conflict
		if renamed, ok := m.renamedVars[target]; ok {
			target = renamed
		}
		return []jen.Code{jen.Id(target).Op("=").Add(expr)}

	case *parser.Return:
		// Check for iteration expression as return value
		if iterVal, ok := s.Value.(*parser.IterationExprAsValue); ok {
			// Generate iteration statements (collect: or select: produce _results)
			iterStmts := g.generateIterationStatement(iterVal.Iteration, m)
			// Return the results as JSON
			returnStmt := jen.List(jen.Id("_resultJSON"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("_results"))
			if m.returnsErr {
				return append(iterStmts, returnStmt, jen.Return(jen.String().Call(jen.Id("_resultJSON")), jen.Nil()))
			}
			return append(iterStmts, returnStmt, jen.Return(jen.String().Call(jen.Id("_resultJSON"))))
		}
		if dynIterVal, ok := s.Value.(*parser.DynamicIterationExprAsValue); ok {
			// Generate dynamic iteration statements (collect: or select: produce _results)
			iterStmts := g.generateDynamicIterationStatement(dynIterVal.Iteration, m)
			// Return the results as JSON
			returnStmt := jen.List(jen.Id("_resultJSON"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("_results"))
			if m.returnsErr {
				return append(iterStmts, returnStmt, jen.Return(jen.String().Call(jen.Id("_resultJSON")), jen.Nil()))
			}
			return append(iterStmts, returnStmt, jen.Return(jen.String().Call(jen.Id("_resultJSON"))))
		}

		expr := g.generateExpr(s.Value, m)
		// Check if the return value is already a string (message sends, string literals, JSON primitives)
		_, isMessageSend := s.Value.(*parser.MessageSend)
		_, isStringLit := s.Value.(*parser.StringLit)
		jsonPrim, isJSONPrimitive := s.Value.(*parser.JSONPrimitiveExpr)
		// Check if return value is an instance variable (generateExpr handles string conversion)
		isIvarReturn := false
		if id, ok := s.Value.(*parser.Identifier); ok {
			isIvarReturn = g.instanceVars[id.Name]
		}
		// Check if JSON primitive returns an array type (needs JSON encoding)
		isArrayReturningPrimitive := false
		if isJSONPrimitive {
			switch jsonPrim.Operation {
			case "objectKeys", "objectValues", "arrayCollect", "arraySelect":
				isArrayReturningPrimitive = true
			}
		}
		if isArrayReturningPrimitive {
			// Array-returning primitives need JSON encoding
			stmts := []jen.Code{
				jen.List(jen.Id("_resultJSON"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(expr),
			}
			if m.returnsErr {
				stmts = append(stmts, jen.Return(jen.String().Call(jen.Id("_resultJSON")), jen.Nil()))
			} else {
				stmts = append(stmts, jen.Return(jen.String().Call(jen.Id("_resultJSON"))))
			}
			return stmts
		}
		if isMessageSend || isStringLit || isJSONPrimitive || isIvarReturn {
			// Already a string, no conversion needed
			if m.returnsErr {
				return []jen.Code{jen.Return(expr, jen.Nil())}
			}
			return []jen.Code{jen.Return(expr)}
		}
		// Other values - use _toStr for interface{} compatibility
		if m.returnsErr {
			return []jen.Code{jen.Return(jen.Id("_toStr").Call(expr), jen.Nil())}
		}
		return []jen.Code{jen.Return(jen.Id("_toStr").Call(expr))}

	case *parser.ExprStmt:
		return []jen.Code{g.generateExpr(s.Expr, m)}

	case *parser.IfExpr:
		return g.generateIfStatement(s, m)

	case *parser.WhileExpr:
		return g.generateWhileStatement(s, m)

	case *parser.IfNilExpr:
		return g.generateIfNilStatement(s, m)

	case *parser.IterationExpr:
		return g.generateIterationStatement(s, m)

	case *parser.DynamicIterationExpr:
		return g.generateDynamicIterationStatement(s, m)

	default:
		return []jen.Code{jen.Comment("unknown statement")}
	}
}

// generateIfStatement generates Go if/else from Trashtalk ifTrue:/ifFalse:
func (g *generator) generateIfStatement(s *parser.IfExpr, m *compiledMethod) []jen.Code {
	condition := g.generateCondition(s.Condition, m)

	// Generate true block statements
	var trueStmts []jen.Code
	for _, stmt := range s.TrueBlock {
		trueStmts = append(trueStmts, g.generateStatement(stmt, m)...)
	}

	// Generate false block statements if present
	var falseStmts []jen.Code
	for _, stmt := range s.FalseBlock {
		falseStmts = append(falseStmts, g.generateStatement(stmt, m)...)
	}

	// Build the if statement
	if len(s.TrueBlock) > 0 && len(s.FalseBlock) > 0 {
		// ifTrue: [true] ifFalse: [false]
		return []jen.Code{
			jen.If(condition).Block(trueStmts...).Else().Block(falseStmts...),
		}
	} else if len(s.TrueBlock) > 0 {
		// ifTrue: [true] only
		return []jen.Code{
			jen.If(condition).Block(trueStmts...),
		}
	} else if len(s.FalseBlock) > 0 {
		// ifFalse: [false] only - negate condition
		return []jen.Code{
			jen.If(jen.Op("!").Parens(condition)).Block(falseStmts...),
		}
	}

	return []jen.Code{jen.Comment("empty if statement")}
}

// generateCondition generates a Go boolean condition from a Trashtalk expression.
// Comparisons return bool directly, but message sends return strings.
// For message sends, we convert to bool with: result != ""
func (g *generator) generateCondition(expr parser.Expr, m *compiledMethod) *jen.Statement {
	// Check if the expression is a comparison (already returns bool)
	switch e := expr.(type) {
	case *parser.ComparisonExpr:
		return g.generateExpr(expr, m)
	case *parser.MessageSend:
		// Check for predicate selectors that should compile to native Go tests
		if goTest, isPredicate := goPredicateTest(e.Selector); isPredicate {
			subject := g.generateExpr(e.Receiver, m)
			return goTest(subject)
		}
	}

	// For message sends and other expressions, wrap in truthiness check
	// In Trashtalk, "true" = truthy (matches bash backend which checks == "true")
	// Methods like objectHasKey: return "true"/"false" strings, not empty/non-empty
	return g.generateExpr(expr, m).Op("==").Lit("true")
}

// goPredicateTest maps predicate selectors to Go code generators.
// Returns (generator func, true) if the selector is a predicate, (nil, false) otherwise.
// This ensures identical semantics between bash and native backends.
func goPredicateTest(selector string) (func(*jen.Statement) *jen.Statement, bool) {
	predicates := map[string]func(*jen.Statement) *jen.Statement{
		// String/variable tests - use len() for efficiency
		"isEmpty": func(subject *jen.Statement) *jen.Statement {
			return jen.Len(subject).Op("==").Lit(0)
		},
		"notEmpty": func(subject *jen.Statement) *jen.Statement {
			return jen.Len(subject).Op(">").Lit(0)
		},
		// File tests - use os.Stat and check error/mode
		"fileExists": func(subject *jen.Statement) *jen.Statement {
			// _, err := os.Stat(subject); err == nil
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("_"), jen.Err()).Op(":=").Qual("os", "Stat").Call(subject),
				jen.Return(jen.Err().Op("==").Nil()),
			).Call()
		},
		"isFile": func(subject *jen.Statement) *jen.Statement {
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(subject),
				jen.Return(jen.Err().Op("==").Nil().Op("&&").Id("info").Dot("Mode").Call().Dot("IsRegular").Call()),
			).Call()
		},
		"isDirectory": func(subject *jen.Statement) *jen.Statement {
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(subject),
				jen.Return(jen.Err().Op("==").Nil().Op("&&").Id("info").Dot("IsDir").Call()),
			).Call()
		},
		"isSymlink": func(subject *jen.Statement) *jen.Statement {
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Lstat").Call(subject),
				jen.Return(jen.Err().Op("==").Nil().Op("&&").Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeSymlink").Op("!=").Lit(0)),
			).Call()
		},
		"isReadable": func(subject *jen.Statement) *jen.Statement {
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("f"), jen.Err()).Op(":=").Qual("os", "Open").Call(subject),
				jen.If(jen.Err().Op("!=").Nil()).Block(jen.Return(jen.False())),
				jen.Id("f").Dot("Close").Call(),
				jen.Return(jen.True()),
			).Call()
		},
		"isWritable": func(subject *jen.Statement) *jen.Statement {
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("f"), jen.Err()).Op(":=").Qual("os", "OpenFile").Call(
					subject,
					jen.Qual("os", "O_WRONLY"),
					jen.Lit(0),
				),
				jen.If(jen.Err().Op("!=").Nil()).Block(jen.Return(jen.False())),
				jen.Id("f").Dot("Close").Call(),
				jen.Return(jen.True()),
			).Call()
		},
		"isExecutable": func(subject *jen.Statement) *jen.Statement {
			return jen.Func().Params().Bool().Block(
				jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(subject),
				jen.Return(jen.Err().Op("==").Nil().Op("&&").Id("info").Dot("Mode").Call().Op("&").Lit(0111).Op("!=").Lit(0)),
			).Call()
		},
	}
	if gen, ok := predicates[selector]; ok {
		return gen, true
	}
	return nil, false
}

// generateWhileStatement generates Go for loop from Trashtalk whileTrue:
func (g *generator) generateWhileStatement(s *parser.WhileExpr, m *compiledMethod) []jen.Code {
	condition := g.generateExpr(s.Condition, m)

	// Generate body statements
	var bodyStmts []jen.Code
	for _, stmt := range s.Body {
		bodyStmts = append(bodyStmts, g.generateStatement(stmt, m)...)
	}

	// In Trashtalk, truthy = non-empty string, falsy = empty string
	// So we need to compare the condition result with "" in Go
	// For comparison expressions (which return bool), use directly
	// For message sends and other expressions (which return string), compare != ""
	var goCondition *jen.Statement
	switch s.Condition.(type) {
	case *parser.BinaryExpr:
		// Binary comparisons already return bool
		be := s.Condition.(*parser.BinaryExpr)
		if be.Op == "<" || be.Op == ">" || be.Op == "<=" || be.Op == ">=" || be.Op == "==" || be.Op == "!=" {
			goCondition = condition
		} else {
			// Other binary ops (arithmetic, string concat) - compare result != ""
			goCondition = condition.Clone().Op("!=").Lit("")
		}
	default:
		// Message sends, identifiers, etc. return strings - compare != ""
		goCondition = condition.Clone().Op("!=").Lit("")
	}

	// Go's "while" is just "for condition"
	return []jen.Code{
		jen.For(goCondition).Block(bodyStmts...),
	}
}

// generateIfNilStatement generates Go if for Trashtalk ifNil:/ifNotNil:
func (g *generator) generateIfNilStatement(s *parser.IfNilExpr, m *compiledMethod) []jen.Code {
	subjectExpr := g.generateExpr(s.Subject, m)

	// Generate nil block statements (for ifNil:)
	var nilStmts []jen.Code
	for _, stmt := range s.NilBlock {
		nilStmts = append(nilStmts, g.generateStatement(stmt, m)...)
	}

	// Generate not-nil block statements (for ifNotNil:)
	var notNilStmts []jen.Code

	// If there's a binding variable, add it at the start of the block
	if s.BindingVar != "" {
		// bindingVar := subject
		notNilStmts = append(notNilStmts, jen.Id(s.BindingVar).Op(":=").Add(subjectExpr))
	}

	for _, stmt := range s.NotNilBlock {
		notNilStmts = append(notNilStmts, g.generateStatement(stmt, m)...)
	}

	// In Trashtalk, nil is empty string ""
	// ifNil: means if subject == ""
	// ifNotNil: means if subject != ""
	nilCondition := subjectExpr.Clone().Op("==").Lit("")

	if len(s.NilBlock) > 0 && len(s.NotNilBlock) > 0 {
		// ifNil: [block1] ifNotNil: [block2]
		// -> if subject == "" { block1 } else { block2 }
		return []jen.Code{
			jen.If(nilCondition).Block(nilStmts...).Else().Block(notNilStmts...),
		}
	} else if len(s.NilBlock) > 0 {
		// ifNil: [block] only
		// -> if subject == "" { block }
		return []jen.Code{
			jen.If(nilCondition).Block(nilStmts...),
		}
	} else if len(s.NotNilBlock) > 0 {
		// ifNotNil: [block] only
		// -> if subject != "" { block }
		notNilCondition := subjectExpr.Clone().Op("!=").Lit("")
		return []jen.Code{
			jen.If(notNilCondition).Block(notNilStmts...),
		}
	}

	return []jen.Code{jen.Comment("empty ifNil/ifNotNil")}
}

// generateIterationStatement generates Go for loop from Trashtalk do:/collect:/select:
func (g *generator) generateIterationStatement(s *parser.IterationExpr, m *compiledMethod) []jen.Code {
	collectionExpr := g.generateExpr(s.Collection, m)
	iterVar := s.IterVar
	rawIterVar := "_" + iterVar // Raw interface{} variable from range

	// Check if collection is a native array (from JSON primitives) vs JSON string
	isNativeArray := g.exprResultsInArray(s.Collection)

	// Type conversion at start of loop: iterVar := toInt(_iterVar)
	typeConversion := jen.Id(iterVar).Op(":=").Id("toInt").Call(jen.Id(rawIterVar))

	switch s.Kind {
	case "do":
		// For do:, generate body statements normally
		var bodyStmts []jen.Code
		for _, stmt := range s.Body {
			bodyStmts = append(bodyStmts, g.generateIterationBodyStatement(stmt, m, iterVar)...)
		}

		// Prepend type conversion
		loopBody := append([]jen.Code{typeConversion}, bodyStmts...)

		if isNativeArray {
			// Native array: iterate directly over []interface{}
			return []jen.Code{
				jen.For(jen.List(jen.Id("_"), jen.Id(rawIterVar)).Op(":=").Range().Add(collectionExpr)).Block(loopBody...),
			}
		}
		// JSON string: unmarshal first (use _toStr to handle interface{})
		return []jen.Code{
			jen.Var().Id("_items").Index().Interface(),
			jen.Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("_toStr").Call(collectionExpr)),
				jen.Op("&").Id("_items"),
			),
			jen.For(jen.List(jen.Id("_"), jen.Id(rawIterVar)).Op(":=").Range().Id("_items")).Block(loopBody...),
		}

	case "collect":
		// For collect:, the last statement's expression becomes the collected value
		bodyStmts, resultExpr := g.generateCollectBody(s.Body, m, iterVar)

		// Prepend type conversion, then body, then append result
		loopBody := append([]jen.Code{typeConversion}, bodyStmts...)
		loopBody = append(loopBody, jen.Id("_results").Op("=").Append(jen.Id("_results"), resultExpr))

		if isNativeArray {
			// Native array: collect directly into []interface{}
			return []jen.Code{
				jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0), jen.Len(collectionExpr)),
				jen.For(jen.List(jen.Id("_"), jen.Id(rawIterVar)).Op(":=").Range().Add(collectionExpr)).Block(loopBody...),
			}
		}
		// JSON string: unmarshal first (use _toStr to handle interface{})
		return []jen.Code{
			jen.Var().Id("_items").Index().Interface(),
			jen.Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("_toStr").Call(collectionExpr)),
				jen.Op("&").Id("_items"),
			),
			jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
			jen.For(jen.List(jen.Id("_"), jen.Id(rawIterVar)).Op(":=").Range().Id("_items")).Block(loopBody...),
		}

	case "select":
		// For select:, the last statement's expression becomes the filter condition
		bodyStmts, conditionExpr := g.generateSelectBody(s.Body, m, iterVar)

		// For select, we need to keep the original interface{} value for appending
		// But use the typed value for the condition
		loopBody := append([]jen.Code{typeConversion}, bodyStmts...)
		loopBody = append(loopBody, jen.If(conditionExpr).Block(
			jen.Id("_results").Op("=").Append(jen.Id("_results"), jen.Id(rawIterVar)),
		))

		if isNativeArray {
			// Native array: filter directly into []interface{}
			return []jen.Code{
				jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
				jen.For(jen.List(jen.Id("_"), jen.Id(rawIterVar)).Op(":=").Range().Add(collectionExpr)).Block(loopBody...),
			}
		}
		// JSON string: unmarshal first (use _toStr to handle interface{})
		return []jen.Code{
			jen.Var().Id("_items").Index().Interface(),
			jen.Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("_toStr").Call(collectionExpr)),
				jen.Op("&").Id("_items"),
			),
			jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
			jen.For(jen.List(jen.Id("_"), jen.Id(rawIterVar)).Op(":=").Range().Id("_items")).Block(loopBody...),
		}

	default:
		return []jen.Code{jen.Comment("unknown iteration kind: " + s.Kind)}
	}
}

// generateCollectBody generates the body statements for a collect: block
// Returns the body statements (all but last) and the result expression (last statement)
func (g *generator) generateCollectBody(body []parser.Statement, m *compiledMethod, iterVar string) ([]jen.Code, *jen.Statement) {
	if len(body) == 0 {
		return nil, jen.Nil()
	}

	var stmts []jen.Code
	// Generate all statements except the last
	for i := 0; i < len(body)-1; i++ {
		stmts = append(stmts, g.generateIterationBodyStatement(body[i], m, iterVar)...)
	}

	// Last statement should be an expression - extract it as the result
	lastStmt := body[len(body)-1]
	if exprStmt, ok := lastStmt.(*parser.ExprStmt); ok {
		return stmts, g.generateExpr(exprStmt.Expr, m)
	}

	// If last statement is not an expression, generate it normally and return nil
	stmts = append(stmts, g.generateIterationBodyStatement(lastStmt, m, iterVar)...)
	return stmts, jen.Nil()
}

// generateSelectBody generates the body statements for a select: block
// Returns the body statements (all but last) and the condition expression (last statement)
func (g *generator) generateSelectBody(body []parser.Statement, m *compiledMethod, iterVar string) ([]jen.Code, *jen.Statement) {
	if len(body) == 0 {
		return nil, jen.Lit(false)
	}

	var stmts []jen.Code
	// Generate all statements except the last
	for i := 0; i < len(body)-1; i++ {
		stmts = append(stmts, g.generateIterationBodyStatement(body[i], m, iterVar)...)
	}

	// Last statement should be an expression (the predicate) - extract it
	lastStmt := body[len(body)-1]
	if exprStmt, ok := lastStmt.(*parser.ExprStmt); ok {
		return stmts, g.generateExpr(exprStmt.Expr, m)
	}

	// If last statement is not an expression, generate it normally and return false
	stmts = append(stmts, g.generateIterationBodyStatement(lastStmt, m, iterVar)...)
	return stmts, jen.Lit(false)
}

// generateIterationBodyStatement generates statements within an iteration block
// The iterVar is available as a local variable
func (g *generator) generateIterationBodyStatement(stmt parser.Statement, m *compiledMethod, iterVar string) []jen.Code {
	// For now, just generate the statement normally
	// The iterVar will be in scope from the for loop
	return g.generateStatement(stmt, m)
}

// generateDynamicIterationStatement generates shell-out iteration for dynamic blocks (Phase 2)
// When the block is a variable/parameter, we call back to Bash for each element
func (g *generator) generateDynamicIterationStatement(s *parser.DynamicIterationExpr, m *compiledMethod) []jen.Code {
	collectionExpr := g.generateExpr(s.Collection, m)
	// Block IDs are strings - don't use the Int conversion
	blockExpr := g.generateExprAsString(s.BlockVar, m)

	// Check if collection is a native array (from JSON primitives) vs JSON string
	isNativeArray := g.exprResultsInArray(s.Collection)

	switch s.Kind {
	case "do":
		if isNativeArray {
			// Native array: iterate directly, call block for each element
			return []jen.Code{
				jen.For(jen.List(jen.Id("_"), jen.Id("_elem")).Op(":=").Range().Add(collectionExpr)).Block(
					jen.Id("_").Op("=").Id("invokeBlock").Call(
						blockExpr,
						jen.Id("_elem"),
					),
				),
			}
		}
		// JSON string: unmarshal first
		return []jen.Code{
			jen.Var().Id("_items").Index().Interface(),
			jen.Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(collectionExpr),
				jen.Op("&").Id("_items"),
			),
			jen.For(jen.List(jen.Id("_"), jen.Id("_elem")).Op(":=").Range().Id("_items")).Block(
				jen.Id("_").Op("=").Id("invokeBlock").Call(
					blockExpr,
					jen.Id("_elem"),
				),
			),
		}

	case "collect":
		if isNativeArray {
			// Native array: collect results from block calls
			return []jen.Code{
				jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0), jen.Len(collectionExpr)),
				jen.For(jen.List(jen.Id("_"), jen.Id("_elem")).Op(":=").Range().Add(collectionExpr)).Block(
					jen.Id("_result").Op(":=").Id("invokeBlock").Call(
						blockExpr,
						jen.Id("_elem"),
					),
					jen.Id("_results").Op("=").Append(jen.Id("_results"), jen.Id("_result")),
				),
			}
		}
		// JSON string: unmarshal first
		return []jen.Code{
			jen.Var().Id("_items").Index().Interface(),
			jen.Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(collectionExpr),
				jen.Op("&").Id("_items"),
			),
			jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
			jen.For(jen.List(jen.Id("_"), jen.Id("_elem")).Op(":=").Range().Id("_items")).Block(
				jen.Id("_result").Op(":=").Id("invokeBlock").Call(
					blockExpr,
					jen.Id("_elem"),
				),
				jen.Id("_results").Op("=").Append(jen.Id("_results"), jen.Id("_result")),
			),
		}

	case "select":
		if isNativeArray {
			// Native array: filter based on block result
			return []jen.Code{
				jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
				jen.For(jen.List(jen.Id("_"), jen.Id("_elem")).Op(":=").Range().Add(collectionExpr)).Block(
					jen.Id("_result").Op(":=").Id("invokeBlock").Call(
						blockExpr,
						jen.Id("_elem"),
					),
					jen.Comment("Non-empty string result means true"),
					jen.If(jen.Id("_result").Op("!=").Lit("")).Block(
						jen.Id("_results").Op("=").Append(jen.Id("_results"), jen.Id("_elem")),
					),
				),
			}
		}
		// JSON string: unmarshal first
		return []jen.Code{
			jen.Var().Id("_items").Index().Interface(),
			jen.Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(collectionExpr),
				jen.Op("&").Id("_items"),
			),
			jen.Id("_results").Op(":=").Make(jen.Index().Interface(), jen.Lit(0)),
			jen.For(jen.List(jen.Id("_"), jen.Id("_elem")).Op(":=").Range().Id("_items")).Block(
				jen.Id("_result").Op(":=").Id("invokeBlock").Call(
					blockExpr,
					jen.Id("_elem"),
				),
				jen.Comment("Non-empty string result means true"),
				jen.If(jen.Id("_result").Op("!=").Lit("")).Block(
					jen.Id("_results").Op("=").Append(jen.Id("_results"), jen.Id("_elem")),
				),
			),
		}

	default:
		return []jen.Code{jen.Comment("unknown dynamic iteration kind: " + s.Kind)}
	}
}

// generateExprAsString generates an expression keeping method args as strings (no int conversion)
// Used for block IDs and other cases where we need the original string parameter
func (g *generator) generateExprAsString(expr parser.Expr, m *compiledMethod) *jen.Statement {
	switch e := expr.(type) {
	case *parser.Identifier:
		name := e.Name
		// Check if it's self (the receiver)
		if name == "self" {
			return jen.Id("c")
		}
		// Check if it's an instance variable
		if g.instanceVars[name] {
			fieldAccess := jen.Id("c").Dot(capitalize(name))
			// JSON vars are json.RawMessage, need to convert to string
			if g.jsonVars[name] {
				return jen.String().Parens(fieldAccess)
			}
			return fieldAccess
		}
		// For method args, use the string parameter directly (no Int conversion)
		return jen.Id(name)
	default:
		// For other expressions, fall back to regular generation
		return g.generateExpr(expr, m)
	}
}

func (g *generator) generateExpr(expr parser.Expr, m *compiledMethod) *jen.Statement {
	switch e := expr.(type) {
	case *parser.BinaryExpr:
		// String concatenation with comma operator
		if e.Op == "," {
			left := g.generateStringArg(e.Left, m)
			right := g.generateStringArg(e.Right, m)
			return left.Op("+").Add(right)
		}
		// Wrap in toInt() for interface{} compatibility (arithmetic)
		left := jen.Id("toInt").Call(g.generateExpr(e.Left, m))
		right := jen.Id("toInt").Call(g.generateExpr(e.Right, m))
		switch e.Op {
		case "+":
			return left.Op("+").Add(right)
		case "-":
			return left.Op("-").Add(right)
		case "*":
			return left.Op("*").Add(right)
		case "/":
			return left.Op("/").Add(right)
		}
		return jen.Comment("unknown op: " + e.Op)

	case *parser.ComparisonExpr:
		// Wrap in toInt() for interface{} compatibility
		left := jen.Id("toInt").Call(g.generateExpr(e.Left, m))
		right := jen.Id("toInt").Call(g.generateExpr(e.Right, m))
		return left.Op(e.Op).Add(right)

	case *parser.Identifier:
		name := e.Name
		// Check if it's self - in instance methods it's the receiver, in class methods it's the class name
		if name == "self" {
			if m.isClass {
				// In class methods, self is the class name as a string
				return jen.Lit(g.class.Name)
			}
			return jen.Id("c")
		}
		// Check if it's a method arg FIRST (params are strings, use as-is)
		// This must come before instance var check to handle cases where
		// a method param has the same name as an instance var
		for _, arg := range m.args {
			if arg == name {
				if renamed, ok := m.renamedVars[name]; ok {
					return jen.Id(renamed)
				}
				return jen.Id(name)
			}
		}
		// Check if it's an instance variable (only for instance methods)
		if !m.isClass && g.instanceVars[name] {
			fieldAccess := jen.Id("c").Dot(capitalize(name))
			// JSON vars are json.RawMessage, need to convert to string
			if g.jsonVars[name] {
				return jen.String().Parens(fieldAccess)
			}
			return fieldAccess
		}
		// Check if this variable was renamed to avoid Go builtin conflict
		if renamed, ok := m.renamedVars[name]; ok {
			return jen.Id(renamed)
		}
		// Check if it's a local variable declared in the method
		if m.body != nil {
			for _, local := range m.body.LocalVars {
				if local == name {
					return jen.Id(name)
				}
			}
		}
		// Check for inherited instance variable (class has parent, not a known local/param)
		// In Trashtalk, unknown variables in subclasses are inherited ivars from parent
		if !m.isClass && g.class.Parent != "" && g.class.Parent != "Object" {
			// Use runtime lookup for inherited ivar
			return jen.Id("getInstanceVar").Call(jen.Id("instanceID"), jen.Lit(name))
		}
		return jen.Id(name)

	case *parser.QualifiedName:
		// Qualified name (Pkg::Class) - return the full name as a string literal
		return jen.Lit(e.FullName())

	case *parser.NumberLit:
		return jen.Lit(mustAtoi(e.Value))

	case *parser.StringLit:
		return jen.Lit(e.Value)

	case *parser.MessageSend:
		if e.IsSelf {
			// Check if target method is raw or skipped (will fall back to bash)
			// If so, use sendMessage to call bash runtime instead of direct Go call
			isTargetSkipped := g.skippedMethods[e.Selector] != ""
			if !isTargetSkipped {
				// Also check if explicitly marked as raw
				for _, method := range g.class.Methods {
					if method.Selector == e.Selector && method.Raw {
						isTargetSkipped = true
						break
					}
				}
			}

			// Determine if target method is a class method
			isTargetClassMethod := false
			for _, method := range g.class.Methods {
				if method.Selector == e.Selector && method.Kind == "class" {
					isTargetClassMethod = true
					break
				}
			}

			if isTargetSkipped {
				// Use sendMessage for skipped/raw methods that aren't compiled to Go
				var receiverArg jen.Code
				if m.isClass {
					// In class method, self is the class name
					receiverArg = jen.Lit(g.class.Name)
				} else {
					receiverArg = jen.Id("c")
				}
				args := []jen.Code{receiverArg, jen.Lit(e.Selector)}
				for _, arg := range e.Args {
					args = append(args, g.generateExprAsString(arg, m))
				}
				return jen.Id("sendMessage").Call(args...)
			}

			// Self send to compiled method
			goMethodName := selectorToGoName(e.Selector)

			// Build args - Go methods take string params
			goArgs := []jen.Code{}
			for _, arg := range e.Args {
				// Check if the arg is a method parameter (already a string)
				if ident, ok := arg.(*parser.Identifier); ok {
					isMethodArg := false
					for _, methodArg := range m.args {
						if methodArg == ident.Name {
							isMethodArg = true
							break
						}
					}
					if isMethodArg {
						// Use original string parameter directly
						goArgs = append(goArgs, jen.Id(ident.Name))
						continue
					}
					// Local variable - is interface{}, need to convert to string
					goArgs = append(goArgs, jen.Id("_toStr").Call(jen.Id(ident.Name)))
					continue
				}
				// For other args, generate and convert if needed
				argExpr := g.generateExpr(arg, m)
				// Wrap numeric literals in strconv.Itoa
				if _, ok := arg.(*parser.NumberLit); ok {
					argExpr = jen.Qual("strconv", "Itoa").Call(argExpr)
				}
				goArgs = append(goArgs, argExpr)
			}

			if isTargetClassMethod {
				// Class method: direct function call (no receiver)
				return jen.Id(goMethodName).Call(goArgs...)
			}

			// Instance method on self
			if m.isClass {
				// We're in a class method trying to call instance method on self
				// This shouldn't normally happen, but fall back to sendMessage
				args := []jen.Code{jen.Lit(g.class.Name), jen.Lit(e.Selector)}
				for _, arg := range e.Args {
					args = append(args, g.generateExprAsString(arg, m))
				}
				return jen.Id("sendMessage").Call(args...)
			}

			// Instance method call on receiver
			return jen.Id("c").Dot(goMethodName).Call(goArgs...)
		}

		// Check for block invocation pattern: @ aBlock value / valueWith: / valueWith:and:
		// When receiver is a method parameter and selector is a block invocation selector,
		// use invokeBlock() instead of sendMessage() for better performance
		if ident, ok := e.Receiver.(*parser.Identifier); ok {
			isMethodParam := false
			for _, arg := range m.args {
				if arg == ident.Name {
					isMethodParam = true
					break
				}
			}
			if isMethodParam && isBlockInvocationSelector(e.Selector) {
				// Generate: invokeBlock(blockID, args...)
				blockArgs := []jen.Code{jen.Id(ident.Name)} // Use string param directly
				for _, arg := range e.Args {
					blockArgs = append(blockArgs, g.generateExprAsString(arg, m))
				}
				return jen.Id("invokeBlock").Call(blockArgs...)
			}
		}

		// Non-self send: shell out to bash runtime
		// Generate: sendMessage(receiver, selector, args...)
		var receiverExpr *jen.Statement
		// Check if receiver is a qualified name (Pkg::Class) - use full name as string literal
		if qn, ok := e.Receiver.(*parser.QualifiedName); ok {
			receiverExpr = jen.Lit(qn.FullName())
		} else if ident, ok := e.Receiver.(*parser.Identifier); ok {
			// Check if receiver is a class name (uppercase identifier that's not a local var)
			name := ident.Name
			isLocalVar := false
			// Check instance vars, method args, and local vars
			if g.instanceVars[name] {
				isLocalVar = true
			}
			for _, arg := range m.args {
				if arg == name {
					isLocalVar = true
					break
				}
			}
			if _, ok := m.renamedVars[name]; ok {
				isLocalVar = true
			}
			// Uppercase name that's not a local var is a class name - use string literal
			if !isLocalVar && len(name) > 0 && name[0] >= 'A' && name[0] <= 'Z' {
				receiverExpr = jen.Lit(name)
			} else {
				receiverExpr = g.generateExpr(e.Receiver, m)
			}
		} else {
			receiverExpr = g.generateExpr(e.Receiver, m)
		}
		args := []jen.Code{
			receiverExpr,
			jen.Lit(e.Selector),
		}
		for _, arg := range e.Args {
			args = append(args, g.generateExpr(arg, m))
		}
		return jen.Id("sendMessage").Call(args...)

	case *parser.JSONPrimitiveExpr:
		return g.generateJSONPrimitive(e, m)

	case *parser.ClassPrimitiveExpr:
		return g.generateClassPrimitive(e, m)

	case *parser.BlockExpr:
		// Block used as expression (e.g., [i < len] whileTrue: [...])
		// If it has a single expression statement, extract and evaluate it
		if len(e.Statements) == 1 {
			if exprStmt, ok := e.Statements[0].(*parser.ExprStmt); ok {
				return g.generateExpr(exprStmt.Expr, m)
			}
		}
		// Complex block - can't inline as expression
		return jen.Comment("complex block expression not supported")

	default:
		return jen.Comment("unknown expr")
	}
}

// generateStringArg generates an expression that keeps the original string value
// (unlike generateExpr which converts method args to int)
func (g *generator) generateStringArg(expr parser.Expr, m *compiledMethod) *jen.Statement {
	switch e := expr.(type) {
	case *parser.Identifier:
		// For identifiers, check if it's a method arg - if so, use the original string
		for _, arg := range m.args {
			if arg == e.Name {
				return jen.Id(e.Name) // Use original string parameter
			}
		}
		// Local variables are interface{}, wrap in _toStr for string conversion
		return jen.Id("_toStr").Call(g.generateExpr(expr, m))
	case *parser.StringLit:
		return jen.Lit(e.Value)
	case *parser.BinaryExpr:
		if e.Op == "," {
			// Nested concatenation - recursively handle
			left := g.generateStringArg(e.Left, m)
			right := g.generateStringArg(e.Right, m)
			return left.Op("+").Add(right)
		}
		// Arithmetic expression - wrap result in _toStr
		return jen.Id("_toStr").Call(g.generateExpr(expr, m))
	default:
		// For other expressions, wrap in _toStr for string conversion
		return jen.Id("_toStr").Call(g.generateExpr(expr, m))
	}
}

// generateJSONPrimitive generates Go code for JSON primitive operations
func (g *generator) generateJSONPrimitive(e *parser.JSONPrimitiveExpr, m *compiledMethod) *jen.Statement {
	receiver := g.generateExpr(e.Receiver, m)

	// Check if receiver expression results in a typed array/object
	// This handles both direct ivar access and chained operations
	isArrayType := g.exprResultsInArray(e.Receiver)
	isObjectType := g.exprResultsInObject(e.Receiver)

	switch e.Operation {
	// Array operations
	case "arrayLength":
		// Return string for consistency - all JSON primitives return strings
		if isArrayType {
			return jen.Qual("strconv", "Itoa").Call(jen.Len(receiver))
		}
		// Fallback: string containing JSON - _jsonArrayLen returns int, wrap in Itoa
		return jen.Qual("strconv", "Itoa").Call(jen.Id("_jsonArrayLen").Call(receiver))

	case "arrayFirst":
		if isArrayType {
			// Return string for consistency - _arrayFirst returns interface{}, convert to string
			return jen.Id("_toStr").Call(jen.Id("_arrayFirst").Call(receiver))
		}
		// JSON array - _jsonArrayFirst returns string
		return jen.Id("_jsonArrayFirst").Call(receiver)

	case "arrayLast":
		if isArrayType {
			// Return string for consistency - _arrayLast returns interface{}, convert to string
			return jen.Id("_toStr").Call(jen.Id("_arrayLast").Call(receiver))
		}
		// JSON array - _jsonArrayLast returns string
		return jen.Id("_jsonArrayLast").Call(receiver)

	case "arrayIsEmpty":
		if isArrayType {
			return jen.Id("_boolToString").Call(jen.Len(receiver).Op("==").Lit(0))
		}
		return jen.Id("_boolToString").Call(jen.Id("_jsonArrayIsEmpty").Call(receiver))

	case "arrayPush":
		arg := g.generateExpr(e.Args[0], m)
		if isArrayType {
			// Optimization: combine chained arrayPush into single append
			// e.g., items arrayPush: x arrayPush: y -> append(c.Items, x, y)
			allArgs, baseReceiver := g.collectArrayPushArgs(e, m)
			if len(allArgs) > 1 {
				// Build append(base, arg1, arg2, ...) - all args in one slice
				codes := make([]jen.Code, 0, len(allArgs)+1)
				codes = append(codes, baseReceiver)
				for _, a := range allArgs {
					codes = append(codes, a)
				}
				return jen.Append(codes...)
			}
			// Single append
			return jen.Append(receiver, arg)
		}
		return jen.Id("_jsonArrayPush").Call(receiver, arg)

	case "arrayAt":
		idx := g.generateExpr(e.Args[0], m)
		if isArrayType {
			// Return string for consistency - element is interface{}, convert to string
			return jen.Id("_toStr").Call(receiver.Clone().Index(jen.Id("toInt").Call(idx)))
		}
		// JSON array - _jsonArrayAt returns string, wrap idx in toInt for interface{} compatibility
		return jen.Id("_jsonArrayAt").Call(receiver, jen.Id("toInt").Call(idx))

	case "arrayAtPut":
		idx := g.generateExpr(e.Args[0], m)
		val := g.generateExpr(e.Args[1], m)
		if isArrayType {
			// Return new slice with updated element (immutable style)
			return jen.Id("_arrayAtPut").Call(receiver, jen.Id("toInt").Call(idx), val)
		}
		return jen.Id("_jsonArrayAtPut").Call(receiver, jen.Id("toInt").Call(idx), val)

	case "arrayRemoveAt":
		idx := g.generateExpr(e.Args[0], m)
		if isArrayType {
			return jen.Id("_arrayRemoveAt").Call(receiver, jen.Id("toInt").Call(idx))
		}
		return jen.Id("_jsonArrayRemoveAt").Call(receiver, jen.Id("toInt").Call(idx))

	// Object operations
	case "objectLength":
		if isObjectType {
			return jen.Qual("strconv", "Itoa").Call(jen.Len(receiver))
		}
		return jen.Qual("strconv", "Itoa").Call(jen.Id("_jsonObjectLen").Call(receiver))

	case "objectKeys":
		if isObjectType {
			return jen.Id("_mapKeys").Call(receiver)
		}
		return jen.Id("_jsonObjectKeys").Call(receiver)

	case "objectValues":
		if isObjectType {
			return jen.Id("_mapValues").Call(receiver)
		}
		return jen.Id("_jsonObjectValues").Call(receiver)

	case "objectIsEmpty":
		if isObjectType {
			return jen.Id("_boolToString").Call(jen.Len(receiver).Op("==").Lit(0))
		}
		return jen.Id("_boolToString").Call(jen.Id("_jsonObjectIsEmpty").Call(receiver))

	case "objectAt":
		key := g.generateStringArg(e.Args[0], m) // Object keys are always strings
		if isObjectType {
			return jen.Id("_toStr").Call(receiver.Clone().Index(key))
		}
		return jen.Id("_jsonObjectAt").Call(receiver, key)

	case "objectAtPut":
		key := g.generateStringArg(e.Args[0], m) // Object keys are always strings
		val := g.generateExpr(e.Args[1], m)
		if isObjectType {
			// Return new map with updated key (immutable style)
			return jen.Id("_mapAtPut").Call(receiver, key, val)
		}
		return jen.Id("_jsonObjectAtPut").Call(receiver, key, val)

	case "objectHasKey":
		key := g.generateStringArg(e.Args[0], m) // Object keys are always strings
		if isObjectType {
			return jen.Id("_boolToString").Call(jen.Id("_mapHasKey").Call(receiver, key))
		}
		return jen.Id("_boolToString").Call(jen.Id("_jsonObjectHasKey").Call(receiver, key))

	case "objectRemoveKey":
		key := g.generateStringArg(e.Args[0], m) // Object keys are always strings
		if isObjectType {
			return jen.Id("_mapRemoveKey").Call(receiver, key)
		}
		return jen.Id("_jsonObjectRemoveKey").Call(receiver, key)

	// String to array conversion
	case "stringToJsonArray":
		// Convert newline-separated text to JSON array
		// receiver is the text string
		return jen.Id("stringToJsonArray").Call(jen.Id("_toStr").Call(receiver))

	default:
		return jen.Comment("unknown JSON primitive: " + e.Operation)
	}
}

// Helper functions

func capitalize(s string) string {
	if len(s) == 0 {
		return s
	}
	return strings.ToUpper(s[0:1]) + s[1:]
}

func selectorToGoName(selector string) string {
	// Preserve trailing underscore - it distinguishes keyword methods (exists_)
	// from unary methods (exists). Both map to valid Go identifiers.
	return capitalize(selector)
}

func mustAtoi(s string) int {
	var n int
	fmt.Sscanf(s, "%d", &n)
	return n
}

// isBlockInvocationSelector returns true if the selector is used to invoke blocks
// These are: value, valueWith:, valueWith:and:
// Note: Parser transforms "valueWith:" to "valueWith_" and "valueWith:and:" to "valueWith_and_"
func isBlockInvocationSelector(selector string) bool {
	switch selector {
	case "value", "valueWith_", "valueWith_and_":
		return true
	default:
		return false
	}
}

// generateClassPrimitive generates Go code for class primitive operations
// like @ String isEmpty: str, @ File exists: path
func (g *generator) generateClassPrimitive(e *parser.ClassPrimitiveExpr, m *compiledMethod) *jen.Statement {
	switch e.ClassName {
	case "String":
		return g.generateStringPrimitive(e, m)
	case "File":
		return g.generateFilePrimitive(e, m)
	default:
		return jen.Comment("unknown class primitive: " + e.ClassName)
	}
}

// generateStringPrimitive generates Go code for String class primitives
func (g *generator) generateStringPrimitive(e *parser.ClassPrimitiveExpr, m *compiledMethod) *jen.Statement {
	switch e.Operation {
	case "stringIsEmpty":
		arg := g.generateStringArg(e.Args[0], m)
		return jen.Id("_boolToString").Call(jen.Len(arg).Op("==").Lit(0))

	case "stringNotEmpty":
		arg := g.generateStringArg(e.Args[0], m)
		return jen.Id("_boolToString").Call(jen.Len(arg).Op(">").Lit(0))

	case "stringContains":
		str := g.generateStringArg(e.Args[0], m)
		sub := g.generateStringArg(e.Args[1], m)
		return jen.Id("_boolToString").Call(
			jen.Qual("strings", "Contains").Call(str, sub),
		)

	case "stringStartsWith":
		str := g.generateStringArg(e.Args[0], m)
		prefix := g.generateStringArg(e.Args[1], m)
		return jen.Id("_boolToString").Call(
			jen.Qual("strings", "HasPrefix").Call(str, prefix),
		)

	case "stringEndsWith":
		str := g.generateStringArg(e.Args[0], m)
		suffix := g.generateStringArg(e.Args[1], m)
		return jen.Id("_boolToString").Call(
			jen.Qual("strings", "HasSuffix").Call(str, suffix),
		)

	case "stringEquals":
		a := g.generateStringArg(e.Args[0], m)
		b := g.generateStringArg(e.Args[1], m)
		return jen.Id("_boolToString").Call(a.Op("==").Add(b))

	case "stringTrimPrefix":
		// trimPrefix: pattern from: str -> TrimPrefix(str, pattern)
		pattern := g.generateStringArg(e.Args[0], m)
		str := g.generateStringArg(e.Args[1], m)
		return jen.Qual("strings", "TrimPrefix").Call(str, pattern)

	case "stringTrimSuffix":
		// trimSuffix: pattern from: str -> TrimSuffix(str, pattern)
		pattern := g.generateStringArg(e.Args[0], m)
		str := g.generateStringArg(e.Args[1], m)
		return jen.Qual("strings", "TrimSuffix").Call(str, pattern)

	case "stringReplace":
		// replace: old with: new in: str -> Replace(str, old, new, 1)
		old := g.generateStringArg(e.Args[0], m)
		newStr := g.generateStringArg(e.Args[1], m)
		str := g.generateStringArg(e.Args[2], m)
		return jen.Qual("strings", "Replace").Call(str, old, newStr, jen.Lit(1))

	case "stringReplaceAll":
		// replaceAll: old with: new in: str -> ReplaceAll(str, old, new)
		old := g.generateStringArg(e.Args[0], m)
		newStr := g.generateStringArg(e.Args[1], m)
		str := g.generateStringArg(e.Args[2], m)
		return jen.Qual("strings", "ReplaceAll").Call(str, old, newStr)

	case "stringSubstring":
		// substring: str from: start length: len
		str := g.generateStringArg(e.Args[0], m)
		start := g.generateExpr(e.Args[1], m)
		length := g.generateExpr(e.Args[2], m)
		return jen.Id("_stringSubstring").Call(str, jen.Id("toInt").Call(start), jen.Id("toInt").Call(length))

	case "stringLength":
		arg := g.generateStringArg(e.Args[0], m)
		return jen.Qual("strconv", "Itoa").Call(jen.Len(arg))

	case "stringUppercase":
		arg := g.generateStringArg(e.Args[0], m)
		return jen.Qual("strings", "ToUpper").Call(arg)

	case "stringLowercase":
		arg := g.generateStringArg(e.Args[0], m)
		return jen.Qual("strings", "ToLower").Call(arg)

	case "stringTrim":
		arg := g.generateStringArg(e.Args[0], m)
		return jen.Qual("strings", "TrimSpace").Call(arg)

	case "stringConcat":
		a := g.generateStringArg(e.Args[0], m)
		b := g.generateStringArg(e.Args[1], m)
		return a.Op("+").Add(b)

	default:
		return jen.Comment("unknown string primitive: " + e.Operation)
	}
}

// generateFilePrimitive generates Go code for File class primitives
func (g *generator) generateFilePrimitive(e *parser.ClassPrimitiveExpr, m *compiledMethod) *jen.Statement {
	switch e.Operation {
	case "fileExists":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileExists").Call(path)

	case "fileIsFile":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsFile").Call(path)

	case "fileIsDirectory":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsDirectory").Call(path)

	case "fileIsSymlink":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsSymlink").Call(path)

	case "fileIsFifo":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsFifo").Call(path)

	case "fileIsSocket":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsSocket").Call(path)

	case "fileIsBlockDevice":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsBlockDevice").Call(path)

	case "fileIsCharDevice":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsCharDevice").Call(path)

	case "fileIsReadable":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsReadable").Call(path)

	case "fileIsWritable":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsWritable").Call(path)

	case "fileIsExecutable":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsExecutable").Call(path)

	case "fileIsEmpty":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileIsEmpty").Call(path)

	case "fileNotEmpty":
		path := g.generateStringArg(e.Args[0], m)
		return jen.Id("_fileNotEmpty").Call(path)

	case "fileIsNewer":
		path1 := g.generateStringArg(e.Args[0], m)
		path2 := g.generateStringArg(e.Args[1], m)
		return jen.Id("_fileIsNewer").Call(path1, path2)

	case "fileIsOlder":
		path1 := g.generateStringArg(e.Args[0], m)
		path2 := g.generateStringArg(e.Args[1], m)
		return jen.Id("_fileIsOlder").Call(path1, path2)

	case "fileIsSame":
		path1 := g.generateStringArg(e.Args[0], m)
		path2 := g.generateStringArg(e.Args[1], m)
		return jen.Id("_fileIsSame").Call(path1, path2)

	default:
		return jen.Comment("unknown file primitive: " + e.Operation)
	}
}

// hasReturnInStatements recursively checks if any statement contains a return
func hasReturnInStatements(stmts []parser.Statement) bool {
	for _, stmt := range stmts {
		switch s := stmt.(type) {
		case *parser.Return:
			return true
		case *parser.IfExpr:
			// Check inside both branches
			if hasReturnInStatements(s.TrueBlock) {
				return true
			}
			if hasReturnInStatements(s.FalseBlock) {
				return true
			}
		case *parser.WhileExpr:
			// Check inside loop body
			if hasReturnInStatements(s.Body) {
				return true
			}
		case *parser.IfNilExpr:
			// Check inside both nil and not-nil blocks
			if hasReturnInStatements(s.NilBlock) {
				return true
			}
			if hasReturnInStatements(s.NotNilBlock) {
				return true
			}
		}
	}
	return false
}

// goBuiltins are Go builtin identifiers that cannot be used as variable names
var goBuiltins = map[string]bool{
	// Builtin functions
	"len": true, "cap": true, "make": true, "new": true, "append": true,
	"copy": true, "delete": true, "close": true, "panic": true, "recover": true,
	"print": true, "println": true, "complex": true, "real": true, "imag": true,
	// Constants
	"true": true, "false": true, "nil": true, "iota": true,
	// Types
	"int": true, "int8": true, "int16": true, "int32": true, "int64": true,
	"uint": true, "uint8": true, "uint16": true, "uint32": true, "uint64": true,
	"float32": true, "float64": true, "complex64": true, "complex128": true,
	"byte": true, "rune": true, "string": true, "bool": true, "error": true,
	// Go keywords (cannot be used as identifiers)
	"break": true, "case": true, "chan": true, "const": true, "continue": true,
	"default": true, "defer": true, "else": true, "fallthrough": true, "for": true,
	"func": true, "go": true, "goto": true, "if": true, "import": true,
	"interface": true, "map": true, "package": true, "range": true, "return": true,
	"select": true, "struct": true, "switch": true, "type": true, "var": true,
}

// safeGoName returns a safe Go identifier, renaming if it conflicts with builtins
func safeGoName(name string) string {
	if goBuiltins[name] {
		return name + "_"
	}
	return name
}

// collectArrayPushArgs collects all arguments from a chain of arrayPush operations
// Returns the collected args and the base receiver (the original array ivar)
// e.g., for "items arrayPush: x arrayPush: y arrayPush: z", returns ([x,y,z], c.Items)
func (g *generator) collectArrayPushArgs(e *parser.JSONPrimitiveExpr, m *compiledMethod) ([]*jen.Statement, *jen.Statement) {
	if e.Operation != "arrayPush" {
		return nil, nil
	}

	// Collect args in reverse order (innermost first)
	var args []*jen.Statement
	current := e

	for {
		// Add current arg
		arg := g.generateExpr(current.Args[0], m)
		args = append(args, arg)

		// Check if receiver is another arrayPush
		if inner, ok := current.Receiver.(*parser.JSONPrimitiveExpr); ok && inner.Operation == "arrayPush" {
			current = inner
		} else {
			// Base case: receiver is not arrayPush
			break
		}
	}

	// Get the base receiver (the original array)
	baseReceiver := g.generateExpr(current.Receiver, m)

	// Reverse args to get correct order (outermost first)
	for i, j := 0, len(args)-1; i < j; i, j = i+1, j-1 {
		args[i], args[j] = args[j], args[i]
	}

	return args, baseReceiver
}

// generateJSONHelpers generates helper functions for JSON primitive operations
func (g *generator) generateJSONHelpers(f *jen.File) {
	// Common conversion helpers
	f.Comment("// Common conversion helpers")

	// _boolToString - convert bool to "true"/"false" string
	f.Func().Id("_boolToString").Params(jen.Id("b").Bool()).String().Block(
		jen.If(jen.Id("b")).Block(
			jen.Return(jen.Lit("true")),
		),
		jen.Return(jen.Lit("false")),
	)
	f.Line()

	// _toStr - convert interface{} to string
	f.Func().Id("_toStr").Params(jen.Id("v").Interface()).String().Block(
		jen.If(jen.Id("v").Op("==").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("v"))),
	)
	f.Line()

	// Array helpers for []interface{} typed fields
	f.Comment("// Array helpers for native slice operations")

	// _arrayFirst - get first element of slice
	f.Func().Id("_arrayFirst").Params(jen.Id("arr").Index().Interface()).Interface().Block(
		jen.If(jen.Len(jen.Id("arr")).Op("==").Lit(0)).Block(
			jen.Return(jen.Nil()),
		),
		jen.Return(jen.Id("arr").Index(jen.Lit(0))),
	)
	f.Line()

	// _arrayLast - get last element of slice
	f.Func().Id("_arrayLast").Params(jen.Id("arr").Index().Interface()).Interface().Block(
		jen.If(jen.Len(jen.Id("arr")).Op("==").Lit(0)).Block(
			jen.Return(jen.Nil()),
		),
		jen.Return(jen.Id("arr").Index(jen.Len(jen.Id("arr")).Op("-").Lit(1))),
	)
	f.Line()

	// _arrayAtPut - return new slice with element at index replaced
	f.Func().Id("_arrayAtPut").Params(
		jen.Id("arr").Index().Interface(),
		jen.Id("idx").Int(),
		jen.Id("val").Interface(),
	).Index().Interface().Block(
		jen.Comment("Handle negative indices"),
		jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
			jen.Id("idx").Op("=").Len(jen.Id("arr")).Op("+").Id("idx"),
		),
		jen.If(jen.Id("idx").Op("<").Lit(0).Op("||").Id("idx").Op(">=").Len(jen.Id("arr"))).Block(
			jen.Return(jen.Id("arr")),
		),
		jen.Id("result").Op(":=").Make(jen.Index().Interface(), jen.Len(jen.Id("arr"))),
		jen.Copy(jen.Id("result"), jen.Id("arr")),
		jen.Id("result").Index(jen.Id("idx")).Op("=").Id("val"),
		jen.Return(jen.Id("result")),
	)
	f.Line()

	// _arrayRemoveAt - return new slice with element at index removed
	f.Func().Id("_arrayRemoveAt").Params(
		jen.Id("arr").Index().Interface(),
		jen.Id("idx").Int(),
	).Index().Interface().Block(
		jen.Comment("Handle negative indices"),
		jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
			jen.Id("idx").Op("=").Len(jen.Id("arr")).Op("+").Id("idx"),
		),
		jen.If(jen.Id("idx").Op("<").Lit(0).Op("||").Id("idx").Op(">=").Len(jen.Id("arr"))).Block(
			jen.Return(jen.Id("arr")),
		),
		jen.Id("result").Op(":=").Make(jen.Index().Interface(), jen.Lit(0), jen.Len(jen.Id("arr")).Op("-").Lit(1)),
		jen.Id("result").Op("=").Append(jen.Id("result"), jen.Id("arr").Index(jen.Op(":").Id("idx")).Op("...")),
		jen.Id("result").Op("=").Append(jen.Id("result"), jen.Id("arr").Index(jen.Id("idx").Op("+").Lit(1).Op(":")).Op("...")),
		jen.Return(jen.Id("result")),
	)
	f.Line()

	// Map helpers for map[string]interface{} typed fields
	f.Comment("// Map helpers for native map operations")

	// _mapKeys - get all keys from map
	f.Func().Id("_mapKeys").Params(jen.Id("m").Map(jen.String()).Interface()).Index().String().Block(
		jen.Id("keys").Op(":=").Make(jen.Index().String(), jen.Lit(0), jen.Len(jen.Id("m"))),
		jen.For(jen.Id("k").Op(":=").Range().Id("m")).Block(
			jen.Id("keys").Op("=").Append(jen.Id("keys"), jen.Id("k")),
		),
		jen.Return(jen.Id("keys")),
	)
	f.Line()

	// _mapValues - get all values from map
	f.Func().Id("_mapValues").Params(jen.Id("m").Map(jen.String()).Interface()).Index().Interface().Block(
		jen.Id("vals").Op(":=").Make(jen.Index().Interface(), jen.Lit(0), jen.Len(jen.Id("m"))),
		jen.For(jen.List(jen.Id("_"), jen.Id("v")).Op(":=").Range().Id("m")).Block(
			jen.Id("vals").Op("=").Append(jen.Id("vals"), jen.Id("v")),
		),
		jen.Return(jen.Id("vals")),
	)
	f.Line()

	// _mapHasKey - check if key exists
	f.Func().Id("_mapHasKey").Params(
		jen.Id("m").Map(jen.String()).Interface(),
		jen.Id("key").String(),
	).Bool().Block(
		jen.List(jen.Id("_"), jen.Id("ok")).Op(":=").Id("m").Index(jen.Id("key")),
		jen.Return(jen.Id("ok")),
	)
	f.Line()

	// _mapAtPut - return new map with key set (immutable style)
	f.Func().Id("_mapAtPut").Params(
		jen.Id("m").Map(jen.String()).Interface(),
		jen.Id("key").String(),
		jen.Id("val").Interface(),
	).Map(jen.String()).Interface().Block(
		jen.Id("result").Op(":=").Make(jen.Map(jen.String()).Interface(), jen.Len(jen.Id("m")).Op("+").Lit(1)),
		jen.For(jen.List(jen.Id("k"), jen.Id("v")).Op(":=").Range().Id("m")).Block(
			jen.Id("result").Index(jen.Id("k")).Op("=").Id("v"),
		),
		jen.Id("result").Index(jen.Id("key")).Op("=").Id("val"),
		jen.Return(jen.Id("result")),
	)
	f.Line()

	// _mapRemoveKey - return new map with key removed
	f.Func().Id("_mapRemoveKey").Params(
		jen.Id("m").Map(jen.String()).Interface(),
		jen.Id("key").String(),
	).Map(jen.String()).Interface().Block(
		jen.Id("result").Op(":=").Make(jen.Map(jen.String()).Interface(), jen.Len(jen.Id("m"))),
		jen.For(jen.List(jen.Id("k"), jen.Id("v")).Op(":=").Range().Id("m")).Block(
			jen.If(jen.Id("k").Op("!=").Id("key")).Block(
				jen.Id("result").Index(jen.Id("k")).Op("=").Id("v"),
			),
		),
		jen.Return(jen.Id("result")),
	)
	f.Line()

	// JSON string parsing helpers (for string-typed fields containing JSON)
	f.Comment("// JSON string parsing helpers (for string-typed variables containing JSON)")

	// _jsonArrayLen
	f.Func().Id("_jsonArrayLen").Params(jen.Id("jsonStr").String()).Int().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(0)),
		),
		jen.Return(jen.Len(jen.Id("arr"))),
	)
	f.Line()

	// _jsonArrayFirst
	f.Func().Id("_jsonArrayFirst").Params(jen.Id("jsonStr").String()).String().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		).Op(";").Err().Op("!=").Nil().Op("||").Len(jen.Id("arr")).Op("==").Lit(0)).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arr").Index(jen.Lit(0)))),
	)
	f.Line()

	// _jsonArrayLast
	f.Func().Id("_jsonArrayLast").Params(jen.Id("jsonStr").String()).String().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		).Op(";").Err().Op("!=").Nil().Op("||").Len(jen.Id("arr")).Op("==").Lit(0)).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arr").Index(jen.Len(jen.Id("arr")).Op("-").Lit(1)))),
	)
	f.Line()

	// _jsonArrayIsEmpty
	f.Func().Id("_jsonArrayIsEmpty").Params(jen.Id("jsonStr").String()).Bool().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.True()),
		),
		jen.Return(jen.Len(jen.Id("arr")).Op("==").Lit(0)),
	)
	f.Line()

	// _jsonArrayPush - accepts interface{} for jsonStr to handle interface{} local variables
	f.Func().Id("_jsonArrayPush").Params(
		jen.Id("jsonVal").Interface(),
		jen.Id("val").Interface(),
	).String().Block(
		jen.Id("jsonStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("jsonVal")),
		jen.Var().Id("arr").Index().Interface(),
		jen.Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		),
		jen.Id("arr").Op("=").Append(jen.Id("arr"), jen.Id("val")),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("arr")),
		jen.Return(jen.String().Parens(jen.Id("result"))),
	)
	f.Line()

	// _jsonArrayAt
	f.Func().Id("_jsonArrayAt").Params(
		jen.Id("jsonStr").String(),
		jen.Id("idx").Int(),
	).String().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
			jen.Id("idx").Op("=").Len(jen.Id("arr")).Op("+").Id("idx"),
		),
		jen.If(jen.Id("idx").Op("<").Lit(0).Op("||").Id("idx").Op(">=").Len(jen.Id("arr"))).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("arr").Index(jen.Id("idx")))),
	)
	f.Line()

	// _jsonArrayAtPut
	f.Func().Id("_jsonArrayAtPut").Params(
		jen.Id("jsonStr").String(),
		jen.Id("idx").Int(),
		jen.Id("val").Interface(),
	).String().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		),
		jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
			jen.Id("idx").Op("=").Len(jen.Id("arr")).Op("+").Id("idx"),
		),
		jen.If(jen.Id("idx").Op(">=").Lit(0).Op("&&").Id("idx").Op("<").Len(jen.Id("arr"))).Block(
			jen.Id("arr").Index(jen.Id("idx")).Op("=").Id("val"),
		),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("arr")),
		jen.Return(jen.String().Parens(jen.Id("result"))),
	)
	f.Line()

	// _jsonArrayRemoveAt
	f.Func().Id("_jsonArrayRemoveAt").Params(
		jen.Id("jsonStr").String(),
		jen.Id("idx").Int(),
	).String().Block(
		jen.Var().Id("arr").Index().Interface(),
		jen.Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("arr"),
		),
		jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
			jen.Id("idx").Op("=").Len(jen.Id("arr")).Op("+").Id("idx"),
		),
		jen.If(jen.Id("idx").Op(">=").Lit(0).Op("&&").Id("idx").Op("<").Len(jen.Id("arr"))).Block(
			jen.Id("arr").Op("=").Append(jen.Id("arr").Index(jen.Op(":").Id("idx")), jen.Id("arr").Index(jen.Id("idx").Op("+").Lit(1).Op(":")).Op("...")),
		),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("arr")),
		jen.Return(jen.String().Parens(jen.Id("result"))),
	)
	f.Line()

	// stringToJsonArray - convert newline-separated text to JSON array string
	// Equivalent to: $(echo "$str" | jq -Rc '[., inputs]')
	f.Func().Id("stringToJsonArray").Params(jen.Id("text").String()).String().Block(
		jen.If(jen.Id("text").Op("==").Lit("")).Block(
			jen.Return(jen.Lit("[]")),
		),
		jen.Id("lines").Op(":=").Qual("strings", "Split").Call(jen.Id("text"), jen.Lit("\n")),
		jen.Id("arr").Op(":=").Make(jen.Index().Interface(), jen.Lit(0), jen.Len(jen.Id("lines"))),
		jen.For(jen.List(jen.Id("_"), jen.Id("line")).Op(":=").Range().Id("lines")).Block(
			jen.Id("arr").Op("=").Append(jen.Id("arr"), jen.Id("line")),
		),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("arr")),
		jen.Return(jen.String().Parens(jen.Id("result"))),
	)
	f.Line()

	// Object JSON helpers
	// _jsonObjectLen
	f.Func().Id("_jsonObjectLen").Params(jen.Id("jsonStr").String()).Int().Block(
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(0)),
		),
		jen.Return(jen.Len(jen.Id("m"))),
	)
	f.Line()

	// _jsonObjectKeys
	f.Func().Id("_jsonObjectKeys").Params(jen.Id("jsonStr").String()).Index().String().Block(
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil()),
		),
		jen.Return(jen.Id("_mapKeys").Call(jen.Id("m"))),
	)
	f.Line()

	// _jsonObjectValues
	f.Func().Id("_jsonObjectValues").Params(jen.Id("jsonStr").String()).Index().Interface().Block(
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil()),
		),
		jen.Return(jen.Id("_mapValues").Call(jen.Id("m"))),
	)
	f.Line()

	// _jsonObjectIsEmpty
	f.Func().Id("_jsonObjectIsEmpty").Params(jen.Id("jsonStr").String()).Bool().Block(
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.True()),
		),
		jen.Return(jen.Len(jen.Id("m")).Op("==").Lit(0)),
	)
	f.Line()

	// _jsonObjectAt - accepts any to handle both string and interface{} local vars
	f.Func().Id("_jsonObjectAt").Params(
		jen.Id("jsonVal").Any(),
		jen.Id("key").String(),
	).String().Block(
		jen.Id("jsonStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("jsonVal")),
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("")),
		),
		jen.If(jen.List(jen.Id("v"), jen.Id("ok")).Op(":=").Id("m").Index(jen.Id("key")).Op(";").Id("ok")).Block(
			jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("v"))),
		),
		jen.Return(jen.Lit("")),
	)
	f.Line()

	// _jsonObjectAtPut - accepts any to handle both string and interface{} local vars
	f.Func().Id("_jsonObjectAtPut").Params(
		jen.Id("jsonVal").Any(),
		jen.Id("key").String(),
		jen.Id("val").Any(),
	).String().Block(
		jen.Id("jsonStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("jsonVal")),
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		),
		jen.If(jen.Id("m").Op("==").Nil()).Block(
			jen.Id("m").Op("=").Make(jen.Map(jen.String()).Interface()),
		),
		jen.Id("m").Index(jen.Id("key")).Op("=").Id("val"),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("m")),
		jen.Return(jen.String().Parens(jen.Id("result"))),
	)
	f.Line()

	// _jsonObjectHasKey - accepts any to handle both string and interface{} local vars
	f.Func().Id("_jsonObjectHasKey").Params(
		jen.Id("jsonVal").Any(),
		jen.Id("key").String(),
	).Bool().Block(
		jen.Id("jsonStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("jsonVal")),
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.False()),
		),
		jen.List(jen.Id("_"), jen.Id("ok")).Op(":=").Id("m").Index(jen.Id("key")),
		jen.Return(jen.Id("ok")),
	)
	f.Line()

	// _jsonObjectRemoveKey - accepts any to handle both string and interface{} local vars
	f.Func().Id("_jsonObjectRemoveKey").Params(
		jen.Id("jsonVal").Any(),
		jen.Id("key").String(),
	).String().Block(
		jen.Id("jsonStr").Op(":=").Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("jsonVal")),
		jen.Var().Id("m").Map(jen.String()).Interface(),
		jen.Qual("encoding/json", "Unmarshal").Call(
			jen.Index().Byte().Parens(jen.Id("jsonStr")),
			jen.Op("&").Id("m"),
		),
		jen.Delete(jen.Id("m"), jen.Id("key")),
		jen.List(jen.Id("result"), jen.Id("_")).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("m")),
		jen.Return(jen.String().Parens(jen.Id("result"))),
	)
}

// generateEnvironmentMethod generates specialized SQLite-based implementations
// for the Environment class storage methods.
func (g *generator) generateEnvironmentMethod(f *jen.File, m *compiledMethod) {
	switch m.selector {
	case "get_":
		// Get_(instanceId string) (string, error) - retrieve instance data
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("data").String(),
			jen.Err().Op("=").Id("db").Dot("QueryRow").Call(
				jen.Lit("SELECT data FROM instances WHERE id = ?"),
				jen.Id("instanceId"),
			).Dot("Scan").Call(jen.Op("&").Id("data")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()), // Return empty string if not found
			),
			jen.Return(jen.Id("data"), jen.Nil()),
		)

	case "set_to_":
		// Set_to_(instanceId, data string) (string, error) - store instance data
		f.Func().Id(m.goName).Params(
			jen.Id("instanceId").String(),
			jen.Id("data").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.List(jen.Id("_"), jen.Err()).Op("=").Id("db").Dot("Exec").Call(
				jen.Lit("INSERT OR REPLACE INTO instances (id, data) VALUES (?, json(?))"),
				jen.Id("instanceId"),
				jen.Id("data"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("instanceId"), jen.Nil()),
		)

	case "delete_":
		// Delete_(instanceId string) (string, error) - remove instance
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.List(jen.Id("_"), jen.Err()).Op("=").Id("db").Dot("Exec").Call(
				jen.Lit("DELETE FROM instances WHERE id = ?"),
				jen.Id("instanceId"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Lit(""), jen.Nil()),
		)

	case "findByClass_":
		// FindByClass_(className string) (string, error) - find all instances of class
		f.Func().Id(m.goName).Params(jen.Id("className").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.List(jen.Id("rows"), jen.Err()).Op(":=").Id("db").Dot("Query").Call(
				jen.Lit("SELECT id FROM instances WHERE class = ?"),
				jen.Id("className"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("rows").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("ids").Index().String(),
			jen.For(jen.Id("rows").Dot("Next").Call()).Block(
				jen.Var().Id("id").String(),
				jen.If(jen.Err().Op(":=").Id("rows").Dot("Scan").Call(jen.Op("&").Id("id")).Op(";").Err().Op("==").Nil()).Block(
					jen.Id("ids").Op("=").Append(jen.Id("ids"), jen.Id("id")),
				),
			),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("ids"), jen.Lit("\n")), jen.Nil()),
		)

	case "exists_":
		// Exists_(instanceId string) (string, error) - check if instance exists
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("exists").Int(),
			jen.Err().Op("=").Id("db").Dot("QueryRow").Call(
				jen.Lit("SELECT 1 FROM instances WHERE id = ?"),
				jen.Id("instanceId"),
			).Dot("Scan").Call(jen.Op("&").Id("exists")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("0"), jen.Nil()),
			),
			jen.Return(jen.Lit("1"), jen.Nil()),
		)

	case "listAll":
		// ListAll() string - get all instance IDs
		f.Func().Id("ListAll").Params().String().Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("")),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.List(jen.Id("rows"), jen.Err()).Op(":=").Id("db").Dot("Query").Call(
				jen.Lit("SELECT id FROM instances"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("")),
			),
			jen.Defer().Id("rows").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("ids").Index().String(),
			jen.For(jen.Id("rows").Dot("Next").Call()).Block(
				jen.Var().Id("id").String(),
				jen.If(jen.Err().Op(":=").Id("rows").Dot("Scan").Call(jen.Op("&").Id("id")).Op(";").Err().Op("==").Nil()).Block(
					jen.Id("ids").Op("=").Append(jen.Id("ids"), jen.Id("id")),
				),
			),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("ids"), jen.Lit("\n"))),
		)

	case "countByClass_":
		// CountByClass_(className string) (string, error) - count instances of class
		f.Func().Id(m.goName).Params(jen.Id("className").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("count").Int(),
			jen.Err().Op("=").Id("db").Dot("QueryRow").Call(
				jen.Lit("SELECT COUNT(*) FROM instances WHERE class = ?"),
				jen.Id("className"),
			).Dot("Scan").Call(jen.Op("&").Id("count")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("0"), jen.Nil()),
			),
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("count")), jen.Nil()),
		)

	default:
		// Unknown method - generate a stub
		f.Comment("// " + m.selector + " - unknown Environment method")
		f.Func().Id(m.goName).Params().String().Block(
			jen.Return(jen.Lit("")),
		)
	}
	f.Line()
}

// generateStringFileHelpers generates helper functions for String and File class primitives
func (g *generator) generateStringFileHelpers(f *jen.File) {
	// String helpers
	f.Comment("// String primitive helpers")

	// _stringSubstring - safe substring extraction with bounds checking
	f.Func().Id("_stringSubstring").Params(
		jen.Id("s").String(),
		jen.Id("start").Int(),
		jen.Id("length").Int(),
	).String().Block(
		jen.If(jen.Id("start").Op("<").Lit(0)).Block(
			jen.Id("start").Op("=").Lit(0),
		),
		jen.If(jen.Id("start").Op(">=").Len(jen.Id("s"))).Block(
			jen.Return(jen.Lit("")),
		),
		jen.Id("end").Op(":=").Id("start").Op("+").Id("length"),
		jen.If(jen.Id("end").Op(">").Len(jen.Id("s"))).Block(
			jen.Id("end").Op("=").Len(jen.Id("s")),
		),
		jen.Return(jen.Id("s").Index(jen.Id("start").Op(":").Id("end"))),
	)
	f.Line()

	// File helpers
	f.Comment("// File primitive helpers")

	// _fileExists - check if file/directory exists
	f.Func().Id("_fileExists").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("_"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.Return(jen.Id("_boolToString").Call(jen.Err().Op("==").Nil())),
	)
	f.Line()

	// _fileIsFile - check if path is a regular file
	f.Func().Id("_fileIsFile").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(jen.Id("info").Dot("Mode").Call().Dot("IsRegular").Call())),
	)
	f.Line()

	// _fileIsDirectory - check if path is a directory
	f.Func().Id("_fileIsDirectory").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(jen.Id("info").Dot("IsDir").Call())),
	)
	f.Line()

	// _fileIsSymlink - check if path is a symlink
	f.Func().Id("_fileIsSymlink").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Lstat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeSymlink").Op("!=").Lit(0),
		)),
	)
	f.Line()

	// _fileIsFifo - check if path is a named pipe
	f.Func().Id("_fileIsFifo").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeNamedPipe").Op("!=").Lit(0),
		)),
	)
	f.Line()

	// _fileIsSocket - check if path is a socket
	f.Func().Id("_fileIsSocket").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeSocket").Op("!=").Lit(0),
		)),
	)
	f.Line()

	// _fileIsBlockDevice - check if path is a block device
	f.Func().Id("_fileIsBlockDevice").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeDevice").Op("!=").Lit(0).Op("&&").
				Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeCharDevice").Op("==").Lit(0),
		)),
	)
	f.Line()

	// _fileIsCharDevice - check if path is a character device
	f.Func().Id("_fileIsCharDevice").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeCharDevice").Op("!=").Lit(0),
		)),
	)
	f.Line()

	// _fileIsReadable - check if path is readable
	f.Func().Id("_fileIsReadable").Params(jen.Id("path").String()).String().Block(
		jen.Err().Op(":=").Qual("golang.org/x/sys/unix", "Access").Call(jen.Id("path"), jen.Qual("golang.org/x/sys/unix", "R_OK")),
		jen.Return(jen.Id("_boolToString").Call(jen.Err().Op("==").Nil())),
	)
	f.Line()

	// _fileIsWritable - check if path is writable
	f.Func().Id("_fileIsWritable").Params(jen.Id("path").String()).String().Block(
		jen.Err().Op(":=").Qual("golang.org/x/sys/unix", "Access").Call(jen.Id("path"), jen.Qual("golang.org/x/sys/unix", "W_OK")),
		jen.Return(jen.Id("_boolToString").Call(jen.Err().Op("==").Nil())),
	)
	f.Line()

	// _fileIsExecutable - check if path is executable
	f.Func().Id("_fileIsExecutable").Params(jen.Id("path").String()).String().Block(
		jen.Err().Op(":=").Qual("golang.org/x/sys/unix", "Access").Call(jen.Id("path"), jen.Qual("golang.org/x/sys/unix", "X_OK")),
		jen.Return(jen.Id("_boolToString").Call(jen.Err().Op("==").Nil())),
	)
	f.Line()

	// _fileIsEmpty - check if file has zero size
	f.Func().Id("_fileIsEmpty").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(jen.Id("info").Dot("Size").Call().Op("==").Lit(0))),
	)
	f.Line()

	// _fileNotEmpty - check if file has non-zero size
	f.Func().Id("_fileNotEmpty").Params(jen.Id("path").String()).String().Block(
		jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("path")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(jen.Id("info").Dot("Size").Call().Op(">").Lit(0))),
	)
	f.Line()

	// _fileIsNewer - check if path1 is newer than path2
	f.Func().Id("_fileIsNewer").Params(
		jen.Id("path1").String(),
		jen.Id("path2").String(),
	).String().Block(
		jen.List(jen.Id("info1"), jen.Id("err1")).Op(":=").Qual("os", "Stat").Call(jen.Id("path1")),
		jen.List(jen.Id("info2"), jen.Id("err2")).Op(":=").Qual("os", "Stat").Call(jen.Id("path2")),
		jen.If(jen.Id("err1").Op("!=").Nil().Op("||").Id("err2").Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info1").Dot("ModTime").Call().Dot("After").Call(jen.Id("info2").Dot("ModTime").Call()),
		)),
	)
	f.Line()

	// _fileIsOlder - check if path1 is older than path2
	f.Func().Id("_fileIsOlder").Params(
		jen.Id("path1").String(),
		jen.Id("path2").String(),
	).String().Block(
		jen.List(jen.Id("info1"), jen.Id("err1")).Op(":=").Qual("os", "Stat").Call(jen.Id("path1")),
		jen.List(jen.Id("info2"), jen.Id("err2")).Op(":=").Qual("os", "Stat").Call(jen.Id("path2")),
		jen.If(jen.Id("err1").Op("!=").Nil().Op("||").Id("err2").Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Id("info1").Dot("ModTime").Call().Dot("Before").Call(jen.Id("info2").Dot("ModTime").Call()),
		)),
	)
	f.Line()

	// _fileIsSame - check if both paths refer to the same file (same inode)
	f.Func().Id("_fileIsSame").Params(
		jen.Id("path1").String(),
		jen.Id("path2").String(),
	).String().Block(
		jen.List(jen.Id("info1"), jen.Id("err1")).Op(":=").Qual("os", "Stat").Call(jen.Id("path1")),
		jen.List(jen.Id("info2"), jen.Id("err2")).Op(":=").Qual("os", "Stat").Call(jen.Id("path2")),
		jen.If(jen.Id("err1").Op("!=").Nil().Op("||").Id("err2").Op("!=").Nil()).Block(
			jen.Return(jen.Lit("false")),
		),
		jen.Return(jen.Id("_boolToString").Call(
			jen.Qual("os", "SameFile").Call(jen.Id("info1"), jen.Id("info2")),
		)),
	)
	f.Line()
}
