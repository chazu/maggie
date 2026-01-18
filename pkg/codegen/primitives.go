// Package codegen generates Go code from Trashtalk AST.
// This file contains primitive method implementations for system classes.
package codegen

import (
	"strings"

	"github.com/dave/jennifer/jen"
)

// primitiveRegistry maps (className, selector) to whether a native implementation exists.
// The actual implementation is in generatePrimitiveMethod.
var primitiveRegistry = map[string]map[string]bool{
	"File": {
		// Factory class methods
		"at_":            true,
		"temp":           true,
		"tempWithPrefix_": true,
		"mkfifo_":        true,
		// Instance methods
		"read":             true,
		"write_":           true,
		"append_":          true,
		"delete":           true,
		"exists":           true,
		"isFile":           true,
		"isDirectory":      true,
		"isFifo":           true,
		"size":             true,
		"path":             true,
		"directory":        true,
		"basename":         true,
		"extension":        true,
		"stem":             true,
		"writeLine_":       true,
		"appendLine_":      true,
		"copyTo_":          true,
		"moveTo_":          true,
		"touch":            true,
		"modificationTime": true,
		"readLines":        true,
		"printString":      true,
		"info":             true,
		// Class methods - file tests
		"exists_":         true,
		"isFile_":         true,
		"isDirectory_":    true,
		"isSymlink_":      true,
		"isFifo_":         true,
		"isSocket_":       true,
		"isBlockDevice_":  true,
		"isCharDevice_":   true,
		"isReadable_":     true,
		"isWritable_":     true,
		"isExecutable_":   true,
		"isEmpty_":        true,
		"notEmpty_":       true,
		"isNewer_than_":   true,
		"isOlder_than_":   true,
		"isSame_as_":      true,
		// Class methods - quick operations
		"read_":     true,
		"write_to_": true,
		"delete_":   true,
	},
	"Env": {
		"get_":    true,
		"set_to_": true,
		"unset_":  true,
		"has_":    true,
	},
	"Console": {
		"print_":  true,
		"write_":  true,
		"error_":  true,
		"newline": true,
	},
	"Block": {
		"params_code_captured_": true,
		"numArgs":               true,
		// value, valueWith_, valueWith_and_ require eval - bash only
	},
	"FIFO": {
		"at_":        true,
		"create":     true,
		"exists":     true,
		"remove":     true,
		"writeLine_": true,
		"readLine":   true,
		"_setPath_":  true,
		// open, close, readLineTimeout_, startWriter_, stopWriter, startReader_, stopReader
		// require background process management - bash only
	},
	"Future": {
		"for_":     true,
		"status":   true,
		"exitCode": true,
		"cleanup":  true,
		"help":     true,
		// start, await, poll, isDone, cancel require process management - bash only
	},
	"Coproc": {
		"for_":         true,
		"isRunning":    true,
		"terminate":    true,
		"kill":         true,
		"_setCommand_": true,
		"_setStatus_":  true,
		"_cleanup":     true,
		// startReadOnly, start, readLine, writeLine_ require pipes - bash only
	},
	"String": {
		// String tests
		"isEmpty_":             true,
		"notEmpty_":            true,
		"contains_substring_":  true,
		"startsWith_prefix_":   true,
		"endsWith_suffix_":     true,
		"equals_to_":           true,
		// String manipulation
		"trimPrefix_from_":      true,
		"trimSuffix_from_":      true,
		"trimShortPrefix_from_": true,
		"trimShortSuffix_from_": true,
		"replace_with_in_":      true,
		"replaceAll_with_in_":   true,
		"substring_from_length_": true,
		"length_":               true,
		"uppercase_":            true,
		"lowercase_":            true,
		// String splitting
		"split_on_":        true,
		"splitToArray_on_": true,
		"before_in_":       true,
		"after_in_":        true,
		// String building
		"concat_with_":       true,
		"concat_with_with_":  true,
		"join_values_":       true,
		"repeat_times_":      true,
		// Whitespace handling
		"trim_":      true,
		"trimLeft_":  true,
		"trimRight_": true,
	},
	"Shell": {
		// Simple execution
		"exec_":     true,
		"run_":      true,
		"silent_":   true,
		"exitCode_": true,
		"succeeds_": true,
		"fails_":    true,
		// Output capture
		"execAll_":  true,
		"execErr_":  true,
		"execFull_": true,
		// Background execution
		"spawn_":                   true,
		"spawn_outputTo_":          true,
		"spawn_stdoutTo_stderrTo_": true,
		"wait_":                    true,
		// Process control
		"isAlive_":     true,
		"signal_to_":   true,
		"terminate_":   true,
		"kill_":        true,
		"pause_":       true,
		"resume_":      true,
		// Piping and chaining
		"exec_pipeTo_":        true,
		"exec_pipeTo_pipeTo_": true,
		// Input/Output
		"exec_withInput_":     true,
		"exec_withInputFrom_": true,
		"exec_outputTo_":      true,
		"exec_appendTo_":      true,
		// Conditional execution
		"if_then_":       true,
		"unless_then_":   true,
		"exec_timeout_":  true,
		// Current shell state
		"pid":          true,
		"ppid":         true,
		"lastExitCode": true,
	},
	"Array": {
		"withValues_": true,
	},
	"Dictionary": {
		"keys":       true,
		"values":     true,
		"withPairs_": true,
		"do_":        true,
		"keysDo_":    true,
		"valuesDo_":  true,
		"collect_":   true,
		"select_":    true,
		"merge_":     true,
		"asJson":     true,
	},
	"Object": {
		// new and printString have native implementations
		// class, id, delete are handled by built-in dispatch cases
		"new":         true,
		"printString": true,
		// Dynamic dispatch (perform:) - falls back to bash, requires runtime
		"perform_":                 true,
		"perform_with_":            true,
		"perform_with_with_":       true,
		"perform_with_with_with_":  true,
	},
	"Protocol": {
		// Protocol methods require runtime introspection - bash only for now
	},
	"Time": {
		// Current time
		"now":           true,
		"nowMillis":     true,
		"nowFormatted_": true,
		"nowISO":        true,
		// Formatting
		"format_as_":      true,
		"formatISO_":      true,
		"formatRelative_": true,
		// Parsing
		"parse_format_": true,
		// Delays
		"sleep_":       true,
		"sleepMillis_": true,
		// Duration/Arithmetic
		"since_":          true,
		"from_to_":        true,
		"add_to_":         true,
		"subtract_from_":  true,
		// Components
		"yearOf_":    true,
		"monthOf_":   true,
		"dayOf_":     true,
		"hourOf_":    true,
		"minuteOf_":  true,
		"secondOf_":  true,
		"weekdayOf_": true,
		// Convenience
		"today":     true,
		"tomorrow":  true,
		"yesterday": true,
	},
	"Runtime": {
		// Instance lifecycle
		"generateId_":   true,
		"create_id_":    true,
		"delete_":       true,
		// Instance data access
		"dataFor_":      true,
		"setData_for_":  true,
		// Introspection
		"classFor_":     true,
	},
	"Store": {
		// Persistence operations
		"put_data_":       true,
		"getInstance_":    true,
		"deleteInstance_": true,
		"exists_":         true,
		"findByClass_":    true,
	},
	"Http": {
		// Core HTTP methods
		"get_":       true,
		"post_to_":   true,
		"put_to_":    true,
		"delete_":    true,
		"head_":      true,
		"status_":    true,
		"ping_":      true,
	},
	"Tool": {
		// Utility methods
		"commandExists_": true,
	},
}

// hasPrimitiveImpl checks if a native implementation exists for a primitive method.
func hasPrimitiveImpl(className, selector string) bool {
	if classMap, ok := primitiveRegistry[className]; ok {
		return classMap[selector]
	}
	return false
}

// ParityResult contains the result of validating native ↔ Bash selector parity.
type ParityResult struct {
	// OrphanedNative lists native implementations with no corresponding Bash method (error).
	OrphanedNative []string
	// MissingNative lists Bash methods without native implementations (warning).
	MissingNative []string
}

// HasErrors returns true if there are orphaned native implementations.
func (r *ParityResult) HasErrors() bool {
	return len(r.OrphanedNative) > 0
}

// ValidatePrimitiveClassParity checks that a primitive class has matching selectors
// between its Bash methods and native implementations in the primitiveRegistry.
//
// Returns:
//   - ParityResult with any mismatches found
//   - nil if the class is not in the primitiveRegistry (no validation performed)
func ValidatePrimitiveClassParity(className string, selectors []string) *ParityResult {
	registered := primitiveRegistry[className]
	if registered == nil {
		// Class not in registry - all methods will use bash fallback
		// This is expected for new classes, not an error
		return nil
	}

	// Build set of method selectors from .trash source
	bashSelectors := make(map[string]bool)
	for _, sel := range selectors {
		bashSelectors[sel] = true
	}

	result := &ParityResult{}

	// Check for native impls without corresponding bash methods (error)
	for selector := range registered {
		if !bashSelectors[selector] {
			result.OrphanedNative = append(result.OrphanedNative, selector)
		}
	}

	// Check for bash methods without native impls (warning only)
	for _, sel := range selectors {
		if !registered[sel] {
			result.MissingNative = append(result.MissingNative, sel)
		}
	}

	return result
}

// generatePrimitiveMethod generates native Go code for a primitive method.
// Returns true if the method was handled, false to fall back to default behavior.
func (g *generator) generatePrimitiveMethod(f *jen.File, m *compiledMethod) bool {
	className := g.class.Name

	switch className {
	case "File":
		return g.generatePrimitiveMethodFile(f, m)
	case "Env":
		return g.generatePrimitiveMethodEnv(f, m)
	case "Console":
		return g.generatePrimitiveMethodConsole(f, m)
	case "Block":
		return g.generatePrimitiveMethodBlock(f, m)
	case "FIFO":
		return g.generatePrimitiveMethodFIFO(f, m)
	case "Future":
		return g.generatePrimitiveMethodFuture(f, m)
	case "Coproc":
		return g.generatePrimitiveMethodCoproc(f, m)
	case "String":
		return g.generatePrimitiveMethodString(f, m)
	case "Shell":
		return g.generatePrimitiveMethodShell(f, m)
	case "Array":
		return g.generatePrimitiveMethodArray(f, m)
	case "Dictionary":
		return g.generatePrimitiveMethodDictionary(f, m)
	case "Object":
		return g.generatePrimitiveMethodObject(f, m)
	case "Protocol":
		return g.generatePrimitiveMethodProtocol(f, m)
	case "Time":
		return g.generatePrimitiveMethodTime(f, m)
	case "Runtime":
		return g.generatePrimitiveMethodRuntime(f, m)
	case "Store":
		return g.generatePrimitiveMethodStore(f, m)
	case "Http":
		return g.generatePrimitiveMethodHttp(f, m)
	case "Tool":
		return g.generatePrimitiveMethodTool(f, m)
	default:
		return false
	}
}

// generatePrimitiveMethodEnv generates native Env class methods.
func (g *generator) generatePrimitiveMethodEnv(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "get_":
		f.Func().Id(m.goName).Params(jen.Id("name").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("os", "Getenv").Call(jen.Id("name")), jen.Nil()),
		)
		f.Line()
		return true

	case "set_to_":
		f.Func().Id(m.goName).Params(
			jen.Id("name").String(),
			jen.Id("value").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Err().Op(":=").Qual("os", "Setenv").Call(jen.Id("name"), jen.Id("value")),
			jen.Return(jen.Lit(""), jen.Err()),
		)
		f.Line()
		return true

	case "unset_":
		f.Func().Id(m.goName).Params(jen.Id("name").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Err().Op(":=").Qual("os", "Unsetenv").Call(jen.Id("name")),
			jen.Return(jen.Lit(""), jen.Err()),
		)
		f.Line()
		return true

	case "has_":
		f.Func().Id(m.goName).Params(jen.Id("name").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("_"), jen.Id("exists")).Op(":=").Qual("os", "LookupEnv").Call(jen.Id("name")),
			jen.If(jen.Id("exists")).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodConsole generates native Console class methods.
func (g *generator) generatePrimitiveMethodConsole(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "print_":
		// Print message to stdout with newline
		f.Func().Id(m.goName).Params(jen.Id("message").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("fmt", "Println").Call(jen.Id("message")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "write_":
		// Print message to stdout without newline
		f.Func().Id(m.goName).Params(jen.Id("message").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("fmt", "Print").Call(jen.Id("message")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "error_":
		// Print message to stderr with newline
		f.Func().Id(m.goName).Params(jen.Id("message").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("fmt", "Fprintln").Call(jen.Qual("os", "Stderr"), jen.Id("message")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "newline":
		// Print a blank line
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("fmt", "Println").Call(),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodBlock generates native Block class methods.
// Block represents closures in Trashtalk. In native Go, these become actual Go closures.
func (g *generator) generatePrimitiveMethodBlock(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "params_code_captured_":
		// Factory: Create a Block with params, code, and captured variables
		// In native Go, blocks are compiled inline as closures, so this is primarily
		// for runtime compatibility. The code is stored as a string but would need
		// an interpreter to execute dynamically.
		f.Func().Id(m.goName).Params(
			jen.Id("params").String(),
			jen.Id("code").String(),
			jen.Id("captured").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Generate instance ID"),
			jen.Id("id").Op(":=").Lit("block_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.Comment("Create instance in database"),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id("Block").Values(jen.Dict{
				jen.Id("Class"):     jen.Lit("Block"),
				jen.Id("CreatedAt"): jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Id("Params"):    jen.Id("params"),
				jen.Id("Code"):      jen.Id("code"),
				jen.Id("Captured"):  jen.Id("captured"),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	case "numArgs":
		// Return the number of parameters
		f.Func().Parens(jen.Id("c").Op("*").Id("Block")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Parse params JSON array and return length"),
			jen.Var().Id("params").Index().String(),
			jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("c").Dot("Params")),
				jen.Op("&").Id("params"),
			), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("0"), jen.Nil()),
			),
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Len(jen.Id("params"))), jen.Nil()),
		)
		f.Line()
		return true

	case "value", "valueWith_", "valueWith_and_":
		// Block execution requires eval-like functionality which isn't available in native Go
		// These would need to fall back to bash or use a different approach
		// For now, return an error indicating dynamic execution isn't supported
		return false

	default:
		return false
	}
}

// generatePrimitiveMethodFIFO generates native FIFO (named pipe) class methods.
func (g *generator) generatePrimitiveMethodFIFO(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "at_":
		// Factory: Create a FIFO instance for a given path
		f.Func().Id(m.goName).Params(jen.Id("path").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("id").Op(":=").Lit("fifo_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id("FIFO").Values(jen.Dict{
				jen.Id("Class"):     jen.Lit("FIFO"),
				jen.Id("CreatedAt"): jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Id("Path"):      jen.Id("path"),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	case "create":
		// Create the named pipe on disk
		f.Func().Parens(jen.Id("c").Op("*").Id("FIFO")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("os", "Remove").Call(jen.Id("c").Dot("Path")),
			jen.Err().Op(":=").Qual("syscall", "Mkfifo").Call(jen.Id("c").Dot("Path"), jen.Lit(0644)),
			jen.Return(jen.Lit(""), jen.Err()),
		)
		f.Line()
		return true

	case "exists":
		// Check if the pipe exists
		f.Func().Parens(jen.Id("c").Op("*").Id("FIFO")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("info"), jen.Err()).Op(":=").Qual("os", "Stat").Call(jen.Id("c").Dot("Path")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.If(jen.Id("info").Dot("Mode").Call().Op("&").Qual("os", "ModeNamedPipe").Op("!=").Lit(0)).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "remove":
		// Remove the pipe from disk
		f.Func().Parens(jen.Id("c").Op("*").Id("FIFO")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("os", "Remove").Call(jen.Id("c").Dot("Path")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "writeLine_":
		// Write a line to the FIFO
		f.Func().Parens(jen.Id("c").Op("*").Id("FIFO")).Id(m.goName).Params(
			jen.Id("text").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("file"), jen.Err()).Op(":=").Qual("os", "OpenFile").Call(
				jen.Id("c").Dot("Path"),
				jen.Qual("os", "O_WRONLY"),
				jen.Lit(0),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("file").Dot("Close").Call(),
			jen.List(jen.Id("_"), jen.Err()).Op("=").Id("file").Dot("WriteString").Call(jen.Id("text").Op("+").Lit("\n")),
			jen.Return(jen.Lit(""), jen.Err()),
		)
		f.Line()
		return true

	case "readLine":
		// Read a line from the FIFO
		f.Func().Parens(jen.Id("c").Op("*").Id("FIFO")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("file"), jen.Err()).Op(":=").Qual("os", "Open").Call(jen.Id("c").Dot("Path")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("file").Dot("Close").Call(),
			jen.Line(),
			jen.Id("reader").Op(":=").Qual("bufio", "NewReader").Call(jen.Id("file")),
			jen.List(jen.Id("line"), jen.Err()).Op(":=").Id("reader").Dot("ReadString").Call(jen.Lit('\n')),
			jen.If(jen.Err().Op("!=").Nil().Op("&&").Err().Op("!=").Qual("io", "EOF")).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Qual("strings", "TrimSuffix").Call(jen.Id("line"), jen.Lit("\n")), jen.Nil()),
		)
		f.Line()
		return true

	case "_setPath_":
		// Private setter for path
		f.Func().Parens(jen.Id("c").Op("*").Id("FIFO")).Id(m.goName).Params(
			jen.Id("path").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("c").Dot("Path").Op("=").Id("path"),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "open", "close", "readLineTimeout_", "startWriter_", "stopWriter", "startReader_", "stopReader":
		// These require background process management which is complex in native Go
		// Fall back to bash for now
		return false

	default:
		return false
	}
}

// generatePrimitiveMethodFuture generates native Future class methods.
func (g *generator) generatePrimitiveMethodFuture(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "for_":
		// Factory: Create a Future for a command
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("id").Op(":=").Lit("future_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.Comment("Set up result file path"),
			jen.Id("resultDir").Op(":=").Lit("/tmp/trashtalk/futures"),
			jen.Qual("os", "MkdirAll").Call(jen.Id("resultDir"), jen.Lit(0755)),
			jen.Id("resultFile").Op(":=").Id("resultDir").Op("+").Lit("/").Op("+").Id("id"),
			jen.Line(),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id("Future").Values(jen.Dict{
				jen.Id("Class"):      jen.Lit("Future"),
				jen.Id("CreatedAt"):  jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Id("Command"):    jen.Id("command"),
				jen.Id("ResultFile"): jen.Id("resultFile"),
				jen.Id("Status"):     jen.Lit("created"),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	case "status":
		// Get current status
		f.Func().Parens(jen.Id("c").Op("*").Id("Future")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("Status"), jen.Nil()),
		)
		f.Line()
		return true

	case "exitCode":
		// Get exit code
		f.Func().Parens(jen.Id("c").Op("*").Id("Future")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("ExitCode"), jen.Nil()),
		)
		f.Line()
		return true

	case "cleanup":
		// Clean up result files
		f.Func().Parens(jen.Id("c").Op("*").Id("Future")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Qual("os", "Remove").Call(jen.Id("c").Dot("ResultFile")),
			jen.Qual("os", "Remove").Call(jen.Id("c").Dot("ResultFile").Op("+").Lit(".exit")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "help":
		// Show help text
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("help").Op(":=").Lit("=== Future - Async Computation ===\n\nUsage:\n  future := @ Future for: 'command'\n  @ future start\n  result := @ future await\n"),
			jen.Return(jen.Id("help"), jen.Nil()),
		)
		f.Line()
		return true

	case "start", "await", "poll", "isDone", "cancel":
		// These require process management with os/exec
		// Complex to implement correctly - fall back to bash
		return false

	default:
		return false
	}
}

// generatePrimitiveMethodCoproc generates native Coproc class methods.
func (g *generator) generatePrimitiveMethodCoproc(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "for_":
		// Factory: Create a Coproc for a command
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("id").Op(":=").Lit("coproc_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id("Coproc").Values(jen.Dict{
				jen.Id("Class"):     jen.Lit("Coproc"),
				jen.Id("CreatedAt"): jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Id("Command"):   jen.Id("command"),
				jen.Id("Status"):    jen.Lit("created"),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	case "isRunning":
		// Check if process is running using stored PID
		f.Func().Parens(jen.Id("c").Op("*").Id("Coproc")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Id("c").Dot("Pid").Op("==").Lit("")).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Line(),
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("c").Dot("Pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Line(),
			jen.List(jen.Id("process"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Line(),
			jen.Comment("On Unix, FindProcess always succeeds. Use Signal(0) to check."),
			jen.Err().Op("=").Id("process").Dot("Signal").Call(jen.Qual("syscall", "Signal").Call(jen.Lit(0))),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Return(jen.Lit("true"), jen.Nil()),
		)
		f.Line()
		return true

	case "terminate":
		// Send SIGTERM to the process
		f.Func().Parens(jen.Id("c").Op("*").Id("Coproc")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Id("c").Dot("Pid").Op("==").Lit("")).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Line(),
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("c").Dot("Pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Line(),
			jen.List(jen.Id("process"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("process").Dot("Signal").Call(jen.Qual("syscall", "SIGTERM")),
			jen.Id("c").Dot("Status").Op("=").Lit("terminated"),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "kill":
		// Send SIGKILL to the process
		f.Func().Parens(jen.Id("c").Op("*").Id("Coproc")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Id("c").Dot("Pid").Op("==").Lit("")).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Line(),
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("c").Dot("Pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Line(),
			jen.List(jen.Id("process"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("process").Dot("Kill").Call(),
			jen.Id("c").Dot("Status").Op("=").Lit("killed"),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "_setCommand_":
		f.Func().Parens(jen.Id("c").Op("*").Id("Coproc")).Id(m.goName).Params(
			jen.Id("cmd").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("c").Dot("Command").Op("=").Id("cmd"),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "_setStatus_":
		f.Func().Parens(jen.Id("c").Op("*").Id("Coproc")).Id(m.goName).Params(
			jen.Id("status").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("c").Dot("Status").Op("=").Id("status"),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "_cleanup":
		// Clean up FIFOs
		f.Func().Parens(jen.Id("c").Op("*").Id("Coproc")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Id("c").Dot("FifoIn").Op("!=").Lit("")).Block(
				jen.Qual("os", "Remove").Call(jen.Id("c").Dot("FifoIn")),
			),
			jen.If(jen.Id("c").Dot("FifoOut").Op("!=").Lit("")).Block(
				jen.Qual("os", "Remove").Call(jen.Id("c").Dot("FifoOut")),
			),
			jen.Id("c").Dot("FifoIn").Op("=").Lit(""),
			jen.Id("c").Dot("FifoOut").Op("=").Lit(""),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "startReadOnly", "start", "readLine", "writeLine_", "readLinesDo_":
		// These require complex process management with exec.Cmd and pipes
		// Fall back to bash for now
		return false

	default:
		return false
	}
}

// generatePrimitiveMethodArray generates native Array class methods.
func (g *generator) generatePrimitiveMethodArray(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "withValues_":
		// Class method: Create a new Array with space-separated values
		f.Func().Id(m.goName).Params(
			jen.Id("values").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Generate instance ID"),
			jen.Id("id").Op(":=").Lit("array_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.Comment("Split values by whitespace and build JSON array"),
			jen.Id("parts").Op(":=").Qual("strings", "Fields").Call(jen.Id("values")),
			jen.Id("jsonArray").Op(":=").Make(jen.Index().Interface(), jen.Len(jen.Id("parts"))),
			jen.For(jen.List(jen.Id("i"), jen.Id("v")).Op(":=").Range().Id("parts")).Block(
				jen.Id("jsonArray").Index(jen.Id("i")).Op("=").Id("v"),
			),
			jen.List(jen.Id("data"), jen.Err()).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("jsonArray")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Line(),
			jen.Comment("Save to database"),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id("Array").Values(jen.Dict{
				jen.Id("Class"):     jen.Lit("Array"),
				jen.Id("CreatedAt"): jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Id("Items"):     jen.Qual("encoding/json", "RawMessage").Call(jen.Id("data")),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodDictionary generates native Dictionary class methods.
func (g *generator) generatePrimitiveMethodDictionary(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "keys":
		// Return keys of dictionary as newline-separated string
		f.Func().Id(m.goName).Params(
			jen.Id("receiver").String(),
			jen.Id("items").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Var().Id("data").Map(jen.String()).Interface(),
			jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("items")),
				jen.Op("&").Id("data"),
			), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("keys").Op(":=").Make(jen.Index().String(), jen.Lit(0), jen.Len(jen.Id("data"))),
			jen.For(jen.Id("k").Op(":=").Range().Id("data")).Block(
				jen.Id("keys").Op("=").Append(jen.Id("keys"), jen.Id("k")),
			),
			jen.Qual("sort", "Strings").Call(jen.Id("keys")),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("keys"), jen.Lit("\n")), jen.Nil()),
		)
		f.Line()
		return true

	case "values":
		// Return values of dictionary as newline-separated string
		f.Func().Id(m.goName).Params(
			jen.Id("receiver").String(),
			jen.Id("items").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Var().Id("data").Map(jen.String()).Interface(),
			jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("items")),
				jen.Op("&").Id("data"),
			), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("vals").Op(":=").Make(jen.Index().String(), jen.Lit(0), jen.Len(jen.Id("data"))),
			jen.For(jen.List(jen.Id("_"), jen.Id("v")).Op(":=").Range().Id("data")).Block(
				jen.Id("vals").Op("=").Append(jen.Id("vals"), jen.Qual("fmt", "Sprintf").Call(jen.Lit("%v"), jen.Id("v"))),
			),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("vals"), jen.Lit("\n")), jen.Nil()),
		)
		f.Line()
		return true

	case "asJson":
		// Return compact JSON representation
		f.Func().Id(m.goName).Params(
			jen.Id("receiver").String(),
			jen.Id("items").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Compact JSON by parsing and re-marshaling"),
			jen.Var().Id("data").Interface(),
			jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("items")),
				jen.Op("&").Id("data"),
			), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.List(jen.Id("compact"), jen.Err()).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("data")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.String().Parens(jen.Id("compact")), jen.Nil()),
		)
		f.Line()
		return true

	case "withPairs_":
		// Class method: Create a new Dictionary from "key:value key2:value2" pairs
		f.Func().Id(m.goName).Params(
			jen.Id("pairs").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Generate instance ID"),
			jen.Id("id").Op(":=").Lit("dict_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.Comment("Build dictionary from pairs"),
			jen.Id("data").Op(":=").Make(jen.Map(jen.String()).String()),
			jen.For(jen.List(jen.Id("_"), jen.Id("pair")).Op(":=").Range().Qual("strings", "Fields").Call(jen.Id("pairs"))).Block(
				jen.Id("parts").Op(":=").Qual("strings", "SplitN").Call(jen.Id("pair"), jen.Lit(":"), jen.Lit(2)),
				jen.If(jen.Len(jen.Id("parts")).Op("==").Lit(2)).Block(
					jen.Id("data").Index(jen.Id("parts").Index(jen.Lit(0))).Op("=").Id("parts").Index(jen.Lit(1)),
				),
			),
			jen.List(jen.Id("jsonBytes"), jen.Err()).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("data")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Line(),
			jen.Comment("Save to database"),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id("Dictionary").Values(jen.Dict{
				jen.Id("Class"):     jen.Lit("Dictionary"),
				jen.Id("CreatedAt"): jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Id("Items"):     jen.Qual("encoding/json", "RawMessage").Call(jen.Id("jsonBytes")),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	case "merge_":
		// Instance method: Merge another dictionary into this one
		f.Func().Parens(jen.Id("c").Op("*").Id("Dictionary")).Id(m.goName).Params(
			jen.Id("otherJson").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Parse current items"),
			jen.Var().Id("data").Map(jen.String()).Interface(),
			jen.If(jen.Len(jen.Id("c").Dot("Items")).Op(">").Lit(0)).Block(
				jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
					jen.Id("c").Dot("Items"),
					jen.Op("&").Id("data"),
				), jen.Err().Op("!=").Nil()).Block(
					jen.Return(jen.Lit(""), jen.Err()),
				),
			).Else().Block(
				jen.Id("data").Op("=").Make(jen.Map(jen.String()).Interface()),
			),
			jen.Line(),
			jen.Comment("Parse other dictionary"),
			jen.Var().Id("other").Map(jen.String()).Interface(),
			jen.If(jen.Err().Op(":=").Qual("encoding/json", "Unmarshal").Call(
				jen.Index().Byte().Parens(jen.Id("otherJson")),
				jen.Op("&").Id("other"),
			), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Line(),
			jen.Comment("Merge"),
			jen.For(jen.List(jen.Id("k"), jen.Id("v")).Op(":=").Range().Id("other")).Block(
				jen.Id("data").Index(jen.Id("k")).Op("=").Id("v"),
			),
			jen.Line(),
			jen.Comment("Marshal merged result back to Items"),
			jen.List(jen.Id("merged"), jen.Err()).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("data")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("c").Dot("Items").Op("=").Id("merged"),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "do_", "keysDo_", "valuesDo_", "collect_", "select_":
		// Block iteration methods - complex, fall back to bash for now
		return false

	default:
		return false
	}
}

// generatePrimitiveMethodObject generates native Object class methods.
// Object is the base class - these methods work for any class through inheritance.
func (g *generator) generatePrimitiveMethodObject(f *jen.File, m *compiledMethod) bool {
	className := g.class.Name

	switch m.selector {
	case "new":
		// Class method: create a new instance
		// This is typically inherited by subclasses
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Generate instance ID"),
			jen.Id("id").Op(":=").Lit(strings.ToLower(className)+"_").Op("+").Qual("strings", "ReplaceAll").Call(
				jen.Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
				jen.Lit("-"),
				jen.Lit(""),
			),
			jen.Line(),
			jen.Comment("Create instance in database"),
			jen.List(jen.Id("db"), jen.Err()).Op(":=").Id("openDB").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Id("instance").Op(":=").Op("&").Id(className).Values(jen.Dict{
				jen.Id("Class"):     jen.Lit(className),
				jen.Id("CreatedAt"): jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
			}),
			jen.Line(),
			jen.If(jen.Err().Op(":=").Id("saveInstance").Call(jen.Id("db"), jen.Id("id"), jen.Id("instance")), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("id"), jen.Nil()),
		)
		f.Line()
		return true

	case "printString":
		// Instance method: Return "<ClassName instanceId>"
		f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Get instance ID from database lookup context"),
			jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("<%s instance>"), jen.Lit(className)), jen.Nil()),
		)
		f.Line()
		return true

	case "class":
		// Handled by built-in dispatch case - no method needed
		return false

	case "id":
		// Handled by built-in dispatch case - no method needed
		return false

	case "isKindOf_":
		// Check class hierarchy - requires runtime support
		// Fall back to bash for now
		return false

	case "conformsTo_":
		// Check protocol conformance - requires runtime support
		// Fall back to bash for now
		return false

	case "delete":
		// Instance method: delete this instance from database
		return false

	case "inspect":
		// Detailed inspection - requires runtime data access
		// Fall back to bash for now
		return false

	case "inspectTo_":
		// Inspect to specific depth
		return false

	case "perform_":
		// Dynamic dispatch: call method by selector name
		// Uses the generated dispatch() function for this class
		f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(m.goName).Params(
			jen.Id("selector").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("dispatch").Call(jen.Id("c"), jen.Id("selector"), jen.Nil())),
		)
		f.Line()
		return true

	case "perform_with_":
		// Dynamic dispatch with one argument
		f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(m.goName).Params(
			jen.Id("selector").String(),
			jen.Id("arg1").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("dispatch").Call(
				jen.Id("c"),
				jen.Id("selector"),
				jen.Index().String().Values(jen.Id("arg1")),
			)),
		)
		f.Line()
		return true

	case "perform_with_with_":
		// Dynamic dispatch with two arguments
		f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(m.goName).Params(
			jen.Id("selector").String(),
			jen.Id("arg1").String(),
			jen.Id("arg2").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("dispatch").Call(
				jen.Id("c"),
				jen.Id("selector"),
				jen.Index().String().Values(jen.Id("arg1"), jen.Id("arg2")),
			)),
		)
		f.Line()
		return true

	case "perform_with_with_with_":
		// Dynamic dispatch with three arguments
		f.Func().Parens(jen.Id("c").Op("*").Id(className)).Id(m.goName).Params(
			jen.Id("selector").String(),
			jen.Id("arg1").String(),
			jen.Id("arg2").String(),
			jen.Id("arg3").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("dispatch").Call(
				jen.Id("c"),
				jen.Id("selector"),
				jen.Index().String().Values(jen.Id("arg1"), jen.Id("arg2"), jen.Id("arg3")),
			)),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodProtocol generates native Protocol class methods.
func (g *generator) generatePrimitiveMethodProtocol(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "requiredMethods":
		// Requires runtime metadata access - fall back to bash
		return false

	case "isSatisfiedBy_":
		// Requires runtime method introspection - fall back to bash
		return false

	default:
		return false
	}
}

// generatePrimitiveMethodTime generates native Time class methods.
func (g *generator) generatePrimitiveMethodTime(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	// ==========================================
	// Current Time
	// ==========================================

	case "now":
		// Get current Unix timestamp (seconds since epoch)
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(
					jen.Qual("time", "Now").Call().Dot("Unix").Call(),
					jen.Lit(10),
				),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "nowMillis":
		// Get current time in milliseconds
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(
					jen.Qual("time", "Now").Call().Dot("UnixMilli").Call(),
					jen.Lit(10),
				),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "nowFormatted_":
		// Get current time formatted with strftime-like pattern
		f.Func().Id(m.goName).Params(jen.Id("format").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Convert strftime format to Go format"),
			jen.Id("goFormat").Op(":=").Qual("strings", "NewReplacer").Call(
				jen.Lit("%Y"), jen.Lit("2006"),
				jen.Lit("%m"), jen.Lit("01"),
				jen.Lit("%d"), jen.Lit("02"),
				jen.Lit("%H"), jen.Lit("15"),
				jen.Lit("%M"), jen.Lit("04"),
				jen.Lit("%S"), jen.Lit("05"),
				jen.Lit("%b"), jen.Lit("Jan"),
				jen.Lit("%B"), jen.Lit("January"),
				jen.Lit("%a"), jen.Lit("Mon"),
				jen.Lit("%A"), jen.Lit("Monday"),
				jen.Lit("%p"), jen.Lit("PM"),
				jen.Lit("%Z"), jen.Lit("MST"),
				jen.Lit("%z"), jen.Lit("-0700"),
				jen.Lit("%%"), jen.Lit("%"),
			).Dot("Replace").Call(jen.Id("format")),
			jen.Return(
				jen.Qual("time", "Now").Call().Dot("Format").Call(jen.Id("goFormat")),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "nowISO":
		// Get ISO 8601 formatted current time
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(
				jen.Qual("time", "Now").Call().Dot("UTC").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	// ==========================================
	// Formatting
	// ==========================================

	case "format_as_":
		// Format a Unix timestamp
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String(), jen.Id("format").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Comment("Convert strftime format to Go format"),
			jen.Id("goFormat").Op(":=").Qual("strings", "NewReplacer").Call(
				jen.Lit("%Y"), jen.Lit("2006"),
				jen.Lit("%m"), jen.Lit("01"),
				jen.Lit("%d"), jen.Lit("02"),
				jen.Lit("%H"), jen.Lit("15"),
				jen.Lit("%M"), jen.Lit("04"),
				jen.Lit("%S"), jen.Lit("05"),
				jen.Lit("%b"), jen.Lit("Jan"),
				jen.Lit("%B"), jen.Lit("January"),
				jen.Lit("%a"), jen.Lit("Mon"),
				jen.Lit("%A"), jen.Lit("Monday"),
				jen.Lit("%p"), jen.Lit("PM"),
				jen.Lit("%Z"), jen.Lit("MST"),
				jen.Lit("%z"), jen.Lit("-0700"),
				jen.Lit("%%"), jen.Lit("%"),
			).Dot("Replace").Call(jen.Id("format")),
			jen.Return(
				jen.Id("t").Dot("Format").Call(jen.Id("goFormat")),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "formatISO_":
		// Format timestamp as ISO 8601
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Id("t").Dot("UTC").Call().Dot("Format").Call(jen.Qual("time", "RFC3339")),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "formatRelative_":
		// Format timestamp as human-readable relative time
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("diff").Op(":=").Qual("time", "Now").Call().Dot("Unix").Call().Op("-").Id("ts"),
			jen.If(jen.Id("diff").Op("<").Lit(60)).Block(
				jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d seconds ago"), jen.Id("diff")), jen.Nil()),
			).Else().If(jen.Id("diff").Op("<").Lit(3600)).Block(
				jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d minutes ago"), jen.Id("diff").Op("/").Lit(60)), jen.Nil()),
			).Else().If(jen.Id("diff").Op("<").Lit(86400)).Block(
				jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d hours ago"), jen.Id("diff").Op("/").Lit(3600)), jen.Nil()),
			).Else().Block(
				jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d days ago"), jen.Id("diff").Op("/").Lit(86400)), jen.Nil()),
			),
		)
		f.Line()
		return true

	// ==========================================
	// Parsing
	// ==========================================

	case "parse_format_":
		// Parse a date string to Unix timestamp
		f.Func().Id(m.goName).Params(jen.Id("dateString").String(), jen.Id("format").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Convert strftime format to Go format"),
			jen.Id("goFormat").Op(":=").Qual("strings", "NewReplacer").Call(
				jen.Lit("%Y"), jen.Lit("2006"),
				jen.Lit("%m"), jen.Lit("01"),
				jen.Lit("%d"), jen.Lit("02"),
				jen.Lit("%H"), jen.Lit("15"),
				jen.Lit("%M"), jen.Lit("04"),
				jen.Lit("%S"), jen.Lit("05"),
				jen.Lit("%b"), jen.Lit("Jan"),
				jen.Lit("%B"), jen.Lit("January"),
				jen.Lit("%a"), jen.Lit("Mon"),
				jen.Lit("%A"), jen.Lit("Monday"),
				jen.Lit("%p"), jen.Lit("PM"),
				jen.Lit("%Z"), jen.Lit("MST"),
				jen.Lit("%z"), jen.Lit("-0700"),
				jen.Lit("%%"), jen.Lit("%"),
			).Dot("Replace").Call(jen.Id("format")),
			jen.List(jen.Id("t"), jen.Id("err")).Op(":=").Qual("time", "Parse").Call(
				jen.Id("goFormat"),
				jen.Id("dateString"),
			),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()), // Return empty string on parse failure (matches bash behavior)
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("t").Dot("Unix").Call(), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	// ==========================================
	// Delays
	// ==========================================

	case "sleep_":
		// Sleep for specified seconds (supports decimals)
		f.Func().Id(m.goName).Params(jen.Id("seconds").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("secs"), jen.Id("err")).Op(":=").Qual("strconv", "ParseFloat").Call(jen.Id("seconds"), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Qual("time", "Sleep").Call(
				jen.Qual("time", "Duration").Call(
					jen.Id("secs").Op("*").Float64().Call(jen.Qual("time", "Second")),
				),
			),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "sleepMillis_":
		// Sleep for specified milliseconds
		f.Func().Id(m.goName).Params(jen.Id("millis").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ms"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("millis"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Qual("time", "Sleep").Call(
				jen.Qual("time", "Duration").Call(jen.Id("ms")).Op("*").Qual("time", "Millisecond"),
			),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	// ==========================================
	// Duration/Arithmetic
	// ==========================================

	case "since_":
		// Calculate duration since a timestamp
		f.Func().Id(m.goName).Params(jen.Id("startTime").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("start"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("startTime"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("diff").Op(":=").Qual("time", "Now").Call().Dot("Unix").Call().Op("-").Id("start"),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("diff"), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "from_to_":
		// Calculate duration between two timestamps
		f.Func().Id(m.goName).Params(jen.Id("startTime").String(), jen.Id("endTime").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("start"), jen.Id("err1")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("startTime"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err1").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err1")),
			),
			jen.List(jen.Id("end"), jen.Id("err2")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("endTime"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err2").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err2")),
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("end").Op("-").Id("start"), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "add_to_":
		// Add seconds to a timestamp
		f.Func().Id(m.goName).Params(jen.Id("seconds").String(), jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("secs"), jen.Id("err1")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("seconds"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err1").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err1")),
			),
			jen.List(jen.Id("ts"), jen.Id("err2")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err2").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err2")),
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("ts").Op("+").Id("secs"), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "subtract_from_":
		// Subtract seconds from a timestamp
		f.Func().Id(m.goName).Params(jen.Id("seconds").String(), jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("secs"), jen.Id("err1")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("seconds"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err1").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err1")),
			),
			jen.List(jen.Id("ts"), jen.Id("err2")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err2").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err2")),
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("ts").Op("-").Id("secs"), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	// ==========================================
	// Components
	// ==========================================

	case "yearOf_":
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Id("t").Dot("Year").Call()),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "monthOf_":
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Int().Call(jen.Id("t").Dot("Month").Call())),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "dayOf_":
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Id("t").Dot("Day").Call()),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "hourOf_":
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Id("t").Dot("Hour").Call()),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "minuteOf_":
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Id("t").Dot("Minute").Call()),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "secondOf_":
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Id("t").Dot("Second").Call()),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "weekdayOf_":
		// Returns 0=Sunday, 6=Saturday (matches bash date +%w)
		f.Func().Id(m.goName).Params(jen.Id("timestamp").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("ts"), jen.Id("err")).Op(":=").Qual("strconv", "ParseInt").Call(jen.Id("timestamp"), jen.Lit(10), jen.Lit(64)),
			jen.If(jen.Id("err").Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Id("err")),
			),
			jen.Id("t").Op(":=").Qual("time", "Unix").Call(jen.Id("ts"), jen.Lit(0)),
			jen.Return(
				jen.Qual("strconv", "Itoa").Call(jen.Int().Call(jen.Id("t").Dot("Weekday").Call())),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	// ==========================================
	// Convenience
	// ==========================================

	case "today":
		// Get timestamp for start of today
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("now").Op(":=").Qual("time", "Now").Call(),
			jen.Id("today").Op(":=").Qual("time", "Date").Call(
				jen.Id("now").Dot("Year").Call(),
				jen.Id("now").Dot("Month").Call(),
				jen.Id("now").Dot("Day").Call(),
				jen.Lit(0), jen.Lit(0), jen.Lit(0), jen.Lit(0),
				jen.Id("now").Dot("Location").Call(),
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("today").Dot("Unix").Call(), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "tomorrow":
		// Get timestamp for start of tomorrow
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("now").Op(":=").Qual("time", "Now").Call(),
			jen.Id("tomorrow").Op(":=").Qual("time", "Date").Call(
				jen.Id("now").Dot("Year").Call(),
				jen.Id("now").Dot("Month").Call(),
				jen.Id("now").Dot("Day").Call().Op("+").Lit(1),
				jen.Lit(0), jen.Lit(0), jen.Lit(0), jen.Lit(0),
				jen.Id("now").Dot("Location").Call(),
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("tomorrow").Dot("Unix").Call(), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	case "yesterday":
		// Get timestamp for start of yesterday
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("now").Op(":=").Qual("time", "Now").Call(),
			jen.Id("yesterday").Op(":=").Qual("time", "Date").Call(
				jen.Id("now").Dot("Year").Call(),
				jen.Id("now").Dot("Month").Call(),
				jen.Id("now").Dot("Day").Call().Op("-").Lit(1),
				jen.Lit(0), jen.Lit(0), jen.Lit(0), jen.Lit(0),
				jen.Id("now").Dot("Location").Call(),
			),
			jen.Return(
				jen.Qual("strconv", "FormatInt").Call(jen.Id("yesterday").Dot("Unix").Call(), jen.Lit(10)),
				jen.Nil(),
			),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodRuntime generates native Runtime class methods.
func (g *generator) generatePrimitiveMethodRuntime(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "generateId_":
		// Generate a unique instance ID: classname_uuid
		f.Func().Id(m.goName).Params(jen.Id("className").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("lower").Op(":=").Qual("strings", "ToLower").Call(jen.Id("className")),
			jen.Id("uuid").Op(":=").Qual("github.com/google/uuid", "New").Call().Dot("String").Call(),
			jen.Comment("Use first 8 chars of UUID for readability"),
			jen.If(jen.Len(jen.Id("uuid")).Op(">").Lit(8)).Block(
				jen.Id("uuid").Op("=").Id("uuid").Index(jen.Empty(), jen.Lit(8)),
			),
			jen.Return(jen.Id("lower").Op("+").Lit("_").Op("+").Id("uuid"), jen.Nil()),
		)
		f.Line()
		return true

	case "create_id_":
		// Create a new instance with given ID in the store
		f.Func().Id(m.goName).Params(
			jen.Id("className").String(),
			jen.Id("instanceId").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Initialize with empty JSON object containing class"),
			jen.Id("initialData").Op(":=").Qual("fmt", "Sprintf").Call(
				jen.Lit(`{"class":"%s"}`),
				jen.Id("className"),
			),
			jen.List(jen.Id("_"), jen.Err()).Op(":=").Id("TT_SendMessage").Call(
				jen.Lit("Store"),
				jen.Lit("put_data_"),
				jen.Id("instanceId"),
				jen.Id("initialData"),
			),
			jen.Return(jen.Id("instanceId"), jen.Err()),
		)
		f.Line()
		return true

	case "delete_":
		// Delete an instance
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("TT_SendMessage").Call(
				jen.Lit("Store"),
				jen.Lit("deleteInstance_"),
				jen.Id("instanceId"),
			)),
		)
		f.Line()
		return true

	case "dataFor_":
		// Get instance data as JSON
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("TT_SendMessage").Call(
				jen.Lit("Store"),
				jen.Lit("getInstance_"),
				jen.Id("instanceId"),
			)),
		)
		f.Line()
		return true

	case "setData_for_":
		// Set instance data from JSON
		f.Func().Id(m.goName).Params(
			jen.Id("json").String(),
			jen.Id("instanceId").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("TT_SendMessage").Call(
				jen.Lit("Store"),
				jen.Lit("put_data_"),
				jen.Id("instanceId"),
				jen.Id("json"),
			)),
		)
		f.Line()
		return true

	case "classFor_":
		// Get class name from instance ID (extract from ID prefix)
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Instance IDs are formatted as classname_uuid"),
			jen.Id("parts").Op(":=").Qual("strings", "Split").Call(jen.Id("instanceId"), jen.Lit("_")),
			jen.If(jen.Len(jen.Id("parts")).Op("==").Lit(0)).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("invalid instance ID: %s"), jen.Id("instanceId"))),
			),
			jen.Comment("Convert lowercase prefix back to class name (first char uppercase)"),
			jen.Id("className").Op(":=").Qual("strings", "Title").Call(jen.Id("parts").Index(jen.Lit(0))),
			jen.Return(jen.Id("className"), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodStore generates native Store class methods.
func (g *generator) generatePrimitiveMethodStore(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "put_data_":
		// Store instance data in SQLite
		stmts := append(dbPathStatements(),
			jen.Id("db").Op(",").Err().Op(":=").Qual("database/sql", "Open").Call(
				jen.Lit("sqlite3"),
				jen.Id("dbPath"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Comment("Upsert the instance data"),
			jen.List(jen.Id("_"), jen.Err()).Op("=").Id("db").Dot("Exec").Call(
				jen.Lit("INSERT INTO instances (id, data) VALUES (?, ?) ON CONFLICT(id) DO UPDATE SET data = ?"),
				jen.Id("instanceId"),
				jen.Id("data"),
				jen.Id("data"),
			),
			jen.Return(jen.Lit(""), jen.Err()),
		)
		f.Func().Id(m.goName).Params(
			jen.Id("instanceId").String(),
			jen.Id("data").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(stmts...)
		f.Line()
		return true

	case "getInstance_":
		// Get instance data from SQLite
		stmts := append(dbPathStatements(),
			jen.Id("db").Op(",").Err().Op(":=").Qual("database/sql", "Open").Call(
				jen.Lit("sqlite3"),
				jen.Id("dbPath"),
			),
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
			jen.If(jen.Err().Op("==").Qual("database/sql", "ErrNoRows")).Block(
				jen.Return(jen.Lit("{}"), jen.Nil()),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Id("data"), jen.Nil()),
		)
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(stmts...)
		f.Line()
		return true

	case "deleteInstance_":
		// Delete instance from SQLite
		stmts := append(dbPathStatements(),
			jen.Id("db").Op(",").Err().Op(":=").Qual("database/sql", "Open").Call(
				jen.Lit("sqlite3"),
				jen.Id("dbPath"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.List(jen.Id("_"), jen.Err()).Op("=").Id("db").Dot("Exec").Call(
				jen.Lit("DELETE FROM instances WHERE id = ?"),
				jen.Id("instanceId"),
			),
			jen.Return(jen.Lit(""), jen.Err()),
		)
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(stmts...)
		f.Line()
		return true

	case "exists_":
		// Check if instance exists in SQLite
		stmts := append(dbPathStatements(),
			jen.Id("db").Op(",").Err().Op(":=").Qual("database/sql", "Open").Call(
				jen.Lit("sqlite3"),
				jen.Id("dbPath"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("count").Int(),
			jen.Err().Op("=").Id("db").Dot("QueryRow").Call(
				jen.Lit("SELECT COUNT(*) FROM instances WHERE id = ?"),
				jen.Id("instanceId"),
			).Dot("Scan").Call(jen.Op("&").Id("count")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Err()),
			),
			jen.If(jen.Id("count").Op(">").Lit(0)).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Func().Id(m.goName).Params(jen.Id("instanceId").String()).Parens(jen.List(jen.String(), jen.Error())).Block(stmts...)
		f.Line()
		return true

	case "findByClass_":
		// Find all instances of a class
		stmts := append(dbPathStatements(),
			jen.Id("db").Op(",").Err().Op(":=").Qual("database/sql", "Open").Call(
				jen.Lit("sqlite3"),
				jen.Id("dbPath"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("[]"), jen.Err()),
			),
			jen.Defer().Id("db").Dot("Close").Call(),
			jen.Line(),
			jen.Comment("Query instances where the JSON data contains the class"),
			jen.Id("rows").Op(",").Err().Op(":=").Id("db").Dot("Query").Call(
				jen.Lit("SELECT id FROM instances WHERE json_extract(data, '$.class') = ?"),
				jen.Id("className"),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("[]"), jen.Err()),
			),
			jen.Defer().Id("rows").Dot("Close").Call(),
			jen.Line(),
			jen.Var().Id("ids").Index().String(),
			jen.For(jen.Id("rows").Dot("Next").Call()).Block(
				jen.Var().Id("id").String(),
				jen.If(jen.Err().Op(":=").Id("rows").Dot("Scan").Call(jen.Op("&").Id("id")), jen.Err().Op("==").Nil()).Block(
					jen.Id("ids").Op("=").Append(jen.Id("ids"), jen.Id("id")),
				),
			),
			jen.Line(),
			jen.Comment("Return as JSON array"),
			jen.Id("result").Op(",").Err().Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("ids")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("[]"), jen.Err()),
			),
			jen.Return(jen.String().Call(jen.Id("result")), jen.Nil()),
		)
		f.Func().Id(m.goName).Params(jen.Id("className").String()).Parens(jen.List(jen.String(), jen.Error())).Block(stmts...)
		f.Line()
		return true

	default:
		return false
	}
}

// dbPathStatements returns jen statements to get the database path.
// This sets a variable called "dbPath" with the appropriate path.
// Uses SQLITE_JSON_DB env var for consistency with bash runtime.
func dbPathStatements() []jen.Code {
	return []jen.Code{
		jen.Id("dbPath").Op(":=").Qual("os", "Getenv").Call(jen.Lit("SQLITE_JSON_DB")),
		jen.If(jen.Id("dbPath").Op("==").Lit("")).Block(
			jen.List(jen.Id("home"), jen.Id("_")).Op(":=").Qual("os", "UserHomeDir").Call(),
			jen.Id("dbPath").Op("=").Qual("path/filepath", "Join").Call(
				jen.Id("home"),
				jen.Lit(".trashtalk"),
				jen.Lit("instances.db"),
			),
		),
	}
}

// generatePrimitiveMethodHttp generates native Http class methods.
func (g *generator) generatePrimitiveMethodHttp(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "get_":
		// Simple GET request
		f.Func().Id(m.goName).Params(jen.Id("url").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("resp").Op(",").Err().Op(":=").Qual("net/http", "Get").Call(jen.Id("url")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.Id("body").Op(",").Err().Op(":=").Qual("io", "ReadAll").Call(jen.Id("resp").Dot("Body")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.String().Call(jen.Id("body")), jen.Nil()),
		)
		f.Line()
		return true

	case "post_to_":
		// POST request with JSON body
		f.Func().Id(m.goName).Params(
			jen.Id("data").String(),
			jen.Id("url").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("resp").Op(",").Err().Op(":=").Qual("net/http", "Post").Call(
				jen.Id("url"),
				jen.Lit("application/json"),
				jen.Qual("strings", "NewReader").Call(jen.Id("data")),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.Id("body").Op(",").Err().Op(":=").Qual("io", "ReadAll").Call(jen.Id("resp").Dot("Body")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.String().Call(jen.Id("body")), jen.Nil()),
		)
		f.Line()
		return true

	case "put_to_":
		// PUT request with JSON body
		f.Func().Id(m.goName).Params(
			jen.Id("data").String(),
			jen.Id("url").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("req").Op(",").Err().Op(":=").Qual("net/http", "NewRequest").Call(
				jen.Lit("PUT"),
				jen.Id("url"),
				jen.Qual("strings", "NewReader").Call(jen.Id("data")),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("req").Dot("Header").Dot("Set").Call(jen.Lit("Content-Type"), jen.Lit("application/json")),
			jen.Id("resp").Op(",").Err().Op(":=").Qual("net/http", "DefaultClient").Dot("Do").Call(jen.Id("req")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.Id("body").Op(",").Err().Op(":=").Qual("io", "ReadAll").Call(jen.Id("resp").Dot("Body")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.String().Call(jen.Id("body")), jen.Nil()),
		)
		f.Line()
		return true

	case "delete_":
		// DELETE request
		f.Func().Id(m.goName).Params(jen.Id("url").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("req").Op(",").Err().Op(":=").Qual("net/http", "NewRequest").Call(
				jen.Lit("DELETE"),
				jen.Id("url"),
				jen.Nil(),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("resp").Op(",").Err().Op(":=").Qual("net/http", "DefaultClient").Dot("Do").Call(jen.Id("req")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.Id("body").Op(",").Err().Op(":=").Qual("io", "ReadAll").Call(jen.Id("resp").Dot("Body")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.String().Call(jen.Id("body")), jen.Nil()),
		)
		f.Line()
		return true

	case "head_":
		// HEAD request (returns headers)
		f.Func().Id(m.goName).Params(jen.Id("url").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("resp").Op(",").Err().Op(":=").Qual("net/http", "Head").Call(jen.Id("url")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.Comment("Build header string"),
			jen.Var().Id("headers").Qual("strings", "Builder"),
			jen.For(jen.List(jen.Id("key"), jen.Id("values")).Op(":=").Range().Id("resp").Dot("Header")).Block(
				jen.For(jen.List(jen.Id("_"), jen.Id("v")).Op(":=").Range().Id("values")).Block(
					jen.Id("headers").Dot("WriteString").Call(jen.Id("key").Op("+").Lit(": ").Op("+").Id("v").Op("+").Lit("\n")),
				),
			),
			jen.Return(jen.Id("headers").Dot("String").Call(), jen.Nil()),
		)
		f.Line()
		return true

	case "status_":
		// Get HTTP status code only
		f.Func().Id(m.goName).Params(jen.Id("url").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("resp").Op(",").Err().Op(":=").Qual("net/http", "Get").Call(jen.Id("url")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("resp").Dot("StatusCode")), jen.Nil()),
		)
		f.Line()
		return true

	case "ping_":
		// Check if URL is reachable (returns true/false)
		f.Func().Id(m.goName).Params(jen.Id("url").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("client").Op(":=").Op("&").Qual("net/http", "Client").Values(jen.Dict{
				jen.Id("Timeout"): jen.Lit(5).Op("*").Qual("time", "Second"),
			}),
			jen.Id("resp").Op(",").Err().Op(":=").Id("client").Dot("Get").Call(jen.Id("url")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Defer().Id("resp").Dot("Body").Dot("Close").Call(),
			jen.If(jen.Id("resp").Dot("StatusCode").Op(">=").Lit(200).Op("&&").Id("resp").Dot("StatusCode").Op("<").Lit(400)).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}

// generatePrimitiveMethodTool generates native Tool class methods.
func (g *generator) generatePrimitiveMethodTool(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "commandExists_":
		// Check if command exists on PATH
		f.Func().Id(m.goName).Params(jen.Id("commandName").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("_"), jen.Err()).Op(":=").Qual("os/exec", "LookPath").Call(jen.Id("commandName")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Return(jen.Lit("true"), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}
