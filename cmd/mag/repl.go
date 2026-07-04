package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// runREPL starts an interactive read-eval-print loop.
func runREPL(vmInst *vm.VM) {
	fmt.Println("Maggie REPL (type 'exit' to quit, ':help' for commands)")
	fmt.Printf("Compiler: %s\n", vmInst.CompilerName())
	fmt.Println()

	scanner := bufio.NewScanner(os.Stdin)
	lineBuffer := strings.Builder{}

	for {
		// Show prompt
		if lineBuffer.Len() == 0 {
			fmt.Print(">> ")
		} else {
			fmt.Print(".. ")
		}

		if !scanner.Scan() {
			break
		}

		line := scanner.Text()

		// Handle exit
		if lineBuffer.Len() == 0 && (line == "exit" || line == "quit") {
			break
		}

		// Handle REPL commands (start with ':')
		if lineBuffer.Len() == 0 && strings.HasPrefix(line, ":") {
			handleREPLCommand(vmInst, line)
			continue
		}

		// Empty line executes accumulated input
		if line == "" && lineBuffer.Len() > 0 {
			input := strings.TrimSpace(lineBuffer.String())
			lineBuffer.Reset()

			if input != "" {
				evalAndPrint(vmInst, input)
			}
			continue
		}

		// Accumulate lines
		if lineBuffer.Len() > 0 {
			lineBuffer.WriteString("\n")
		}
		lineBuffer.WriteString(line)

		// If line ends with '.', execute immediately
		if strings.HasSuffix(strings.TrimSpace(line), ".") {
			input := strings.TrimSpace(lineBuffer.String())
			lineBuffer.Reset()

			if input != "" {
				evalAndPrint(vmInst, input)
			}
		}
	}

	fmt.Println()
}

// handleREPLCommand handles REPL meta-commands.
func handleREPLCommand(vmInst *vm.VM, cmd string) {
	parts := strings.Fields(cmd)
	baseCmd := parts[0]

	switch baseCmd {
	case ":help", ":h", ":?":
		if len(parts) > 1 {
			handleHelpLookup(vmInst, parts[1])
			return
		}
		fmt.Println("REPL Commands:")
		fmt.Println("  :help, :h, :?           Show this help")
		fmt.Println("  :help ClassName          Show class documentation")
		fmt.Println("  :help Class>>method      Show method documentation")
		fmt.Println("  :compiler                Show current compiler")
		fmt.Println("  :use-go                  Switch to Go compiler (default)")
		fmt.Println("  exit, quit               Exit REPL")
	case ":compiler":
		fmt.Printf("Current compiler: %s\n", vmInst.CompilerName())
	case ":use-go":
		vmInst.UseGoCompiler(compiler.Compile)
		fmt.Println("Switched to Go compiler")
	default:
		fmt.Printf("Unknown command: %s (type :help for commands)\n", cmd)
	}
}

// handleHelpCommand handles the "mag help" subcommand.
// With no args, lists all classes. With an arg, shows class or method help.
func handleHelpCommand(vmInst *vm.VM, args []string) {
	// Bare `help` (and `help --all`) list the class index. By default we hide
	// tutorial (GuideNN…) and IDE (Yutani) classes so an agent or newcomer sees
	// the orthogonal core, not ~50 tutorial/widget entries. `--all` shows them.
	if len(args) == 0 || (len(args) == 1 && args[0] == "--all") {
		showAll := len(args) == 1 && args[0] == "--all"
		classes := vmInst.Classes.All()
		sort.Slice(classes, func(i, j int) bool {
			return classes[i].Name < classes[j].Name
		})
		shown := 0
		for _, cls := range classes {
			if !showAll && isNonCoreHelpClass(cls) {
				continue
			}
			shown++
			name := cls.Name
			if cls.Namespace != "" {
				name = cls.Namespace + "::" + cls.Name
			}
			if cls.DocString != "" {
				summary := cls.DocString
				if idx := strings.IndexByte(summary, '\n'); idx != -1 {
					summary = summary[:idx]
				}
				fmt.Printf("  %-30s %s\n", name, summary)
			} else {
				fmt.Printf("  %s\n", name)
			}
		}
		if !showAll {
			fmt.Printf("\n(%d core classes shown; `mag help --all` includes tutorial and IDE classes)\n", shown)
		}
		return
	}

	query := strings.Join(args, " ")
	query = strings.ReplaceAll(query, " >> ", ">>")
	query = strings.TrimSpace(query)

	handleHelpLookup(vmInst, query)
}

// isNonCoreHelpClass reports whether a class is a tutorial (GuideNN…) or IDE
// (Yutani namespace) class that clutters the default `mag help` index.
func isNonCoreHelpClass(cls *vm.Class) bool {
	if strings.HasPrefix(cls.Namespace, "Yutani") {
		return true
	}
	// Tutorial classes are named GuideNN… (e.g. Guide01GettingStarted).
	n := cls.Name
	if strings.HasPrefix(n, "Guide") && len(n) > 5 && n[5] >= '0' && n[5] <= '9' {
		return true
	}
	return false
}

// handleHelpLookup handles :help ClassName and :help ClassName>>methodName.
func handleHelpLookup(vmInst *vm.VM, query string) {
	if idx := strings.Index(query, ">>"); idx != -1 {
		className := query[:idx]
		methodName := query[idx+2:]

		cls := vmInst.Classes.Lookup(className)
		if cls == nil {
			fmt.Printf("Unknown class: %s\n", className)
			return
		}

		m := cls.MethodByName(methodName)
		if m == nil {
			// Try class-side methods
			m = cls.ClassMethodByName(methodName)
		}
		if m == nil {
			fmt.Printf("%s does not define #%s\n", className, methodName)
			return
		}

		fmt.Printf("%s>>%s\n", className, methodName)
		doc := vm.MethodDocString(m)
		if doc != "" {
			fmt.Printf("\n%s\n", doc)
		} else {
			fmt.Println("\n(no documentation)")
		}
		return
	}

	cls := vmInst.Classes.Lookup(query)
	if cls == nil {
		fmt.Printf("Unknown class: %s\n", query)
		return
	}

	fmt.Printf("%s", vm.FormatClassHelp(cls, vmInst.Selectors))
}

// evalAndPrint compiles and executes an expression, printing the result.
func evalAndPrint(vmInst *vm.VM, input string) {
	source := input
	if !looksLikeMethodDef(input) {
		source = "doIt\n    ^" + strings.TrimSuffix(input, ".")
	}

	method, err := vmInst.Compile(source, nil)
	if err != nil {
		fmt.Printf("Compile error: %v\n", err)
		return
	}
	if method == nil {
		fmt.Println("Compile error: compiler returned nil")
		return
	}

	result, err := vmInst.ExecuteSafe(method, vm.Nil, nil)
	if err != nil {
		fmt.Printf("%v\n", err)
		return
	}
	printValue(vmInst, result)
}

// looksLikeMethodDef checks if input appears to be a method definition.
func looksLikeMethodDef(input string) bool {
	lines := strings.Split(input, "\n")
	if len(lines) <= 1 {
		return false
	}

	first := strings.TrimSpace(lines[0])
	if len(lines) > 1 && len(lines[1]) > 0 {
		if lines[1][0] == ' ' || lines[1][0] == '\t' {
			if len(first) > 0 && first[0] != ' ' && first[0] != '\t' {
				return true
			}
		}
	}

	return false
}

// printValue prints a value in a readable format.
func printValue(vmInst *vm.VM, v vm.Value) {
	switch {
	case v == vm.Nil:
		fmt.Println("nil")
	case v == vm.True:
		fmt.Println("true")
	case v == vm.False:
		fmt.Println("false")
	case v.IsSmallInt():
		fmt.Println(v.SmallInt())
	case v.IsFloat():
		fmt.Println(v.Float64())
	case vm.IsStringValue(v):
		fmt.Printf("'%s'\n", vmInst.Registry().GetStringContent(v))
	case vm.IsDictionaryValue(v):
		result := vmInst.Send(v, "printString", nil)
		if vm.IsStringValue(result) {
			fmt.Printf("'%s'\n", vmInst.Registry().GetStringContent(result))
		} else {
			fmt.Println("a Dictionary")
		}
	case v.IsSymbol():
		name := vmInst.Symbols.Name(v.SymbolID())
		fmt.Printf("#%s\n", name)
	case v.IsObject():
		result := vmInst.Send(v, "printString", nil)
		if result.IsSmallInt() {
			fmt.Println("an Object")
		} else {
			printValue(vmInst, result)
		}
	default:
		fmt.Printf("<%v>\n", v)
	}
}
