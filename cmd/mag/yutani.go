package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
)

// runYutaniIDE loads the Yutani library and starts the IDE.
func runYutaniIDE(vmInst *vm.VM, addr string, tool string, verbose bool) error {
	yutaniPath, err := findYutaniLib()
	if err != nil {
		return fmt.Errorf("cannot find Yutani library: %w", err)
	}

	if verbose {
		fmt.Printf("Loading Yutani library from %s\n", yutaniPath)
	}

	p := &pipeline.Pipeline{VM: vmInst}
	if verbose {
		p.Verbose = os.Stdout
	}
	_, err = p.CompilePath(yutaniPath + "/...")
	if err != nil {
		return fmt.Errorf("loading Yutani library: %w", err)
	}

	var startupCode string
	switch tool {
	case "launcher", "ide", "desktop":
		startupCode = fmt.Sprintf("MaggieDesktop openIn: (YutaniSession connectTo: '%s')", addr)
	case "browser":
		return fmt.Errorf("ClassBrowser is shelved (see lib/yutani/ide/shelved/). Use: launcher, inspector, repl")
	case "inspector":
		startupCode = fmt.Sprintf("Inspector inspect: nil in: (YutaniSession connectTo: '%s')", addr)
	case "repl":
		startupCode = fmt.Sprintf("MaggieREPL openIn: (YutaniSession connectTo: '%s')", addr)
	case "editor":
		return fmt.Errorf("CodeEditor is shelved (see lib/yutani/ide/shelved/). Use: launcher, inspector, repl")
	default:
		return fmt.Errorf("unknown IDE tool: %s (use: launcher, inspector, repl)", tool)
	}

	fmt.Printf("Starting Yutani IDE (%s), connecting to %s...\n", tool, addr)
	fmt.Printf("Make sure the Yutani server is running: yutani server\n")

	source := "doIt\n    ^" + startupCode
	if verbose {
		fmt.Printf("Compiling: %s\n", startupCode)
	}
	method, err := compiler.Compile(source, vmInst.Selectors, vmInst.Symbols, vmInst.Registry())
	if err != nil {
		return fmt.Errorf("compiling startup code: %w", err)
	}

	if verbose {
		fmt.Println("Executing startup code...")
	}
	result, err := vmInst.ExecuteSafe(method, vm.Nil, nil)
	if err != nil {
		return err
	}
	if verbose {
		fmt.Printf("Execution returned: %v (isNil=%v)\n", result, result == vm.Nil)
	}
	if result == vm.Nil {
		fmt.Println("IDE exited. (If it exited immediately, check that yutani server is running)")
	}
	return nil
}

// findYutaniLib locates the Yutani library directory.
func findYutaniLib() (string, error) {
	exe, err := os.Executable()
	if err == nil {
		exeDir := filepath.Dir(exe)
		candidate := filepath.Join(exeDir, "..", "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
		candidate = filepath.Join(exeDir, "..", "..", "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
	}

	cwd, err := os.Getwd()
	if err == nil {
		candidate := filepath.Join(cwd, "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
	}

	if home := os.Getenv("MAGGIE_HOME"); home != "" {
		candidate := filepath.Join(home, "lib", "yutani")
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
	}

	return "", fmt.Errorf("Yutani library not found. Set MAGGIE_HOME or run from project root")
}
