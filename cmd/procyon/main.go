// Procyon - Trashtalk to Go compiler
// Named after the genus for raccoons - because what goes better with trash?
package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/chazu/procyon/pkg/ast"
	"github.com/chazu/procyon/pkg/codegen"
	"github.com/chazu/procyon/pkg/ir"
)

var (
	strict     = flag.Bool("strict", false, "fail on unsupported constructs instead of warning")
	dryRun     = flag.Bool("dry-run", false, "show what would be generated without outputting")
	version    = flag.Bool("version", false, "print version and exit")
	mode       = flag.String("mode", "shared", "output mode: bash (Bash script) or shared (Go dylib linking against libtrashtalk)")
	sourceFile = flag.String("source-file", "", "path to original source file for embedding (bash mode only)")
	skipVet    = flag.Bool("skip-vet", false, "skip Go validation of generated code (plugin/shared modes)")
)

const versionStr = "0.8.0"

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Procyon - Trashtalk to Go compiler\n\n")
		fmt.Fprintf(os.Stderr, "Usage:\n")
		fmt.Fprintf(os.Stderr, "  procyon [options] < ast.json > output.go\n")
		fmt.Fprintf(os.Stderr, "  trashtalk-parser Class.trash | procyon > class/main.go\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
	}

	flag.Parse()

	if *version {
		fmt.Printf("procyon version %s\n", versionStr)
		os.Exit(0)
	}

	// Read AST from stdin
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}

	if len(input) == 0 {
		fmt.Fprintf(os.Stderr, "Error: no input provided\n")
		fmt.Fprintf(os.Stderr, "Usage: procyon < ast.json\n")
		os.Exit(1)
	}

	// Parse AST (supports both plain Class and CompilationUnit with traits)
	unit, err := ast.ParseCompilationUnit(input)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing AST: %v\n", err)
		os.Exit(1)
	}

	// Merge trait methods into the class
	merged, missing := unit.MergeTraits()
	if len(merged) > 0 {
		fmt.Fprintf(os.Stderr, "Merged trait methods: %v\n", merged)
	}
	if len(missing) > 0 {
		fmt.Fprintf(os.Stderr, "Warning: traits not provided (will fall back to Bash): %v\n", missing)
	}
	class := unit.Class

	// Note: primitiveClass classes are now handled by plugin/shared modes.
	// They generate plugins using built-in implementations from primitiveRegistry.

	// Validate primitiveClass constraint (must check after trait methods are merged)
	if err := class.ValidatePrimitiveClass(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Validate native ↔ Bash selector parity for primitive classes
	if class.IsPrimitiveClass() {
		selectors := make([]string, len(class.Methods))
		for i, m := range class.Methods {
			selectors[i] = m.Selector
		}
		parity := codegen.ValidatePrimitiveClassParity(class.Name, selectors)
		if parity != nil {
			// Orphaned native implementations are errors (registry has selectors that don't exist in source)
			if parity.HasErrors() {
				fmt.Fprintf(os.Stderr, "Error: primitive class '%s' has native implementations for non-existent methods: %v\n",
					class.Name, parity.OrphanedNative)
				fmt.Fprintf(os.Stderr, "Either add these methods to the .trash source or remove them from primitiveRegistry.\n")
				os.Exit(1)
			}
			// Missing native implementations are warnings (bash fallback will be used)
			if len(parity.MissingNative) > 0 {
				fmt.Fprintf(os.Stderr, "Warning: primitive class '%s' has methods without native implementations: %v\n",
					class.Name, parity.MissingNative)
				fmt.Fprintf(os.Stderr, "These methods will use Bash fallback at runtime.\n")
			}
		}
	}

	// Generate code based on mode
	var result *codegen.Result
	switch *mode {
	case "bash":
		// Bash mode: convert AST to IR, then generate Bash
		builder := ir.NewBuilder(class)
		prog, errs, warnings := builder.Build()
		for _, w := range warnings {
			fmt.Fprintf(os.Stderr, "Warning: %s\n", w)
		}
		if len(errs) > 0 {
			for _, e := range errs {
				fmt.Fprintf(os.Stderr, "Error: %s\n", e)
			}
			os.Exit(1)
		}
		// Read source file for embedding if provided
		if *sourceFile != "" {
			sourceBytes, err := os.ReadFile(*sourceFile)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Warning: could not read source file for embedding: %v\n", err)
			} else {
				prog.SourceCode = string(sourceBytes)
			}
		}
		backend := codegen.NewBashBackend()
		code, err := backend.Generate(prog)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error generating Bash: %v\n", err)
			os.Exit(1)
		}
		if *dryRun {
			fmt.Fprintf(os.Stderr, "Dry run - would generate %d bytes of Bash code\n", len(code))
			os.Exit(0)
		}
		fmt.Print(code)
		return
	case "shared":
		opts := codegen.GenerateOptions{SkipValidation: *skipVet}
		result = codegen.GenerateSharedPluginWithOptions(class, opts)
	default:
		fmt.Fprintf(os.Stderr, "Error: unknown mode %q (use 'bash' or 'shared')\n", *mode)
		os.Exit(1)
	}

	// Report skipped methods
	if len(result.SkippedMethods) > 0 {
		fmt.Fprintf(os.Stderr, "procyon: %s.trash\n", class.Name)

		// Count compiled methods
		compiled := len(class.Methods) - len(result.SkippedMethods)

		for _, m := range class.Methods {
			skipped := false
			var reason string
			for _, s := range result.SkippedMethods {
				if s.Selector == m.Selector {
					skipped = true
					reason = s.Reason
					break
				}
			}
			if skipped {
				fmt.Fprintf(os.Stderr, "  ⚠ %s - skipped: %s\n", m.Selector, reason)
			} else {
				fmt.Fprintf(os.Stderr, "  ✓ %s - compiled\n", m.Selector)
			}
		}

		fmt.Fprintf(os.Stderr, "\nGenerated %d/%d methods. %d will fall back to Bash.\n\n",
			compiled, len(class.Methods), len(result.SkippedMethods))

		if *strict {
			fmt.Fprintf(os.Stderr, "Error: --strict mode enabled, refusing to generate with skipped methods\n")
			os.Exit(1)
		}
	}

	// Report warnings
	for _, w := range result.Warnings {
		fmt.Fprintf(os.Stderr, "Warning: %s\n", w)
	}

	// Output
	if *dryRun {
		fmt.Fprintf(os.Stderr, "Dry run - would generate %d bytes of Go code\n", len(result.Code))
		os.Exit(0)
	}

	fmt.Print(result.Code)
}
