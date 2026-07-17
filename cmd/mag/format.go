package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/format"
)

// Format is a thin wrapper over the shared format package so the CLI
// (and its tests) keep a local entry point; the LSP calls format.Format too.
func Format(source string) (string, error) { return format.Format(source) }

// ---------------------------------------------------------------------------
// CLI command: mag fmt
// ---------------------------------------------------------------------------

func handleFmtCommand(args []string) {
	if wantsHelp(args) {
		subcmdUsage("fmt [--check] [files or dirs...]",
			"Format Maggie source files to canonical style.",
			usageFlags([][2]string{
				{"--check", "Check formatting without modifying files (exit 1 if changes needed)"},
			}),
			usageExamples([][2]string{
				{"mag fmt", "Format all .mag files in current directory"},
				{"mag fmt src/", "Format all .mag files in src/"},
				{"mag fmt lib/Array.mag", "Format a specific file"},
				{"mag fmt --check lib/", "Check without modifying"},
			}),
		)
	}

	checkMode := false
	var files []string

	for _, arg := range args {
		if arg == "--check" {
			checkMode = true
		} else {
			files = append(files, arg)
		}
	}

	// Default: current directory
	if len(files) == 0 {
		files = []string{"."}
	}

	// Resolve files/directories to a list of .mag files
	magFiles, err := collectMagFiles(files)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	if len(magFiles) == 0 {
		fmt.Fprintf(os.Stderr, "No .mag files found\n")
		os.Exit(0)
	}

	anyChanged := false
	for _, path := range magFiles {
		changed, err := formatFile(path, checkMode)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error formatting %s: %v\n", path, err)
			os.Exit(1)
		}
		if changed {
			anyChanged = true
		}
	}

	if checkMode && anyChanged {
		os.Exit(1)
	}
}

// formatFile formats a single .mag file.
// In check mode, returns true if the file would be changed.
// Otherwise, rewrites the file in place and returns true if it changed.
func formatFile(path string, checkMode bool) (bool, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return false, err
	}

	original := string(content)
	formatted, err := Format(original)
	if err != nil {
		return false, fmt.Errorf("parse error: %w", err)
	}

	if original == formatted {
		return false, nil
	}

	if checkMode {
		fmt.Printf("would format: %s\n", path)
		return true, nil
	}

	// Write back
	err = os.WriteFile(path, []byte(formatted), 0644)
	if err != nil {
		return false, err
	}

	fmt.Printf("formatted: %s\n", path)
	return true, nil
}

// collectMagFiles resolves paths to a flat list of .mag file paths.
func collectMagFiles(paths []string) ([]string, error) {
	var result []string

	for _, p := range paths {
		abs, err := filepath.Abs(p)
		if err != nil {
			return nil, fmt.Errorf("invalid path %q: %w", p, err)
		}

		info, err := os.Stat(abs)
		if err != nil {
			return nil, fmt.Errorf("cannot access %q: %w", abs, err)
		}

		if info.IsDir() {
			err := filepath.Walk(abs, func(path string, fi os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !fi.IsDir() && strings.HasSuffix(path, ".mag") {
					result = append(result, path)
				}
				return nil
			})
			if err != nil {
				return nil, err
			}
		} else {
			if strings.HasSuffix(abs, ".mag") {
				result = append(result, abs)
			} else {
				return nil, fmt.Errorf("%q is not a .mag file", abs)
			}
		}
	}

	return result, nil
}
