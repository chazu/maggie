// Maggie docstring test runner — Phase 3.2 of the documentation system.
//
// Extracts test blocks from method docstrings across all classes, parses
// >>> assertion lines, compiles and evaluates both sides, and compares results.
//
// Usage:
//
//	mag doctest                    # Run all docstring tests
//	mag doctest --verbose          # Show each test as it runs
//	mag doctest --class Array      # Run tests only for Array class
package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"time"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Test model
// ---------------------------------------------------------------------------

// doctestAssertion represents a single >>> assertion within a test block.
type doctestAssertion struct {
	Line     string // full original line
	Expr     string // left-hand expression
	Expected string // right-hand expected expression (empty for setup lines)
}

// doctestResult captures the outcome of a single assertion.
type doctestResult struct {
	Assertion   doctestAssertion
	Passed      bool
	ActualStr   string // printString of actual result
	ExpectedStr string // printString of expected result
	Error       string // non-empty if compilation or execution failed
}

// doctestMethodResult groups results for one method.
type doctestMethodResult struct {
	ClassName   string
	Selector    string
	IsClassSide bool
	Results     []doctestResult
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

// handleDoctestCommand is the entry point for `mag doctest`.
func handleDoctestCommand(vmInst *vm.VM, args []string) {
	verbose := false
	classFilter := ""

	// Parse doctest-specific flags.
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--verbose", "-v":
			verbose = true
		case "--class", "-c":
			if i+1 < len(args) {
				i++
				classFilter = args[i]
			} else {
				fmt.Fprintf(os.Stderr, "Error: --class requires a class name argument\n")
				os.Exit(1)
			}
		default:
			if strings.HasPrefix(args[i], "--class=") {
				classFilter = strings.TrimPrefix(args[i], "--class=")
			} else if strings.HasPrefix(args[i], "-") {
				fmt.Fprintf(os.Stderr, "Unknown doctest flag: %s\n", args[i])
				os.Exit(1)
			}
		}
	}

	fmt.Println("Running docstring tests...")
	fmt.Println()

	startTime := time.Now()
	allResults := collectAndRunDoctests(vmInst, classFilter, verbose)
	elapsed := time.Since(startTime)

	// Print results grouped by class.
	printDoctestResults(allResults, verbose)

	// Summarize.
	passed, failed, total := tallyDoctestResults(allResults)

	fmt.Println("\033[90m" + strings.Repeat("\u2500", 40) + "\033[0m")
	if failed > 0 {
		fmt.Printf("Results: \033[32m%d passed\033[0m, \033[31m%d failed\033[0m, %d total (%s)\n",
			passed, failed, total, elapsed.Round(time.Millisecond))
	} else if total > 0 {
		fmt.Printf("Results: \033[32m%d passed\033[0m, %d total (%s)\n",
			passed, total, elapsed.Round(time.Millisecond))
	} else {
		fmt.Printf("No docstring tests found.\n")
	}

	if failed > 0 {
		os.Exit(1)
	}
}

// ---------------------------------------------------------------------------
// Test collection and execution
// ---------------------------------------------------------------------------

// collectAndRunDoctests walks all classes and methods, extracts test blocks
// from docstrings, and executes the assertions. Results are grouped by method.
func collectAndRunDoctests(vmInst *vm.VM, classFilter string, verbose bool) []doctestMethodResult {
	var allResults []doctestMethodResult

	classes := vmInst.Classes.All()
	// Sort classes by name for deterministic output.
	sort.Slice(classes, func(i, j int) bool {
		return classes[i].FullName() < classes[j].FullName()
	})

	for _, cls := range classes {
		if classFilter != "" && cls.Name != classFilter && cls.FullName() != classFilter {
			continue
		}

		// Instance methods.
		instanceMethods := cls.VTable.LocalMethods()
		allResults = append(allResults,
			runDoctestMethods(vmInst, cls, instanceMethods, false, verbose)...)

		// Class methods.
		classMethods := cls.ClassVTable.LocalMethods()
		allResults = append(allResults,
			runDoctestMethods(vmInst, cls, classMethods, true, verbose)...)
	}

	return allResults
}

// runDoctestMethods runs docstring tests for a map of methods on a single class.
func runDoctestMethods(vmInst *vm.VM, cls *vm.Class, methods map[int]vm.Method, isClassSide bool, verbose bool) []doctestMethodResult {
	var results []doctestMethodResult

	// Collect and sort selector IDs for deterministic order.
	selectorIDs := make([]int, 0, len(methods))
	for selID := range methods {
		selectorIDs = append(selectorIDs, selID)
	}
	sort.Ints(selectorIDs)

	for _, selID := range selectorIDs {
		method := methods[selID]

		docStr := vm.MethodDocString(method)
		if docStr == "" {
			continue
		}

		sections := ParseDocString(docStr)
		if len(sections) == 0 {
			continue
		}

		selectorName := vmInst.Selectors.Name(selID)
		if selectorName == "" {
			selectorName = vm.MethodName(method)
		}

		var methodResults []doctestResult

		for _, sec := range sections {
			if sec.Type != DocTest {
				continue
			}

			assertions := parseDoctestAssertions(sec.Content)
			if len(assertions) == 0 {
				continue
			}

			// Run setup lines and assertions.
			for _, asrt := range assertions {
				result := runDoctestAssertion(vmInst, asrt, verbose)
				methodResults = append(methodResults, result)
			}
		}

		if len(methodResults) > 0 {
			results = append(results, doctestMethodResult{
				ClassName:   cls.FullName(),
				Selector:    selectorName,
				IsClassSide: isClassSide,
				Results:     methodResults,
			})
		}
	}

	return results
}

// parseDoctestAssertions extracts assertion lines (containing >>>) and setup
// lines from a test block's content. Setup lines are evaluated for side
// effects only.
func parseDoctestAssertions(content string) []doctestAssertion {
	var assertions []doctestAssertion

	for _, line := range strings.Split(content, "\n") {
		trimmed := strings.TrimSpace(line)
		if trimmed == "" {
			continue
		}

		if !strings.Contains(trimmed, ">>>") {
			// Setup line: wrap as an assertion with no expected value.
			// We still run it; it just always "passes" (unless it errors).
			assertions = append(assertions, doctestAssertion{
				Line: trimmed,
				Expr: trimmed,
			})
			continue
		}

		parts := strings.SplitN(trimmed, ">>>", 2)
		if len(parts) != 2 {
			continue
		}

		expr := strings.TrimSpace(parts[0])
		expected := strings.TrimSpace(parts[1])

		if expr == "" {
			// Malformed: nothing before >>>. Skip with warning.
			fmt.Fprintf(os.Stderr, "Warning: skipping malformed assertion (empty expression): %s\n", trimmed)
			continue
		}
		if expected == "" {
			// Malformed: nothing after >>>. Skip with warning.
			fmt.Fprintf(os.Stderr, "Warning: skipping malformed assertion (empty expected value): %s\n", trimmed)
			continue
		}

		assertions = append(assertions, doctestAssertion{
			Line:     trimmed,
			Expr:     expr,
			Expected: expected,
		})
	}

	return assertions
}

// runDoctestAssertion compiles and evaluates a single assertion or setup line.
func runDoctestAssertion(vmInst *vm.VM, asrt doctestAssertion, verbose bool) doctestResult {
	// Setup line (no expected value): just run the expression.
	if asrt.Expected == "" {
		_, err := doctestEvalExpression(vmInst, asrt.Expr)
		if err != nil {
			return doctestResult{
				Assertion: asrt,
				Passed:    false,
				Error:     err.Error(),
			}
		}
		return doctestResult{
			Assertion: asrt,
			Passed:    true,
		}
	}

	// Assertion line: evaluate both sides and compare.
	actual, err := doctestEvalExpression(vmInst, asrt.Expr)
	if err != nil {
		return doctestResult{
			Assertion: asrt,
			Passed:    false,
			Error:     fmt.Sprintf("expression error: %s", err),
		}
	}

	expected, err := doctestEvalExpression(vmInst, asrt.Expected)
	if err != nil {
		return doctestResult{
			Assertion: asrt,
			Passed:    false,
			Error:     fmt.Sprintf("expected-value error: %s", err),
		}
	}

	// Compare values.
	pass, actualStr, expectedStr := doctestCompareValues(vmInst, actual, expected)

	return doctestResult{
		Assertion:   asrt,
		Passed:      pass,
		ActualStr:   actualStr,
		ExpectedStr: expectedStr,
	}
}

// ---------------------------------------------------------------------------
// Expression evaluation
// ---------------------------------------------------------------------------

// doctestEvalExpression compiles and executes a Maggie expression, returning
// the result. Uses ExecuteSafe to catch Maggie-level panics.
func doctestEvalExpression(vmInst *vm.VM, expr string) (vm.Value, error) {
	expr = strings.TrimSpace(expr)
	if expr == "" {
		return vm.Nil, nil
	}

	// Wrap expression as a doIt method (same pattern as the REPL).
	source := "doIt\n    ^" + strings.TrimSuffix(expr, ".")

	method, err := vmInst.Compile(source, nil)
	if err != nil {
		return vm.Nil, fmt.Errorf("compile: %v", err)
	}
	if method == nil {
		return vm.Nil, fmt.Errorf("compile: compiler returned nil")
	}

	result, err := vmInst.ExecuteSafe(method, vm.Nil, nil)
	if err != nil {
		return vm.Nil, fmt.Errorf("runtime: %v", err)
	}

	return result, nil
}

// ---------------------------------------------------------------------------
// Value comparison
// ---------------------------------------------------------------------------

// doctestCompareValues compares two Maggie values. It first tries direct
// bitwise equality (fast path), then falls back to comparing their printString
// output. Returns (passed, actualStr, expectedStr).
func doctestCompareValues(vmInst *vm.VM, actual, expected vm.Value) (bool, string, string) {
	// Fast path: bitwise-identical values always match.
	if actual == expected {
		str := doctestPrintString(vmInst, actual)
		return true, str, str
	}

	// Compare via printString.
	actualStr := doctestPrintString(vmInst, actual)
	expectedStr := doctestPrintString(vmInst, expected)

	return actualStr == expectedStr, actualStr, expectedStr
}

// doctestPrintString sends printString to a value and returns the Go string.
// Falls back to a simple representation if printString fails.
func doctestPrintString(vmInst *vm.VM, v vm.Value) string {
	// Handle well-known values directly to avoid dispatch overhead and panics.
	switch v {
	case vm.Nil:
		return "nil"
	case vm.True:
		return "true"
	case vm.False:
		return "false"
	}

	if v.IsSmallInt() {
		return fmt.Sprintf("%d", v.SmallInt())
	}

	// Try sending printString via a safe wrapper to catch panics.
	result, ok := doctestSafeSend(vmInst, v, "printString")
	if ok && vm.IsStringValue(result) {
		return vm.GetStringContent(result)
	}

	// Fallback for floats.
	if v.IsFloat() {
		return fmt.Sprintf("%g", v.Float64())
	}

	// Fallback for strings (they are encoded as symbols).
	if vm.IsStringValue(v) {
		return "'" + vm.GetStringContent(v) + "'"
	}

	// Last resort: hex representation.
	return fmt.Sprintf("<value:0x%X>", uint64(v))
}

// doctestSafeSend sends a message to a value and recovers from any panic.
func doctestSafeSend(vmInst *vm.VM, receiver vm.Value, selector string) (result vm.Value, ok bool) {
	defer func() {
		if r := recover(); r != nil {
			result = vm.Nil
			ok = false
		}
	}()
	result = vmInst.Send(receiver, selector, nil)
	return result, true
}

// ---------------------------------------------------------------------------
// Output
// ---------------------------------------------------------------------------

// ANSI color codes.
const (
	dtColorReset  = "\033[0m"
	dtColorBold   = "\033[1m"
	dtColorRed    = "\033[31m"
	dtColorGreen  = "\033[32m"
	dtColorDim    = "\033[90m"
)

// printDoctestResults renders the test results grouped by class and method.
func printDoctestResults(results []doctestMethodResult, verbose bool) {
	if len(results) == 0 {
		return
	}

	lastClass := ""

	for _, mr := range results {
		// Print class heading when it changes.
		if mr.ClassName != lastClass {
			if lastClass != "" {
				fmt.Println() // blank line between classes
			}
			fmt.Printf("%s%s%s\n", dtColorBold, mr.ClassName, dtColorReset)
			lastClass = mr.ClassName
		}

		// Print method heading.
		if mr.IsClassSide {
			fmt.Printf("  class>>%s\n", mr.Selector)
		} else {
			fmt.Printf("  >>%s\n", mr.Selector)
		}

		// Print each assertion result.
		for _, r := range mr.Results {
			// Skip setup lines in non-verbose mode if they passed.
			if r.Assertion.Expected == "" {
				if r.Passed {
					if verbose {
						fmt.Printf("    %s%s (setup)%s\n", dtColorDim, r.Assertion.Expr, dtColorReset)
					}
					continue
				}
				// Failed setup line.
				fmt.Printf("    %s\u2717 %s (setup)%s\n", dtColorRed, r.Assertion.Expr, dtColorReset)
				if r.Error != "" {
					fmt.Printf("      %sError: %s%s\n", dtColorRed, r.Error, dtColorReset)
				}
				continue
			}

			if r.Passed {
				fmt.Printf("    %s\u2713%s %s\n", dtColorGreen, dtColorReset, r.Assertion.Line)
			} else {
				fmt.Printf("    %s\u2717%s %s\n", dtColorRed, dtColorReset, r.Assertion.Line)
				if r.Error != "" {
					fmt.Printf("      %sError: %s%s\n", dtColorRed, r.Error, dtColorReset)
				} else {
					fmt.Printf("      Expected: %s\n", r.ExpectedStr)
					fmt.Printf("      Got:      %s\n", r.ActualStr)
				}
			}
		}
	}

	fmt.Println()
}

// tallyDoctestResults counts passed, failed, and total assertions.
// Setup lines that passed are not counted in the total.
func tallyDoctestResults(results []doctestMethodResult) (passed, failed, total int) {
	for _, mr := range results {
		for _, r := range mr.Results {
			// Only count assertion lines (those with expected values) and
			// failed setup lines in the tally.
			if r.Assertion.Expected == "" && r.Passed {
				continue // successful setup -- not counted
			}
			total++
			if r.Passed {
				passed++
			} else {
				failed++
			}
		}
	}
	return
}
