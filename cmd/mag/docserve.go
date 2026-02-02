package main

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/chazu/maggie/vm"
)

// handleDocServe starts an HTTP server that serves generated documentation
// from docDir and provides an /api/eval endpoint for running Maggie expressions.
func handleDocServe(vmInst *vm.VM, docDir string, port int) {
	mux := http.NewServeMux()

	// Static file serving for generated docs
	mux.Handle("/", http.FileServer(http.Dir(docDir)))

	// Eval endpoint with CORS support
	evalHandler := makeEvalHandler(vmInst)
	mux.HandleFunc("/api/eval", func(w http.ResponseWriter, r *http.Request) {
		// Set CORS headers for all requests
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

		if r.Method == "OPTIONS" {
			w.WriteHeader(200)
			return
		}

		evalHandler(w, r)
	})

	addr := fmt.Sprintf(":%d", port)
	fmt.Printf("Documentation server running at http://localhost:%d\n", port)

	server := &http.Server{
		Addr:    addr,
		Handler: mux,
	}

	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		fmt.Printf("Server error: %v\n", err)
	}
}

// makeEvalHandler returns an http.HandlerFunc that evaluates Maggie expressions.
func makeEvalHandler(vmInst *vm.VM) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != "POST" {
			http.Error(w, "Method not allowed", 405)
			return
		}

		w.Header().Set("Content-Type", "text/plain; charset=utf-8")

		// Read body with 10KB limit
		body, err := io.ReadAll(io.LimitReader(r.Body, 10*1024))
		if err != nil {
			http.Error(w, "Failed to read body", 400)
			return
		}
		defer r.Body.Close()

		expr := strings.TrimSpace(string(body))
		if expr == "" {
			http.Error(w, "Empty expression", 400)
			return
		}

		// Execute with 5-second timeout
		result, err := evalWithTimeout(vmInst, expr, 5*time.Second)
		if err != nil {
			http.Error(w, err.Error(), 400)
			return
		}

		w.WriteHeader(200)
		w.Write([]byte(result))
	}
}

// evalWithTimeout executes a Maggie expression with a timeout.
// It compiles and runs the expression in a goroutine and returns the result
// or an error if the evaluation times out or fails.
func evalWithTimeout(vmInst *vm.VM, expr string, timeout time.Duration) (string, error) {
	type evalResult struct {
		value string
		err   error
	}

	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	ch := make(chan evalResult, 1)

	go func() {
		defer func() {
			if r := recover(); r != nil {
				ch <- evalResult{err: fmt.Errorf("%v", r)}
			}
		}()

		// Wrap as doIt method (same pattern as REPL and doctest)
		source := "doIt\n    ^" + strings.TrimSuffix(expr, ".")

		method, err := vmInst.Compile(source, nil)
		if err != nil {
			ch <- evalResult{err: fmt.Errorf("Compile error: %v", err)}
			return
		}
		if method == nil {
			ch <- evalResult{err: fmt.Errorf("Compile error: compiler returned nil")}
			return
		}

		result, err := vmInst.ExecuteSafe(method, vm.Nil, nil)
		if err != nil {
			ch <- evalResult{err: err}
			return
		}

		// Convert result to string
		ch <- evalResult{value: formatEvalResult(vmInst, result)}
	}()

	select {
	case res := <-ch:
		return res.value, res.err
	case <-ctx.Done():
		return "", fmt.Errorf("Evaluation timed out after %v", timeout)
	}
}

// formatEvalResult converts a VM value to a display string.
func formatEvalResult(vmInst *vm.VM, v vm.Value) string {
	switch {
	case v == vm.Nil:
		return "nil"
	case v == vm.True:
		return "true"
	case v == vm.False:
		return "false"
	case v.IsSmallInt():
		return fmt.Sprintf("%d", v.SmallInt())
	case v.IsFloat():
		return fmt.Sprintf("%g", v.Float64())
	case vm.IsStringValue(v):
		return vm.GetStringContent(v)
	default:
		// Try sending printString
		result, ok := safeSend(vmInst, v, "printString")
		if ok && vm.IsStringValue(result) {
			return vm.GetStringContent(result)
		}
		return fmt.Sprintf("%v", v)
	}
}

// safeSend sends a message to a value and recovers from any panic.
func safeSend(vmInst *vm.VM, receiver vm.Value, selector string) (result vm.Value, ok bool) {
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
// Doc arg helpers — parse --serve, --port, --output from doc subcommand args
// ---------------------------------------------------------------------------

// docArgsContain checks whether the given flag appears in the doc args.
func docArgsContain(args []string, flag string) bool {
	for _, a := range args {
		if a == flag {
			return true
		}
	}
	return false
}

// docArgPort extracts the --port value from doc args. Defaults to 8080.
func docArgPort(args []string) int {
	for i := 0; i < len(args); i++ {
		if args[i] == "--port" && i+1 < len(args) {
			if p, err := strconv.Atoi(args[i+1]); err == nil {
				return p
			}
		}
		if strings.HasPrefix(args[i], "--port=") {
			val := strings.TrimPrefix(args[i], "--port=")
			if p, err := strconv.Atoi(val); err == nil {
				return p
			}
		}
	}
	return 8080
}

// docArgOutput extracts the --output value from doc args. Defaults to "docs/api".
func docArgOutput(args []string) string {
	for i := 0; i < len(args); i++ {
		if args[i] == "--output" && i+1 < len(args) {
			return args[i+1]
		}
		if strings.HasPrefix(args[i], "--output=") {
			return strings.TrimPrefix(args[i], "--output=")
		}
	}
	return "docs/api"
}
