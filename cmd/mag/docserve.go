package main

import (
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"

	"github.com/chazu/maggie/vm"
)

const (
	// maxEvalBodyBytes bounds a playground eval request body.
	maxEvalBodyBytes = 10 * 1024
	// evalTimeout bounds how long a request waits for its evaluation.
	evalTimeout = 5 * time.Second
	// maxConcurrentEvals caps evaluations running at once. The VM has no way
	// to interrupt a running interpreter, so an evaluation that outlives its
	// timeout keeps running (and holding its slot) until it finishes; the cap
	// keeps runaway loops from piling up without bound.
	maxConcurrentEvals = 4
)

// evalSlots is the semaphore enforcing maxConcurrentEvals.
var evalSlots = make(chan struct{}, maxConcurrentEvals)

// errEvalBusy is returned when every evaluation slot is taken.
var errEvalBusy = errors.New("Evaluator busy: too many evaluations in progress (a previous evaluation may still be running)")

// errEvalTimeout is returned when an evaluation exceeds its timeout.
var errEvalTimeout = errors.New("Evaluation timed out")

// handleDocServe starts an HTTP server that serves generated documentation
// from docDir and provides an /api/eval endpoint for running Maggie expressions.
//
// /api/eval runs arbitrary code, so the server binds to loopback only and the
// endpoint rejects cross-origin and non-loopback-Host requests (see
// requireLocalRequest).
func handleDocServe(vmInst *vm.VM, docDir string, port int) {
	addr := docServeAddr(port)
	fmt.Printf("Documentation server running at http://localhost:%d (loopback only)\n", port)

	server := &http.Server{
		Addr:    addr,
		Handler: newDocServeMux(vmInst, docDir),
	}

	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		fmt.Printf("Server error: %v\n", err)
	}
}

// docServeAddr is the doc server's listen address: loopback only, since
// /api/eval is unauthenticated.
func docServeAddr(port int) string {
	return net.JoinHostPort("127.0.0.1", strconv.Itoa(port))
}

// newDocServeMux builds the doc server's routes: static docs plus /api/eval.
func newDocServeMux(vmInst *vm.VM, docDir string) *http.ServeMux {
	mux := http.NewServeMux()
	mux.Handle("/", http.FileServer(http.Dir(docDir)))
	mux.Handle("/api/eval", requireLocalRequest(makeEvalHandler(vmInst)))
	return mux
}

// requireLocalRequest guards the eval endpoint against other websites and
// other hosts: the Host header must name a loopback address (defeats DNS
// rebinding), and a present Origin header must be this server's own origin
// (defeats cross-site requests, including CORS "simple" text/plain POSTs,
// which browsers send without a preflight). The playground page fetches
// /api/eval same-origin, so it passes both checks.
func requireLocalRequest(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if !isLoopbackHostHeader(r.Host) {
			http.Error(w, "Forbidden: eval is only served to localhost", http.StatusForbidden)
			return
		}
		if origin := r.Header.Get("Origin"); origin != "" {
			u, err := url.Parse(origin)
			if err != nil || (u.Scheme != "http" && u.Scheme != "https") || !strings.EqualFold(u.Host, r.Host) {
				http.Error(w, "Forbidden: cross-origin eval request", http.StatusForbidden)
				return
			}
		}
		next.ServeHTTP(w, r)
	})
}

// isLoopbackHostHeader reports whether a Host header value (host or
// host:port) names localhost or a loopback IP.
func isLoopbackHostHeader(hostport string) bool {
	host := hostport
	if h, _, err := net.SplitHostPort(hostport); err == nil {
		host = h
	}
	host = strings.ToLower(strings.TrimSuffix(strings.Trim(host, "[]"), "."))
	return isLoopbackHost(host)
}

// makeEvalHandler returns an http.HandlerFunc that evaluates Maggie expressions.
func makeEvalHandler(vmInst *vm.VM) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}

		body, err := io.ReadAll(http.MaxBytesReader(w, r.Body, maxEvalBodyBytes))
		if err != nil {
			var tooLarge *http.MaxBytesError
			if errors.As(err, &tooLarge) {
				http.Error(w, fmt.Sprintf("Expression too large (limit %d bytes)", maxEvalBodyBytes), http.StatusRequestEntityTooLarge)
				return
			}
			http.Error(w, "Failed to read body", http.StatusBadRequest)
			return
		}

		expr := strings.TrimSpace(string(body))
		if expr == "" {
			http.Error(w, "Empty expression", http.StatusBadRequest)
			return
		}

		result, err := evalWithTimeout(vmInst, expr, evalTimeout)
		switch {
		case errors.Is(err, errEvalBusy):
			http.Error(w, err.Error(), http.StatusServiceUnavailable)
			return
		case err != nil:
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(result))
	}
}

// evalWithTimeout compiles and runs expr on its own per-call interpreter
// (vm.RunIsolated), so concurrent requests never share the main interpreter's
// stack. It returns errEvalBusy when maxConcurrentEvals evaluations are
// already running. On timeout it returns errEvalTimeout, but the evaluation
// itself cannot be interrupted: it keeps running, holding its slot, until it
// finishes.
func evalWithTimeout(vmInst *vm.VM, expr string, timeout time.Duration) (string, error) {
	type evalResult struct {
		value string
		err   error
	}

	select {
	case evalSlots <- struct{}{}:
	default:
		return "", errEvalBusy
	}

	ch := make(chan evalResult, 1)
	go func() {
		var res evalResult
		vmInst.RunIsolated(func() {
			defer func() {
				if r := recover(); r != nil {
					res = evalResult{err: fmt.Errorf("%v", r)}
				}
			}()
			res.value, res.err = evalExpression(vmInst, expr)
		})
		<-evalSlots // free the slot before reporting, so a caller's next eval can take it
		ch <- res
	}()

	timer := time.NewTimer(timeout)
	defer timer.Stop()
	select {
	case res := <-ch:
		return res.value, res.err
	case <-timer.C:
		return "", fmt.Errorf("%w after %v (the evaluation cannot be interrupted and continues in the background)", errEvalTimeout, timeout)
	}
}

// evalExpression compiles expr with the doIt compiler (a statement sequence
// with optional leading temps, answering the value of the last statement) and
// runs it. Must be called on a goroutine with a registered interpreter
// (RunIsolated).
func evalExpression(vmInst *vm.VM, expr string) (string, error) {
	methods, err := compileEvalSource(vmInst, expr)
	if err != nil {
		return "", fmt.Errorf("Compile error: %v", err)
	}
	result := vm.Nil
	for _, method := range methods {
		result, err = vmInst.ExecuteSafe(method, vm.Nil, nil)
		if err != nil {
			return "", err
		}
	}
	return formatEvalResult(vmInst, result), nil
}

// compileEvalSource compiles playground input into one or more doIt methods
// to run in order; the last one's value is the answer.
//
// Input that parses as a statement sequence compiles as a single doIt. Doc
// examples, however, often hold several independent snippets separated by a
// blank line, each declaring its own temps, or put one expression per line
// without separating periods. So the source is first split into snippets at
// blank lines followed by a temp declaration (splitEvalSnippets), and a
// snippet that does not compile as a whole is split further at line
// boundaries the parser itself confirms (splitEvalChunks). Source is never
// edited within a line, so string literals, comments and blocks are never
// corrupted. If splitting does not help, the snippet's compile error is
// returned.
func compileEvalSource(vmInst *vm.VM, src string) ([]*vm.CompiledMethod, error) {
	var methods []*vm.CompiledMethod
	for _, snippet := range splitEvalSnippets(src) {
		ms, err := compileEvalSnippet(vmInst, snippet)
		if err != nil {
			return nil, err
		}
		methods = append(methods, ms...)
	}
	if len(methods) == 0 {
		return nil, errors.New("empty expression")
	}
	return methods, nil
}

// compileEvalSnippet compiles one snippet as a doIt, falling back to one
// doIt per parser-confirmed chunk (see compileEvalSource).
func compileEvalSnippet(vmInst *vm.VM, src string) ([]*vm.CompiledMethod, error) {
	method, err := vmInst.CompileExpression(src)
	if err == nil && method != nil {
		return []*vm.CompiledMethod{method}, nil
	}
	if err == nil {
		err = errors.New("compiler returned nil")
	}
	chunks := splitEvalChunks(vmInst, src)
	if len(chunks) < 2 {
		return nil, err
	}
	methods := make([]*vm.CompiledMethod, 0, len(chunks))
	for _, chunk := range chunks {
		m, cerr := vmInst.CompileExpression(chunk)
		if cerr != nil || m == nil {
			return nil, err
		}
		methods = append(methods, m)
	}
	return methods, nil
}

// splitEvalSnippets splits src before each temp declaration ("| x |") that
// follows a blank line after earlier code. Temps may only open a doIt, so
// such a line always starts a new, independent snippet.
func splitEvalSnippets(src string) []string {
	var snippets []string
	var cur strings.Builder
	prevBlank := false
	for _, line := range strings.Split(src, "\n") {
		trimmed := strings.TrimSpace(line)
		if prevBlank && strings.HasPrefix(trimmed, "|") && strings.TrimSpace(cur.String()) != "" {
			snippets = append(snippets, cur.String())
			cur.Reset()
		}
		cur.WriteString(line)
		cur.WriteByte('\n')
		prevBlank = trimmed == ""
	}
	if strings.TrimSpace(cur.String()) != "" {
		snippets = append(snippets, cur.String())
	}
	return snippets
}

// splitEvalChunks splits src at line boundaries where the text so far
// compiles on its own but stops compiling once the next line is appended —
// i.e. where the next line starts a new statement rather than continuing the
// current one (a keyword continuation, a block body, or a trailing comment
// keeps the chunk together).
func splitEvalChunks(vmInst *vm.VM, src string) []string {
	compiles := func(s string) bool {
		m, err := vmInst.CompileExpression(s)
		return err == nil && m != nil
	}
	var chunks []string
	cur := ""
	for _, line := range strings.Split(src, "\n") {
		if strings.TrimSpace(line) == "" || strings.TrimSpace(cur) == "" {
			cur += line + "\n"
			continue
		}
		if compiles(cur) && !compiles(cur+line) {
			chunks = append(chunks, cur)
			cur = ""
		}
		cur += line + "\n"
	}
	if strings.TrimSpace(cur) != "" {
		chunks = append(chunks, cur)
	}
	return chunks
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
		return vmInst.Registry().GetStringContent(v)
	default:
		// Try sending printString
		result, ok := safeSend(vmInst, v, "printString")
		if ok && vm.IsStringValue(result) {
			return vmInst.Registry().GetStringContent(result)
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
