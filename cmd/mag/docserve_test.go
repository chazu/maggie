package main

import (
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Request guard (loopback bind, Host and Origin checks)
// ---------------------------------------------------------------------------

func TestDocServe_BindsLoopback(t *testing.T) {
	if got := docServeAddr(8080); got != "127.0.0.1:8080" {
		t.Errorf("docServeAddr(8080) = %q, want 127.0.0.1:8080", got)
	}
}

func TestDocServe_isLoopbackHostHeader(t *testing.T) {
	for host, want := range map[string]bool{
		"localhost":              true,
		"localhost:8080":         true,
		"LOCALHOST:8080":         true,
		"localhost.:8080":        true,
		"127.0.0.1:8080":         true,
		"127.0.0.2":              true,
		"[::1]:8080":             true,
		"":                       false,
		"evil.example:8080":      false,
		"127.0.0.1.evil.example": false,
		"192.168.1.10:8080":      false,
		"0.0.0.0:8080":           false,
	} {
		if got := isLoopbackHostHeader(host); got != want {
			t.Errorf("isLoopbackHostHeader(%q) = %v, want %v", host, got, want)
		}
	}
}

func TestDocServe_EvalRequestGuard(t *testing.T) {
	mux := newDocServeMux(newTestVM(t), t.TempDir())

	tests := []struct {
		name   string
		method string
		host   string
		origin string
		body   string
		want   int
	}{
		{"same-origin playground fetch", "POST", "localhost:8080", "http://localhost:8080", "3 + 4", http.StatusOK},
		{"no Origin (curl)", "POST", "127.0.0.1:8080", "", "3 + 4", http.StatusOK},
		{"cross-site simple request", "POST", "localhost:8080", "http://evil.example", "3 + 4", http.StatusForbidden},
		{"other local port", "POST", "localhost:8080", "http://localhost:9999", "3 + 4", http.StatusForbidden},
		{"opaque origin", "POST", "localhost:8080", "null", "3 + 4", http.StatusForbidden},
		{"DNS rebinding Host", "POST", "evil.example:8080", "http://evil.example:8080", "3 + 4", http.StatusForbidden},
		{"LAN Host", "POST", "192.168.1.10:8080", "", "3 + 4", http.StatusForbidden},
		{"preflight", "OPTIONS", "localhost:8080", "http://evil.example", "", http.StatusForbidden},
		{"GET", "GET", "localhost:8080", "", "", http.StatusMethodNotAllowed},
		{"oversized body", "POST", "localhost:8080", "", strings.Repeat("1", maxEvalBodyBytes+1), http.StatusRequestEntityTooLarge},
		{"body at limit", "POST", "localhost:8080", "", strings.Repeat(" ", maxEvalBodyBytes-1) + "7", http.StatusOK},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			req := httptest.NewRequest(tt.method, "/api/eval", strings.NewReader(tt.body))
			req.Host = tt.host
			req.Header.Set("Content-Type", "text/plain")
			if tt.origin != "" {
				req.Header.Set("Origin", tt.origin)
			}
			rec := httptest.NewRecorder()
			mux.ServeHTTP(rec, req)
			if rec.Code != tt.want {
				t.Fatalf("status = %d, want %d (body %q)", rec.Code, tt.want, rec.Body.String())
			}
			if acao := rec.Header().Get("Access-Control-Allow-Origin"); acao != "" {
				t.Errorf("unexpected Access-Control-Allow-Origin: %q", acao)
			}
			if tt.want == http.StatusOK && rec.Body.String() != "7" {
				t.Errorf("body = %q, want 7", rec.Body.String())
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Evaluation: doIt compilation of doc examples
// ---------------------------------------------------------------------------

func TestDocServe_EvalExamples(t *testing.T) {
	v := newTestVM(t)
	tests := []struct{ name, src, want string }{
		{"single expression", "3 + 4", "7"},
		{"answers last statement", "1. 2. 3", "3"},
		{"trailing period", "3 + 4.", "7"},
		{"temps", "| x |\nx := 3.\nx + 4", "7"},
		{"typed temps", "| x <Integer> |\nx := 3.\nx + 4", "7"},
		{"comment-only lines", "\"Add two numbers\"\n3 + 4\n\"=> 7\"", "7"},
		{"period inside string", "'a. b' size", "4"},
		{"multi-line keyword message", "#(1 2 3 4 5)\n    inject: 0\n    into: [:a :b | a + b]", "15"},
		{"line starting with ]", "#(1 2 3) inject: 0 into: [:a :b |\n    a + b\n]", "6"},
		{"line starting with )", "(3 +\n4\n) * 2", "14"},
		{"cascade lines", "3\n    + 4;\n    * 10", "30"},
		{"one expression per line", "3.5 rounded    \"=> 4\"\n3.4 rounded    \"=> 3\"", "3"},
		{"independent snippets with own temps", "#(1 2 3) size \"=> 3\"\n\n| s |\ns := 10.\ns + 1", "11"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := evalWithTimeout(v, tt.src, 5*time.Second)
			if err != nil {
				t.Fatalf("eval(%q): %v", tt.src, err)
			}
			if got != tt.want {
				t.Errorf("eval(%q) = %q, want %q", tt.src, got, tt.want)
			}
		})
	}

	if _, err := evalWithTimeout(v, "3 +", 5*time.Second); err == nil || !strings.Contains(err.Error(), "Compile error") {
		t.Errorf("eval of bad source: err = %v, want a compile error", err)
	}
}

// Every example in a core (non-guide) class docstring that is Maggie code
// must compile through the playground path. Class-definition snippets
// (method: ...) are not evaluable and are skipped.
func TestDocServe_CoreExamplesCompile(t *testing.T) {
	v := newTestVM(t)
	n := 0
	for _, cls := range v.Classes.All() {
		if isGuideClass(cls.Name) {
			continue
		}
		for _, vt := range []*vm.VTable{cls.VTable, cls.ClassVTable} {
			if vt == nil {
				continue
			}
			for _, cat := range collectMethods(vt, v.Selectors, false) {
				for _, m := range cat.Methods {
					for _, sec := range m.DocSections {
						if sec.Type != DocExample || strings.Contains(sec.Content, "method:") {
							continue
						}
						n++
						if _, err := compileEvalSource(v, strings.TrimSpace(sec.Content)); err != nil {
							t.Errorf("%s>>%s example does not compile: %v\n%s", cls.Name, m.Selector, err, sec.Content)
						}
					}
				}
			}
		}
	}
	if n == 0 {
		t.Fatal("no core-class examples found")
	}
}

// ---------------------------------------------------------------------------
// Evaluation: isolation, timeout, concurrency cap
// ---------------------------------------------------------------------------

func TestDocServe_ConcurrentEvalsIsolated(t *testing.T) {
	v := newTestVM(t)
	var wg sync.WaitGroup
	errs := make(chan string, 64)
	for g := 0; g < maxConcurrentEvals; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < 10; i++ {
				got, err := evalWithTimeout(v, "| s | s := 0. 1 to: 2000 do: [:i | s := s + i]. s", 10*time.Second)
				if err != nil || got != "2001000" {
					errs <- got + " / " + errString(err)
					return
				}
			}
		}()
	}
	wg.Wait()
	close(errs)
	for e := range errs {
		t.Errorf("concurrent eval: %s", e)
	}
}

func TestDocServe_EvalTimeoutAndBusy(t *testing.T) {
	v := newTestVM(t)

	_, err := evalWithTimeout(v, "Process sleep: 300. 1", 20*time.Millisecond)
	if !errors.Is(err, errEvalTimeout) {
		t.Fatalf("err = %v, want errEvalTimeout", err)
	}
	// Other evaluations still work while the runaway holds its slot.
	if got, err := evalWithTimeout(v, "3 + 4", 5*time.Second); err != nil || got != "7" {
		t.Fatalf("eval after timeout = %q, %v", got, err)
	}

	// With every slot taken, new evaluations are refused rather than queued.
	held := 0
fill:
	for {
		select {
		case evalSlots <- struct{}{}:
			held++
		default:
			break fill
		}
	}
	_, err = evalWithTimeout(v, "3 + 4", 5*time.Second)
	for ; held > 0; held-- {
		<-evalSlots
	}
	if !errors.Is(err, errEvalBusy) {
		t.Fatalf("err = %v, want errEvalBusy", err)
	}
}

func errString(err error) string {
	if err == nil {
		return "<nil>"
	}
	return err.Error()
}
