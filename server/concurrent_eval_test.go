package server

import (
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/chazu/maggie/compiler"
	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/vm"
)

// TestConcurrentEvaluateRace is the Stage-5a experiment
// (docs/plans/2026-07-04-stage5-server-parallelism.md): can many requests
// compile + execute concurrently against ONE shared VM without the
// single-goroutine VMWorker funnel?
//
// Each goroutine runs its Evaluate on its own per-request interpreter via
// vm.RunIsolated — mirroring the production fork path — and bypasses
// worker.Do entirely, so nothing serializes the callers. A clean pass under
// the race detector means Evaluate can become a concurrent reader in the gate
// design. Reproduce:
//
//	go test -race -run TestConcurrentEvaluateRace ./server/
//
// Uses pure arithmetic (Go-primitive sends only), so no image/lib load is
// required for the VM to resolve the workload.
func TestConcurrentEvaluateRace(t *testing.T) {
	v := vm.NewVM()
	v.UseGoCompiler(compiler.Compile)
	svc := NewEvalService(nil, NewHandleStore(nil), nil)

	// A moderately long arithmetic chain: real per-request compile + interpreter
	// work, so overlap is genuine rather than swamped by setup.
	src := "1" + strings.Repeat(" + 1", 200)

	// Warm up once single-threaded: builds inline caches on the shared doIt
	// method's send sites before the goroutines pile on (concurrent IC fill is
	// itself covered by the audit, but warming keeps the assertion crisp).
	v.RunIsolated(func() {
		if resp := svc.evaluate(v, src); !resp.Success {
			t.Fatalf("warmup eval failed: %s", resp.ErrorMessage)
		}
	})

	const G, N = 32, 200
	var wg sync.WaitGroup
	wg.Add(G)
	start := make(chan struct{})
	errs := make(chan string, G)

	for g := 0; g < G; g++ {
		go func() {
			defer wg.Done()
			<-start
			for i := 0; i < N; i++ {
				var resp *maggiev1.EvaluateResponse
				v.RunIsolated(func() {
					resp = svc.evaluate(v, src)
				})
				if !resp.Success {
					errs <- resp.ErrorMessage
					return
				}
				if resp.Result != "201" {
					errs <- fmt.Sprintf("wrong result: got %q want %q", resp.Result, "201")
					return
				}
			}
		}()
	}
	close(start)
	wg.Wait()
	close(errs)

	if msg, ok := <-errs; ok {
		t.Fatalf("concurrent evaluate failed: %s", msg)
	}
}

// TestConcurrentDistinctExpressionsRace compiles DIFFERENT source per goroutine
// (distinct constants, distinct string literals) so every request drives a
// fresh parse + compile + literal allocation, maximizing pressure on the shared
// SelectorTable / SymbolTable / ObjectRegistry the compiler writes into. String
// literals also exercise concurrent heap-object (kindString) allocation on the
// post-migration pointer heap.
func TestConcurrentDistinctExpressionsRace(t *testing.T) {
	v := vm.NewVM()
	v.UseGoCompiler(compiler.Compile)
	svc := NewEvalService(nil, NewHandleStore(nil), nil)

	const G, N = 24, 150
	var wg sync.WaitGroup
	wg.Add(G)
	start := make(chan struct{})
	errs := make(chan string, G)

	for g := 0; g < G; g++ {
		go func(g int) {
			defer wg.Done()
			<-start
			for i := 0; i < N; i++ {
				// Unique per (g,i): a bare String literal. Compiling it allocates a
				// fresh kindString heap object on the shared pointer heap and interns
				// into the shared tables; executing it just returns the literal.
				// (Avoids lib selectors like `,` that a bare, image-less test VM does
				// not resolve — a documented harness artifact, not a VM limitation.)
				want := fmt.Sprintf("'g%di%d'", g, i)
				src := want // a String literal renders back to itself via formatValue
				var resp *maggiev1.EvaluateResponse
				v.RunIsolated(func() {
					resp = svc.evaluate(v, src)
				})
				if !resp.Success {
					errs <- resp.ErrorMessage
					return
				}
				if resp.Result != want {
					errs <- fmt.Sprintf("g%d i%d: got %q want %q", g, i, resp.Result, want)
					return
				}
			}
		}(g)
	}
	close(start)
	wg.Wait()
	close(errs)

	if msg, ok := <-errs; ok {
		t.Fatalf("concurrent distinct-expression evaluate failed: %s", msg)
	}
}
