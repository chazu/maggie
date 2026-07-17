package server

import (
	"strings"
	"testing"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
)

// BenchmarkServerEvalParallel is a MEASUREMENT SPIKE for the review's
// server-request-parallelism question, not a normal benchmark. Every Evaluate
// routes through VMWorker.Do -> a single VM goroutine (server/vm_worker.go
// loop()), because the VM's registries are not thread-safe. So server request
// throughput is expected to be FLAT as concurrency rises: aggregate ns/op stays
// ~constant across -cpu values instead of falling, i.e. extra cores buy no
// throughput for a Maggie language/web server.
//
// Reproduce the scaling curve:
//
//	go test -run=^$ -bench=BenchmarkServerEvalParallel -benchtime=2s -cpu=1,2,4,8 ./server/
//
// Interpretation: if ns/op is ~identical at -cpu=1 and -cpu=8, the server is
// serialized (the current state). If it dropped ~Nx at -cpu=N, requests would
// be running in parallel. See docs/spikes/ for the captured numbers + analysis.
func BenchmarkServerEvalParallel(b *testing.B) {
	svc := newTestEvalService()
	// A long arithmetic chain (~400 primitive sends per request) gives real
	// per-request interpreter work so the single-goroutine bottleneck is visible
	// rather than swamped by channel/RPC overhead. Uses only Go-primitive sends
	// (+), which the eval VM resolves.
	src := "1" + strings.Repeat(" + 1", 400)
	// Warm up: compile once, build inline caches.
	if resp, err := svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{Source: src})); err != nil || !resp.Msg.Success {
		b.Fatalf("warmup eval failed: err=%v success=%v", err, resp.Msg.Success)
	}

	b.ResetTimer()
	b.RunParallel(func(pb *testing.PB) {
		for pb.Next() {
			svc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{Source: src}))
		}
	})
}
