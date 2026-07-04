# Spike: server request parallelism

**Question (review #2):** does the Maggie language/web server scale request
throughput with cores, or is it serialized?

**Answer: fully serialized.** Every request routes through `VMWorker.Do` →
`vm_worker.go loop()`, a single goroutine, because the VM's registries are not
thread-safe (`http_primitives.go:503` and the whole `vm/dispatch.go` funnel say
so). Added cores buy nothing.

## Measurement

`BenchmarkServerEvalParallel` (server/parallelism_spike_test.go) fires the same
Evaluate request from `-cpu` concurrent goroutines. Workload: a ~400-term
arithmetic chain (`1 + 1 + … + 1`), compiled + evaluated per request through the
real EvalService → VMWorker path.

```
go test -run=^$ -bench=BenchmarkServerEvalParallel -benchtime=2s -cpu=1,2,4,8 ./server/

BenchmarkServerEvalParallel      312   7715116 ns/op
BenchmarkServerEvalParallel-2    318   7501093 ns/op
BenchmarkServerEvalParallel-4    318   7562614 ns/op
BenchmarkServerEvalParallel-8    315   7604787 ns/op
```

**ns/op is flat across 1→8 cores.** Parallel execution would show ns/op falling
~Nx at `-cpu=N` (≈950k ns/op at 8). It doesn't move — the single worker
goroutine is the hard ceiling. (The absolute time is compile-dominated because
each request recompiles the source; irrelevant to the scaling conclusion —
compilation runs on the same serialized worker.)

## Why, and what unblocks it

The serialization is a direct consequence of the NaN-boxed-Value + side-registry
design (review #1): because heap Values live in `map`-backed registries behind
locks and the interpreter shares one set, concurrent mutators would race. The
`VMWorker`/`Dispatch` funnel exists to paper over that.

So request parallelism is **blocked on the Value/GC thread-safety decision**:
a thread-safe (or per-request-isolated) heap is the prerequisite. Once that
exists, each request can run on its own registered interpreter and the funnel
retires. Sequencing: settle the Value-representation prototype first (see the
companion Value/GC spike), then parallelize the server.

## Note surfaced while measuring

While writing the workload I saw lib selectors (`factorial`, `max:`) fail in the
benchmark. Follow-up review clarified this is a **test-harness artifact, not a
production bug**: `server/test_helpers_test.go` builds the shared test VM with
`vm.NewVM()` and never loads the embedded image, so only Go primitives resolve
there — a real `mag --serve` loads the full image and has the lib methods.

The genuine bug it exposed (now FIXED): an unhandled Maggie exception was
reported as a garbled raw Value in `ErrorMessage` (`{9222246137082150913 0x…}`).
`VMWorker.execute` now renders it via `VM.DescribePanic` → the exception's
message text (e.g. "Message not understood: foo"). Remaining LOW follow-up: load
the image in the server tests' `TestMain` so server-side lib dispatch is
actually exercised in CI.
