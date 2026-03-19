package vm

import (
	"fmt"
	"io"
	"sort"
	"sync"
	"sync/atomic"
	"time"
)

// namedMethod is satisfied by all primitive method types (Method0, Method1, etc.)
// and CompiledMethod. Used by the interpreter to get the method name for
// primitive tagging without adding Name() to the minimal Method interface.
type namedMethod interface {
	Name() string
}

// ---------------------------------------------------------------------------
// SamplingProfiler: Wall-clock sampling profiler for the Maggie VM
// ---------------------------------------------------------------------------

// SamplingProfiler periodically snapshots interpreter call stacks and
// aggregates them into a trie for flamegraph-compatible output.
type SamplingProfiler struct {
	vm       *VM
	interval time.Duration

	mu       sync.Mutex
	root     *trieNode
	samples  uint64
	dropped  uint64

	stop chan struct{}
	done chan struct{}
}

// trieNode is a node in the aggregation trie. Each path from root to leaf
// represents a unique call stack, with count tracking how many samples
// terminated at that node.
type trieNode struct {
	label    string
	count    uint64
	children map[string]*trieNode
}

// SamplingProfilerStats holds aggregate profiling statistics.
type SamplingProfilerStats struct {
	TotalSamples uint64
	Dropped      uint64
}

// NewSamplingProfiler creates a new sampling profiler attached to the given VM.
func NewSamplingProfiler(vm *VM, interval time.Duration) *SamplingProfiler {
	return &SamplingProfiler{
		vm:       vm,
		interval: interval,
		root:     &trieNode{children: make(map[string]*trieNode)},
	}
}

// Start begins the sampling loop in a background goroutine.
func (sp *SamplingProfiler) Start() {
	sp.stop = make(chan struct{})
	sp.done = make(chan struct{})
	go sp.loop()
}

// Stop signals the sampling goroutine to stop and waits for it to finish.
func (sp *SamplingProfiler) Stop() {
	if sp.stop != nil {
		close(sp.stop)
		<-sp.done
		sp.stop = nil
		sp.done = nil
	}
}

// Reset clears all accumulated samples.
func (sp *SamplingProfiler) Reset() {
	sp.mu.Lock()
	defer sp.mu.Unlock()
	sp.root = &trieNode{children: make(map[string]*trieNode)}
	atomic.StoreUint64(&sp.samples, 0)
	atomic.StoreUint64(&sp.dropped, 0)
}

// Stats returns aggregate profiling statistics.
func (sp *SamplingProfiler) Stats() SamplingProfilerStats {
	return SamplingProfilerStats{
		TotalSamples: atomic.LoadUint64(&sp.samples),
		Dropped:      atomic.LoadUint64(&sp.dropped),
	}
}

// WriteFoldedStacks writes the aggregated stacks in folded-stack format
// (compatible with flamegraph.pl and speedscope).
// Format: frame1;frame2;...;frameN count\n
func (sp *SamplingProfiler) WriteFoldedStacks(w io.Writer) error {
	sp.mu.Lock()
	defer sp.mu.Unlock()
	return sp.writeTrie(w, sp.root, nil)
}

func (sp *SamplingProfiler) writeTrie(w io.Writer, node *trieNode, path []string) error {
	if node.count > 0 && len(path) > 0 {
		for i, frame := range path {
			if i > 0 {
				if _, err := fmt.Fprint(w, ";"); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprint(w, frame); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprintf(w, " %d\n", node.count); err != nil {
			return err
		}
	}
	// Sort children for deterministic output
	keys := make([]string, 0, len(node.children))
	for k := range node.children {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		if err := sp.writeTrie(w, node.children[k], append(path, k)); err != nil {
			return err
		}
	}
	return nil
}

// loop is the main sampling goroutine.
func (sp *SamplingProfiler) loop() {
	defer close(sp.done)
	ticker := time.NewTicker(sp.interval)
	defer ticker.Stop()

	for {
		select {
		case <-sp.stop:
			return
		case <-ticker.C:
			sp.sampleAll()
		}
	}
}

// sampleAll takes a snapshot of all interpreter stacks.
func (sp *SamplingProfiler) sampleAll() {
	// Sample main interpreter
	sp.sampleInterpreter(sp.vm.interpreter)

	// Sample all forked interpreters
	sp.vm.interpreters.Range(func(key, value interface{}) bool {
		if interp, ok := value.(*Interpreter); ok {
			sp.sampleInterpreter(interp)
		}
		return true
	})
}

// sampleInterpreter captures one interpreter's stack. Wrapped in recover()
// to handle torn reads gracefully.
func (sp *SamplingProfiler) sampleInterpreter(interp *Interpreter) {
	defer func() {
		if r := recover(); r != nil {
			atomic.AddUint64(&sp.dropped, 1)
		}
	}()

	fp := interp.fp
	if fp < 0 {
		return // idle interpreter
	}

	// Build stack from bottom to top (root of call tree first)
	stack := make([]string, 0, fp+1)
	for i := 0; i <= fp; i++ {
		frame := interp.frames[i]
		if frame == nil {
			continue
		}
		label := sp.frameLabel(frame)
		if label != "" {
			stack = append(stack, label)
		}
	}

	// Check if there's an active primitive at the top
	if ptr := atomic.LoadPointer(&interp.activePrimitiveName); ptr != nil {
		name := *(*string)(ptr)
		stack = append(stack, "[primitive "+name+"]")
	}

	if len(stack) == 0 {
		return
	}

	// Insert into trie
	sp.mu.Lock()
	node := sp.root
	for _, frame := range stack {
		child, ok := node.children[frame]
		if !ok {
			child = &trieNode{label: frame, children: make(map[string]*trieNode)}
			node.children[frame] = child
		}
		node = child
	}
	node.count++
	sp.mu.Unlock()

	atomic.AddUint64(&sp.samples, 1)
}

// frameLabel returns the display label for a call frame.
func (sp *SamplingProfiler) frameLabel(f *CallFrame) string {
	if f.Block != nil {
		// Block frame: find the outer method
		outer := f.Block.Outer
		if outer != nil {
			className := "UnknownClass"
			if cls := outer.Class(); cls != nil {
				className = cls.Name
			}
			return "[block in " + className + ">>" + outer.Name() + "]"
		}
		return "[block]"
	}
	if f.Method != nil {
		className := "UnknownClass"
		if cls := f.Method.Class(); cls != nil {
			className = cls.Name
		}
		return className + ">>" + f.Method.Name()
	}
	return ""
}
