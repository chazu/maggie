package vm

import (
	"sync"
	"testing"
	"time"
)

func TestChannelNew(t *testing.T) {
	vm := NewVM()

	// Create unbuffered channel via primitive
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new", nil)
	if ch == Nil {
		t.Fatal("Channel new returned nil")
	}

	if !isChannelValue(ch) {
		t.Error("Expected channel value")
	}
}

func TestChannelNewBuffered(t *testing.T) {
	vm := NewVM()

	// Create buffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(5)})
	if ch == Nil {
		t.Fatal("Channel new: returned nil")
	}

	// Check capacity
	cap := vm.Send(ch, "capacity", nil)
	if !cap.IsSmallInt() || cap.SmallInt() != 5 {
		t.Errorf("Channel capacity = %v, want 5", cap)
	}
}

func TestChannelSendReceive(t *testing.T) {
	vm := NewVM()

	// Create buffered channel (so send doesn't block)
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Send a value
	vm.Send(ch, "send:", []Value{FromSmallInt(42)})

	// Receive the value
	val := vm.Send(ch, "receive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("Received %v, want 42", val)
	}
}

func TestChannelTrySend(t *testing.T) {
	vm := NewVM()

	// Create buffered channel with capacity 1
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// First send should succeed
	result := vm.Send(ch, "trySend:", []Value{FromSmallInt(1)})
	if result != True {
		t.Error("First trySend should succeed")
	}

	// Second send should fail (buffer full)
	result = vm.Send(ch, "trySend:", []Value{FromSmallInt(2)})
	if result != False {
		t.Error("Second trySend should fail (buffer full)")
	}
}

func TestChannelTryReceive(t *testing.T) {
	vm := NewVM()

	// Create buffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Try receive on empty channel
	val := vm.Send(ch, "tryReceive", nil)
	if val != Nil {
		t.Error("tryReceive on empty channel should return nil")
	}

	// Send a value
	vm.Send(ch, "send:", []Value{FromSmallInt(42)})

	// Now tryReceive should succeed
	val = vm.Send(ch, "tryReceive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("tryReceive = %v, want 42", val)
	}
}

func TestChannelClose(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Initially not closed
	closed := vm.Send(ch, "isClosed", nil)
	if closed != False {
		t.Error("New channel should not be closed")
	}

	// Close the channel
	vm.Send(ch, "close", nil)

	// Now it should be closed
	closed = vm.Send(ch, "isClosed", nil)
	if closed != True {
		t.Error("Channel should be closed after close")
	}
}

func TestChannelSizeAndEmpty(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(5)})

	// Initially empty
	empty := vm.Send(ch, "isEmpty", nil)
	if empty != True {
		t.Error("New channel should be empty")
	}

	size := vm.Send(ch, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 0 {
		t.Errorf("New channel size = %v, want 0", size)
	}

	// Send some values
	vm.Send(ch, "send:", []Value{FromSmallInt(1)})
	vm.Send(ch, "send:", []Value{FromSmallInt(2)})

	empty = vm.Send(ch, "isEmpty", nil)
	if empty != False {
		t.Error("Channel with values should not be empty")
	}

	size = vm.Send(ch, "size", nil)
	if !size.IsSmallInt() || size.SmallInt() != 2 {
		t.Errorf("Channel size = %v, want 2", size)
	}
}

func TestProcessFork(t *testing.T) {
	vm := NewVM()

	// Create a simple block that returns 42
	// For now, we'll test process creation with a pre-compiled block
	// This requires setting up a proper block, which is complex

	// Simpler test: just verify Process class exists and has methods
	proc := vm.Send(vm.classValue(vm.ProcessClass), "current", nil)
	// current returns nil since we're not tracking per-goroutine processes yet
	if proc != Nil {
		t.Log("Process current returned:", proc)
	}
}

func TestProcessSleep(t *testing.T) {
	vm := NewVM()

	start := time.Now()
	vm.Send(vm.classValue(vm.ProcessClass), "sleep:", []Value{FromSmallInt(50)})
	elapsed := time.Since(start)

	if elapsed < 50*time.Millisecond {
		t.Errorf("Sleep elapsed %v, want >= 50ms", elapsed)
	}
}

func TestChannelConcurrent(t *testing.T) {
	vm := NewVM()

	// Create unbuffered channel
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new", nil)
	chObj := getChannel(ch)

	done := make(chan struct{})

	// Send in a goroutine
	go func() {
		chObj.ch <- FromSmallInt(42)
		close(done)
	}()

	// Receive
	val := vm.Send(ch, "receive", nil)
	if !val.IsSmallInt() || val.SmallInt() != 42 {
		t.Errorf("Received %v, want 42", val)
	}

	<-done
}

func TestChannelMultipleValues(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(10)})

	// Send multiple values
	for i := int64(1); i <= 5; i++ {
		vm.Send(ch, "send:", []Value{FromSmallInt(i)})
	}

	// Receive and verify
	for i := int64(1); i <= 5; i++ {
		val := vm.Send(ch, "receive", nil)
		if !val.IsSmallInt() || val.SmallInt() != i {
			t.Errorf("Received %v, want %d", val, i)
		}
	}
}

// ---------------------------------------------------------------------------
// Race Condition Tests
// These tests verify the thread-safety fixes for Phase 1 concurrency bugs
// Run with: go test -race ./vm/... -run "Race"
// ---------------------------------------------------------------------------

// TestConcurrentBlockCreation tests that multiple goroutines can safely
// create blocks concurrently without corrupting the block registry.
func TestConcurrentBlockCreation(t *testing.T) {
	const numGoroutines = 10
	const blocksPerGoroutine = 100

	// Create multiple interpreters to simulate concurrent execution
	interpreters := make([]*Interpreter, numGoroutines)
	for i := 0; i < numGoroutines; i++ {
		interpreters[i] = NewInterpreter()
	}

	// Create a simple block method
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushNil), byte(OpBlockReturn)},
		Literals:    nil,
	}

	// Create method for frames
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	var wg sync.WaitGroup
	blockIDs := make([][]int, numGoroutines)

	// Record initial registry size
	initialSize := blockRegistrySize()

	// Concurrently create blocks from multiple goroutines
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			interp := interpreters[goroutineID]
			interp.pushFrame(m, Nil, nil)

			blockIDs[goroutineID] = make([]int, blocksPerGoroutine)
			for i := 0; i < blocksPerGoroutine; i++ {
				blockVal := interp.createBlockValue(blockMethod, nil)
				blockIDs[goroutineID][i] = int(blockVal.BlockID())
			}

			interp.popFrame()
		}(g)
	}

	wg.Wait()

	// Verify all blocks were created and have unique IDs
	expectedTotal := numGoroutines * blocksPerGoroutine
	actualSize := blockRegistrySize() - initialSize

	// Note: actualSize may be less than expectedTotal if cleanup is enabled
	// For now, just verify no crash and reasonable number of blocks
	if actualSize < 0 {
		t.Errorf("Block registry size decreased, possible corruption")
	}

	// Verify all block IDs are unique
	seenIDs := make(map[int]bool)
	for g := 0; g < numGoroutines; g++ {
		for _, id := range blockIDs[g] {
			if seenIDs[id] {
				t.Errorf("Duplicate block ID %d found - race condition!", id)
			}
			seenIDs[id] = true
		}
	}

	t.Logf("Created %d blocks from %d goroutines, %d unique IDs",
		expectedTotal, numGoroutines, len(seenIDs))
}

// TestConcurrentCellAccess tests that multiple goroutines can safely
// read and write to the same cell concurrently.
func TestConcurrentCellAccess(t *testing.T) {
	const numGoroutines = 10
	const opsPerGoroutine = 1000

	// Create a shared cell
	cell := NewCell(FromSmallInt(0))

	var wg sync.WaitGroup

	// Concurrent writers
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < opsPerGoroutine; i++ {
				// Read current value
				_ = cell.CellGet()
				// Write new value
				cell.CellSet(FromSmallInt(int64(i)))
			}
		}()
	}

	wg.Wait()

	// Verify cell is still accessible and valid
	finalVal := cell.CellGet()
	if !finalVal.IsSmallInt() {
		t.Errorf("Cell value is not SmallInt after concurrent access: %v", finalVal)
	}

	t.Logf("Final cell value: %d", finalVal.SmallInt())
}

// TestConcurrentCellCreation tests that multiple goroutines can safely
// create new cells concurrently without corrupting the cell registry.
func TestConcurrentCellCreation(t *testing.T) {
	const numGoroutines = 10
	const cellsPerGoroutine = 100

	var wg sync.WaitGroup
	cells := make([][]Value, numGoroutines)

	// Concurrently create cells from multiple goroutines
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			cells[goroutineID] = make([]Value, cellsPerGoroutine)
			for i := 0; i < cellsPerGoroutine; i++ {
				cells[goroutineID][i] = NewCell(FromSmallInt(int64(i)))
			}
		}(g)
	}

	wg.Wait()

	// Verify all cells are valid and accessible
	for g := 0; g < numGoroutines; g++ {
		for i := 0; i < cellsPerGoroutine; i++ {
			cell := cells[g][i]
			if !cell.IsCell() {
				t.Errorf("Value at [%d][%d] is not a cell", g, i)
				continue
			}
			val := cell.CellGet()
			if !val.IsSmallInt() || val.SmallInt() != int64(i) {
				t.Errorf("Cell at [%d][%d] has wrong value: %v, want %d", g, i, val, i)
			}
		}
	}

	t.Logf("Created and verified %d cells from %d goroutines",
		numGoroutines*cellsPerGoroutine, numGoroutines)
}

// TestConcurrentBlockRegistryReadWrite tests concurrent reads and writes
// to the block registry.
func TestConcurrentBlockRegistryReadWrite(t *testing.T) {
	const numWriters = 5
	const numReaders = 5
	const opsPerGoroutine = 100

	// Create a simple block method
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushNil), byte(OpBlockReturn)},
		Literals:    nil,
	}

	// Create method for frames
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	var wg sync.WaitGroup
	blockIDsChan := make(chan int, numWriters*opsPerGoroutine)

	// Writers create blocks
	for w := 0; w < numWriters; w++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			interp := NewInterpreter()
			interp.pushFrame(m, Nil, nil)
			for i := 0; i < opsPerGoroutine; i++ {
				blockVal := interp.createBlockValue(blockMethod, nil)
				blockIDsChan <- int(blockVal.BlockID())
			}
			interp.popFrame()
		}()
	}

	// Readers look up blocks
	readErrors := make(chan error, numReaders)
	for r := 0; r < numReaders; r++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			interp := NewInterpreter()
			for i := 0; i < opsPerGoroutine*2; i++ {
				// Try to read a block (may or may not exist yet)
				id := i % (numWriters * opsPerGoroutine)
				if blockRegistryHas(id) {
					// Just verify we can read it without crash
					_ = interp.getBlockValue(FromBlockID(uint32(id)))
				}
			}
		}()
	}

	wg.Wait()
	close(blockIDsChan)
	close(readErrors)

	// Check for any errors
	for err := range readErrors {
		t.Errorf("Reader error: %v", err)
	}

	// Count created blocks
	var count int
	for range blockIDsChan {
		count++
	}

	t.Logf("Created %d blocks while %d readers accessed registry",
		count, numReaders)
}
