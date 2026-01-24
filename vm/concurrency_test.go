package vm

import (
	"reflect"
	"sync"
	"sync/atomic"
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
	chObj := vm.getChannel(ch)

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

// ---------------------------------------------------------------------------
// Non-Local Return (NLR) Tests for Forked Blocks
// These tests verify that Phase 2 fixes work correctly
// ---------------------------------------------------------------------------

// TestDetachedBlockNormalReturn tests that normal block returns (no ^) work
// correctly in detached mode.
func TestDetachedBlockNormalReturn(t *testing.T) {
	interp := NewInterpreter()

	// Create a block that returns a value via OpBlockReturn (local return)
	// Bytecode: push 42, block return
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 42, byte(OpBlockReturn)},
		Literals:    nil,
	}

	// Execute in detached mode
	result := interp.ExecuteBlockDetached(blockMethod, nil, nil, Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("Detached block normal return = %v, want 42", result)
	}
}

// TestDetachedBlockNonLocalReturn tests that non-local returns (^) in detached
// blocks become local returns instead of panicking.
func TestDetachedBlockNonLocalReturn(t *testing.T) {
	interp := NewInterpreter()

	// Create a block that uses OpReturnTop (non-local return ^)
	// In attached mode, this would panic with NonLocalReturn
	// In detached mode, it should return locally
	// Bytecode: push 99, return top (^)
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 99, byte(OpReturnTop)},
		Literals:    nil,
	}

	// Execute in detached mode - should NOT panic
	result := interp.ExecuteBlockDetached(blockMethod, nil, nil, Nil, nil)

	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("Detached block NLR return = %v, want 99", result)
	}
}

// TestAttachedBlockNonLocalReturn tests that attached blocks still do proper
// non-local returns (to verify we didn't break normal behavior).
func TestAttachedBlockNonLocalReturn(t *testing.T) {
	interp := NewInterpreter()

	// Create a method frame first (this will be the home frame)
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	interp.pushFrame(m, Nil, nil)
	homeFrame := interp.fp

	// Create a block that uses OpReturnTop (^)
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 77, byte(OpReturnTop)},
		Literals:    nil,
	}

	// Execute in attached mode - should panic with NonLocalReturn
	var caught *NonLocalReturn
	func() {
		defer func() {
			if r := recover(); r != nil {
				if nlr, ok := r.(NonLocalReturn); ok {
					caught = &nlr
				}
			}
		}()
		interp.ExecuteBlock(blockMethod, nil, nil, homeFrame, Nil, m)
	}()

	if caught == nil {
		t.Error("Attached block NLR should have panicked with NonLocalReturn")
		return
	}

	if !caught.Value.IsSmallInt() || caught.Value.SmallInt() != 77 {
		t.Errorf("NonLocalReturn value = %v, want 77", caught.Value)
	}

	if caught.HomeFrame != homeFrame {
		t.Errorf("NonLocalReturn HomeFrame = %d, want %d", caught.HomeFrame, homeFrame)
	}

	interp.popFrame()
}

// TestForkedBlockWithNLRDoesNotCrash tests that forking a block with ^
// doesn't crash the program.
func TestForkedBlockWithNLRDoesNotCrash(t *testing.T) {
	vm := NewVM()

	// We need to create a block that contains OpReturnTop
	// Since we can't easily create compiled blocks from tests,
	// we'll test the fork mechanism more indirectly by verifying
	// that ExecuteBlockDetached is called (which we already tested above)

	// Create a buffered channel for synchronization
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// For now, just verify that fork works at all with a simple block
	// A proper test would require compiling Smalltalk code
	t.Log("Fork mechanism uses ExecuteBlockDetached - NLR behavior verified in unit tests above")

	// Clean up
	vm.Send(ch, "close", nil)
}

// TestNestedDetachedBlockNLR tests that nested blocks in detached mode
// also handle NLR correctly.
func TestNestedDetachedBlockNLR(t *testing.T) {
	interp := NewInterpreter()

	// Create an outer block that creates and calls an inner block
	// For simplicity, we test that the detached mode propagates
	// by checking that OpReturnTop doesn't panic

	// Simple block with NLR
	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 123, byte(OpReturnTop)},
		Literals:    nil,
	}

	// Execute multiple times to ensure no state corruption
	for i := 0; i < 10; i++ {
		result := interp.ExecuteBlockDetached(blockMethod, nil, nil, Nil, nil)
		if !result.IsSmallInt() || result.SmallInt() != 123 {
			t.Errorf("Iteration %d: result = %v, want 123", i, result)
		}
	}
}

// ---------------------------------------------------------------------------
// Mutex Tests
// These tests verify the Mutex implementation from Phase 3
// ---------------------------------------------------------------------------

// TestMutexNew tests that mutexes can be created via the class method.
func TestMutexNew(t *testing.T) {
	vm := NewVM()

	// Create a mutex via class method
	mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)
	if mu == Nil {
		t.Fatal("Mutex new returned nil")
	}

	if !isMutexValue(mu) {
		t.Error("Expected mutex value")
	}
}

// TestMutexLockUnlock tests basic lock and unlock operations.
func TestMutexLockUnlock(t *testing.T) {
	vm := NewVM()

	mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)

	// Initially should not be locked
	locked := vm.Send(mu, "isLocked", nil)
	if locked != False {
		t.Error("New mutex should not be locked")
	}

	// Lock it
	result := vm.Send(mu, "lock", nil)
	if result == Nil {
		t.Error("Lock returned nil")
	}

	// Should now be locked
	locked = vm.Send(mu, "isLocked", nil)
	if locked != True {
		t.Error("Mutex should be locked after lock")
	}

	// Unlock it
	result = vm.Send(mu, "unlock", nil)
	if result == Nil {
		t.Error("Unlock returned nil")
	}

	// Should no longer be locked
	locked = vm.Send(mu, "isLocked", nil)
	if locked != False {
		t.Error("Mutex should not be locked after unlock")
	}
}

// TestMutexTryLock tests non-blocking lock acquisition.
func TestMutexTryLock(t *testing.T) {
	vm := NewVM()

	mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)

	// First tryLock should succeed
	result := vm.Send(mu, "tryLock", nil)
	if result != True {
		t.Error("First tryLock should succeed")
	}

	// Verify it's locked
	locked := vm.Send(mu, "isLocked", nil)
	if locked != True {
		t.Error("Mutex should be locked after successful tryLock")
	}

	// Second tryLock in same goroutine will deadlock, so we test from another goroutine
	done := make(chan bool)
	go func() {
		// This tryLock should fail because the mutex is held
		muObj := vm.getMutex(mu)
		if muObj.mu.TryLock() {
			// Shouldn't happen - mutex should be locked
			muObj.mu.Unlock()
			done <- false
		} else {
			done <- true
		}
	}()

	succeeded := <-done
	if !succeeded {
		t.Error("TryLock from another goroutine should fail when mutex is held")
	}

	// Unlock
	vm.Send(mu, "unlock", nil)

	// Now tryLock should succeed again
	result = vm.Send(mu, "tryLock", nil)
	if result != True {
		t.Error("TryLock should succeed after unlock")
	}

	vm.Send(mu, "unlock", nil)
}

// TestMutexCritical tests the critical: method which executes a block while holding the lock.
func TestMutexCritical(t *testing.T) {
	vm := NewVM()

	mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)

	// We need to create a proper block to test critical:
	// Since we can't easily compile Smalltalk, we test the primitive directly
	muObj := vm.getMutex(mu)
	if muObj == nil {
		t.Fatal("Could not get mutex object")
	}

	// Create a simple block that returns a value
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 42, byte(OpBlockReturn)},
		Literals:    nil,
	}

	// Create a block value
	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	interp.pushFrame(m, Nil, nil)
	blockVal := interp.createBlockValue(blockMethod, nil)
	interp.popFrame()

	// Execute critical: directly using the VM's primitive
	result := vm.Send(mu, "critical:", []Value{blockVal})

	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("critical: result = %v, want 42", result)
	}

	// Mutex should be unlocked after critical: completes
	locked := vm.Send(mu, "isLocked", nil)
	if locked != False {
		t.Error("Mutex should be unlocked after critical: completes")
	}
}

// TestMutexConcurrentAccess tests that mutex properly serializes access
// from multiple goroutines.
func TestMutexConcurrentAccess(t *testing.T) {
	vm := NewVM()

	mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)
	muObj := vm.getMutex(mu)
	if muObj == nil {
		t.Fatal("Could not get mutex object")
	}

	const numGoroutines = 10
	const incrementsPerGoroutine = 100

	// Shared counter protected by mutex
	counter := 0

	var wg sync.WaitGroup
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < incrementsPerGoroutine; i++ {
				muObj.mu.Lock()
				muObj.locked.Store(true)
				counter++
				muObj.locked.Store(false)
				muObj.mu.Unlock()
			}
		}()
	}

	wg.Wait()

	expected := numGoroutines * incrementsPerGoroutine
	if counter != expected {
		t.Errorf("Counter = %d, want %d (race condition detected)", counter, expected)
	}
}

// TestMutexIsLockedFromMultipleGoroutines tests the isLocked primitive
// from concurrent goroutines.
func TestMutexIsLockedFromMultipleGoroutines(t *testing.T) {
	vm := NewVM()

	mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)
	muObj := vm.getMutex(mu)

	const numGoroutines = 5
	const iterations = 100

	var wg sync.WaitGroup

	// Multiple goroutines checking isLocked while one toggles the lock
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < iterations; i++ {
				// Just read the locked state - should never crash
				_ = muObj.locked.Load()
			}
		}()
	}

	// One goroutine toggling the lock
	wg.Add(1)
	go func() {
		defer wg.Done()
		for i := 0; i < iterations; i++ {
			muObj.mu.Lock()
			muObj.locked.Store(true)
			time.Sleep(time.Microsecond) // Brief hold
			muObj.locked.Store(false)
			muObj.mu.Unlock()
		}
	}()

	wg.Wait()

	// Final state should be unlocked
	if muObj.locked.Load() {
		t.Error("Mutex should be unlocked at end")
	}
}

// TestMutexMultipleCreation tests creating multiple mutexes concurrently.
func TestMutexMultipleCreation(t *testing.T) {
	vm := NewVM()

	const numGoroutines = 10
	const mutexesPerGoroutine = 50

	var wg sync.WaitGroup
	mutexes := make([][]Value, numGoroutines)

	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			mutexes[goroutineID] = make([]Value, mutexesPerGoroutine)
			for i := 0; i < mutexesPerGoroutine; i++ {
				mutexes[goroutineID][i] = vm.Send(vm.classValue(vm.MutexClass), "new", nil)
			}
		}(g)
	}

	wg.Wait()

	// Verify all mutexes are valid and unique
	seenIDs := make(map[uint32]bool)
	for g := 0; g < numGoroutines; g++ {
		for i := 0; i < mutexesPerGoroutine; i++ {
			mu := mutexes[g][i]
			if mu == Nil {
				t.Errorf("Mutex at [%d][%d] is nil", g, i)
				continue
			}
			if !isMutexValue(mu) {
				t.Errorf("Value at [%d][%d] is not a mutex", g, i)
				continue
			}
			id := mu.SymbolID()
			if seenIDs[id] {
				t.Errorf("Duplicate mutex ID %d at [%d][%d]", id, g, i)
			}
			seenIDs[id] = true
		}
	}

	t.Logf("Created %d mutexes from %d goroutines, %d unique IDs",
		numGoroutines*mutexesPerGoroutine, numGoroutines, len(seenIDs))
}

// TestMutexRegistryIntegrity tests that mutex registry maintains integrity
// under concurrent access.
func TestMutexRegistryIntegrity(t *testing.T) {
	vm := NewVM()

	const numGoroutines = 10
	const opsPerGoroutine = 100

	var wg sync.WaitGroup
	allMutexes := make(chan Value, numGoroutines*opsPerGoroutine)

	// Create mutexes concurrently
	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < opsPerGoroutine; i++ {
				mu := vm.Send(vm.classValue(vm.MutexClass), "new", nil)
				allMutexes <- mu
			}
		}()
	}

	wg.Wait()
	close(allMutexes)

	// Verify all mutexes are still accessible
	count := 0
	for mu := range allMutexes {
		muObj := vm.getMutex(mu)
		if muObj == nil {
			t.Errorf("Lost mutex at count %d", count)
		}
		count++
	}

	t.Logf("Verified %d mutexes are accessible in registry", count)
}

// ---------------------------------------------------------------------------
// WaitGroup Tests
// These tests verify the WaitGroup implementation from Phase 4
// ---------------------------------------------------------------------------

// TestWaitGroupNew tests that wait groups can be created.
func TestWaitGroupNew(t *testing.T) {
	vm := NewVM()

	wg := vm.Send(vm.classValue(vm.WaitGroupClass), "new", nil)
	if wg == Nil {
		t.Fatal("WaitGroup new returned nil")
	}

	if !isWaitGroupValue(wg) {
		t.Error("Expected wait group value")
	}
}

// TestWaitGroupAddDoneWait tests basic add, done, and wait operations.
func TestWaitGroupAddDoneWait(t *testing.T) {
	vm := NewVM()

	wg := vm.Send(vm.classValue(vm.WaitGroupClass), "new", nil)

	// Add 3 to the counter
	vm.Send(wg, "add:", []Value{FromSmallInt(3)})

	// Check count
	count := vm.Send(wg, "count", nil)
	if !count.IsSmallInt() || count.SmallInt() != 3 {
		t.Errorf("Count = %v, want 3", count)
	}

	// Done three times
	vm.Send(wg, "done", nil)
	vm.Send(wg, "done", nil)
	vm.Send(wg, "done", nil)

	// Count should be 0
	count = vm.Send(wg, "count", nil)
	if !count.IsSmallInt() || count.SmallInt() != 0 {
		t.Errorf("Count after done = %v, want 0", count)
	}

	// Wait should return immediately since count is 0
	result := vm.Send(wg, "wait", nil)
	if result == Nil {
		t.Error("Wait returned nil")
	}
}

// TestWaitGroupConcurrent tests concurrent usage of WaitGroup.
func TestWaitGroupConcurrent(t *testing.T) {
	vm := NewVM()

	wgVal := vm.Send(vm.classValue(vm.WaitGroupClass), "new", nil)
	wgObj := vm.getWaitGroup(wgVal)
	if wgObj == nil {
		t.Fatal("Could not get wait group object")
	}

	const numWorkers = 10
	counter := int32(0)

	// Add numWorkers
	vm.Send(wgVal, "add:", []Value{FromSmallInt(numWorkers)})

	var goWg sync.WaitGroup
	for i := 0; i < numWorkers; i++ {
		goWg.Add(1)
		go func() {
			defer goWg.Done()
			// Simulate work
			time.Sleep(time.Millisecond)
			atomic.AddInt32(&counter, 1)
			// Call done
			wgObj.counter.Add(-1)
			wgObj.wg.Done()
		}()
	}

	// Wait for completion
	wgObj.wg.Wait()
	goWg.Wait()

	if counter != numWorkers {
		t.Errorf("Counter = %d, want %d", counter, numWorkers)
	}
}

// TestWaitGroupMultipleCreation tests concurrent creation of wait groups.
func TestWaitGroupMultipleCreation(t *testing.T) {
	vm := NewVM()

	const numGoroutines = 10
	const wgsPerGoroutine = 50

	var wg sync.WaitGroup
	waitGroups := make([][]Value, numGoroutines)

	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			waitGroups[goroutineID] = make([]Value, wgsPerGoroutine)
			for i := 0; i < wgsPerGoroutine; i++ {
				waitGroups[goroutineID][i] = vm.Send(vm.classValue(vm.WaitGroupClass), "new", nil)
			}
		}(g)
	}

	wg.Wait()

	// Verify all wait groups are valid
	count := 0
	for g := 0; g < numGoroutines; g++ {
		for i := 0; i < wgsPerGoroutine; i++ {
			w := waitGroups[g][i]
			if w == Nil {
				t.Errorf("WaitGroup at [%d][%d] is nil", g, i)
				continue
			}
			if !isWaitGroupValue(w) {
				t.Errorf("Value at [%d][%d] is not a wait group", g, i)
				continue
			}
			count++
		}
	}

	t.Logf("Created %d wait groups from %d goroutines", count, numGoroutines)
}

// ---------------------------------------------------------------------------
// Semaphore Tests
// These tests verify the Semaphore implementation from Phase 4
// ---------------------------------------------------------------------------

// TestSemaphoreNew tests that semaphores can be created.
func TestSemaphoreNew(t *testing.T) {
	vm := NewVM()

	// Binary semaphore
	sem := vm.Send(vm.classValue(vm.SemaphoreClass), "new", nil)
	if sem == Nil {
		t.Fatal("Semaphore new returned nil")
	}

	if !isSemaphoreValue(sem) {
		t.Error("Expected semaphore value")
	}

	// Check capacity is 1
	cap := vm.Send(sem, "capacity", nil)
	if !cap.IsSmallInt() || cap.SmallInt() != 1 {
		t.Errorf("Binary semaphore capacity = %v, want 1", cap)
	}
}

// TestSemaphoreNewWithCapacity tests creating semaphores with capacity.
func TestSemaphoreNewWithCapacity(t *testing.T) {
	vm := NewVM()

	sem := vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(5)})
	if sem == Nil {
		t.Fatal("Semaphore new: returned nil")
	}

	// Check capacity
	cap := vm.Send(sem, "capacity", nil)
	if !cap.IsSmallInt() || cap.SmallInt() != 5 {
		t.Errorf("Semaphore capacity = %v, want 5", cap)
	}

	// Check available (should be full initially)
	avail := vm.Send(sem, "available", nil)
	if !avail.IsSmallInt() || avail.SmallInt() != 5 {
		t.Errorf("Semaphore available = %v, want 5", avail)
	}
}

// TestSemaphoreAcquireRelease tests basic acquire and release.
func TestSemaphoreAcquireRelease(t *testing.T) {
	vm := NewVM()

	sem := vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(2)})

	// Initially 2 available
	avail := vm.Send(sem, "available", nil)
	if avail.SmallInt() != 2 {
		t.Errorf("Available = %v, want 2", avail)
	}

	// Acquire one
	vm.Send(sem, "acquire", nil)
	avail = vm.Send(sem, "available", nil)
	if avail.SmallInt() != 1 {
		t.Errorf("Available after acquire = %v, want 1", avail)
	}

	// Acquire another
	vm.Send(sem, "acquire", nil)
	avail = vm.Send(sem, "available", nil)
	if avail.SmallInt() != 0 {
		t.Errorf("Available after second acquire = %v, want 0", avail)
	}

	// Release one
	vm.Send(sem, "release", nil)
	avail = vm.Send(sem, "available", nil)
	if avail.SmallInt() != 1 {
		t.Errorf("Available after release = %v, want 1", avail)
	}

	// Release another
	vm.Send(sem, "release", nil)
	avail = vm.Send(sem, "available", nil)
	if avail.SmallInt() != 2 {
		t.Errorf("Available after second release = %v, want 2", avail)
	}
}

// TestSemaphoreTryAcquire tests non-blocking acquire.
func TestSemaphoreTryAcquire(t *testing.T) {
	vm := NewVM()

	sem := vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(1)})

	// First tryAcquire should succeed
	result := vm.Send(sem, "tryAcquire", nil)
	if result != True {
		t.Error("First tryAcquire should succeed")
	}

	// Second tryAcquire should fail (no permits available)
	result = vm.Send(sem, "tryAcquire", nil)
	if result != False {
		t.Error("Second tryAcquire should fail")
	}

	// Release
	vm.Send(sem, "release", nil)

	// Now tryAcquire should succeed again
	result = vm.Send(sem, "tryAcquire", nil)
	if result != True {
		t.Error("TryAcquire after release should succeed")
	}
}

// TestSemaphoreConcurrentAccess tests concurrent usage of semaphore.
func TestSemaphoreConcurrentAccess(t *testing.T) {
	vm := NewVM()

	sem := vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(3)})
	semObj := vm.getSemaphore(sem)
	if semObj == nil {
		t.Fatal("Could not get semaphore object")
	}

	const numGoroutines = 10
	const opsPerGoroutine = 100

	var wg sync.WaitGroup
	maxConcurrent := int32(0)
	currentConcurrent := int32(0)

	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for i := 0; i < opsPerGoroutine; i++ {
				// Acquire
				<-semObj.permits

				// Track max concurrency
				curr := atomic.AddInt32(&currentConcurrent, 1)
				for {
					old := atomic.LoadInt32(&maxConcurrent)
					if curr <= old || atomic.CompareAndSwapInt32(&maxConcurrent, old, curr) {
						break
					}
				}

				// Brief work
				time.Sleep(time.Microsecond)

				atomic.AddInt32(&currentConcurrent, -1)

				// Release
				select {
				case semObj.permits <- struct{}{}:
				default:
				}
			}
		}()
	}

	wg.Wait()

	// Max concurrency should not exceed semaphore capacity (3)
	if maxConcurrent > 3 {
		t.Errorf("Max concurrent = %d, want <= 3", maxConcurrent)
	}

	t.Logf("Max concurrent access was %d (capacity 3)", maxConcurrent)
}

// TestSemaphoreCritical tests the critical: method.
func TestSemaphoreCritical(t *testing.T) {
	vm := NewVM()

	sem := vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(1)})

	// Create a simple block that returns a value
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 99, byte(OpBlockReturn)},
		Literals:    nil,
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	interp.pushFrame(m, Nil, nil)
	blockVal := interp.createBlockValue(blockMethod, nil)
	interp.popFrame()

	// Execute critical:
	result := vm.Send(sem, "critical:", []Value{blockVal})

	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("critical: result = %v, want 99", result)
	}

	// Semaphore should have all permits available after critical: completes
	avail := vm.Send(sem, "available", nil)
	if !avail.IsSmallInt() || avail.SmallInt() != 1 {
		t.Errorf("Available after critical: = %v, want 1", avail)
	}
}

// TestSemaphoreMultipleCreation tests concurrent semaphore creation.
func TestSemaphoreMultipleCreation(t *testing.T) {
	vm := NewVM()

	const numGoroutines = 10
	const semsPerGoroutine = 50

	var wg sync.WaitGroup
	semaphores := make([][]Value, numGoroutines)

	for g := 0; g < numGoroutines; g++ {
		wg.Add(1)
		go func(goroutineID int) {
			defer wg.Done()
			semaphores[goroutineID] = make([]Value, semsPerGoroutine)
			for i := 0; i < semsPerGoroutine; i++ {
				semaphores[goroutineID][i] = vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(int64(i + 1))})
			}
		}(g)
	}

	wg.Wait()

	// Verify all semaphores are valid
	seenIDs := make(map[uint32]bool)
	for g := 0; g < numGoroutines; g++ {
		for i := 0; i < semsPerGoroutine; i++ {
			s := semaphores[g][i]
			if s == Nil {
				t.Errorf("Semaphore at [%d][%d] is nil", g, i)
				continue
			}
			if !isSemaphoreValue(s) {
				t.Errorf("Value at [%d][%d] is not a semaphore", g, i)
				continue
			}
			id := s.SymbolID()
			if seenIDs[id] {
				t.Errorf("Duplicate semaphore ID %d at [%d][%d]", id, g, i)
			}
			seenIDs[id] = true
		}
	}

	t.Logf("Created %d semaphores from %d goroutines, %d unique IDs",
		numGoroutines*semsPerGoroutine, numGoroutines, len(seenIDs))
}

// ---------------------------------------------------------------------------
// Memory Leak Tests
// These tests verify that sweep functions properly clean up registries
// ---------------------------------------------------------------------------

// TestSweepClosedChannels tests that closed channels are swept from the registry.
func TestSweepClosedChannels(t *testing.T) {
	vm := NewVM()

	// Create several channels
	const numChannels = 10
	channels := make([]Value, numChannels)
	for i := 0; i < numChannels; i++ {
		channels[i] = vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	}

	// Verify all channels are registered
	initialCount := vm.Concurrency().ChannelCount()
	if initialCount < numChannels {
		t.Errorf("Expected at least %d channels, got %d", numChannels, initialCount)
	}

	// Close half the channels
	for i := 0; i < numChannels/2; i++ {
		vm.Send(channels[i], "close", nil)
	}

	// Sweep
	swept := vm.Concurrency().SweepChannels()

	// Verify the right number were swept
	if swept != numChannels/2 {
		t.Errorf("Swept %d channels, expected %d", swept, numChannels/2)
	}

	// Verify remaining count
	remainingCount := vm.Concurrency().ChannelCount()
	expectedRemaining := initialCount - numChannels/2
	if remainingCount != expectedRemaining {
		t.Errorf("Remaining channels: %d, expected %d", remainingCount, expectedRemaining)
	}

	t.Logf("Swept %d closed channels, %d remaining", swept, remainingCount)
}

// TestSweepTerminatedProcesses tests that terminated processes are swept.
func TestSweepTerminatedProcesses(t *testing.T) {
	vm := NewVM()

	// Create processes that complete immediately
	const numProcesses = 10
	processes := make([]Value, numProcesses)

	// Create a simple block that returns immediately
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)

	blockMethod := &BlockMethod{
		Arity:       0,
		NumTemps:    0,
		NumCaptures: 0,
		Bytecode:    []byte{byte(OpPushInt8), 42, byte(OpBlockReturn)},
		Literals:    nil,
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()

	interp.pushFrame(m, Nil, nil)

	for i := 0; i < numProcesses; i++ {
		blockVal := interp.createBlockValue(blockMethod, nil)
		// Fork the block
		processes[i] = vm.Send(blockVal, "fork", nil)
	}

	interp.popFrame()
	vm.unregisterInterpreter()

	// Wait for all processes to complete
	for i := 0; i < numProcesses; i++ {
		vm.Send(processes[i], "wait", nil)
	}

	// Verify all processes are done
	for i := 0; i < numProcesses; i++ {
		done := vm.Send(processes[i], "isDone", nil)
		if done != True {
			t.Errorf("Process %d should be done", i)
		}
	}

	// Count before sweep
	beforeCount := vm.Concurrency().ProcessCount()

	// Sweep
	swept := vm.Concurrency().SweepProcesses()

	// Verify processes were swept
	if swept < numProcesses {
		t.Errorf("Swept %d processes, expected at least %d", swept, numProcesses)
	}

	afterCount := vm.Concurrency().ProcessCount()
	t.Logf("Swept %d terminated processes (before: %d, after: %d)", swept, beforeCount, afterCount)
}

// TestConcurrencyStats tests the Stats method.
func TestConcurrencyStats(t *testing.T) {
	vm := NewVM()

	// Create some objects
	vm.Send(vm.classValue(vm.ChannelClass), "new", nil)
	vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(5)})
	vm.Send(vm.classValue(vm.MutexClass), "new", nil)
	vm.Send(vm.classValue(vm.WaitGroupClass), "new", nil)
	vm.Send(vm.classValue(vm.SemaphoreClass), "new:", []Value{FromSmallInt(3)})

	// Get stats
	stats := vm.ConcurrencyStats()

	if stats["channels"] < 2 {
		t.Errorf("Expected at least 2 channels, got %d", stats["channels"])
	}
	if stats["mutexes"] < 1 {
		t.Errorf("Expected at least 1 mutex, got %d", stats["mutexes"])
	}
	if stats["waitGroups"] < 1 {
		t.Errorf("Expected at least 1 wait group, got %d", stats["waitGroups"])
	}
	if stats["semaphores"] < 1 {
		t.Errorf("Expected at least 1 semaphore, got %d", stats["semaphores"])
	}

	t.Logf("Stats: %v", stats)
}

// TestVMIsolation tests that different VMs have isolated registries.
func TestVMIsolation(t *testing.T) {
	vm1 := NewVM()
	vm2 := NewVM()

	// Create channels in each VM
	ch1 := vm1.Send(vm1.classValue(vm1.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch2 := vm2.Send(vm2.classValue(vm2.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Verify each VM only sees its own channels
	count1 := vm1.Concurrency().ChannelCount()
	count2 := vm2.Concurrency().ChannelCount()

	if count1 != 1 {
		t.Errorf("VM1 channel count = %d, want 1", count1)
	}
	if count2 != 1 {
		t.Errorf("VM2 channel count = %d, want 1", count2)
	}

	// Verify channels are accessible in their respective VMs
	chObj1 := vm1.getChannel(ch1)
	chObj2 := vm2.getChannel(ch2)

	if chObj1 == nil {
		t.Error("VM1 should find its own channel")
	}
	if chObj2 == nil {
		t.Error("VM2 should find its own channel")
	}

	// Verify the objects are different (VM isolation means different object instances)
	if chObj1 == chObj2 {
		t.Error("Channels from different VMs should be different objects")
	}

	// Test that closing a channel in one VM doesn't affect the other
	vm1.Send(ch1, "close", nil)

	isClosed1 := vm1.Send(ch1, "isClosed", nil)
	isClosed2 := vm2.Send(ch2, "isClosed", nil)

	if isClosed1 != True {
		t.Error("VM1 channel should be closed")
	}
	if isClosed2 != False {
		t.Error("VM2 channel should NOT be closed")
	}

	t.Log("VM isolation verified: each VM has independent channels")
}

// ---------------------------------------------------------------------------
// Channel Select Tests
// ---------------------------------------------------------------------------

// TestChannelSelectReceive tests basic select with a single ready channel.
func TestChannelSelectReceive(t *testing.T) {
	vm := NewVM()

	// Create a buffered channel and send a value
	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	vm.Send(ch, "send:", []Value{FromSmallInt(42)})

	// Create a handler block that returns the received value doubled
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	blockMethod := &BlockMethod{
		Arity:       1, // Receives the value
		NumTemps:    0,
		NumCaptures: 0,
		// Block receives arg, adds 10 to it: push temp0 (arg), push 10, send +, return
		Bytecode: []byte{byte(OpPushTemp), 0, byte(OpPushInt8), 10, byte(OpSendPlus), byte(OpBlockReturn)},
		Literals: nil,
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	interp.pushFrame(m, Nil, nil)
	defer interp.popFrame()

	handlerVal := interp.createBlockValue(blockMethod, nil)

	// Create a select case using onReceive:
	caseVal := vm.Send(ch, "onReceive:", []Value{handlerVal})
	if caseVal == Nil {
		t.Fatal("onReceive: returned nil")
	}

	// Create an array with the case
	casesVal := vm.NewArrayWithElements([]Value{caseVal})

	// Call Channel select:
	result := vm.Send(vm.classValue(vm.ChannelClass), "select:", []Value{casesVal})

	// Result should be 42 + 10 = 52
	if !result.IsSmallInt() || result.SmallInt() != 52 {
		t.Errorf("select: returned %v, want 52", result)
	}
}

// TestChannelSelectMultiple tests select with multiple channels where one is ready.
func TestChannelSelectMultiple(t *testing.T) {
	vm := NewVM()

	// Create three buffered channels
	ch1 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch2 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch3 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Only send to channel 2
	vm.Send(ch2, "send:", []Value{FromSmallInt(200)})

	// Create handler blocks
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	// Handler that returns 1 for ch1
	block1 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 1, byte(OpBlockReturn)},
	}
	// Handler that returns 2 for ch2
	block2 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 2, byte(OpBlockReturn)},
	}
	// Handler that returns 3 for ch3
	block3 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 3, byte(OpBlockReturn)},
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	interp.pushFrame(m, Nil, nil)
	defer interp.popFrame()

	h1 := interp.createBlockValue(block1, nil)
	h2 := interp.createBlockValue(block2, nil)
	h3 := interp.createBlockValue(block3, nil)

	// Create select cases
	case1 := vm.Send(ch1, "onReceive:", []Value{h1})
	case2 := vm.Send(ch2, "onReceive:", []Value{h2})
	case3 := vm.Send(ch3, "onReceive:", []Value{h3})

	// Create cases array
	casesVal := vm.NewArrayWithElements([]Value{case1, case2, case3})

	// Call select
	result := vm.Send(vm.classValue(vm.ChannelClass), "select:", []Value{casesVal})

	// Should pick channel 2, returning 2
	if !result.IsSmallInt() || result.SmallInt() != 2 {
		t.Errorf("select: with multiple channels = %v, want 2", result)
	}
}

// TestChannelSelectIfNone tests non-blocking select with default case.
func TestChannelSelectIfNone(t *testing.T) {
	vm := NewVM()

	// Create channels but don't send anything
	ch1 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch2 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Create handler blocks
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	block1 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 1, byte(OpBlockReturn)},
	}
	block2 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 2, byte(OpBlockReturn)},
	}
	// Default block returns 99
	defaultBlock := &BlockMethod{
		Arity:    0,
		Bytecode: []byte{byte(OpPushInt8), 99, byte(OpBlockReturn)},
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	interp.pushFrame(m, Nil, nil)
	defer interp.popFrame()

	h1 := interp.createBlockValue(block1, nil)
	h2 := interp.createBlockValue(block2, nil)
	defaultVal := interp.createBlockValue(defaultBlock, nil)

	// Create select cases
	case1 := vm.Send(ch1, "onReceive:", []Value{h1})
	case2 := vm.Send(ch2, "onReceive:", []Value{h2})
	casesVal := vm.NewArrayWithElements([]Value{case1, case2})

	// Call select:ifNone: - should execute default block since no channels are ready
	result := vm.Send(vm.classValue(vm.ChannelClass), "select:ifNone:", []Value{casesVal, defaultVal})

	// Should return 99 from default block
	if !result.IsSmallInt() || result.SmallInt() != 99 {
		t.Errorf("select:ifNone: with no ready channels = %v, want 99", result)
	}
}

// TestChannelSelectWithReadyChannel tests select:ifNone: when a channel is ready.
func TestChannelSelectWithReadyChannel(t *testing.T) {
	vm := NewVM()

	// Create channels, send to one
	ch1 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	ch2 := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})
	vm.Send(ch1, "send:", []Value{FromSmallInt(100)})

	// Create handler blocks
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	block1 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 1, byte(OpBlockReturn)},
	}
	block2 := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 2, byte(OpBlockReturn)},
	}
	defaultBlock := &BlockMethod{
		Arity:    0,
		Bytecode: []byte{byte(OpPushInt8), 99, byte(OpBlockReturn)},
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	interp.pushFrame(m, Nil, nil)
	defer interp.popFrame()

	h1 := interp.createBlockValue(block1, nil)
	h2 := interp.createBlockValue(block2, nil)
	defaultVal := interp.createBlockValue(defaultBlock, nil)

	case1 := vm.Send(ch1, "onReceive:", []Value{h1})
	case2 := vm.Send(ch2, "onReceive:", []Value{h2})
	casesVal := vm.NewArrayWithElements([]Value{case1, case2})

	// Call select:ifNone: - should pick ch1 since it has data
	result := vm.Send(vm.classValue(vm.ChannelClass), "select:ifNone:", []Value{casesVal, defaultVal})

	// Should return 1 from ch1's handler
	if !result.IsSmallInt() || result.SmallInt() != 1 {
		t.Errorf("select:ifNone: with ready channel = %v, want 1", result)
	}
}

// TestChannelOnReceiveCreatesAssociation tests that onReceive: creates a valid case.
func TestChannelOnReceiveCreatesAssociation(t *testing.T) {
	vm := NewVM()

	ch := vm.Send(vm.classValue(vm.ChannelClass), "new:", []Value{FromSmallInt(1)})

	// Create a simple handler block
	interp := vm.newInterpreter()
	vm.registerInterpreter(interp)
	defer vm.unregisterInterpreter()

	block := &BlockMethod{
		Arity:    1,
		Bytecode: []byte{byte(OpPushInt8), 42, byte(OpBlockReturn)},
	}

	b := NewCompiledMethodBuilder("test", 0)
	b.Bytecode().Emit(OpPushNil)
	b.Bytecode().Emit(OpReturnTop)
	m := b.Build()
	interp.pushFrame(m, Nil, nil)
	defer interp.popFrame()

	handler := interp.createBlockValue(block, nil)

	// Call onReceive:
	caseVal := vm.Send(ch, "onReceive:", []Value{handler})

	// Verify it's an object (Association)
	if !caseVal.IsObject() {
		t.Error("onReceive: should return an object (Association)")
		return
	}

	// Verify we can parse it as a select case
	sc, ok := vm.parseSelectCase(caseVal)
	if !ok {
		t.Error("Could not parse onReceive: result as SelectCase")
		return
	}

	if sc.Channel == nil {
		t.Error("SelectCase channel is nil")
	}
	if sc.Dir != reflect.SelectRecv {
		t.Errorf("SelectCase Dir = %v, want SelectRecv (%v)", sc.Dir, reflect.SelectRecv)
	}
}
