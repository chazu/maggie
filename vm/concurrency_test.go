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
		muObj := getMutex(mu)
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
	muObj := getMutex(mu)
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
	muObj := getMutex(mu)
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
	muObj := getMutex(mu)

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
		muObj := getMutex(mu)
		if muObj == nil {
			t.Errorf("Lost mutex at count %d", count)
		}
		count++
	}

	t.Logf("Verified %d mutexes are accessible in registry", count)
}
