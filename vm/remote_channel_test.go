package vm

import (
	"testing"
)

// ---------------------------------------------------------------------------
// RemoteChannelRef tests
// ---------------------------------------------------------------------------

func TestRemoteChannelRef_ClosedState(t *testing.T) {
	ref := &RemoteChannelRef{
		ChannelID: 1,
		Capacity:  5,
	}
	if ref.IsClosed() {
		t.Error("should start open")
	}
	ref.MarkClosed()
	if !ref.IsClosed() {
		t.Error("should be closed after MarkClosed")
	}
}

// ---------------------------------------------------------------------------
// Registry tests
// ---------------------------------------------------------------------------

func TestRemoteChannelRegistry(t *testing.T) {
	reg := newRemoteChannelRegistry()

	ref1 := &RemoteChannelRef{ChannelID: 10}
	ref2 := &RemoteChannelRef{ChannelID: 20}

	id1 := reg.register(ref1)
	id2 := reg.register(ref2)

	if id1 == id2 {
		t.Error("IDs should be unique")
	}

	if got := reg.get(id1); got != ref1 {
		t.Error("should get back ref1")
	}
	if got := reg.get(id2); got != ref2 {
		t.Error("should get back ref2")
	}
	if got := reg.get(999); got != nil {
		t.Error("should return nil for unknown ID")
	}
}

func TestRemoteChannelRegistry_DrainNode(t *testing.T) {
	reg := newRemoteChannelRegistry()

	nodeA := [32]byte{1, 2, 3}
	nodeB := [32]byte{4, 5, 6}

	ref1 := &RemoteChannelRef{OwnerNode: nodeA, ChannelID: 1}
	ref2 := &RemoteChannelRef{OwnerNode: nodeA, ChannelID: 2}
	ref3 := &RemoteChannelRef{OwnerNode: nodeB, ChannelID: 3}

	reg.register(ref1)
	reg.register(ref2)
	reg.register(ref3)

	reg.drainNode(nodeA)

	if !ref1.IsClosed() {
		t.Error("ref1 should be closed (node A)")
	}
	if !ref2.IsClosed() {
		t.Error("ref2 should be closed (node A)")
	}
	if ref3.IsClosed() {
		t.Error("ref3 should still be open (node B)")
	}
}

func TestChannelExportRegistry(t *testing.T) {
	reg := newChannelExportRegistry()

	ch1 := createChannel(5)
	ch2 := createChannel(0)

	id1 := reg.Export(ch1)
	id2 := reg.Export(ch2)

	if id1 == id2 {
		t.Error("export IDs should be unique")
	}

	// Dedup: exporting the same channel again returns the same ID
	id1b := reg.Export(ch1)
	if id1b != id1 {
		t.Errorf("dedup failed: got %d, want %d", id1b, id1)
	}

	// Lookup
	if got := reg.Lookup(id1); got != ch1 {
		t.Error("should find ch1")
	}
	if got := reg.Lookup(id2); got != ch2 {
		t.Error("should find ch2")
	}
	if got := reg.Lookup(999); got != nil {
		t.Error("should return nil for unknown ID")
	}
}

// ---------------------------------------------------------------------------
// NaN-boxing tests
// ---------------------------------------------------------------------------

func TestRemoteChannel_NanBoxing(t *testing.T) {
	v := remoteChannelToValue(42)
	if !isRemoteChannelValue(v) {
		t.Error("should be recognized as remote channel")
	}
	if id := remoteChannelIDFromValue(v); id != 42 {
		t.Errorf("ID: got %d, want 42", id)
	}

	// Regular values should not be remote channels
	if isRemoteChannelValue(FromSmallInt(42)) {
		t.Error("SmallInt should not be remote channel")
	}
	if isRemoteChannelValue(Nil) {
		t.Error("Nil should not be remote channel")
	}
}

// ---------------------------------------------------------------------------
// VM integration tests
// ---------------------------------------------------------------------------

func TestVM_RegisterAndGetRemoteChannel(t *testing.T) {
	v := NewVM()

	ref := &RemoteChannelRef{
		OwnerNode: [32]byte{10, 20, 30},
		ChannelID: 42,
		Capacity:  10,
	}

	val := v.RegisterRemoteChannelValue(ref)
	if !isRemoteChannelValue(val) {
		t.Error("should be remote channel value")
	}

	got := v.GetRemoteChannel(val)
	if got != ref {
		t.Error("should get back the same ref")
	}
}

func TestVM_ExportAndLookupChannel(t *testing.T) {
	v := NewVM()

	ch := createChannel(5)
	v.registerChannel(ch)

	exportID := v.ExportChannel(ch)
	if exportID == 0 {
		t.Error("export ID should be non-zero")
	}

	got := v.LookupExportedChannel(exportID)
	if got != ch {
		t.Error("should find the exported channel")
	}
}

func TestVM_FindChannelValue(t *testing.T) {
	v := NewVM()

	ch := createChannel(3)
	chVal := v.registerChannel(ch)

	found := v.findChannelValue(ch)
	if found != chVal {
		t.Error("findChannelValue should return the registered value")
	}
}

// ---------------------------------------------------------------------------
// Channel serialization round-trip tests
// ---------------------------------------------------------------------------

func TestSerializeChannel_LocalRoundTrip(t *testing.T) {
	v := NewVM()
	// Set identity so owner node is deterministic (non-zero)
	pub := make([]byte, 32)
	pub[0] = 1
	v.SetNodeIdentityKeys(pub, make([]byte, 64))

	ch := createChannel(5)
	chVal := v.registerChannel(ch)
	v.ExportChannel(ch)

	// Serialize
	data, err := v.SerializeValue(chVal)
	if err != nil {
		t.Fatalf("serialize channel: %v", err)
	}
	if len(data) == 0 {
		t.Fatal("serialized data should not be empty")
	}

	// Deserialize on the same VM (should get back the same local channel)
	result, err := v.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize channel: %v", err)
	}

	// Should resolve to a local channel (same as original)
	if !isChannelValue(result) {
		t.Error("should deserialize back to a local channel")
	}
}

func TestSerializeChannel_RemoteCreation(t *testing.T) {
	// VM-A serializes a channel
	vmA := NewVM()
	vmA.SetNodeIdentityKeys([]byte{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32}, make([]byte, 64))

	ch := createChannel(5)
	chVal := vmA.registerChannel(ch)
	_ = chVal

	data, err := vmA.SerializeValue(chVal)
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}

	// VM-B deserializes — should create a remote channel proxy
	vmB := NewVM()
	vmB.SetNodeIdentityKeys([]byte{99, 98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68}, make([]byte, 64))

	result, err := vmB.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize: %v", err)
	}

	if !isRemoteChannelValue(result) {
		t.Error("should create a remote channel on VM-B")
	}

	ref := vmB.getRemoteChannel(result)
	if ref == nil {
		t.Fatal("should have a RemoteChannelRef")
	}
	if ref.Capacity != 5 {
		t.Errorf("capacity: got %d, want 5", ref.Capacity)
	}
}

// ---------------------------------------------------------------------------
// Remote channel primitive tests (with mock callbacks)
// ---------------------------------------------------------------------------

func TestRemoteChannel_MockSendReceive(t *testing.T) {
	v := NewVM()

	// Create a buffered "remote" channel backed by a Go channel
	backingCh := make(chan []byte, 5)

	ref := &RemoteChannelRef{
		ChannelID: 1,
		Capacity:  5,
		SendFunc: func(id uint64, data []byte) error {
			backingCh <- data
			return nil
		},
		ReceiveFunc: func(id uint64) ([]byte, bool, error) {
			data := <-backingCh
			return data, true, nil
		},
		TrySendFunc: func(id uint64, data []byte) (bool, error) {
			select {
			case backingCh <- data:
				return true, nil
			default:
				return false, nil
			}
		},
		TryReceiveFunc: func(id uint64) ([]byte, bool, bool, error) {
			select {
			case data := <-backingCh:
				return data, true, true, nil
			default:
				return nil, false, true, nil
			}
		},
	}

	rcVal := v.registerRemoteChannel(ref)

	// Test send via primitive
	data42, _ := v.SerializeValue(FromSmallInt(42))

	// trySend should work
	sent, _ := ref.TrySendFunc(ref.ChannelID, data42)
	if !sent {
		t.Error("trySend should succeed")
	}

	// tryReceive should get back the value
	data, gotValue, _, _ := ref.TryReceiveFunc(ref.ChannelID)
	if !gotValue {
		t.Error("tryReceive should get a value")
	}

	val, _ := v.DeserializeValue(data)
	if val.SmallInt() != 42 {
		t.Errorf("received value: got %d, want 42", val.SmallInt())
	}

	_ = rcVal
}
