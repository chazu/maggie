package vm

import "testing"

type fakeClusterCore struct {
	infos     []ClusterMemberInfo
	connected map[[32]byte]*NodeRefData
	joinCalls []string
	metaCalls []map[string]string
}

func (f *fakeClusterCore) AliveMemberInfos() []ClusterMemberInfo { return f.infos }
func (f *fakeClusterCore) ConnectAsync(addr string)              {}
func (f *fakeClusterCore) ConnectedRef(peer [32]byte) *NodeRefData {
	return f.connected[peer]
}
func (f *fakeClusterCore) SetMetadata(md map[string]string) { f.metaCalls = append(f.metaCalls, md) }
func (f *fakeClusterCore) SetJoinPolicy(p string)           { f.joinCalls = append(f.joinCalls, p) }

func TestClusterMember_Accessors(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	var id [32]byte
	id[0], id[31] = 0xAB, 0xCD
	info := ClusterMemberInfo{ID: id, Addr: "node2:8081", Status: ClusterStatusAlive,
		Metadata: map[string]string{"zone": "eu-west"}}
	member := vm.newClusterMemberValue(info)

	if got := vm.Send(member, "addr", nil); vm.registry.GetStringContent(got) != "node2:8081" {
		t.Errorf("addr: got %v", got)
	}
	if got := vm.Send(member, "status", nil); !got.IsSymbol() || vm.Symbols.Name(got.SymbolID()) != "alive" {
		t.Errorf("status should be #alive, got %v", got)
	}
	idStr := vm.registry.GetStringContent(vm.Send(member, "id", nil))
	if len(idStr) != 64 || idStr[:2] != "ab" {
		t.Errorf("id should be 64-char hex starting ab, got %q", idStr)
	}
	md := vm.Send(member, "metadata", nil)
	if v := vm.DictionaryAt(md, vm.registry.NewStringValue("zone")); vm.registry.GetStringContent(v) != "eu-west" {
		t.Errorf("metadata zone: got %v", v)
	}
	// Not connected → isConnected false, node nil.
	if got := vm.Send(member, "isConnected", nil); got != False {
		t.Error("isConnected should be false with no core")
	}
}

func TestClusterMember_NonSerializable(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()
	member := vm.newClusterMemberValue(ClusterMemberInfo{Addr: "x:1"})
	if _, err := vm.SerializeValue(member); err == nil {
		t.Fatal("ClusterMember must not be serializable")
	}
}

func TestClusterMember_NodeReusesCoreRef(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	var id [32]byte
	id[0] = 0x11
	pub, priv := testKeys(t)
	ref := NewNodeRefData("node2:8081", pub, priv)
	core := &fakeClusterCore{connected: map[[32]byte]*NodeRefData{id: ref}}
	vm.SetClusterCore(core)

	member := vm.newClusterMemberValue(ClusterMemberInfo{ID: id, Addr: "node2:8081", Status: ClusterStatusAlive})
	if got := vm.Send(member, "isConnected", nil); got != True {
		t.Error("isConnected should be true when the core holds a ref")
	}
	node := vm.Send(member, "node", nil)
	if vm.getNodeRef(node) != ref {
		t.Error("member node must reuse the core's exact NodeRefData, not a new one")
	}
}

func TestClusterRuntime_MembersAndDelegation(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()

	var a, b [32]byte
	a[0], b[0] = 1, 2
	core := &fakeClusterCore{infos: []ClusterMemberInfo{
		{ID: a, Addr: "a:1", Status: ClusterStatusAlive},
		{ID: b, Addr: "b:2", Status: ClusterStatusAlive},
	}}
	vm.SetClusterCore(core)
	runtime := vm.globals["ClusterRuntime"]

	arr := vm.Send(runtime, "members", nil)
	arrObj := ObjectFromValue(arr)
	if arrObj == nil || arrObj.NumSlots() != 2 {
		t.Fatalf("members should return 2 ClusterMembers, got %v", arr)
	}
	if got := vm.Send(runtime, "hasCore", nil); got != True {
		t.Error("hasCore should be true")
	}

	vm.Send(runtime, "setJoinPolicy:", []Value{vm.Symbols.SymbolValue("lazy")})
	if len(core.joinCalls) != 1 || core.joinCalls[0] != "lazy" {
		t.Errorf("setJoinPolicy: should delegate 'lazy', got %v", core.joinCalls)
	}

	dict := vm.NewDictionary()
	vm.DictionaryAtPut(dict, vm.registry.NewStringValue("zone"), vm.registry.NewStringValue("us"))
	vm.Send(runtime, "setMetadata:", []Value{dict})
	if len(core.metaCalls) != 1 || core.metaCalls[0]["zone"] != "us" {
		t.Errorf("setMetadata: should delegate {zone:us}, got %v", core.metaCalls)
	}
}

func TestDeliverClusterEvent_ToMailbox(t *testing.T) {
	vm := NewVM()
	defer vm.Shutdown()
	vm.RegisterProcessName(ClusterEventProcessName, vm.MainProcessID())
	proc := vm.GetProcessByID(vm.MainProcessID())

	var id [32]byte
	id[0] = 0x22
	vm.DeliverClusterEvent(SelectorMemberUp, ClusterMemberInfo{ID: id, Addr: "n:1", Status: ClusterStatusAlive})

	msg, ok := proc.Mailbox().TryReceive()
	if !ok {
		t.Fatal("no cluster event delivered to mailbox")
	}
	mo := ObjectFromValue(msg)
	if mo == nil {
		t.Fatal("event message is not an object")
	}
	// slot 1 = selector symbol, slot 2 = ClusterMember payload
	if got := vm.Send(mo.GetSlot(2), "addr", nil); vm.registry.GetStringContent(got) != "n:1" {
		t.Errorf("delivered member addr: got %v", got)
	}
}
