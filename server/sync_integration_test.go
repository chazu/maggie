package server

import (
	"context"
	"crypto/sha256"
	"fmt"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"testing"
	"time"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// startTestServer creates an in-process sync server on a random port and
// returns the base URL and a stop function.
func startTestServer(t *testing.T, store *vm.ContentStore, compile func(string) ([32]byte, error), policy *dist.CapabilityPolicy) (string, func()) {
	t.Helper()

	if policy == nil {
		policy = dist.NewPermissivePolicy()
	}

	worker := NewVMWorker(vm.NewVM())
	peers := dist.NewPeerStore()
	svc := NewSyncService(worker, store, peers, policy, compile, nil)

	mux := http.NewServeMux()
	path, handler := maggiev1connect.NewSyncServiceHandler(svc)
	mux.Handle(path, handler)

	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}

	srv := &http.Server{Handler: mux}
	go func() { _ = srv.Serve(listener) }()

	baseURL := fmt.Sprintf("http://%s", listener.Addr().String())
	stop := func() {
		srv.Close()
		worker.Stop()
	}

	return baseURL, stop
}

// TestEndToEnd_PushPull tests the full push/pull cycle between two VMs.
//
// VM-A has a class with methods. We push A→B, then verify B received
// all methods and the class digest.
func TestEndToEnd_PushPull(t *testing.T) {
	// --- VM-A: populate with methods and a class ---
	storeA := vm.NewContentStore()
	compile := func(source string) ([32]byte, error) {
		return sha256.Sum256([]byte(source)), nil
	}

	// Create methods
	m1Source := "method: hello [ ^'hello' ]"
	m1h := sha256.Sum256([]byte(m1Source))
	m1 := vm.NewCompiledMethodBuilder("hello", 0).Build()
	m1.Source = m1Source
	m1.SetContentHash(m1h)
	storeA.IndexMethod(m1)

	m2Source := "method: world [ ^'world' ]"
	m2h := sha256.Sum256([]byte(m2Source))
	m2 := vm.NewCompiledMethodBuilder("world", 0).Build()
	m2.Source = m2Source
	m2.SetContentHash(m2h)
	storeA.IndexMethod(m2)

	// Create class digest
	classDigest := &vm.ClassDigest{
		Name:         "Greeter",
		Namespace:    "MyApp",
		MethodHashes: [][32]byte{m1h, m2h},
	}
	classDigest.Hash = vm.HashClass(
		classDigest.Name, classDigest.Namespace, "",
		nil, nil, "", classDigest.MethodHashes,
	)
	storeA.IndexClass(classDigest)

	// --- VM-B: empty store, start server ---
	storeB := vm.NewContentStore()
	baseURL, stop := startTestServer(t, storeB, compile, nil)
	defer stop()

	// Give server a moment to start
	time.Sleep(10 * time.Millisecond)

	// --- Push from A to B ---
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	allHashes := storeA.AllHashes()
	hashBytes := make([][]byte, len(allHashes))
	for i, h := range allHashes {
		hCopy := h
		hashBytes[i] = hCopy[:]
	}

	// Announce
	annResp, err := client.Announce(ctx, connect.NewRequest(&maggiev1.AnnounceRequest{
		RootHash:  classDigest.Hash[:],
		AllHashes: hashBytes,
	}))
	if err != nil {
		t.Fatalf("Announce failed: %v", err)
	}
	if annResp.Msg.Status != maggiev1.AnnounceStatus_ANNOUNCE_ACCEPTED {
		t.Fatalf("Expected ACCEPTED, got %v", annResp.Msg.Status)
	}
	if len(annResp.Msg.Want) != 3 { // 2 methods + 1 class
		t.Errorf("Want: got %d, want 3", len(annResp.Msg.Want))
	}

	// Build chunks (methods first, then class)
	var methodChunks [][]byte
	var classChunks [][]byte
	for _, wantHash := range annResp.Msg.Want {
		var h [32]byte
		copy(h[:], wantHash)

		if m := storeA.LookupMethod(h); m != nil {
			chunk := dist.MethodToChunk(m, nil)
			data, _ := dist.MarshalChunk(chunk)
			methodChunks = append(methodChunks, data)
		} else if d := storeA.LookupClass(h); d != nil {
			chunk := dist.ClassToChunk(d, nil)
			data, _ := dist.MarshalChunk(chunk)
			classChunks = append(classChunks, data)
		}
	}
	chunks := append(methodChunks, classChunks...)

	// Transfer
	txResp, err := client.Transfer(ctx, connect.NewRequest(&maggiev1.TransferRequest{
		Chunks: chunks,
	}))
	if err != nil {
		t.Fatalf("Transfer failed: %v", err)
	}
	if txResp.Msg.Accepted != 3 {
		t.Errorf("Accepted: got %d, want 3", txResp.Msg.Accepted)
	}
	if txResp.Msg.Rejected != 0 {
		t.Errorf("Rejected: got %d, want 0", txResp.Msg.Rejected)
	}

	// --- Verify B has everything ---
	if storeB.MethodCount() != 2 {
		t.Errorf("Store B methods: got %d, want 2", storeB.MethodCount())
	}
	if storeB.ClassCount() != 1 {
		t.Errorf("Store B classes: got %d, want 1", storeB.ClassCount())
	}
	if !storeB.HasHash(m1h) {
		t.Error("Store B missing method1 hash")
	}
	if !storeB.HasHash(m2h) {
		t.Error("Store B missing method2 hash")
	}
	if !storeB.HasHash(classDigest.Hash) {
		t.Error("Store B missing class hash")
	}

	// --- Pull from B (should get everything back) ---
	// Create a VM-C with empty store, pull from B
	storeC := vm.NewContentStore()

	serveResp, err := client.Serve(ctx, connect.NewRequest(&maggiev1.ServeRequest{
		RootHash: classDigest.Hash[:],
	}))
	if err != nil {
		t.Fatalf("Serve (pull) failed: %v", err)
	}
	if len(serveResp.Msg.Available) != 3 {
		t.Errorf("Available: got %d, want 3", len(serveResp.Msg.Available))
	}
	if len(serveResp.Msg.Chunks) != 3 {
		t.Errorf("Chunks: got %d, want 3", len(serveResp.Msg.Chunks))
	}

	// Verify and index in C
	for _, chunkBytes := range serveResp.Msg.Chunks {
		chunk, err := dist.UnmarshalChunk(chunkBytes)
		if err != nil {
			t.Fatalf("Unmarshal chunk: %v", err)
		}
		switch chunk.Type {
		case dist.ChunkMethod:
			m := &vm.CompiledMethod{Source: chunk.Content}
			m.SetContentHash(chunk.Hash)
			storeC.IndexMethod(m)
		case dist.ChunkClass:
			d, decErr := dist.DecodeClassContent(chunk.Content)
			if decErr != nil {
				d = &vm.ClassDigest{Name: chunk.Content}
			}
			d.Hash = chunk.Hash
			d.MethodHashes = chunk.Dependencies
			storeC.IndexClass(d)
		}
	}

	if storeC.MethodCount() != 2 {
		t.Errorf("Store C methods: got %d, want 2", storeC.MethodCount())
	}
	if storeC.ClassCount() != 1 {
		t.Errorf("Store C classes: got %d, want 1", storeC.ClassCount())
	}

	// Verify that class digest on C has rich metadata (from EncodeClassContent)
	for _, ch := range storeC.ClassHashes() {
		d := storeC.LookupClass(ch)
		if d == nil {
			continue
		}
		if d.Name != "Greeter" {
			t.Errorf("Store C class name: got %q, want %q", d.Name, "Greeter")
		}
		if d.Namespace != "MyApp" {
			t.Errorf("Store C class namespace: got %q, want %q", d.Namespace, "MyApp")
		}
	}
}

// TestEndToEnd_PushPullRehydrate tests the full push/pull/rehydrate cycle.
// VM-A creates a real compiled class, pushes to VM-B (server), pulls to VM-C,
// rehydrates on VM-C, and verifies the method is callable.
func TestEndToEnd_PushPullRehydrate(t *testing.T) {
	// --- VM-A: create a real compiled class ---
	vmA := newTestVMForServer(t)
	storeA := vmA.ContentStore()

	// Create a class with a method using the pipeline
	pipe := &pipeline.Pipeline{VM: vmA}
	source := `TestSyncGreeter subclass: Object
  method: answer [ ^42 ]
  classMethod: classAnswer [ ^99 ]
`
	compiled, err := pipe.CompileSourceFile(source, "test.mag", "")
	if err != nil {
		t.Fatalf("CompileSourceFile: %v", err)
	}
	if compiled == 0 {
		t.Fatal("expected compiled methods")
	}

	// Find the class digest
	var classDigest *vm.ClassDigest
	for _, ch := range storeA.ClassHashes() {
		d := storeA.LookupClass(ch)
		if d != nil && d.Name == "TestSyncGreeter" {
			classDigest = d
			break
		}
	}
	if classDigest == nil {
		t.Fatal("TestSyncGreeter class digest not found in store A")
	}

	// --- VM-B: start server ---
	storeB := vm.NewContentStore()
	// Use a real compile function that matches the content-hashing system
	realCompile := func(source string) ([32]byte, error) {
		md, err := compiler.ParseMethodDef(source)
		if err != nil {
			return [32]byte{}, err
		}
		return hash.HashMethod(md, nil, func(name string) string { return name }), nil
	}
	baseURL, stop := startTestServer(t, storeB, realCompile, nil)
	defer stop()
	time.Sleep(10 * time.Millisecond)

	// --- Push from A to B ---
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	allHashes := storeA.AllHashes()
	hashBytes := make([][]byte, len(allHashes))
	for i, h := range allHashes {
		hCopy := h
		hashBytes[i] = hCopy[:]
	}

	annResp, err := client.Announce(ctx, connect.NewRequest(&maggiev1.AnnounceRequest{
		RootHash:  classDigest.Hash[:],
		AllHashes: hashBytes,
	}))
	if err != nil {
		t.Fatalf("Announce failed: %v", err)
	}
	if annResp.Msg.Status != maggiev1.AnnounceStatus_ANNOUNCE_ACCEPTED {
		t.Fatalf("Expected ACCEPTED, got %v", annResp.Msg.Status)
	}

	// Build and transfer chunks
	var methodChunks, classChunks [][]byte
	for _, wantHash := range annResp.Msg.Want {
		var h [32]byte
		copy(h[:], wantHash)
		if m := storeA.LookupMethod(h); m != nil {
			chunk := dist.MethodToChunk(m, nil)
			data, _ := dist.MarshalChunk(chunk)
			methodChunks = append(methodChunks, data)
		} else if d := storeA.LookupClass(h); d != nil {
			chunk := dist.ClassToChunk(d, nil)
			data, _ := dist.MarshalChunk(chunk)
			classChunks = append(classChunks, data)
		}
	}
	chunks := append(methodChunks, classChunks...)

	_, err = client.Transfer(ctx, connect.NewRequest(&maggiev1.TransferRequest{
		Chunks: chunks,
	}))
	if err != nil {
		t.Fatalf("Transfer failed: %v", err)
	}

	// --- Pull from B to C ---
	vmC := newTestVMForServer(t)
	storeC := vmC.ContentStore()

	serveResp, err := client.Serve(ctx, connect.NewRequest(&maggiev1.ServeRequest{
		RootHash: classDigest.Hash[:],
	}))
	if err != nil {
		t.Fatalf("Serve failed: %v", err)
	}

	// Index chunks in C
	for _, chunkBytes := range serveResp.Msg.Chunks {
		chunk, err := dist.UnmarshalChunk(chunkBytes)
		if err != nil {
			t.Fatalf("Unmarshal: %v", err)
		}
		switch chunk.Type {
		case dist.ChunkMethod:
			m := &vm.CompiledMethod{Source: chunk.Content}
			m.SetContentHash(chunk.Hash)
			storeC.IndexMethod(m)
		case dist.ChunkClass:
			d, decErr := dist.DecodeClassContent(chunk.Content)
			if decErr != nil {
				d = &vm.ClassDigest{Name: chunk.Content}
			}
			d.Hash = chunk.Hash
			d.MethodHashes = chunk.Dependencies
			storeC.IndexClass(d)
		}
	}

	// --- Rehydrate on VM-C ---
	rehydrated, err := pipeline.RehydrateFromStore(vmC)
	if err != nil {
		t.Fatalf("RehydrateFromStore: %v", err)
	}
	if rehydrated != 2 { // answer + classAnswer
		t.Errorf("Rehydrated: got %d, want 2", rehydrated)
	}

	// --- Verify the class exists and methods work ---
	cls := vmC.Classes.Lookup("TestSyncGreeter")
	if cls == nil {
		t.Fatal("TestSyncGreeter not found in VM-C ClassTable")
	}
	if _, ok := vmC.Globals["TestSyncGreeter"]; !ok {
		t.Error("TestSyncGreeter not found in VM-C Globals")
	}

	// Test instance method
	inst := vmC.Send(vmC.ClassValue(cls), "new", nil)
	result := vmC.Send(inst, "answer", nil)
	if !result.IsSmallInt() || result.SmallInt() != 42 {
		t.Errorf("TestSyncGreeter>>answer = %v, want 42", result)
	}

	// Test class method
	classResult := vmC.Send(vmC.ClassValue(cls), "classAnswer", nil)
	if !classResult.IsSmallInt() || classResult.SmallInt() != 99 {
		t.Errorf("TestSyncGreeter class>>classAnswer = %v, want 99", classResult)
	}
}

// newTestVMForServer creates a VM loaded with the standard image, suitable
// for integration tests that need a fully functional VM.
func newTestVMForServer(t *testing.T) *vm.VM {
	t.Helper()
	vmInst := vm.NewVM()
	data, err := os.ReadFile(filepath.Join("..", "cmd", "mag", "maggie.image"))
	if err != nil {
		t.Fatalf("reading maggie.image: %v", err)
	}
	if err := vmInst.LoadImageFromBytes(data); err != nil {
		t.Fatalf("loading image: %v", err)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)
	return vmInst
}

// TestEndToEnd_CapabilityRejection verifies that a receiver with restricted
// capabilities rejects announcements requiring denied capabilities.
func TestEndToEnd_CapabilityRejection(t *testing.T) {
	store := vm.NewContentStore()
	// Server only allows "File" capability
	policy := dist.NewRestrictedPolicy([]string{"File"})
	baseURL, stop := startTestServer(t, store, nil, policy)
	defer stop()

	time.Sleep(10 * time.Millisecond)

	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	rootHash := sha256.Sum256([]byte("root"))

	// Build capability manifest requiring "Network" (not allowed)
	capManifest := &dist.CapabilityManifest{Required: []string{"Network"}}
	capBytes, err := dist.MarshalCapabilityManifest(capManifest)
	if err != nil {
		t.Fatalf("marshal capability: %v", err)
	}

	resp, err := client.Announce(ctx, connect.NewRequest(&maggiev1.AnnounceRequest{
		RootHash:           rootHash[:],
		AllHashes:          [][]byte{rootHash[:]},
		CapabilityManifest: capBytes,
	}))
	if err != nil {
		t.Fatalf("Announce failed: %v", err)
	}
	if resp.Msg.Status != maggiev1.AnnounceStatus_ANNOUNCE_REJECTED {
		t.Errorf("Expected REJECTED, got %v", resp.Msg.Status)
	}
	if resp.Msg.RejectReason == "" {
		t.Error("Expected reject reason to be set")
	}
}

// TestEndToEnd_BannedPeer simulates repeated hash mismatches and verifies
// that the peer gets banned.
func TestEndToEnd_BannedPeer(t *testing.T) {
	store := vm.NewContentStore()
	compile := func(source string) ([32]byte, error) {
		return sha256.Sum256([]byte(source)), nil
	}
	baseURL, stop := startTestServer(t, store, compile, nil)
	defer stop()

	time.Sleep(10 * time.Millisecond)

	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	// Send 3 chunks with hash mismatches to trigger ban (threshold=3)
	for i := 0; i < 3; i++ {
		h := sha256.Sum256([]byte(fmt.Sprintf("claimed-%d", i)))
		chunk := &dist.Chunk{
			Hash:    h,
			Type:    dist.ChunkMethod,
			Content: fmt.Sprintf("actual-%d", i), // different from hash
		}
		data, _ := dist.MarshalChunk(chunk)

		_, err := client.Transfer(ctx, connect.NewRequest(&maggiev1.TransferRequest{
			Chunks: [][]byte{data},
		}))
		if err != nil {
			// After ban threshold, subsequent requests may fail
			// This is expected behavior
			continue
		}
	}

	// Now try to announce — should be rejected because peer uses "unknown"
	// identity (since peerFromRequest returns "unknown" for test HTTP client)
	rootHash := sha256.Sum256([]byte("root"))
	_, err := client.Announce(ctx, connect.NewRequest(&maggiev1.AnnounceRequest{
		RootHash:  rootHash[:],
		AllHashes: [][]byte{rootHash[:]},
	}))
	if err == nil {
		t.Fatal("Announce from banned peer should fail")
	}
}
