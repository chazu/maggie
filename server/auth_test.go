package server

import (
	"context"
	"crypto/sha256"
	"errors"
	"fmt"
	"net"
	"net/http"
	"strconv"
	"testing"
	"time"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
	"github.com/chazu/maggie/vm/wire"
)

// startAuthedTestServer mounts the sync service EXACTLY as production does:
// behind the auth interceptor with the standard permission table.
func startAuthedTestServer(t *testing.T, store *vm.ContentStore, trust *dist.TrustStore) (string, func()) {
	t.Helper()
	worker := NewVMWorker(vm.NewVM())
	svc := NewSyncService(worker, store, trust, nil, nil)

	mux := http.NewServeMux()
	path, handler := maggiev1connect.NewSyncServiceHandler(svc,
		connect.WithInterceptors(NewAuthInterceptor(trust, SyncPermTable())))
	mux.Handle(path, handler)

	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	srv := &http.Server{Handler: mux}
	go func() { _ = srv.Serve(listener) }()

	baseURL := fmt.Sprintf("http://%s", listener.Addr().String())
	return baseURL, func() {
		srv.Close()
		worker.Stop()
	}
}

// signedClient returns a sync client that authenticates as id.
func signedClient(baseURL string, id *dist.NodeIdentity) maggiev1connect.SyncServiceClient {
	return maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL,
		connect.WithInterceptors(NewClientAuthInterceptor(id.NodeID(), id.Sign)))
}

// fixedAuthClient signs every request with the SAME timestamp+nonce — for
// replay tests.
func fixedAuthClient(baseURL string, id *dist.NodeIdentity, ts int64, nonce uint64) maggiev1connect.SyncServiceClient {
	interceptor := connect.UnaryInterceptorFunc(func(next connect.UnaryFunc) connect.UnaryFunc {
		return func(ctx context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
			sig := wire.SignRequest(id.PrivateKey, req.Spec().Procedure, ts, nonce)
			h := req.Header()
			h.Set(wire.HeaderNodeID, id.NodeIDHex())
			h.Set(wire.HeaderTimestamp, strconv.FormatInt(ts, 10))
			h.Set(wire.HeaderNonce, strconv.FormatUint(nonce, 10))
			h.Set(wire.HeaderSignature, fmt.Sprintf("%x", sig))
			return next(ctx, req)
		}
	})
	return maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL,
		connect.WithInterceptors(interceptor))
}

func mustIdentity(t *testing.T) *dist.NodeIdentity {
	t.Helper()
	id, err := dist.GenerateIdentity()
	if err != nil {
		t.Fatalf("GenerateIdentity: %v", err)
	}
	return id
}

func isCode(err error, code connect.Code) bool {
	var cerr *connect.Error
	return errors.As(err, &cerr) && cerr.Code() == code
}

// Unauthenticated requests to any non-anonymous procedure are rejected.
func TestAuth_UnauthenticatedRejected(t *testing.T) {
	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), dist.NewPermissiveTrustStore())
	defer stop()

	// No auth interceptor on this client.
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	_, err := client.List(context.Background(), connect.NewRequest(&maggiev1.ListRequest{}))
	if !isCode(err, connect.CodeUnauthenticated) {
		t.Fatalf("unauthenticated List: got %v, want CodeUnauthenticated", err)
	}
}

// A signed request from a peer with the required permission passes.
func TestAuth_AuthenticatedAllowed(t *testing.T) {
	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), dist.NewPermissiveTrustStore())
	defer stop()

	client := signedClient(baseURL, mustIdentity(t))
	if _, err := client.List(context.Background(), connect.NewRequest(&maggiev1.ListRequest{})); err != nil {
		t.Fatalf("authenticated List: %v", err)
	}
}

// A peer whose trust record lacks the required permission is denied —
// deny-by-default for unknown peers under a secure policy.
func TestAuth_PermissionDenied(t *testing.T) {
	trust := dist.NewSecureTrustStore() // unknown peers: PermNone
	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), trust)
	defer stop()

	client := signedClient(baseURL, mustIdentity(t))
	rootHash := sha256.Sum256([]byte("root"))
	_, err := client.Announce(context.Background(), connect.NewRequest(&maggiev1.AnnounceRequest{
		RootHash:  rootHash[:],
		AllHashes: [][]byte{rootHash[:]},
	}))
	if !isCode(err, connect.CodePermissionDenied) {
		t.Fatalf("Announce without PermSync: got %v, want CodePermissionDenied", err)
	}
}

// A configured peer holding only PermSync cannot touch the message plane.
func TestAuth_PerProcedurePerms(t *testing.T) {
	trust := dist.NewSecureTrustStore()
	id := mustIdentity(t)
	trust.AddConfiguredPeer(dist.NodeIDFromBytes(id.PublicKey), "sync-only", dist.PermSync)

	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), trust)
	defer stop()

	client := signedClient(baseURL, id)
	ctx := context.Background()

	if _, err := client.List(ctx, connect.NewRequest(&maggiev1.ListRequest{})); err != nil {
		t.Fatalf("PermSync peer List: %v", err)
	}
	_, err := client.ChannelStatus(ctx, connect.NewRequest(&maggiev1.ChannelStatusRequest{ChannelId: 1}))
	if !isCode(err, connect.CodePermissionDenied) {
		t.Fatalf("PermSync peer ChannelStatus: got %v, want CodePermissionDenied", err)
	}
}

// A banned peer is rejected even with a valid signature and permission.
func TestAuth_BannedPeerRejected(t *testing.T) {
	trust := dist.NewPermissiveTrustStore()
	id := mustIdentity(t)
	peerID := dist.NodeIDFromBytes(id.PublicKey)
	trust.RecordHashMismatch(peerID)
	trust.RecordHashMismatch(peerID)
	trust.RecordHashMismatch(peerID) // threshold 3 → banned

	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), trust)
	defer stop()

	client := signedClient(baseURL, id)
	_, err := client.List(context.Background(), connect.NewRequest(&maggiev1.ListRequest{}))
	if !isCode(err, connect.CodePermissionDenied) {
		t.Fatalf("banned peer List: got %v, want CodePermissionDenied", err)
	}
}

// Replaying an identical signed request (same nonce) is rejected.
func TestAuth_NonceReplayRejected(t *testing.T) {
	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), dist.NewPermissiveTrustStore())
	defer stop()

	id := mustIdentity(t)
	client := fixedAuthClient(baseURL, id, time.Now().UnixNano(), 42)
	ctx := context.Background()

	if _, err := client.List(ctx, connect.NewRequest(&maggiev1.ListRequest{})); err != nil {
		t.Fatalf("first request: %v", err)
	}
	_, err := client.List(ctx, connect.NewRequest(&maggiev1.ListRequest{}))
	if !isCode(err, connect.CodeUnauthenticated) {
		t.Fatalf("replayed request: got %v, want CodeUnauthenticated", err)
	}
}

// Stale timestamps are rejected.
func TestAuth_StaleTimestampRejected(t *testing.T) {
	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), dist.NewPermissiveTrustStore())
	defer stop()

	id := mustIdentity(t)
	stale := time.Now().Add(-2 * wire.MaxClockSkew).UnixNano()
	client := fixedAuthClient(baseURL, id, stale, 7)
	_, err := client.List(context.Background(), connect.NewRequest(&maggiev1.ListRequest{}))
	if !isCode(err, connect.CodeUnauthenticated) {
		t.Fatalf("stale request: got %v, want CodeUnauthenticated", err)
	}
}

// Ping is anonymous (liveness only): unauthenticated callers succeed but get
// no content count; authenticated callers get the count. Anonymous calls
// must not create trust-store records.
func TestAuth_PingAnonymousAndAuthenticated(t *testing.T) {
	store := vm.NewContentStore()
	m := vm.NewCompiledMethodBuilder("m1", 0).Build()
	mh := sha256.Sum256([]byte("m1"))
	m.SetContentHash(mh)
	store.IndexMethod(m)

	trust := dist.NewPermissiveTrustStore()
	baseURL, stop := startAuthedTestServer(t, store, trust)
	defer stop()
	ctx := context.Background()

	// Anonymous: succeeds, count withheld, no PeerRecord created.
	anon := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	resp, err := anon.Ping(ctx, connect.NewRequest(&maggiev1.PingRequest{}))
	if err != nil {
		t.Fatalf("anonymous Ping: %v", err)
	}
	if resp.Msg.ContentCount != 0 {
		t.Errorf("anonymous ContentCount: got %d, want 0", resp.Msg.ContentCount)
	}
	if n := trust.PeerCount(); n != 0 {
		t.Errorf("anonymous Ping created %d peer records, want 0", n)
	}

	// Authenticated: gets the count.
	authed := signedClient(baseURL, mustIdentity(t))
	resp, err = authed.Ping(ctx, connect.NewRequest(&maggiev1.PingRequest{}))
	if err != nil {
		t.Fatalf("authenticated Ping: %v", err)
	}
	if resp.Msg.ContentCount != 1 {
		t.Errorf("authenticated ContentCount: got %d, want 1", resp.Msg.ContentCount)
	}
}

// Garbage signatures with a spoofed victim identity must not create peer
// records or strikes — an attacker cannot frame a victim into a ban.
func TestAuth_SpoofedIdentityCannotFrameVictim(t *testing.T) {
	trust := dist.NewPermissiveTrustStore()
	baseURL, stop := startAuthedTestServer(t, vm.NewContentStore(), trust)
	defer stop()

	victim := mustIdentity(t)
	attacker := mustIdentity(t)
	ctx := context.Background()

	// Attacker claims the victim's NodeID but signs with their own key.
	forged := connect.UnaryInterceptorFunc(func(next connect.UnaryFunc) connect.UnaryFunc {
		return func(ctx context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
			ts := time.Now().UnixNano()
			sig := wire.SignRequest(attacker.PrivateKey, req.Spec().Procedure, ts, 99)
			h := req.Header()
			h.Set(wire.HeaderNodeID, victim.NodeIDHex()) // spoofed
			h.Set(wire.HeaderTimestamp, strconv.FormatInt(ts, 10))
			h.Set(wire.HeaderNonce, "99")
			h.Set(wire.HeaderSignature, fmt.Sprintf("%x", sig))
			return next(ctx, req)
		}
	})
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL,
		connect.WithInterceptors(forged))

	for i := 0; i < 5; i++ {
		_, err := client.List(ctx, connect.NewRequest(&maggiev1.ListRequest{}))
		if !isCode(err, connect.CodeUnauthenticated) {
			t.Fatalf("forged request %d: got %v, want CodeUnauthenticated", i, err)
		}
	}

	victimID := dist.NodeIDFromBytes(victim.PublicKey)
	if trust.IsBanned(victimID) {
		t.Fatal("victim was banned by forged requests")
	}
	if rec := trust.Peer(victimID); rec != nil {
		t.Fatalf("forged requests created a peer record for the victim: %+v", rec)
	}

	// The victim can still operate normally.
	if _, err := signedClient(baseURL, victim).List(ctx, connect.NewRequest(&maggiev1.ListRequest{})); err != nil {
		t.Fatalf("victim's own request after forgery attempts: %v", err)
	}
}
