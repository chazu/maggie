package server

import (
	"context"
	"crypto/ed25519"
	"crypto/rand"
	"strconv"
	"sync/atomic"
	"testing"
	"time"

	"connectrpc.com/connect"
	"github.com/fxamacker/cbor/v2"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
	"github.com/chazu/maggie/vm/wire"
)

// Request-auth nonces are checked per SENDER identity in one replay window,
// so every client interceptor signing as the same identity must share one
// monotonic counter. Per-interceptor counters seeded from wall-clock nanos let
// a newer client (e.g. the per-address remote-channel client) lock out an
// older one ("nonce … below replay window").
func TestClientAuthInterceptor_SharedNonceAcrossInterceptors(t *testing.T) {
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	var nodeID [32]byte
	copy(nodeID[:], pub)
	sign := func(b []byte) []byte { return ed25519.Sign(priv, b) }

	older := NewClientAuthInterceptor(nodeID, sign)
	time.Sleep(2 * time.Millisecond)
	newer := NewClientAuthInterceptor(nodeID, sign)

	nonceVia := func(ic connect.UnaryInterceptorFunc) uint64 {
		var got uint64
		next := func(_ context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
			got, _ = strconv.ParseUint(req.Header().Get(wire.HeaderNonce), 10, 64)
			return nil, nil
		}
		_, _ = ic(next)(context.Background(), connect.NewRequest(&maggiev1.PingRequest{}))
		return got
	}

	trust := dist.NewTrustStore(dist.TrustPolicy{DefaultPerms: dist.PermMessage})
	peer := dist.NodeIDFromBytes(nodeID[:])
	// The newer client races far ahead (heartbeats, channel ops) ...
	for i := 0; i < 2000; i++ {
		if err := trust.CheckNonce(peer, dist.NonceStreamRequest, nonceVia(newer)); err != nil {
			t.Fatalf("newer interceptor rejected: %v", err)
		}
	}
	// ... and the older client must still be accepted.
	if err := trust.CheckNonce(peer, dist.NonceStreamRequest, nonceVia(older)); err != nil {
		t.Fatalf("older interceptor locked out by newer one for same identity: %v", err)
	}
}

// A receive on a channel that was closed and then unexported (fully drained)
// on the owner must answer "closed", not "channel not found".
func TestChannelOps_UnknownExportAnswersClosed(t *testing.T) {
	svc, _, cleanup := newChannelTestService(t)
	defer cleanup()
	ctx := context.Background()
	const unknown = 0xDEADBEEF

	rr, err := svc.ChannelReceive(ctx, connect.NewRequest(&maggiev1.ChannelReceiveRequest{ChannelId: unknown}))
	if err != nil {
		t.Fatal(err)
	}
	if !rr.Msg.Success || rr.Msg.ChannelOpen {
		t.Fatalf("receive on unknown export: got success=%v open=%v err=%q, want closed", rr.Msg.Success, rr.Msg.ChannelOpen, rr.Msg.Error)
	}

	tr, err := svc.ChannelTryReceive(ctx, connect.NewRequest(&maggiev1.ChannelReceiveRequest{ChannelId: unknown}))
	if err != nil {
		t.Fatal(err)
	}
	if tr.Msg.GotValue || tr.Msg.ChannelOpen || tr.Msg.Error != "" {
		t.Fatalf("tryReceive on unknown export: %+v, want closed without error", tr.Msg)
	}

	ts, err := svc.ChannelTrySend(ctx, connect.NewRequest(&maggiev1.ChannelSendRequest{ChannelId: unknown}))
	if err != nil {
		t.Fatal(err)
	}
	if ts.Msg.Sent || ts.Msg.Error != "" {
		t.Fatalf("trySend on unknown export: %+v, want not-sent without error", ts.Msg)
	}

	sr, err := svc.ChannelSend(ctx, connect.NewRequest(&maggiev1.ChannelSendRequest{ChannelId: unknown}))
	if err != nil {
		t.Fatal(err)
	}
	if sr.Msg.Success || sr.Msg.Error != vm.RemoteChannelClosedMsg {
		t.Fatalf("send on unknown export: %+v, want the closed-channel answer", sr.Msg)
	}
}

// Demonitor must also detach the monitor from the watched process, or the
// DOWN is still sent when it exits.
func TestDemonitorProcess_SuppressesDown(t *testing.T) {
	svc, v, cleanup := newChannelTestService(t)
	defer cleanup()

	v.RegisterProcessName("svc", v.MainProcessID())
	watcherNode := [32]byte{0x5A}
	ctx := context.WithValue(context.Background(), peerIdentityKey{}, dist.NodeIDFromBytes(watcherNode[:]))

	pub, priv, _ := ed25519.GenerateKey(rand.Reader)
	ref := vm.NewNodeRefData("watcher:1", pub, priv)
	ref.SetPeerID(watcherNode)
	var downs atomic.Int32
	ref.SendFunc = func([]byte) ([]byte, string, string, error) {
		downs.Add(1)
		return nil, "", "", nil
	}
	v.RegisterNodeRef(ref)

	if _, err := svc.MonitorProcess(ctx, connect.NewRequest(&maggiev1.MonitorProcessRequest{
		MonitorRefId: 1, WatcherId: 10, TargetName: "svc"})); err != nil {
		t.Fatal(err)
	}
	if _, err := svc.DemonitorProcess(ctx, connect.NewRequest(&maggiev1.DemonitorProcessRequest{
		MonitorRefId: 1})); err != nil {
		t.Fatal(err)
	}

	v.FinishProcess(v.GetProcessByID(v.MainProcessID()), vm.ExitNormal(vm.Nil))
	time.Sleep(50 * time.Millisecond)
	if n := downs.Load(); n != 0 {
		t.Fatalf("DOWN sent for a cancelled monitor (%d)", n)
	}
}

// A DOWN for one of our outbound monitors must come from the node we are
// monitoring; another peer must not be able to fire (and consume) it.
func TestHandleRemoteDown_RejectsWrongSender(t *testing.T) {
	svc, v, cleanup := newChannelTestService(t)
	defer cleanup()

	watched := [32]byte{0x11}
	v.RemoteWatches().AddOutboundMonitor(&vm.RemoteMonitorRef{
		RefID: 7, WatcherID: v.MainProcessID(), RemoteNode: watched, Outbound: true})

	payload, _ := cbor.Marshal(struct {
		RefID  uint64 `cbor:"1,keyasint"`
		Signal string `cbor:"2,keyasint"`
		Normal bool   `cbor:"3,keyasint"`
	}{RefID: 7, Signal: "killed"})
	env := &dist.MessageEnvelope{Selector: vm.SelectorDown, Payload: payload}

	imposter := dist.NodeIDFromBytes([]byte{0x22, 31: 0})
	if _, err := svc.handleRemoteDown(env, imposter); err != nil {
		t.Fatal(err)
	}
	if v.RemoteWatches().OutboundCount() != 1 {
		t.Fatal("DOWN from a non-watched peer consumed our monitor")
	}
	if _, err := svc.handleRemoteDown(env, dist.NodeIDFromBytes(watched[:])); err != nil {
		t.Fatal(err)
	}
	if v.RemoteWatches().OutboundCount() != 0 {
		t.Fatal("DOWN from the watched peer should consume the monitor")
	}
}
