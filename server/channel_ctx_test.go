package server

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/vm"
)

// SD-7 regression: ChannelSend/ChannelReceive must honor ctx cancellation so
// a disconnected client frees the handler goroutine instead of pinning it
// forever on a full/empty channel.

func newChannelTestService(t *testing.T) (*SyncService, *vm.VM, func()) {
	t.Helper()
	v := vm.NewVM()
	worker := NewVMWorker(v)
	svc := NewSyncService(worker, vm.NewContentStore(), nil, nil, nil)
	return svc, v, func() {
		worker.Stop()
		v.Shutdown()
	}
}

func TestChannelReceive_CtxCanceled(t *testing.T) {
	svc, v, cleanup := newChannelTestService(t)
	defer cleanup()

	ch := vm.NewChannelObject(0) // empty, never receives a value
	id := v.ExportChannel(ch)

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	start := time.Now()
	_, err := svc.ChannelReceive(ctx, connect.NewRequest(
		&maggiev1.ChannelReceiveRequest{ChannelId: id}))
	elapsed := time.Since(start)

	if err == nil {
		t.Fatal("expected ctx cancellation error, handler returned normally")
	}
	if connect.CodeOf(err) != connect.CodeCanceled {
		t.Errorf("code: got %v, want canceled", connect.CodeOf(err))
	}
	if elapsed > 5*time.Second {
		t.Errorf("handler blocked %v despite ctx deadline", elapsed)
	}
}

func TestChannelSend_CtxCanceled(t *testing.T) {
	svc, v, cleanup := newChannelTestService(t)
	defer cleanup()

	ch := vm.NewChannelObject(1)
	ch.SafeSend(vm.FromSmallInt(1)) // fill the buffer so the send blocks
	id := v.ExportChannel(ch)

	payload, err := v.SerializeValue(vm.FromSmallInt(2))
	if err != nil {
		t.Fatalf("serialize: %v", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	start := time.Now()
	_, err = svc.ChannelSend(ctx, connect.NewRequest(
		&maggiev1.ChannelSendRequest{ChannelId: id, Value: payload}))
	elapsed := time.Since(start)

	if err == nil {
		t.Fatal("expected ctx cancellation error, handler returned normally")
	}
	if connect.CodeOf(err) != connect.CodeCanceled {
		t.Errorf("code: got %v, want canceled", connect.CodeOf(err))
	}
	if elapsed > 5*time.Second {
		t.Errorf("handler blocked %v despite ctx deadline", elapsed)
	}
}

func TestChannelReceive_DrainedUnexports(t *testing.T) {
	svc, v, cleanup := newChannelTestService(t)
	defer cleanup()

	ch := vm.NewChannelObject(1)
	ch.SafeSend(vm.FromSmallInt(9))
	id := v.ExportChannel(ch)
	ch.Close()

	// First receive drains the buffered value
	resp, err := svc.ChannelReceive(context.Background(), connect.NewRequest(
		&maggiev1.ChannelReceiveRequest{ChannelId: id}))
	if err != nil || !resp.Msg.Success || !resp.Msg.ChannelOpen {
		t.Fatalf("first receive: err=%v resp=%+v", err, resp)
	}

	// Second receive observes closed+empty and releases the export
	resp, err = svc.ChannelReceive(context.Background(), connect.NewRequest(
		&maggiev1.ChannelReceiveRequest{ChannelId: id}))
	if err != nil || resp.Msg.ChannelOpen {
		t.Fatalf("second receive: err=%v resp=%+v", err, resp)
	}

	if v.LookupExportedChannel(id) != nil {
		t.Error("drained channel should be unexported")
	}
}
