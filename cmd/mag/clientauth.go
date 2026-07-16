package main

import (
	"crypto/ed25519"
	"fmt"
	"os"
	"sync"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/server"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// Peer-facing servers require every request to be signed with the caller's
// node identity (see server/auth.go). The helpers below attach the client
// auth interceptor to outgoing sync clients.

var (
	localIdentityOnce sync.Once
	localIdentity     *dist.NodeIdentity
)

// loadLocalIdentity loads (or creates on first use) the node identity from
// .maggie/node.key in the current directory. Returns nil if the key cannot
// be loaded or created.
func loadLocalIdentity() *dist.NodeIdentity {
	localIdentityOnce.Do(func() {
		id, err := dist.LoadOrCreateIdentity(".maggie")
		if err != nil {
			fmt.Fprintf(os.Stderr, "warning: no node identity (%v); peer requests will be unauthenticated\n", err)
			return
		}
		localIdentity = id
	})
	return localIdentity
}

// syncClientOptions returns connect client options that sign every outgoing
// request with the local node identity.
func syncClientOptions() []connect.ClientOption {
	id := loadLocalIdentity()
	if id == nil {
		return nil
	}
	return []connect.ClientOption{
		connect.WithInterceptors(server.NewClientAuthInterceptor(id.NodeID(), id.Sign)),
	}
}

// nodeRefClientOptions returns connect client options that sign requests
// with a NodeRefData's keys (the VM's identity for that connection).
func nodeRefClientOptions(ref *vm.NodeRefData) []connect.ClientOption {
	return []connect.ClientOption{
		connect.WithInterceptors(server.NewClientAuthInterceptor(ref.NodeID(), ref.Sign)),
	}
}

// applyLocalIdentity installs the persistent node identity on the VM so
// Node connect: and envelope signing use the same key the request-auth
// layer uses (instead of a fresh ephemeral key per process).
func applyLocalIdentity(vmInst *vm.VM) {
	if id := loadLocalIdentity(); id != nil {
		vmInst.SetNodeIdentityKeys(id.PublicKey, ed25519.PrivateKey(id.PrivateKey))
	}
}
