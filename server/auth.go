package server

import (
	"context"
	"encoding/hex"
	"fmt"
	"strconv"
	"sync/atomic"
	"time"

	"connectrpc.com/connect"

	"github.com/chazu/maggie/vm/dist"
	"github.com/chazu/maggie/vm/wire"
)

// ---------------------------------------------------------------------------
// Peer-facing request authentication
// ---------------------------------------------------------------------------
//
// One interceptor authenticates every peer-facing RPC: it verifies the
// request signature (identity + freshness + nonce window), checks the ban
// list, and enforces a static per-procedure permission table — deny by
// default. Handlers read the proven identity from the context and never do
// their own header parsing or permission checks.
//
// The single anonymous exception is Ping (liveness only). Anonymous
// requests never touch the trust store, so spoofed identities cannot
// create PeerRecords or feed ban bookkeeping.

// peerIdentityKey is the context key for the authenticated peer identity.
type peerIdentityKey struct{}

// PeerIdentity returns the signature-proven peer NodeID for the request,
// if the auth interceptor authenticated one.
func PeerIdentity(ctx context.Context) (dist.NodeID, bool) {
	id, ok := ctx.Value(peerIdentityKey{}).(dist.NodeID)
	return id, ok
}

// anonymousOK marks procedures callable without authentication.
const anonymousOK = dist.Perm(0)

// SyncPermTable is the per-procedure permission table for the peer-facing
// sync service. Deny by default: procedures not listed here are rejected
// outright.
//
//   - Ping is the sole anonymous endpoint (liveness only — the content
//     count is withheld from unauthenticated callers).
//   - Code sync (announce/transfer/serve/resolve/list) requires PermSync.
//     Resolve and List are gated too: they enumerate every class name and
//     content hash in the store — a code census — and content addressing
//     makes hashes globally comparable version oracles.
//   - Message delivery, monitors, and channels share PermMessage: channels
//     and mailboxes are the same data plane over the same envelope
//     machinery.
//   - Remote spawn requires PermSpawn.
func SyncPermTable() map[string]dist.Perm {
	return map[string]dist.Perm{
		"/maggie.v1.SyncService/Ping":              anonymousOK,
		"/maggie.v1.SyncService/Announce":          dist.PermSync,
		"/maggie.v1.SyncService/Transfer":          dist.PermSync,
		"/maggie.v1.SyncService/Serve":             dist.PermSync,
		"/maggie.v1.SyncService/Resolve":           dist.PermSync,
		"/maggie.v1.SyncService/List":              dist.PermSync,
		"/maggie.v1.SyncService/DeliverMessage":    dist.PermMessage,
		"/maggie.v1.SyncService/MonitorProcess":    dist.PermMessage,
		"/maggie.v1.SyncService/DemonitorProcess":  dist.PermMessage,
		"/maggie.v1.SyncService/ChannelSend":       dist.PermMessage,
		"/maggie.v1.SyncService/ChannelReceive":    dist.PermMessage,
		"/maggie.v1.SyncService/ChannelTrySend":    dist.PermMessage,
		"/maggie.v1.SyncService/ChannelTryReceive": dist.PermMessage,
		"/maggie.v1.SyncService/ChannelClose":      dist.PermMessage,
		"/maggie.v1.SyncService/ChannelStatus":     dist.PermMessage,
		"/maggie.v1.SyncService/SpawnProcess":      dist.PermSpawn,
	}
}

// NewAuthInterceptor builds the server-side auth interceptor for the
// peer-facing sync service.
func NewAuthInterceptor(trust *dist.TrustStore, table map[string]dist.Perm) connect.UnaryInterceptorFunc {
	return func(next connect.UnaryFunc) connect.UnaryFunc {
		return func(ctx context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
			procedure := req.Spec().Procedure

			requiredPerm, known := table[procedure]
			if !known {
				// Deny by default: unlisted procedures are not peer-callable.
				return nil, connect.NewError(connect.CodePermissionDenied,
					fmt.Errorf("procedure %s is not peer-callable", procedure))
			}

			if requiredPerm == anonymousOK {
				// Anonymous endpoint. Authenticate opportunistically when the
				// caller supplied auth headers (so authenticated peers get the
				// fuller response), but never fail the request and never touch
				// the trust store for unproven identities (no PeerRecord
				// creation, no ban bookkeeping).
				if req.Header().Get(wire.HeaderNodeID) != "" {
					if peerID, err := verifyRequestAuth(req, trust); err == nil && !trust.IsBanned(peerID) {
						ctx = context.WithValue(ctx, peerIdentityKey{}, peerID)
					}
				}
				return next(ctx, req)
			}

			peerID, err := verifyRequestAuth(req, trust)
			if err != nil {
				return nil, connect.NewError(connect.CodeUnauthenticated, err)
			}

			if trust.IsBanned(peerID) {
				return nil, connect.NewError(connect.CodePermissionDenied,
					fmt.Errorf("peer is banned"))
			}
			if !trust.Check(peerID, requiredPerm) {
				return nil, connect.NewError(connect.CodePermissionDenied,
					fmt.Errorf("peer %s lacks permission for %s", peerID, procedure))
			}

			return next(context.WithValue(ctx, peerIdentityKey{}, peerID), req)
		}
	}
}

// verifyRequestAuth checks the request-auth headers: signature over
// (procedure, timestamp, nonce), clock skew, and per-peer nonce replay.
// Returns the signature-proven peer identity.
func verifyRequestAuth(req connect.AnyRequest, trust *dist.TrustStore) (dist.NodeID, error) {
	var zero dist.NodeID

	idHex := req.Header().Get(wire.HeaderNodeID)
	if idHex == "" {
		return zero, fmt.Errorf("missing %s header", wire.HeaderNodeID)
	}
	peerID, err := dist.ParseNodeID(idHex)
	if err != nil {
		return zero, fmt.Errorf("invalid %s header: %w", wire.HeaderNodeID, err)
	}

	ts, err := strconv.ParseInt(req.Header().Get(wire.HeaderTimestamp), 10, 64)
	if err != nil {
		return zero, fmt.Errorf("invalid %s header", wire.HeaderTimestamp)
	}
	nonce, err := strconv.ParseUint(req.Header().Get(wire.HeaderNonce), 10, 64)
	if err != nil {
		return zero, fmt.Errorf("invalid %s header", wire.HeaderNonce)
	}
	sig, err := hex.DecodeString(req.Header().Get(wire.HeaderSignature))
	if err != nil {
		return zero, fmt.Errorf("invalid %s header", wire.HeaderSignature)
	}

	if err := wire.VerifyRequest([32]byte(peerID), req.Spec().Procedure, ts, nonce, sig, time.Now()); err != nil {
		return zero, err
	}

	// Nonce replay check happens only AFTER the signature proves the
	// identity, so spoofed ids cannot pollute a victim's window.
	if err := trust.CheckNonce(peerID, nonce); err != nil {
		return zero, err
	}

	return peerID, nil
}

// NewClientAuthInterceptor builds the client-side interceptor that signs
// every outgoing request with the local node identity. nodeID is the
// Ed25519 public key; sign is typically ed25519.Sign closed over the
// private key (callback form so NodeRefData and dist.NodeIdentity can both
// drive it).
func NewClientAuthInterceptor(nodeID [32]byte, sign func([]byte) []byte) connect.UnaryInterceptorFunc {
	var nonce atomic.Uint64
	nonce.Store(wire.NonceSeed())
	idHex := hex.EncodeToString(nodeID[:])

	return func(next connect.UnaryFunc) connect.UnaryFunc {
		return func(ctx context.Context, req connect.AnyRequest) (connect.AnyResponse, error) {
			ts := time.Now().UnixNano()
			n := nonce.Add(1)
			sig := sign(wire.RequestSigningBytes(req.Spec().Procedure, ts, n))

			h := req.Header()
			h.Set(wire.HeaderNodeID, idHex)
			h.Set(wire.HeaderTimestamp, strconv.FormatInt(ts, 10))
			h.Set(wire.HeaderNonce, strconv.FormatUint(n, 10))
			h.Set(wire.HeaderSignature, hex.EncodeToString(sig))
			return next(ctx, req)
		}
	}
}
