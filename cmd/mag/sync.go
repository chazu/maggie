package main

import (
	"context"
	"fmt"
	"net/http"
	"os"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// handleSyncCommand processes the `mag sync` subcommand.
// Usage:
//
//	mag sync push <peer-addr>                Push local project to peer
//	mag sync pull <peer-addr> <root-hash>    Pull specific module from peer
//	mag sync status                          Show content store stats
func handleSyncCommand(args []string, vmInst *vm.VM, verbose bool) {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: mag sync [push|pull|status] ...")
		fmt.Fprintln(os.Stderr, "  push <peer-addr>              Push local project to peer")
		fmt.Fprintln(os.Stderr, "  pull <peer-addr> <root-hash>  Pull specific module from peer")
		fmt.Fprintln(os.Stderr, "  status                        Show content store stats")
		os.Exit(1)
	}

	switch args[0] {
	case "status":
		handleSyncStatus(vmInst)
	case "push":
		if len(args) < 2 {
			fmt.Fprintln(os.Stderr, "Usage: mag sync push <peer-addr>")
			os.Exit(1)
		}
		handleSyncPush(vmInst, args[1], verbose)
	case "pull":
		if len(args) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: mag sync pull <peer-addr> <root-hash-hex>")
			os.Exit(1)
		}
		handleSyncPull(vmInst, args[1], args[2], verbose)
	default:
		fmt.Fprintf(os.Stderr, "Unknown sync subcommand: %s\n", args[0])
		os.Exit(1)
	}
}

func handleSyncStatus(vmInst *vm.VM) {
	store := vmInst.ContentStore()
	fmt.Printf("Content store:\n")
	fmt.Printf("  Methods: %d\n", store.MethodCount())
	fmt.Printf("  Classes: %d\n", store.ClassCount())
	fmt.Printf("  Total:   %d\n", store.MethodCount()+store.ClassCount())
}

func handleSyncPush(vmInst *vm.VM, peerAddr string, verbose bool) {
	store := vmInst.ContentStore()

	allHashes := store.AllHashes()
	if len(allHashes) == 0 {
		fmt.Println("Nothing to push (content store is empty)")
		return
	}

	rootHash := allHashes[0]

	hashBytes := make([][]byte, len(allHashes))
	for i, h := range allHashes {
		hCopy := h
		hashBytes[i] = hCopy[:]
	}

	baseURL := normalizeAddr(peerAddr)
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)

	if verbose {
		fmt.Printf("Announcing %d hashes to %s\n", len(allHashes), baseURL)
	}

	ctx := context.Background()

	annResp, err := client.Announce(ctx, connect.NewRequest(&maggiev1.AnnounceRequest{
		RootHash:  rootHash[:],
		AllHashes: hashBytes,
	}))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Announce failed: %v\n", err)
		os.Exit(1)
	}

	switch annResp.Msg.Status {
	case maggiev1.AnnounceStatus_ANNOUNCE_ALREADY_HAVE:
		fmt.Println("Peer already has all content")
		return
	case maggiev1.AnnounceStatus_ANNOUNCE_REJECTED:
		fmt.Fprintf(os.Stderr, "Peer rejected: %s\n", annResp.Msg.RejectReason)
		os.Exit(1)
	case maggiev1.AnnounceStatus_ANNOUNCE_ACCEPTED:
		// continue
	}

	var chunkBytes [][]byte
	for _, wantHash := range annResp.Msg.Want {
		var h [32]byte
		copy(h[:], wantHash)
		m := store.LookupMethod(h)
		if m != nil {
			chunk := dist.MethodToChunk(m, nil)
			data, err := dist.MarshalChunk(chunk)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Warning: failed to marshal chunk: %v\n", err)
				continue
			}
			chunkBytes = append(chunkBytes, data)
		}
	}

	if len(chunkBytes) == 0 {
		fmt.Println("No chunks to transfer")
		return
	}

	if verbose {
		fmt.Printf("Transferring %d chunks\n", len(chunkBytes))
	}

	txResp, err := client.Transfer(ctx, connect.NewRequest(&maggiev1.TransferRequest{
		Chunks: chunkBytes,
	}))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Transfer failed: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Transfer complete: %d accepted, %d rejected\n",
		txResp.Msg.Accepted, txResp.Msg.Rejected)
}

func handleSyncPull(vmInst *vm.VM, peerAddr string, rootHashHex string, verbose bool) {
	if len(rootHashHex) != 64 {
		fmt.Fprintln(os.Stderr, "Root hash must be 64 hex characters (32 bytes)")
		os.Exit(1)
	}
	rootHash, err := hexToHash(rootHashHex)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Invalid hex hash: %v\n", err)
		os.Exit(1)
	}

	baseURL := normalizeAddr(peerAddr)
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	pingResp, err := client.Ping(ctx, connect.NewRequest(&maggiev1.PingRequest{}))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Ping failed: %v\n", err)
		os.Exit(1)
	}
	if verbose {
		fmt.Printf("Peer has %d content items\n", pingResp.Msg.ContentCount)
	}

	store := vmInst.ContentStore()
	if store.HasHash(rootHash) {
		fmt.Println("Already have this content")
		return
	}

	fmt.Printf("Requesting content with root hash %x\n", rootHash)
	fmt.Println("Pull not yet fully implemented (requires peer-side content serving)")
}

func normalizeAddr(addr string) string {
	if addr == "" {
		return ""
	}
	if addr[0] == ':' {
		return "http://localhost" + addr
	}
	if len(addr) > 5 && addr[:5] == "https" {
		return addr
	}
	if len(addr) > 4 && addr[:4] == "http" {
		return addr
	}
	return "http://" + addr
}

func hexToHash(hex string) ([32]byte, error) {
	var h [32]byte
	if len(hex) != 64 {
		return h, fmt.Errorf("expected 64 hex chars, got %d", len(hex))
	}
	for i := 0; i < 32; i++ {
		b, err := hexByte(hex[i*2], hex[i*2+1])
		if err != nil {
			return h, err
		}
		h[i] = b
	}
	return h, nil
}

func hexByte(hi, lo byte) (byte, error) {
	h, err := hexNibble(hi)
	if err != nil {
		return 0, err
	}
	l, err := hexNibble(lo)
	if err != nil {
		return 0, err
	}
	return h<<4 | l, nil
}

func hexNibble(c byte) (byte, error) {
	switch {
	case c >= '0' && c <= '9':
		return c - '0', nil
	case c >= 'a' && c <= 'f':
		return c - 'a' + 10, nil
	case c >= 'A' && c <= 'F':
		return c - 'A' + 10, nil
	default:
		return 0, fmt.Errorf("invalid hex char: %c", c)
	}
}
