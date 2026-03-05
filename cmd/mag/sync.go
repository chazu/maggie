package main

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"sort"
	"strings"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/gen/maggie/v1/maggiev1connect"
	"github.com/chazu/maggie/manifest"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
	"github.com/chazu/maggie/vm/dist"
)

// handleSyncCommand processes the `mag sync` subcommand.
// Usage:
//
//	mag sync push [peer-addr]                  Push local project to peer(s)
//	mag sync pull <peer-addr> <class-or-hash>  Pull by class name or root hash
//	mag sync status                            Show content store stats
//	mag sync list                              List all local content
//	mag sync diff <peer-addr>                  Compare local vs remote content
//	mag sync show <hash-prefix>                Show details for a hash
func handleSyncCommand(args []string, vmInst *vm.VM, m *manifest.Manifest, verbose bool) {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: mag sync [push|pull|status|list|diff|show] ...")
		fmt.Fprintln(os.Stderr, "  push [peer-addr]                Push local project to peer (uses manifest peers if omitted)")
		fmt.Fprintln(os.Stderr, "  pull <peer-addr> <class-or-hash>  Pull by class name or root hash")
		fmt.Fprintln(os.Stderr, "  status                          Show content store stats")
		fmt.Fprintln(os.Stderr, "  list                            List all local content")
		fmt.Fprintln(os.Stderr, "  diff <peer-addr>                Compare local vs remote content")
		fmt.Fprintln(os.Stderr, "  show <hash-prefix>              Show details for a content hash")
		os.Exit(1)
	}

	switch args[0] {
	case "status":
		handleSyncStatus(vmInst)
	case "push":
		if len(args) >= 2 {
			handleSyncPush(vmInst, args[1], verbose)
		} else if m != nil && len(m.Sync.Peers) > 0 {
			// Use manifest peers
			for _, peer := range m.Sync.Peers {
				fmt.Printf("Pushing to manifest peer: %s\n", peer)
				handleSyncPush(vmInst, peer, verbose)
			}
		} else {
			fmt.Fprintln(os.Stderr, "Usage: mag sync push <peer-addr>")
			fmt.Fprintln(os.Stderr, "  (or configure [sync].peers in maggie.toml)")
			os.Exit(1)
		}
	case "pull":
		if len(args) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: mag sync pull <peer-addr> <class-name-or-hash>")
			os.Exit(1)
		}
		handleSyncPull(vmInst, args[1], args[2], verbose)
	case "list":
		handleSyncList(vmInst)
	case "diff":
		if len(args) < 2 {
			fmt.Fprintln(os.Stderr, "Usage: mag sync diff <peer-addr>")
			os.Exit(1)
		}
		handleSyncDiff(vmInst, args[1], verbose)
	case "show":
		if len(args) < 2 {
			fmt.Fprintln(os.Stderr, "Usage: mag sync show <hash-prefix>")
			os.Exit(1)
		}
		handleSyncShow(vmInst, args[1])
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

	// Build chunks in dependency order: methods first, then classes
	var methodChunks [][]byte
	var classChunks [][]byte
	for _, wantHash := range annResp.Msg.Want {
		var h [32]byte
		copy(h[:], wantHash)

		if m := store.LookupMethod(h); m != nil {
			chunk := dist.MethodToChunk(m, nil)
			data, err := dist.MarshalChunk(chunk)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Warning: failed to marshal method chunk: %v\n", err)
				continue
			}
			methodChunks = append(methodChunks, data)
		} else if d := store.LookupClass(h); d != nil {
			chunk := dist.ClassToChunk(d, nil)
			data, err := dist.MarshalChunk(chunk)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Warning: failed to marshal class chunk: %v\n", err)
				continue
			}
			classChunks = append(classChunks, data)
		}
	}
	// Methods before classes ensures dependencies are available for verification
	chunkBytes := append(methodChunks, classChunks...)

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
	baseURL := normalizeAddr(peerAddr)
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	var rootHash [32]byte
	var err error

	if len(rootHashHex) == 64 {
		// Looks like a hex hash
		rootHash, err = hexToHash(rootHashHex)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Invalid hex hash: %v\n", err)
			os.Exit(1)
		}
	} else {
		// Treat as a class name — resolve via the Resolve RPC
		if verbose {
			fmt.Printf("Resolving class name %q on %s\n", rootHashHex, baseURL)
		}
		resolveResp, resolveErr := client.Resolve(ctx, connect.NewRequest(&maggiev1.ResolveRequest{
			ClassName: rootHashHex,
		}))
		if resolveErr != nil {
			fmt.Fprintf(os.Stderr, "Resolve failed: %v\n", resolveErr)
			os.Exit(1)
		}
		if !resolveResp.Msg.Found {
			fmt.Fprintf(os.Stderr, "Class %q not found on peer\n", rootHashHex)
			os.Exit(1)
		}
		if len(resolveResp.Msg.RootHash) != 32 {
			fmt.Fprintln(os.Stderr, "Peer returned invalid hash length")
			os.Exit(1)
		}
		copy(rootHash[:], resolveResp.Msg.RootHash)
		if verbose {
			fmt.Printf("Resolved to hash %x\n", rootHash)
		}
	}

	store := vmInst.ContentStore()
	if store.HasHash(rootHash) {
		fmt.Println("Already have this content")
		return
	}

	// Collect local hashes to avoid re-downloading
	localHashes := store.AllHashes()
	haveBytes := make([][]byte, len(localHashes))
	for i, h := range localHashes {
		hCopy := h
		haveBytes[i] = hCopy[:]
	}

	if verbose {
		fmt.Printf("Requesting content from %s (root=%x, local=%d hashes)\n",
			baseURL, rootHash, len(localHashes))
	}

	serveResp, err := client.Serve(ctx, connect.NewRequest(&maggiev1.ServeRequest{
		RootHash: rootHash[:],
		Have:     haveBytes,
	}))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Serve failed: %v\n", err)
		os.Exit(1)
	}

	if verbose {
		fmt.Printf("Peer returned %d available hashes, %d chunks\n",
			len(serveResp.Msg.Available), len(serveResp.Msg.Chunks))
	}

	// Verify and index received chunks
	var accepted, rejected int
	for _, chunkBytes := range serveResp.Msg.Chunks {
		chunk, err := dist.UnmarshalChunk(chunkBytes)
		if err != nil {
			rejected++
			if verbose {
				fmt.Fprintf(os.Stderr, "Warning: failed to unmarshal chunk: %v\n", err)
			}
			continue
		}

		switch chunk.Type {
		case dist.ChunkMethod:
			// Index the method (trust verified by sender's hash)
			m := &vm.CompiledMethod{Source: chunk.Content}
			m.SetContentHash(chunk.Hash)
			store.IndexMethod(m)
			accepted++
		case dist.ChunkClass:
			// Verify class dependencies exist before indexing
			if err := dist.VerifyChunkClass(chunk, store); err != nil {
				rejected++
				if verbose {
					fmt.Fprintf(os.Stderr, "Warning: class chunk failed verification: %v\n", err)
				}
				continue
			}
			d, decErr := dist.DecodeClassContent(chunk.Content)
			if decErr != nil {
				// Fallback: treat Content as bare class name for backward compat
				d = &vm.ClassDigest{Name: chunk.Content}
			}
			d.Hash = chunk.Hash
			d.MethodHashes = chunk.Dependencies
			store.IndexClass(d)
			accepted++
		default:
			rejected++
		}
	}

	fmt.Printf("Pull complete: %d accepted, %d rejected\n", accepted, rejected)
	fmt.Printf("Content store: %d methods, %d classes\n",
		store.MethodCount(), store.ClassCount())

	// Verify completeness: check all method hashes referenced by class digests exist
	missing := verifyCompleteness(store, rootHash)
	if len(missing) > 0 {
		fmt.Fprintf(os.Stderr, "Warning: %d method hashes referenced by class digests are missing from the store\n", len(missing))
		if verbose {
			for _, mh := range missing {
				fmt.Fprintf(os.Stderr, "  missing: %x\n", mh[:8])
			}
		}
	}

	// Rehydrate: compile synced content into runnable classes
	compiled, err := pipeline.RehydrateFromStore(vmInst)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Rehydration failed: %v\n", err)
		os.Exit(1)
	}
	if compiled > 0 {
		fmt.Printf("Rehydrated: %d methods compiled\n", compiled)
	}
}

// verifyCompleteness checks that all method hashes referenced by class
// digests exist in the store. Returns missing hashes.
func verifyCompleteness(store *vm.ContentStore, rootHash [32]byte) [][32]byte {
	var missing [][32]byte
	seen := make(map[[32]byte]bool)

	for _, ch := range store.ClassHashes() {
		d := store.LookupClass(ch)
		if d == nil {
			continue
		}
		for _, mh := range d.MethodHashes {
			if seen[mh] {
				continue
			}
			seen[mh] = true
			if store.LookupMethod(mh) == nil {
				missing = append(missing, mh)
			}
		}
	}
	return missing
}

// shortHash returns the first 12 hex characters of a hash for display.
func shortHash(h [32]byte) string {
	return fmt.Sprintf("%x", h)[:12]
}

func handleSyncList(vmInst *vm.VM) {
	store := vmInst.ContentStore()

	classDigests := store.AllClassDigests()
	methodHashes := store.AllMethodHashes()

	// Build reverse index: method hash -> class name
	methodToClass := make(map[[32]byte]string)
	for _, d := range classDigests {
		for _, mh := range d.MethodHashes {
			methodToClass[mh] = d.Name
		}
	}

	// Sort classes by name for stable output
	sort.Slice(classDigests, func(i, j int) bool {
		return classDigests[i].Name < classDigests[j].Name
	})

	// Print classes
	fmt.Printf("Classes (%d):\n", len(classDigests))
	for _, d := range classDigests {
		fmt.Printf("  %-30s %s  (%d methods)\n", d.Name, shortHash(d.Hash), len(d.MethodHashes))
	}

	// Sort method hashes for stable output (by hex string)
	sort.Slice(methodHashes, func(i, j int) bool {
		hi := fmt.Sprintf("%x", methodHashes[i])
		hj := fmt.Sprintf("%x", methodHashes[j])
		return hi < hj
	})

	// Print methods
	fmt.Printf("\nMethods (%d):\n", len(methodHashes))
	for _, mh := range methodHashes {
		// Try to get the method name from the compiled method
		m := store.LookupMethod(mh)
		name := "(unknown)"
		if m != nil && m.Name() != "" {
			name = m.Name()
		} else if m != nil && m.Source != "" {
			// Try to extract selector from source text
			name = extractSelector(m.Source)
		}

		className := ""
		if cn, ok := methodToClass[mh]; ok {
			className = fmt.Sprintf("  (%s)", cn)
		}

		fmt.Printf("  %-30s %s%s\n", name, shortHash(mh), className)
	}

	fmt.Printf("\nTotal: %d classes, %d methods\n", len(classDigests), len(methodHashes))
}

// extractSelector attempts to pull a selector name from method source text.
// It handles common patterns like "method: name [" and "method: name: arg [".
func extractSelector(source string) string {
	source = strings.TrimSpace(source)
	// Look for "method: <selector>" pattern
	if idx := strings.Index(source, "method:"); idx >= 0 {
		rest := strings.TrimSpace(source[idx+7:])
		// Take everything up to the first '[' or newline
		for i, c := range rest {
			if c == '[' || c == '\n' {
				sel := strings.TrimSpace(rest[:i])
				if sel != "" {
					return sel
				}
				break
			}
		}
	}
	return "(unknown)"
}

func handleSyncDiff(vmInst *vm.VM, peerAddr string, verbose bool) {
	store := vmInst.ContentStore()

	baseURL := normalizeAddr(peerAddr)
	client := maggiev1connect.NewSyncServiceClient(http.DefaultClient, baseURL)
	ctx := context.Background()

	// Call List RPC on peer
	listResp, err := client.List(ctx, connect.NewRequest(&maggiev1.ListRequest{}))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to list remote content: %v\n", err)
		os.Exit(1)
	}

	// Build remote hash sets
	remoteMethodSet := make(map[[32]byte]bool)
	for _, hb := range listResp.Msg.MethodHashes {
		if len(hb) == 32 {
			var h [32]byte
			copy(h[:], hb)
			remoteMethodSet[h] = true
		}
	}

	remoteClassSet := make(map[[32]byte]string) // hash -> name
	for i, hb := range listResp.Msg.ClassHashes {
		if len(hb) == 32 {
			var h [32]byte
			copy(h[:], hb)
			name := ""
			if i < len(listResp.Msg.ClassNames) {
				name = listResp.Msg.ClassNames[i]
			}
			remoteClassSet[h] = name
		}
	}

	// Build local hash sets
	localMethods := store.AllMethodHashes()
	localMethodSet := make(map[[32]byte]bool, len(localMethods))
	for _, h := range localMethods {
		localMethodSet[h] = true
	}

	localClasses := store.AllClassDigests()
	localClassSet := make(map[[32]byte]string, len(localClasses))
	for _, d := range localClasses {
		localClassSet[d.Hash] = d.Name
	}

	// Compute differences
	// Classes remote has, we don't
	var remoteOnly []struct {
		name string
		hash [32]byte
	}
	for h, name := range remoteClassSet {
		if _, ok := localClassSet[h]; !ok {
			remoteOnly = append(remoteOnly, struct {
				name string
				hash [32]byte
			}{name, h})
		}
	}
	sort.Slice(remoteOnly, func(i, j int) bool {
		return remoteOnly[i].name < remoteOnly[j].name
	})

	// Classes we have, remote doesn't
	var localOnly []struct {
		name string
		hash [32]byte
	}
	for h, name := range localClassSet {
		if _, ok := remoteClassSet[h]; !ok {
			localOnly = append(localOnly, struct {
				name string
				hash [32]byte
			}{name, h})
		}
	}
	sort.Slice(localOnly, func(i, j int) bool {
		return localOnly[i].name < localOnly[j].name
	})

	// Count common
	commonMethods := 0
	for h := range localMethodSet {
		if remoteMethodSet[h] {
			commonMethods++
		}
	}
	commonClasses := 0
	for h := range localClassSet {
		if _, ok := remoteClassSet[h]; ok {
			commonClasses++
		}
	}

	// Count method-only differences
	remoteOnlyMethods := 0
	for h := range remoteMethodSet {
		if !localMethodSet[h] {
			remoteOnlyMethods++
		}
	}
	localOnlyMethods := 0
	for h := range localMethodSet {
		if !remoteMethodSet[h] {
			localOnlyMethods++
		}
	}

	// Print results
	if len(remoteOnly) > 0 || remoteOnlyMethods > 0 {
		fmt.Printf("Remote has (we don't):\n")
		for _, item := range remoteOnly {
			fmt.Printf("  %-30s %s\n", item.name, shortHash(item.hash))
		}
		if remoteOnlyMethods > 0 {
			fmt.Printf("  ... and %d methods\n", remoteOnlyMethods)
		}
		fmt.Println()
	}

	if len(localOnly) > 0 || localOnlyMethods > 0 {
		fmt.Printf("We have (remote doesn't):\n")
		for _, item := range localOnly {
			fmt.Printf("  %-30s %s\n", item.name, shortHash(item.hash))
		}
		if localOnlyMethods > 0 {
			fmt.Printf("  ... and %d methods\n", localOnlyMethods)
		}
		fmt.Println()
	}

	if len(remoteOnly) == 0 && len(localOnly) == 0 && remoteOnlyMethods == 0 && localOnlyMethods == 0 {
		fmt.Println("Local and remote are in sync.")
	}

	fmt.Printf("Common: %d methods, %d classes\n", commonMethods, commonClasses)
}

func handleSyncShow(vmInst *vm.VM, prefix string) {
	store := vmInst.ContentStore()

	hash, typ, err := store.LookupByPrefix(prefix)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	fullHash := fmt.Sprintf("%x", hash)

	switch typ {
	case "class":
		d := store.LookupClass(hash)
		if d == nil {
			fmt.Fprintf(os.Stderr, "Class not found (internal error)\n")
			os.Exit(1)
		}

		fmt.Printf("Class: %s (%s)\n", d.Name, fullHash[:12])
		if d.Namespace != "" {
			fmt.Printf("  Namespace:  %s\n", d.Namespace)
		}
		if d.SuperclassName != "" {
			fmt.Printf("  Superclass: %s\n", d.SuperclassName)
		}
		if len(d.InstVars) > 0 {
			fmt.Printf("  Instance vars: %s\n", strings.Join(d.InstVars, ", "))
		}
		if len(d.ClassVars) > 0 {
			fmt.Printf("  Class vars: %s\n", strings.Join(d.ClassVars, ", "))
		}
		if d.DocString != "" {
			fmt.Printf("  Doc: %s\n", d.DocString)
		}
		if len(d.MethodHashes) > 0 {
			fmt.Printf("  Methods:\n")
			for _, mh := range d.MethodHashes {
				// Try to get method name
				m := store.LookupMethod(mh)
				name := "(unknown)"
				if m != nil && m.Name() != "" {
					name = m.Name()
				} else if m != nil && m.Source != "" {
					name = extractSelector(m.Source)
				}
				fmt.Printf("    %-26s %s\n", name, shortHash(mh))
			}
		}

	case "method":
		m := store.LookupMethod(hash)
		if m == nil {
			fmt.Fprintf(os.Stderr, "Method not found (internal error)\n")
			os.Exit(1)
		}

		name := "(unknown)"
		if m.Name() != "" {
			name = m.Name()
		}
		fmt.Printf("Method: %s (%s)\n", name, fullHash[:12])
		if m.Source != "" {
			fmt.Printf("\nSource:\n%s\n", m.Source)
		} else {
			fmt.Println("\n(no source available)")
		}
	}
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
