package dist

import "context"

// PeerAddr is a candidate peer surfaced by a Discovery provider. Source names
// the provider (e.g. "static", "mdns") so the join policy can treat peers from
// different sources differently — eager-connect static seeds, lazy for mDNS.
type PeerAddr struct {
	Addr   string
	Source string
}

// Discovery streams candidate peer addresses for the membership layer to try.
// The membership core runs every configured provider concurrently and merges
// their streams, so static seeds, mDNS, and an external registry can all be
// active at once. A provider runs until ctx is cancelled: static emits its
// seeds once and then idles; mDNS/registry keep emitting as peers appear.
//
// Discovery is bootstrap only — it tells you an address to talk to. Once
// connected, gossip disseminates the full membership view; providers never need
// to enumerate the whole cluster.
type Discovery interface {
	Discover(ctx context.Context) (<-chan PeerAddr, error)
	Name() string
}

// StaticDiscovery emits a fixed list of seed addresses (from maggie.toml
// [sync].peers / [cluster].seeds). It is the always-available baseline.
type StaticDiscovery struct {
	Seeds []string
}

// NewStaticDiscovery builds a StaticDiscovery from seed addresses.
func NewStaticDiscovery(seeds []string) *StaticDiscovery {
	return &StaticDiscovery{Seeds: seeds}
}

func (s *StaticDiscovery) Name() string { return "static" }

// Discover emits each seed once, then leaves the channel open (closing only on
// ctx cancellation) so the merge loop treats it uniformly with live providers.
func (s *StaticDiscovery) Discover(ctx context.Context) (<-chan PeerAddr, error) {
	out := make(chan PeerAddr)
	go func() {
		defer close(out)
		for _, addr := range s.Seeds {
			select {
			case out <- PeerAddr{Addr: addr, Source: "static"}:
			case <-ctx.Done():
				return
			}
		}
		<-ctx.Done()
	}()
	return out, nil
}
