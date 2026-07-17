package vm

// ClusterMemberInfo is the vm-side view of one membership record — deliberately
// free of any vm/dist type so the Maggie-facing Cluster/ClusterMember primitives
// can consume the membership core through the ClusterCore interface without the
// forbidden vm→vm/dist import.
type ClusterMemberInfo struct {
	ID       [32]byte
	Addr     string
	Status   uint8 // mirrors dist.MemberStatus: 0=Alive 1=Suspect 2=Dead 3=Left
	Metadata map[string]string
}

// Cluster member status codes (mirror dist.MemberStatus) for mapping to the
// Maggie-facing symbols in the ClusterMember primitives.
const (
	ClusterStatusAlive   uint8 = 0
	ClusterStatusSuspect uint8 = 1
	ClusterStatusDead    uint8 = 2
	ClusterStatusLeft    uint8 = 3
)

// ClusterCore is the membership handle the Cluster/ClusterMember primitives
// delegate to. dist.Membership implements it. Method names avoid colliding with
// dist.Membership's own API (e.g. AliveMemberInfos, not Members).
type ClusterCore interface {
	// AliveMemberInfos is the authoritative live view: only StatusAlive peers,
	// self excluded. The Maggie side rebuilds its ring from this on every event.
	AliveMemberInfos() []ClusterMemberInfo
	// ConnectAsync fires a connect to addr in the background and returns
	// immediately — safe to call from the event-loop goroutine (never blocks it).
	ConnectAsync(addr string)
	// ConnectedRef returns the core's existing NodeRef for peer, or nil if not
	// connected. ClusterMember>>node wraps this so the Maggie Node reuses the
	// exact ref the core routes and heartbeats on.
	ConnectedRef(peer [32]byte) *NodeRefData
	// SetMetadata replaces this node's gossiped metadata (bumps incarnation).
	SetMetadata(md map[string]string)
	// SetJoinPolicy sets the coarse join policy: "eager" | "lazy" | "manual".
	SetJoinPolicy(policy string)
}

// SetClusterCore installs the membership core (called by the wiring layer). Safe
// to leave unset — the Cluster primitives degrade to an empty view.
func (vm *VM) SetClusterCore(c ClusterCore) { vm.clusterCore = c }

// GetClusterCore returns the installed membership core, or nil.
func (vm *VM) GetClusterCore() ClusterCore { return vm.clusterCore }
