package dist

import "fmt"

// CapabilityPolicy controls which capabilities are allowed when receiving
// chunks from a peer. A nil AllowedCapabilities means "allow all".
type CapabilityPolicy struct {
	AllowedCapabilities map[string]bool // nil = allow all
	DeniedCapabilities  map[string]bool
}

// NewPermissivePolicy creates a policy that allows all capabilities.
func NewPermissivePolicy() *CapabilityPolicy {
	return &CapabilityPolicy{}
}

// NewRestrictedPolicy creates a policy that only allows the specified
// capabilities.
func NewRestrictedPolicy(allowed []string) *CapabilityPolicy {
	m := make(map[string]bool, len(allowed))
	for _, c := range allowed {
		m[c] = true
	}
	return &CapabilityPolicy{AllowedCapabilities: m}
}

// Check verifies that all capabilities required by a manifest are allowed
// by this policy. Returns an error listing any denied capabilities.
func (p *CapabilityPolicy) Check(manifest *CapabilityManifest) error {
	if manifest == nil {
		return nil
	}
	for _, cap := range manifest.Required {
		if p.DeniedCapabilities != nil && p.DeniedCapabilities[cap] {
			return fmt.Errorf("dist: capability %q is explicitly denied", cap)
		}
		if p.AllowedCapabilities != nil && !p.AllowedCapabilities[cap] {
			return fmt.Errorf("dist: capability %q is not allowed", cap)
		}
	}
	return nil
}

// Deny adds a capability to the deny list.
func (p *CapabilityPolicy) Deny(cap string) {
	if p.DeniedCapabilities == nil {
		p.DeniedCapabilities = make(map[string]bool)
	}
	p.DeniedCapabilities[cap] = true
}
