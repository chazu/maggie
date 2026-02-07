package dist

import "testing"

func TestPermissivePolicy_AllowsEverything(t *testing.T) {
	p := NewPermissivePolicy()
	m := &CapabilityManifest{Required: []string{"File", "HTTP", "Network"}}

	if err := p.Check(m); err != nil {
		t.Errorf("permissive policy should allow all: %v", err)
	}
}

func TestPermissivePolicy_NilManifest(t *testing.T) {
	p := NewPermissivePolicy()
	if err := p.Check(nil); err != nil {
		t.Errorf("nil manifest should be allowed: %v", err)
	}
}

func TestRestrictedPolicy_AllowsListed(t *testing.T) {
	p := NewRestrictedPolicy([]string{"File", "HTTP"})
	m := &CapabilityManifest{Required: []string{"File"}}

	if err := p.Check(m); err != nil {
		t.Errorf("should allow listed capability: %v", err)
	}
}

func TestRestrictedPolicy_DeniesUnlisted(t *testing.T) {
	p := NewRestrictedPolicy([]string{"File"})
	m := &CapabilityManifest{Required: []string{"Network"}}

	if err := p.Check(m); err == nil {
		t.Error("should deny unlisted capability")
	}
}

func TestCapabilityPolicy_ExplicitDeny(t *testing.T) {
	p := NewPermissivePolicy()
	p.Deny("Network")

	m := &CapabilityManifest{Required: []string{"Network"}}
	if err := p.Check(m); err == nil {
		t.Error("should deny explicitly denied capability")
	}
}

func TestCapabilityPolicy_DenyOverridesAllow(t *testing.T) {
	p := NewRestrictedPolicy([]string{"File", "Network"})
	p.Deny("Network")

	m := &CapabilityManifest{Required: []string{"Network"}}
	if err := p.Check(m); err == nil {
		t.Error("deny should override allow")
	}
}

func TestRestrictedPolicy_EmptyManifest(t *testing.T) {
	p := NewRestrictedPolicy([]string{"File"})
	m := &CapabilityManifest{}

	if err := p.Check(m); err != nil {
		t.Errorf("empty manifest should pass: %v", err)
	}
}
