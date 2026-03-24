package manifest

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCheckVersionConstraint(t *testing.T) {
	tests := []struct {
		constraint string
		version    string
		wantErr    bool
	}{
		// >= constraints
		{">=0.9.0", "0.10.0", false},
		{">=0.10.0", "0.10.0", false},
		{">=1.0.0", "0.10.0", true},
		{">=0.10.1", "0.10.0", true},

		// < constraints
		{"<1.0.0", "0.10.0", false},
		{"<0.10.0", "0.10.0", true},
		{"<0.9.0", "0.10.0", true},

		// > constraints
		{">0.9.0", "0.10.0", false},
		{">0.10.0", "0.10.0", true},

		// <= constraints
		{"<=0.10.0", "0.10.0", false},
		{"<=0.9.0", "0.10.0", true},
		{"<=1.0.0", "0.10.0", false},

		// Range constraints (space-separated)
		{">=0.9.0 <1.0.0", "0.10.0", false},
		{">=0.9.0 <0.10.0", "0.10.0", true},

		// Pessimistic constraints (~)
		{"~0.10.0", "0.10.5", false},
		{"~0.10.0", "0.10.0", false},
		{"~0.10.0", "0.11.0", true},
		{"~0.10.0", "0.9.0", true},
	}

	for _, tt := range tests {
		t.Run(tt.constraint+"_vs_"+tt.version, func(t *testing.T) {
			err := checkVersionConstraint(tt.constraint, tt.version)
			if (err != nil) != tt.wantErr {
				t.Errorf("checkVersionConstraint(%q, %q) error = %v, wantErr %v", tt.constraint, tt.version, err, tt.wantErr)
			}
		})
	}
}

func TestCheckVersionConstraintMalformed(t *testing.T) {
	err := checkVersionConstraint("garbage", "0.10.0")
	if err == nil {
		t.Error("expected error for malformed constraint")
	}

	err = checkVersionConstraint(">=not.a.version", "0.10.0")
	if err == nil {
		t.Error("expected error for non-numeric constraint")
	}
}

func TestValidateDepMutualExclusion(t *testing.T) {
	m := &Manifest{
		Dependencies: map[string]Dependency{
			"bad": {Git: "https://example.com/bad", Tag: "v1.0", Branch: "main"},
		},
	}
	if err := m.Validate("0.10.0"); err == nil {
		t.Error("expected error for tag+branch on same dep")
	}

	m.Dependencies["bad"] = Dependency{Git: "https://example.com/bad", Tag: "v1.0", Commit: "abc123"}
	if err := m.Validate("0.10.0"); err == nil {
		t.Error("expected error for tag+commit on same dep")
	}

	// Single ref is fine
	m.Dependencies["bad"] = Dependency{Git: "https://example.com/bad", Branch: "main"}
	if err := m.Validate("0.10.0"); err != nil {
		t.Errorf("unexpected error for single branch ref: %v", err)
	}
}

func TestValidateDepRefWithoutGit(t *testing.T) {
	m := &Manifest{
		Dependencies: map[string]Dependency{
			"bad": {Branch: "main"},
		},
	}
	if err := m.Validate("0.10.0"); err == nil {
		t.Error("expected error for branch without git URL")
	}
}

func TestValidateDevDepMutualExclusion(t *testing.T) {
	m := &Manifest{
		DevDependencies: map[string]Dependency{
			"bad": {Git: "https://example.com/bad", Tag: "v1.0", Branch: "main"},
		},
	}
	if err := m.Validate("0.10.0"); err == nil {
		t.Error("expected error for tag+branch on dev dep")
	}
}

func TestValidateTargetDuplicateNames(t *testing.T) {
	m := &Manifest{
		Targets: []TargetConfig{
			{Name: "server"},
			{Name: "server"},
		},
	}
	if err := m.Validate("0.10.0"); err == nil {
		t.Error("expected error for duplicate target names")
	}
}

func TestValidateTargetMissingName(t *testing.T) {
	m := &Manifest{
		Targets: []TargetConfig{
			{Entry: "Main.start"},
		},
	}
	if err := m.Validate("0.10.0"); err == nil {
		t.Error("expected error for target without name")
	}
}

func TestValidateVersionConstraintFromLoad(t *testing.T) {
	dir := t.TempDir()
	toml := `
[project]
name = "test"
maggie = ">=99.0.0"
`
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(toml), 0644); err != nil {
		t.Fatal(err)
	}

	_, err := Load(dir)
	if err == nil {
		t.Error("expected Load to fail with version constraint mismatch")
	}
}

func TestParseSemver(t *testing.T) {
	v, err := parseSemver("1.2.3")
	if err != nil {
		t.Fatal(err)
	}
	if v.Major != 1 || v.Minor != 2 || v.Patch != 3 {
		t.Errorf("parseSemver(1.2.3) = %+v", v)
	}

	// With v prefix
	v, err = parseSemver("v1.2.3")
	if err != nil {
		t.Fatal(err)
	}
	if v.Major != 1 {
		t.Errorf("parseSemver(v1.2.3) major = %d", v.Major)
	}

	// Invalid
	_, err = parseSemver("not.a.version")
	if err == nil {
		t.Error("expected error for invalid semver")
	}
}
