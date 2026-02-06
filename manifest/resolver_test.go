package manifest

import (
	"os"
	"path/filepath"
	"testing"
)

func TestResolveNamespace(t *testing.T) {
	tests := []struct {
		name        string
		depName     string
		dep         Dependency
		depManifest *Manifest
		wantNS      string
		wantErr     bool
	}{
		{
			name:    "consumer override wins",
			depName: "yutani",
			dep:     Dependency{Path: "../y", Namespace: "Custom"},
			depManifest: &Manifest{
				Project: Project{Namespace: "Yutani"},
			},
			wantNS: "Custom",
		},
		{
			name:    "producer namespace when no consumer override",
			depName: "yutani",
			dep:     Dependency{Path: "../y"},
			depManifest: &Manifest{
				Project: Project{Namespace: "Yutani"},
			},
			wantNS: "Yutani",
		},
		{
			name:        "PascalCase fallback when no manifest",
			depName:     "my-lib",
			dep:         Dependency{Path: "../my-lib"},
			depManifest: nil,
			wantNS:      "MyLib",
		},
		{
			name:    "PascalCase fallback when manifest has no namespace",
			depName: "my-lib",
			dep:     Dependency{Path: "../my-lib"},
			depManifest: &Manifest{
				Project: Project{Name: "my-lib"},
			},
			wantNS: "MyLib",
		},
		{
			name:        "reserved namespace rejected",
			depName:     "array",
			dep:         Dependency{Path: "../array", Namespace: "Array"},
			depManifest: nil,
			wantErr:     true,
		},
		{
			name:        "reserved namespace via PascalCase fallback",
			depName:     "string",
			dep:         Dependency{Path: "../string"},
			depManifest: nil,
			wantErr:     true,
		},
		{
			name:        "multi-segment with non-reserved root is OK",
			depName:     "tp",
			dep:         Dependency{Path: "../tp", Namespace: "ThirdParty::Array"},
			depManifest: nil,
			wantNS:      "ThirdParty::Array",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ns, err := resolveNamespace(tc.depName, tc.dep, tc.depManifest)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("expected error, got namespace %q", ns)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if ns != tc.wantNS {
				t.Errorf("namespace = %q, want %q", ns, tc.wantNS)
			}
		})
	}
}

func TestManifestNamespaceField(t *testing.T) {
	dir := t.TempDir()
	tomlContent := `
[project]
name = "test"

[dependencies]
yutani = { path = "../y", namespace = "Custom::Yutani" }
`
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(tomlContent), 0644); err != nil {
		t.Fatal(err)
	}

	m, err := Load(dir)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}

	dep, ok := m.Dependencies["yutani"]
	if !ok {
		t.Fatal("missing yutani dependency")
	}
	if dep.Namespace != "Custom::Yutani" {
		t.Errorf("dep.Namespace = %q, want %q", dep.Namespace, "Custom::Yutani")
	}
}
