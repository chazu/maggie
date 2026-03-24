package manifest

import (
	"os"
	"path/filepath"
	"testing"
)

func TestResolveTargetMergesDirs(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src", "lib"}},
		Targets: []TargetConfig{
			{Name: "server", ExtraDirs: []string{"tools"}},
		},
	}
	rt, err := m.ResolveTarget("server")
	if err != nil {
		t.Fatal(err)
	}
	if len(rt.Dirs) != 3 {
		t.Fatalf("expected 3 dirs, got %v", rt.Dirs)
	}
	if rt.Dirs[2] != "tools" {
		t.Errorf("expected dirs[2]=tools, got %q", rt.Dirs[2])
	}
}

func TestResolveTargetExcludeDirs(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src", "lib", "admin"}},
		Targets: []TargetConfig{
			{Name: "worker", ExcludeDirs: []string{"admin"}},
		},
	}
	rt, err := m.ResolveTarget("worker")
	if err != nil {
		t.Fatal(err)
	}
	if len(rt.Dirs) != 2 {
		t.Fatalf("expected 2 dirs, got %v", rt.Dirs)
	}
	for _, d := range rt.Dirs {
		if d == "admin" {
			t.Error("admin should have been excluded")
		}
	}
}

func TestResolveTargetMergesExclude(t *testing.T) {
	m := &Manifest{
		Source: Source{
			Dirs:    []string{"src"},
			Exclude: []string{"*_test.mag"},
		},
		Targets: []TargetConfig{
			{Name: "prod", Exclude: []string{"*_debug.mag"}},
		},
	}
	rt, err := m.ResolveTarget("prod")
	if err != nil {
		t.Fatal(err)
	}
	if len(rt.Exclude) != 2 {
		t.Fatalf("expected 2 exclude patterns, got %v", rt.Exclude)
	}
}

func TestResolveTargetGoWrapMerge(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src"}},
		GoWrap: GoWrapConfig{
			Output:   "wrap",
			Packages: []GoWrapPackage{{Import: "strings"}},
		},
		Targets: []TargetConfig{
			{
				Name: "server",
				GoWrap: GoWrapConfig{
					Packages: []GoWrapPackage{{Import: "net/http"}},
				},
			},
		},
	}
	rt, err := m.ResolveTarget("server")
	if err != nil {
		t.Fatal(err)
	}
	if len(rt.GoWrap.Packages) != 2 {
		t.Fatalf("expected 2 go-wrap packages, got %d", len(rt.GoWrap.Packages))
	}
	if rt.GoWrap.Output != "wrap" {
		t.Errorf("expected go-wrap output 'wrap', got %q", rt.GoWrap.Output)
	}
}

func TestResolveTargetEntryOverride(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src"}, Entry: "Main.start"},
		Targets: []TargetConfig{
			{Name: "server", Entry: "Server.start"},
			{Name: "cli"}, // inherits base entry
		},
	}
	rt, _ := m.ResolveTarget("server")
	if rt.Entry != "Server.start" {
		t.Errorf("expected Server.start, got %q", rt.Entry)
	}

	rt, _ = m.ResolveTarget("cli")
	if rt.Entry != "Main.start" {
		t.Errorf("expected inherited Main.start, got %q", rt.Entry)
	}
}

func TestResolveTargetNotFound(t *testing.T) {
	m := &Manifest{
		Targets: []TargetConfig{{Name: "server"}},
	}
	_, err := m.ResolveTarget("nonexistent")
	if err == nil {
		t.Error("expected error for missing target")
	}
}

func TestResolveDefaultTargetSynthesized(t *testing.T) {
	m := &Manifest{
		Project: Project{Name: "myapp"},
		Source:  Source{Dirs: []string{"src"}, Entry: "Main.start"},
	}
	rt := m.ResolveDefaultTarget()
	if rt.Name != "default" {
		t.Errorf("expected name 'default', got %q", rt.Name)
	}
	if rt.Entry != "Main.start" {
		t.Errorf("expected entry Main.start, got %q", rt.Entry)
	}
	if rt.Output != "myapp" {
		t.Errorf("expected output 'myapp', got %q", rt.Output)
	}
}

func TestResolveDefaultTargetUsesFirst(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src"}},
		Targets: []TargetConfig{
			{Name: "server", Entry: "Server.start"},
			{Name: "cli", Entry: "CLI.start"},
		},
	}
	rt := m.ResolveDefaultTarget()
	if rt.Name != "server" {
		t.Errorf("expected first target 'server', got %q", rt.Name)
	}
}

func TestResolveAllTargets(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src"}},
		Targets: []TargetConfig{
			{Name: "server", Entry: "Server.start"},
			{Name: "cli", Entry: "CLI.start"},
		},
	}
	targets, err := m.ResolveAllTargets()
	if err != nil {
		t.Fatal(err)
	}
	if len(targets) != 2 {
		t.Fatalf("expected 2 targets, got %d", len(targets))
	}
}

func TestResolveAllTargetsSynthesized(t *testing.T) {
	m := &Manifest{
		Project: Project{Name: "myapp"},
		Source:  Source{Dirs: []string{"src"}, Entry: "Main.start"},
	}
	targets, err := m.ResolveAllTargets()
	if err != nil {
		t.Fatal(err)
	}
	if len(targets) != 1 {
		t.Fatalf("expected 1 synthesized target, got %d", len(targets))
	}
	if targets[0].Entry != "Main.start" {
		t.Errorf("expected Main.start, got %q", targets[0].Entry)
	}
}

func TestResolveTargetOutputDefault(t *testing.T) {
	m := &Manifest{
		Source: Source{Dirs: []string{"src"}},
		Targets: []TargetConfig{
			{Name: "server"}, // no output specified
		},
	}
	rt, _ := m.ResolveTarget("server")
	if rt.Output != "server" {
		t.Errorf("expected output to default to target name 'server', got %q", rt.Output)
	}
}

func TestLoadManifestTargets(t *testing.T) {
	dir := t.TempDir()
	toml := `
[project]
name = "multi"

[source]
dirs = ["src"]
entry = "Main.start"

[[target]]
name = "server"
entry = "Server.start"
output = "my-server"
full = true
extra-dirs = ["tools"]
exclude-dirs = ["admin"]

[[target]]
name = "cli"
entry = "CLI.main"
`
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(toml), 0644); err != nil {
		t.Fatal(err)
	}

	m, err := Load(dir)
	if err != nil {
		t.Fatalf("Load failed: %v", err)
	}

	if len(m.Targets) != 2 {
		t.Fatalf("expected 2 targets, got %d", len(m.Targets))
	}
	if m.Targets[0].Name != "server" {
		t.Errorf("target[0].Name = %q, want server", m.Targets[0].Name)
	}
	if !m.Targets[0].Full {
		t.Error("target[0].Full should be true")
	}
	if m.Targets[0].Output != "my-server" {
		t.Errorf("target[0].Output = %q, want my-server", m.Targets[0].Output)
	}
	if len(m.Targets[0].ExtraDirs) != 1 || m.Targets[0].ExtraDirs[0] != "tools" {
		t.Errorf("target[0].ExtraDirs = %v, want [tools]", m.Targets[0].ExtraDirs)
	}
}
