package manifest

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func writeManifest(t *testing.T, dir, content string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "maggie.toml"), []byte(content), 0644); err != nil {
		t.Fatal(err)
	}
}

// A transitive path dependency must resolve relative to the directory of the
// dependency that declares it, not the root project.
func TestResolveTransitivePathRelativeToDeclaringDep(t *testing.T) {
	root := t.TempDir()
	app := filepath.Join(root, "app")
	libA := filepath.Join(root, "libs", "a")
	libB := filepath.Join(root, "libs", "b")

	writeManifest(t, app, "[project]\nname = \"app\"\n\n[dependencies]\na = { path = \"../libs/a\" }\n")
	writeManifest(t, libA, "[project]\nname = \"a\"\n\n[dependencies]\nb = { path = \"../b\" }\n")
	writeManifest(t, libB, "[project]\nname = \"b\"\n")

	m, err := Load(app)
	if err != nil {
		t.Fatal(err)
	}
	order, err := NewResolver(m, false).Resolve()
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if len(order) != 2 || order[0].Name != "b" || order[1].Name != "a" {
		t.Fatalf("unexpected order: %+v", order)
	}
	wantB, _ := filepath.EvalSymlinks(libB)
	gotB, _ := filepath.EvalSymlinks(order[0].LocalPath)
	if gotB != wantB {
		t.Errorf("b resolved to %s, want %s", gotB, wantB)
	}
}

func TestValidateRejectsUnsafeDependencyNames(t *testing.T) {
	bad := []string{"", ".", "..", "../../../escaped", "a/b", `a\b`, "/abs"}
	for _, name := range bad {
		m := &Manifest{Dependencies: map[string]Dependency{name: {Git: "https://example.com/x.git"}}}
		if err := m.Validate("99.0.0"); err == nil {
			t.Errorf("Validate accepted dependency name %q", name)
		}
		m = &Manifest{DevDependencies: map[string]Dependency{name: {Path: "../x"}}}
		if err := m.Validate("99.0.0"); err == nil {
			t.Errorf("Validate accepted dev-dependency name %q", name)
		}
	}
	ok := &Manifest{Dependencies: map[string]Dependency{"my-lib_2.x": {Path: "../x"}}}
	if err := ok.Validate("99.0.0"); err != nil {
		t.Errorf("Validate rejected valid name: %v", err)
	}
}

func TestLoadRejectsEscapingDependencyName(t *testing.T) {
	dir := t.TempDir()
	writeManifest(t, dir, "[project]\nname = \"app\"\n\n[dependencies]\n\"../../../escaped\" = { git = \"https://example.com/x.git\" }\n")
	if _, err := Load(dir); err == nil {
		t.Fatal("Load accepted a dependency name that escapes the deps dir")
	}
}

// The resolver must defend against unsafe names even if Validate was bypassed
// (e.g. a hand-constructed Manifest), and must not touch the filesystem.
func TestResolverRejectsUnsafeDependencyName(t *testing.T) {
	dir := t.TempDir()
	m := &Manifest{Dir: dir, Dependencies: map[string]Dependency{
		"../escaped": {Git: filepath.Join(dir, "nonexistent-repo")},
	}}
	_, err := NewResolver(m, false).Resolve()
	if err == nil || !strings.Contains(err.Error(), "invalid dependency name") {
		t.Fatalf("expected invalid dependency name error, got %v", err)
	}
	if _, statErr := os.Stat(filepath.Join(dir, ".maggie", "escaped")); !os.IsNotExist(statErr) {
		t.Errorf("resolver created a directory outside deps dir")
	}
}

func gitRun(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command("git", args...)
	cmd.Dir = dir
	cmd.Env = append(os.Environ(),
		"GIT_AUTHOR_NAME=t", "GIT_AUTHOR_EMAIL=t@example.com",
		"GIT_COMMITTER_NAME=t", "GIT_COMMITTER_EMAIL=t@example.com")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("git %v: %v\n%s", args, err, out)
	}
	return strings.TrimSpace(string(out))
}

// Transitive git dependencies must be pinned in the lock file with their git
// URL and resolved commit, not just their name.
func TestWriteLockPinsTransitiveGitDeps(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not available")
	}
	root := t.TempDir()
	repoB := filepath.Join(root, "repo-b")
	writeManifest(t, repoB, "[project]\nname = \"b\"\n")
	gitRun(t, repoB, "init", "--quiet")
	gitRun(t, repoB, "add", ".")
	gitRun(t, repoB, "commit", "--quiet", "-m", "init")
	commitB := gitRun(t, repoB, "rev-parse", "HEAD")

	app := filepath.Join(root, "app")
	libA := filepath.Join(root, "a")
	writeManifest(t, app, "[project]\nname = \"app\"\n\n[dependencies]\na = { path = \"../a\" }\n")
	writeManifest(t, libA, "[project]\nname = \"a\"\n\n[dependencies]\nb = { git = \""+filepath.ToSlash(repoB)+"\" }\n")

	m, err := Load(app)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := NewResolver(m, false).Resolve(); err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	lf, err := ReadLock(m.LockFilePath())
	if err != nil || lf == nil {
		t.Fatalf("ReadLock: %v", err)
	}
	var b *LockedDep
	for i := range lf.Deps {
		if lf.Deps[i].Name == "b" {
			b = &lf.Deps[i]
		}
	}
	if b == nil {
		t.Fatalf("lock missing b: %+v", lf.Deps)
	}
	if b.Git != filepath.ToSlash(repoB) {
		t.Errorf("lock b.Git = %q, want %q", b.Git, repoB)
	}
	if b.Commit != commitB {
		t.Errorf("lock b.Commit = %q, want %q", b.Commit, commitB)
	}
}
