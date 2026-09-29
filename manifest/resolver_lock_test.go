package manifest

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// newGitRepo creates a repo with a maggie.toml and one commit per message,
// returning its path and the commit hashes in order.
func newGitRepo(t *testing.T, dir string, msgs ...string) []string {
	t.Helper()
	writeManifest(t, dir, "[project]\nname = \"dep\"\n")
	gitRun(t, dir, "init", "--quiet", "--initial-branch=main")
	var commits []string
	for _, msg := range msgs {
		gitRun(t, dir, "add", ".")
		gitRun(t, dir, "commit", "--quiet", "--allow-empty", "-m", msg)
		commits = append(commits, gitRun(t, dir, "rev-parse", "HEAD"))
	}
	return commits
}

func requireGit(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not available")
	}
}

// A fresh clone (CI, new checkout) of a branch dependency must build the
// locked commit, not whatever the branch tip is now.
func TestResolveChecksOutLockedCommit(t *testing.T) {
	requireGit(t)
	root := t.TempDir()
	repo := filepath.Join(root, "repo")
	commits := newGitRepo(t, repo, "one")
	url := "file://" + filepath.ToSlash(repo)

	app := filepath.Join(root, "app")
	writeManifest(t, app, "[project]\nname = \"app\"\n\n[dependencies]\ndep = { git = \""+url+"\", branch = \"main\" }\n")
	m, err := Load(app)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := NewResolver(m, false).Resolve(); err != nil {
		t.Fatalf("first Resolve: %v", err)
	}

	// Upstream advances; the working copy is discarded (fresh checkout).
	gitRun(t, repo, "commit", "--quiet", "--allow-empty", "-m", "two")
	if err := os.RemoveAll(m.DepsDir()); err != nil {
		t.Fatal(err)
	}

	deps, err := NewResolver(m, false).Resolve()
	if err != nil {
		t.Fatalf("second Resolve: %v", err)
	}
	if got := gitRun(t, deps[0].LocalPath, "rev-parse", "HEAD"); got != commits[0] {
		t.Errorf("checked out %s, want locked %s", got, commits[0])
	}
}

// Pointing a dependency at a different repository must replace the clone,
// not keep fetching from the old origin.
func TestResolveRecloneOnURLChange(t *testing.T) {
	requireGit(t)
	root := t.TempDir()
	oldRepo := filepath.Join(root, "old")
	newRepo := filepath.Join(root, "new")
	newGitRepo(t, oldRepo, "old")
	newCommits := newGitRepo(t, newRepo, "new")

	app := filepath.Join(root, "app")
	writeManifest(t, app, "[project]\nname = \"app\"\n\n[dependencies]\ndep = { git = \"file://"+filepath.ToSlash(oldRepo)+"\" }\n")
	m, err := Load(app)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := NewResolver(m, false).Resolve(); err != nil {
		t.Fatalf("first Resolve: %v", err)
	}

	writeManifest(t, app, "[project]\nname = \"app\"\n\n[dependencies]\ndep = { git = \"file://"+filepath.ToSlash(newRepo)+"\" }\n")
	if m, err = Load(app); err != nil {
		t.Fatal(err)
	}
	deps, err := NewResolver(m, false).Resolve()
	if err != nil {
		t.Fatalf("second Resolve: %v", err)
	}
	if got := gitRun(t, deps[0].LocalPath, "rev-parse", "HEAD"); got != newCommits[0] {
		t.Errorf("dep at %s, want new repo's %s", got, newCommits[0])
	}
}

// Resolving without dev-dependencies (mag build) must keep their lock pins.
func TestResolveWithoutDevDepsKeepsTheirPins(t *testing.T) {
	requireGit(t)
	root := t.TempDir()
	repo := filepath.Join(root, "repo")
	newGitRepo(t, repo, "one")
	url := "file://" + filepath.ToSlash(repo)

	app := filepath.Join(root, "app")
	writeManifest(t, app, "[project]\nname = \"app\"\n\n[dependencies]\nlib = { git = \""+url+"\" }\n\n[dev-dependencies]\ntestkit = { git = \""+url+"\" }\n")
	m, err := Load(app)
	if err != nil {
		t.Fatal(err)
	}
	all, err := m.AllDependencies()
	if err != nil {
		t.Fatal(err)
	}
	if _, err := NewResolver(m, false, all).Resolve(); err != nil {
		t.Fatalf("dev Resolve: %v", err)
	}
	if _, err := NewResolver(m, false).Resolve(); err != nil {
		t.Fatalf("build Resolve: %v", err)
	}
	lf, err := ReadLock(m.LockFilePath())
	if err != nil {
		t.Fatal(err)
	}
	if lf.FindLockedDep("testkit") == nil {
		t.Errorf("dev-dependency pin dropped: %+v", lf.Deps)
	}
	if lf.FindLockedDep("lib") == nil {
		t.Errorf("dependency pin missing: %+v", lf.Deps)
	}
}
