package manifest

import (
	"fmt"
	"os/exec"
	"strings"
)

// gitClone clones a git repository to dest.
func gitClone(url, dest string) error {
	cmd := exec.Command("git", "clone", "--quiet", url, dest)
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("git clone %s: %s: %w", url, strings.TrimSpace(string(out)), err)
	}
	return nil
}

// gitCheckout checks out a specific ref (tag, branch, or commit) in a repo.
func gitCheckout(dir, ref string) error {
	cmd := exec.Command("git", "checkout", "--quiet", ref)
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("git checkout %s in %s: %s: %w", ref, dir, strings.TrimSpace(string(out)), err)
	}
	return nil
}

// gitFetch fetches updates from the remote.
func gitFetch(dir string) error {
	cmd := exec.Command("git", "fetch", "--quiet", "--all", "--tags")
	cmd.Dir = dir
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("git fetch in %s: %s: %w", dir, strings.TrimSpace(string(out)), err)
	}
	return nil
}

// gitCurrentCommit returns the current HEAD commit hash.
func gitCurrentCommit(dir string) (string, error) {
	cmd := exec.Command("git", "rev-parse", "HEAD")
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("git rev-parse HEAD in %s: %w", dir, err)
	}
	return strings.TrimSpace(string(out)), nil
}

// gitIsClean returns true if the working directory has no uncommitted changes.
func gitIsClean(dir string) (bool, error) {
	cmd := exec.Command("git", "status", "--porcelain")
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		return false, fmt.Errorf("git status in %s: %w", dir, err)
	}
	return len(strings.TrimSpace(string(out))) == 0, nil
}
