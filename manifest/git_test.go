package manifest

import (
	"strings"
	"testing"
)

// A dependency's git URL and refs come from (possibly transitive, untrusted)
// maggie.toml files; a value beginning with "-" must be refused before it can
// reach git's argv as an option.
func TestGitRejectsOptionLikeArgs(t *testing.T) {
	dir := t.TempDir()
	checks := map[string]func() error{
		"clone url":  func() error { return gitClone("--upload-pack=touch x", dir+"/dest") },
		"checkout":   func() error { return gitCheckout(dir, "--orphan=x") },
		"reset hard": func() error { return gitResetHard(dir, "-q") },
	}
	for name, fn := range checks {
		err := fn()
		if err == nil || !strings.Contains(err.Error(), "must not begin with '-'") {
			t.Errorf("%s: got %v, want option-like rejection", name, err)
		}
	}
}
