package manifest

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// ResolvedDep represents a dependency that has been resolved to a local path.
type ResolvedDep struct {
	Name      string    // dependency name
	LocalPath string    // local filesystem path
	Namespace string    // namespace for this dependency
	Manifest  *Manifest // the dependency's own manifest (may be nil)

	// Dep is the dependency declaration this was resolved from (from the root
	// manifest for direct deps, from the declaring dep's manifest for
	// transitive ones). writeLock uses it to pin git URL/ref/commit.
	Dep Dependency
}

// Resolver manages dependency resolution.
type Resolver struct {
	manifest *Manifest
	deps     map[string]Dependency // deps to resolve (may include dev-deps)
	lock     *LockFile
	verbose  bool
}

// NewResolver creates a new dependency resolver.
// If deps is provided, it overrides m.Dependencies (used when including dev-deps).
func NewResolver(m *Manifest, verbose bool, deps ...map[string]Dependency) *Resolver {
	r := &Resolver{
		manifest: m,
		verbose:  verbose,
	}
	if len(deps) > 0 && deps[0] != nil {
		r.deps = deps[0]
	} else {
		r.deps = m.Dependencies
	}
	return r
}

// Resolve resolves all dependencies and returns them in load order
// (topologically sorted: dependencies before dependents).
func (r *Resolver) Resolve() ([]ResolvedDep, error) {
	// Read existing lock file
	lock, err := ReadLock(r.manifest.LockFilePath())
	if err != nil {
		return nil, fmt.Errorf("reading lock file: %w", err)
	}
	r.lock = lock

	// Ensure .maggie/deps directory exists
	depsDir := r.manifest.DepsDir()
	if err := os.MkdirAll(depsDir, 0755); err != nil {
		return nil, fmt.Errorf("creating deps dir: %w", err)
	}

	// Resolve each direct dependency
	resolved := make(map[string]*ResolvedDep)
	order, err := r.resolveAll(r.deps, r.manifest.Dir, resolved)
	if err != nil {
		return nil, err
	}

	// Write updated lock file
	if err := r.writeLock(resolved); err != nil {
		return nil, fmt.Errorf("writing lock file: %w", err)
	}

	return order, nil
}

// resolveAll resolves a set of dependencies recursively. baseDir is the
// directory of the manifest that declares deps; relative `path` dependencies
// resolve against it.
// Returns dependencies in topological order (deps before dependents).
func (r *Resolver) resolveAll(deps map[string]Dependency, baseDir string, resolved map[string]*ResolvedDep) ([]ResolvedDep, error) {
	var order []ResolvedDep

	// Iterate in sorted name order: the returned load order (and the lock file
	// derived from it) must be deterministic, otherwise the image build order
	// varies between runs (it matters when sibling deps extend the same class).
	names := make([]string, 0, len(deps))
	for name := range deps {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		dep := deps[name]
		if _, ok := resolved[name]; ok {
			continue // already resolved
		}

		rd, err := r.resolveOne(name, dep, baseDir)
		if err != nil {
			return nil, fmt.Errorf("resolving %s: %w", name, err)
		}

		resolved[name] = rd

		// Check for transitive dependencies
		if rd.Manifest != nil && len(rd.Manifest.Dependencies) > 0 {
			transitive, err := r.resolveAll(rd.Manifest.Dependencies, rd.LocalPath, resolved)
			if err != nil {
				return nil, err
			}
			order = append(order, transitive...)
		}

		order = append(order, *rd)
	}

	return order, nil
}

// resolveNamespace determines the effective namespace for a dependency using
// the three-level resolution order:
//  1. Consumer override (dep.Namespace from TOML)
//  2. Producer manifest (depManifest.Project.Namespace)
//  3. PascalCase fallback (ToPascalCase(name))
func resolveNamespace(name string, dep Dependency, depManifest *Manifest) (string, error) {
	var ns string
	switch {
	case dep.Namespace != "":
		ns = dep.Namespace
	case depManifest != nil && depManifest.Project.Namespace != "":
		ns = depManifest.Project.Namespace
	default:
		ns = ToPascalCase(name)
	}

	if IsReservedNamespace(ns) {
		return "", fmt.Errorf("dependency %q resolves to reserved namespace %q (used by a core VM class); add namespace = \"...\" override in [dependencies]", name, ns)
	}

	return ns, nil
}

// resolveOne resolves a single dependency. Relative path dependencies are
// resolved against baseDir (the declaring manifest's directory).
func (r *Resolver) resolveOne(name string, dep Dependency, baseDir string) (*ResolvedDep, error) {
	// Defensive: the name becomes a directory under depsDir, so never trust it
	// even if manifest validation was bypassed.
	if err := ValidateDependencyName(name); err != nil {
		return nil, err
	}
	depsDir := r.manifest.DepsDir()

	if dep.Path != "" {
		// Local path dependency
		localPath := dep.Path
		if !filepath.IsAbs(localPath) {
			localPath = filepath.Join(baseDir, localPath)
		}

		localPath, err := filepath.Abs(localPath)
		if err != nil {
			return nil, fmt.Errorf("invalid path %q: %w", dep.Path, err)
		}

		// Verify it exists
		if _, err := os.Stat(localPath); err != nil {
			return nil, fmt.Errorf("local dependency %q not found at %s: %w", name, localPath, err)
		}

		// Try to load its manifest
		depManifest, _ := Load(localPath)

		ns, err := resolveNamespace(name, dep, depManifest)
		if err != nil {
			return nil, err
		}

		return &ResolvedDep{
			Name:      name,
			LocalPath: localPath,
			Namespace: ns,
			Manifest:  depManifest,
			Dep:       dep,
		}, nil
	}

	if dep.Git != "" {
		// Git dependency
		depDir := filepath.Join(depsDir, name)

		// A clone of a different repository (the manifest's git URL changed)
		// must not be reused: fetching would keep pulling from the old origin.
		if _, err := os.Stat(depDir); err == nil {
			if r.cloneIsStale(name, dep, depDir) {
				if r.verbose {
					fmt.Printf("  Re-cloning %s (origin changed to %s)\n", name, dep.Git)
				}
				if err := os.RemoveAll(depDir); err != nil {
					return nil, fmt.Errorf("removing stale clone of %s: %w", name, err)
				}
			}
		}

		// The lock pins the commit when it was written for this same URL and
		// ref. An explicit commit in the manifest must agree with it.
		var lockedCommit string
		if locked := r.lock.FindLockedDep(name); locked != nil &&
			locked.Commit != "" && locked.Git == dep.Git &&
			locked.Tag == dep.Tag && locked.Branch == dep.Branch &&
			(dep.Commit == "" || dep.Commit == locked.Commit) {
			lockedCommit = locked.Commit
		}

		if _, err := os.Stat(depDir); os.IsNotExist(err) {
			if r.verbose {
				fmt.Printf("  Cloning %s from %s\n", name, dep.Git)
			}
			if err := gitClone(dep.Git, depDir); err != nil {
				return nil, err
			}
		} else if lockedCommit == "" {
			if r.verbose {
				fmt.Printf("  Fetching %s\n", name)
			}
			if err := gitFetch(depDir); err != nil {
				return nil, err
			}
		}

		if lockedCommit != "" {
			// Check out exactly the locked commit — not the branch tip — so
			// every checkout (fresh clone, CI) builds the same code. Fetch only
			// if the clone doesn't have it yet.
			if err := gitCheckout(depDir, lockedCommit); err != nil {
				if fetchErr := gitFetch(depDir); fetchErr != nil {
					return nil, fetchErr
				}
				if err := gitCheckout(depDir, lockedCommit); err != nil {
					return nil, err
				}
			}
		} else {
			// Checkout the requested ref (tag, branch, or commit)
			ref := dep.Tag
			if ref == "" {
				ref = dep.Branch
			}
			if ref == "" {
				ref = dep.Commit
			}
			if ref != "" {
				if err := gitCheckout(depDir, ref); err != nil {
					return nil, err
				}
				// For a branch, `git checkout <branch>` on an already-checked-out
				// local branch leaves it at its old commit — it never advances to
				// the fetched remote tip. Hard-reset to origin/<branch> so the dep
				// actually tracks the branch. (Tags/commits are immutable refs and
				// need no reset.)
				if dep.Tag == "" && dep.Commit == "" && dep.Branch != "" {
					if err := gitResetHard(depDir, "origin/"+dep.Branch); err != nil {
						return nil, err
					}
				}
			}
		}

		// Try to load its manifest
		depManifest, _ := Load(depDir)

		ns, err := resolveNamespace(name, dep, depManifest)
		if err != nil {
			return nil, err
		}

		return &ResolvedDep{
			Name:      name,
			LocalPath: depDir,
			Namespace: ns,
			Manifest:  depManifest,
			Dep:       dep,
		}, nil
	}

	return nil, fmt.Errorf("dependency %q has no git or path specified", name)
}

// cloneIsStale reports whether the existing clone at depDir came from a
// different repository than dep.Git: either the lock recorded another URL, or
// the clone's origin differs. The origin comparison is skipped for local-path
// URLs, which git stores rewritten (absolute, uncleaned), so they would never
// compare equal.
func (r *Resolver) cloneIsStale(name string, dep Dependency, depDir string) bool {
	if locked := r.lock.FindLockedDep(name); locked != nil && locked.Git != "" && locked.Git != dep.Git {
		return true
	}
	if !strings.Contains(dep.Git, "://") && !strings.Contains(dep.Git, "@") {
		return false // local path
	}
	origin, err := gitRemoteURL(depDir)
	return err != nil || origin != dep.Git
}

// excludesDevDeps reports whether this resolver was given a dependency set
// that leaves out some of the manifest's dev-dependencies.
func (r *Resolver) excludesDevDeps() bool {
	for name := range r.manifest.DevDependencies {
		if _, ok := r.deps[name]; !ok {
			return true
		}
	}
	return false
}

// writeLock writes the resolved dependencies to the lock file.
func (r *Resolver) writeLock(resolved map[string]*ResolvedDep) error {
	lf := &LockFile{}

	// Emit lock entries in sorted name order so lock.toml doesn't churn between
	// otherwise-identical runs.
	names := make([]string, 0, len(resolved))
	for name := range resolved {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		rd := resolved[name]
		ld := LockedDep{
			Name: rd.Name,
		}

		dep := rd.Dep
		if dep.Git != "" {
			ld.Git = dep.Git
			ld.Tag = dep.Tag
			ld.Branch = dep.Branch
			// Get current commit
			if commit, err := gitCurrentCommit(rd.LocalPath); err == nil {
				ld.Commit = commit
			}
		} else if dep.Path != "" {
			// Record the path relative to the root project so transitive path
			// deps (declared relative to their parent) stay meaningful.
			ld.Path = dep.Path
			if !filepath.IsAbs(dep.Path) {
				if rel, err := filepath.Rel(r.manifest.Dir, rd.LocalPath); err == nil {
					ld.Path = filepath.ToSlash(rel)
				} else {
					ld.Path = rd.LocalPath
				}
			}
		}

		lf.Deps = append(lf.Deps, ld)
	}

	// A run that leaves out declared dev-dependencies (e.g. `mag build`) must
	// not drop their pins — and those of their transitive deps — from the
	// lock: carry over every earlier entry this run did not resolve. A full
	// run rewrites the lock from scratch, pruning removed dependencies.
	if r.excludesDevDeps() && r.lock != nil {
		for _, prev := range r.lock.Deps {
			if _, ok := resolved[prev.Name]; !ok {
				lf.Deps = append(lf.Deps, prev)
			}
		}
		sort.Slice(lf.Deps, func(i, j int) bool { return lf.Deps[i].Name < lf.Deps[j].Name })
	}

	// Ensure directory exists
	lockDir := filepath.Dir(r.manifest.LockFilePath())
	if err := os.MkdirAll(lockDir, 0755); err != nil {
		return fmt.Errorf("creating lock file directory %q: %w", lockDir, err)
	}

	return WriteLock(r.manifest.LockFilePath(), lf)
}
