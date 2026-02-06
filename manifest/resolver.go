package manifest

import (
	"fmt"
	"os"
	"path/filepath"
)

// ResolvedDep represents a dependency that has been resolved to a local path.
type ResolvedDep struct {
	Name      string    // dependency name
	LocalPath string    // local filesystem path
	Namespace string    // namespace for this dependency
	Manifest  *Manifest // the dependency's own manifest (may be nil)
}

// Resolver manages dependency resolution.
type Resolver struct {
	manifest *Manifest
	lock     *LockFile
	verbose  bool
}

// NewResolver creates a new dependency resolver.
func NewResolver(m *Manifest, verbose bool) *Resolver {
	return &Resolver{
		manifest: m,
		verbose:  verbose,
	}
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
	order, err := r.resolveAll(r.manifest.Dependencies, resolved)
	if err != nil {
		return nil, err
	}

	// Write updated lock file
	if err := r.writeLock(resolved); err != nil {
		return nil, fmt.Errorf("writing lock file: %w", err)
	}

	return order, nil
}

// resolveAll resolves a set of dependencies recursively.
// Returns dependencies in topological order (deps before dependents).
func (r *Resolver) resolveAll(deps map[string]Dependency, resolved map[string]*ResolvedDep) ([]ResolvedDep, error) {
	var order []ResolvedDep

	for name, dep := range deps {
		if _, ok := resolved[name]; ok {
			continue // already resolved
		}

		rd, err := r.resolveOne(name, dep)
		if err != nil {
			return nil, fmt.Errorf("resolving %s: %w", name, err)
		}

		resolved[name] = rd

		// Check for transitive dependencies
		if rd.Manifest != nil && len(rd.Manifest.Dependencies) > 0 {
			transitive, err := r.resolveAll(rd.Manifest.Dependencies, resolved)
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

// resolveOne resolves a single dependency.
func (r *Resolver) resolveOne(name string, dep Dependency) (*ResolvedDep, error) {
	depsDir := r.manifest.DepsDir()

	if dep.Path != "" {
		// Local path dependency
		localPath := dep.Path
		if !filepath.IsAbs(localPath) {
			localPath = filepath.Join(r.manifest.Dir, localPath)
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
		}, nil
	}

	if dep.Git != "" {
		// Git dependency
		depDir := filepath.Join(depsDir, name)

		// Check if already cloned
		if _, err := os.Stat(depDir); os.IsNotExist(err) {
			if r.verbose {
				fmt.Printf("  Cloning %s from %s\n", name, dep.Git)
			}
			if err := gitClone(dep.Git, depDir); err != nil {
				return nil, err
			}
		} else {
			// Check if we need to update
			locked := r.lock.FindLockedDep(name)
			if locked != nil && locked.Tag == dep.Tag {
				// Already at correct version, skip fetch
			} else {
				if r.verbose {
					fmt.Printf("  Fetching %s\n", name)
				}
				if err := gitFetch(depDir); err != nil {
					return nil, err
				}
			}
		}

		// Checkout the requested ref
		if dep.Tag != "" {
			if err := gitCheckout(depDir, dep.Tag); err != nil {
				return nil, err
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
		}, nil
	}

	return nil, fmt.Errorf("dependency %q has no git or path specified", name)
}

// writeLock writes the resolved dependencies to the lock file.
func (r *Resolver) writeLock(resolved map[string]*ResolvedDep) error {
	lf := &LockFile{}

	for _, rd := range resolved {
		ld := LockedDep{
			Name: rd.Name,
		}

		dep := r.manifest.Dependencies[rd.Name]
		if dep.Git != "" {
			ld.Git = dep.Git
			ld.Tag = dep.Tag
			// Get current commit
			if commit, err := gitCurrentCommit(rd.LocalPath); err == nil {
				ld.Commit = commit
			}
		} else if dep.Path != "" {
			ld.Path = dep.Path
		}

		lf.Deps = append(lf.Deps, ld)
	}

	// Ensure directory exists
	lockDir := filepath.Dir(r.manifest.LockFilePath())
	if err := os.MkdirAll(lockDir, 0755); err != nil {
		return err
	}

	return WriteLock(r.manifest.LockFilePath(), lf)
}
