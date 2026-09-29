package manifest

import (
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
)

// Validate checks the manifest for internal consistency.
// toolVersion is the running mag version (e.g. "0.10.0").
func (m *Manifest) Validate(toolVersion string) error {
	// Check maggie version constraint
	if m.Project.Maggie != "" {
		if err := checkVersionConstraint(m.Project.Maggie, toolVersion); err != nil {
			return fmt.Errorf("version mismatch: %w", err)
		}
	}

	// Validate dependency ref exclusivity
	allDeps := make(map[string]Dependency)
	for k, v := range m.Dependencies {
		allDeps[k] = v
	}
	for k, v := range m.DevDependencies {
		allDeps[k] = v
	}
	for name, dep := range allDeps {
		if err := ValidateDependencyName(name); err != nil {
			return err
		}
		refCount := 0
		if dep.Tag != "" {
			refCount++
		}
		if dep.Branch != "" {
			refCount++
		}
		if dep.Commit != "" {
			refCount++
		}
		if refCount > 1 {
			return fmt.Errorf("dependency %q: tag, branch, and commit are mutually exclusive; pick one", name)
		}
		if dep.Git == "" && refCount > 0 {
			return fmt.Errorf("dependency %q: branch/tag/commit requires a git URL", name)
		}
	}

	// Validate target names are unique
	if len(m.Targets) > 0 {
		seen := make(map[string]bool, len(m.Targets))
		for _, t := range m.Targets {
			if t.Name == "" {
				return fmt.Errorf("[[target]] entry is missing required 'name' field")
			}
			if seen[t.Name] {
				return fmt.Errorf("duplicate target name %q", t.Name)
			}
			seen[t.Name] = true
		}
	}

	return nil
}

// ValidateDependencyName rejects dependency names that are unsafe to use as a
// directory name under .maggie/deps: empty, ".", "..", absolute paths, or any
// name containing a path separator. The dependency key comes straight from
// maggie.toml, so without this check a manifest could direct git clone/fetch/
// reset at an arbitrary directory.
func ValidateDependencyName(name string) error {
	switch {
	case name == "", name == ".", name == "..":
	case strings.ContainsAny(name, "/\\\x00"):
	case filepath.IsAbs(name), filepath.VolumeName(name) != "":
	default:
		return nil
	}
	return fmt.Errorf("invalid dependency name %q: must be a plain name without path separators or \"..\"", name)
}

// semver holds a parsed semantic version.
type semver struct {
	Major, Minor, Patch int
}

// parseSemver parses "X.Y.Z" into its components.
func parseSemver(s string) (semver, error) {
	s = strings.TrimPrefix(s, "v")
	parts := strings.SplitN(s, ".", 3)
	if len(parts) != 3 {
		return semver{}, fmt.Errorf("invalid semver %q (expected X.Y.Z)", s)
	}
	major, err := strconv.Atoi(parts[0])
	if err != nil {
		return semver{}, fmt.Errorf("invalid major version in %q: %w", s, err)
	}
	minor, err := strconv.Atoi(parts[1])
	if err != nil {
		return semver{}, fmt.Errorf("invalid minor version in %q: %w", s, err)
	}
	patch, err := strconv.Atoi(parts[2])
	if err != nil {
		return semver{}, fmt.Errorf("invalid patch version in %q: %w", s, err)
	}
	return semver{major, minor, patch}, nil
}

// compare returns -1, 0, or 1 comparing a to b.
func (a semver) compare(b semver) int {
	if a.Major != b.Major {
		if a.Major < b.Major {
			return -1
		}
		return 1
	}
	if a.Minor != b.Minor {
		if a.Minor < b.Minor {
			return -1
		}
		return 1
	}
	if a.Patch != b.Patch {
		if a.Patch < b.Patch {
			return -1
		}
		return 1
	}
	return 0
}

// checkVersionConstraint checks whether actual satisfies the constraint string.
// Supported forms:
//   - ">=X.Y.Z"       — at least this version
//   - "<X.Y.Z"        — below this version
//   - ">=X.Y.Z <A.B.C" — range (space-separated)
//   - "~X.Y.Z"        — pessimistic: >=X.Y.Z and <X.(Y+1).0
func checkVersionConstraint(constraint, actual string) error {
	actualVer, err := parseSemver(actual)
	if err != nil {
		return fmt.Errorf("cannot parse tool version: %w", err)
	}

	parts := strings.Fields(constraint)
	for _, part := range parts {
		if err := checkSingleConstraint(part, actualVer); err != nil {
			return fmt.Errorf("checking version constraint %q: %w", part, err)
		}
	}
	return nil
}

func checkSingleConstraint(c string, actual semver) error {
	switch {
	case strings.HasPrefix(c, ">="):
		required, err := parseSemver(strings.TrimPrefix(c, ">="))
		if err != nil {
			return fmt.Errorf("invalid constraint %q: %w", c, err)
		}
		if actual.compare(required) < 0 {
			return fmt.Errorf("requires maggie %s but running %d.%d.%d", c, actual.Major, actual.Minor, actual.Patch)
		}
	case strings.HasPrefix(c, ">") && !strings.HasPrefix(c, ">="):
		required, err := parseSemver(strings.TrimPrefix(c, ">"))
		if err != nil {
			return fmt.Errorf("invalid constraint %q: %w", c, err)
		}
		if actual.compare(required) <= 0 {
			return fmt.Errorf("requires maggie %s but running %d.%d.%d", c, actual.Major, actual.Minor, actual.Patch)
		}
	case strings.HasPrefix(c, "<="):
		required, err := parseSemver(strings.TrimPrefix(c, "<="))
		if err != nil {
			return fmt.Errorf("invalid constraint %q: %w", c, err)
		}
		if actual.compare(required) > 0 {
			return fmt.Errorf("requires maggie %s but running %d.%d.%d", c, actual.Major, actual.Minor, actual.Patch)
		}
	case strings.HasPrefix(c, "<"):
		required, err := parseSemver(strings.TrimPrefix(c, "<"))
		if err != nil {
			return fmt.Errorf("invalid constraint %q: %w", c, err)
		}
		if actual.compare(required) >= 0 {
			return fmt.Errorf("requires maggie %s but running %d.%d.%d", c, actual.Major, actual.Minor, actual.Patch)
		}
	case strings.HasPrefix(c, "~"):
		required, err := parseSemver(strings.TrimPrefix(c, "~"))
		if err != nil {
			return fmt.Errorf("invalid constraint %q: %w", c, err)
		}
		upper := semver{required.Major, required.Minor + 1, 0}
		if actual.compare(required) < 0 || actual.compare(upper) >= 0 {
			return fmt.Errorf("requires maggie %s (>=%d.%d.%d, <%d.%d.0) but running %d.%d.%d",
				c, required.Major, required.Minor, required.Patch, required.Major, required.Minor+1,
				actual.Major, actual.Minor, actual.Patch)
		}
	default:
		return fmt.Errorf("unsupported version constraint %q (use >=, <, <=, >, or ~)", c)
	}
	return nil
}
