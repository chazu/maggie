package manifest

import "fmt"

// ResolvedTarget holds the fully merged configuration for a single build target.
type ResolvedTarget struct {
	Name    string
	Entry   string
	Dirs    []string     // base source.dirs + target extra-dirs
	Exclude []string     // base source.exclude + target exclude
	GoWrap  GoWrapConfig // merged: top-level + target-specific
	Image   ImageConfig
	Output  string // output binary path
	Full    bool
}

// ResolveTarget returns a fully-resolved target configuration by name.
func (m *Manifest) ResolveTarget(name string) (*ResolvedTarget, error) {
	for i := range m.Targets {
		if m.Targets[i].Name == name {
			return m.mergeTarget(&m.Targets[i]), nil
		}
	}
	return nil, fmt.Errorf("target %q not found in maggie.toml", name)
}

// ResolveDefaultTarget returns the default target: the first declared target,
// or a synthesized one from top-level config if no targets are declared.
func (m *Manifest) ResolveDefaultTarget() *ResolvedTarget {
	if len(m.Targets) > 0 {
		return m.mergeTarget(&m.Targets[0])
	}
	return m.synthesizeTarget()
}

// ResolveAllTargets returns all declared targets fully resolved.
// If no targets are declared, returns a single synthesized target.
func (m *Manifest) ResolveAllTargets() ([]ResolvedTarget, error) {
	if len(m.Targets) == 0 {
		return []ResolvedTarget{*m.synthesizeTarget()}, nil
	}
	var result []ResolvedTarget
	for i := range m.Targets {
		result = append(result, *m.mergeTarget(&m.Targets[i]))
	}
	return result, nil
}

// mergeTarget combines top-level config with a target's overrides.
func (m *Manifest) mergeTarget(tc *TargetConfig) *ResolvedTarget {
	// Start with base source dirs, add extra, remove excluded
	dirs := append([]string{}, m.Source.Dirs...)
	dirs = append(dirs, tc.ExtraDirs...)
	if len(tc.ExcludeDirs) > 0 {
		excludeSet := make(map[string]bool, len(tc.ExcludeDirs))
		for _, d := range tc.ExcludeDirs {
			excludeSet[d] = true
		}
		var filtered []string
		for _, d := range dirs {
			if !excludeSet[d] {
				filtered = append(filtered, d)
			}
		}
		dirs = filtered
	}

	// Merge exclude patterns
	exclude := append([]string{}, m.Source.Exclude...)
	exclude = append(exclude, tc.Exclude...)

	// Merge go-wrap: base packages + target packages
	var goWrap GoWrapConfig
	goWrap.Output = m.GoWrap.Output
	if tc.GoWrap.Output != "" {
		goWrap.Output = tc.GoWrap.Output
	}
	goWrap.Packages = append(append([]GoWrapPackage{}, m.GoWrap.Packages...), tc.GoWrap.Packages...)

	// Entry: target overrides base
	entry := tc.Entry
	if entry == "" {
		entry = m.Source.Entry
	}

	// Image: target overrides base
	image := tc.Image
	if image.Output == "" && !image.IncludeSource {
		image = m.Image
	}

	// Output binary
	output := tc.Output
	if output == "" {
		output = tc.Name
	}

	return &ResolvedTarget{
		Name:    tc.Name,
		Entry:   entry,
		Dirs:    dirs,
		Exclude: exclude,
		GoWrap:  goWrap,
		Image:   image,
		Output:  output,
		Full:    tc.Full,
	}
}

// synthesizeTarget creates a target from top-level config when no [[target]] exists.
func (m *Manifest) synthesizeTarget() *ResolvedTarget {
	output := m.Project.Name
	if output == "" {
		output = "mag-custom"
	}
	return &ResolvedTarget{
		Name:    "default",
		Entry:   m.Source.Entry,
		Dirs:    append([]string{}, m.Source.Dirs...),
		Exclude: append([]string{}, m.Source.Exclude...),
		GoWrap:  m.GoWrap,
		Image:   m.Image,
		Output:  output,
	}
}
