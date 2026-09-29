package manifest

import (
	"fmt"
	"path/filepath"
)

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
	// Start with base source dirs, add extra (once each: a repeated dir
	// would compile the same files twice), remove excluded
	var dirs []string
	seenDir := make(map[string]bool)
	for _, d := range append(append([]string{}, m.Source.Dirs...), tc.ExtraDirs...) {
		if key := filepath.Clean(d); !seenDir[key] {
			seenDir[key] = true
			dirs = append(dirs, d)
		}
	}
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

	// Image: merged per field. A target that set only output used to drop
	// the top-level include-source. (include-source is a plain bool, so a
	// target can add it but not switch off a top-level true.)
	image := m.Image
	if tc.Image.Output != "" {
		image.Output = tc.Image.Output
	}
	image.IncludeSource = image.IncludeSource || tc.Image.IncludeSource

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
