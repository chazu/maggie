// Package manifest handles maggie.toml project configuration.
package manifest

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/BurntSushi/toml"
)

// Manifest represents a maggie.toml project configuration.
type Manifest struct {
	Project      Project                `toml:"project"`
	Source       Source                 `toml:"source"`
	Dependencies map[string]Dependency  `toml:"dependencies"`
	Image        ImageConfig            `toml:"image"`

	// Dir is the directory containing the maggie.toml file (set at load time).
	Dir string `toml:"-"`
}

// Project contains project metadata.
type Project struct {
	Name      string `toml:"name"`
	Namespace string `toml:"namespace"`
	Version   string `toml:"version"`
}

// Source configures source file locations.
type Source struct {
	Dirs  []string `toml:"dirs"`
	Entry string   `toml:"entry"`
}

// Dependency represents a single project dependency.
type Dependency struct {
	Git       string `toml:"git"`
	Tag       string `toml:"tag"`
	Path      string `toml:"path"`
	Namespace string `toml:"namespace"`
}

// ImageConfig configures image output.
type ImageConfig struct {
	Output        string `toml:"output"`
	IncludeSource bool   `toml:"include-source"`
}

// Load parses a maggie.toml file from the given directory.
func Load(dir string) (*Manifest, error) {
	path := filepath.Join(dir, "maggie.toml")
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("cannot read %s: %w", path, err)
	}

	var m Manifest
	if err := toml.Unmarshal(data, &m); err != nil {
		return nil, fmt.Errorf("parse error in %s: %w", path, err)
	}

	m.Dir, err = filepath.Abs(dir)
	if err != nil {
		return nil, fmt.Errorf("cannot resolve path %s: %w", dir, err)
	}

	// Defaults
	if len(m.Source.Dirs) == 0 {
		m.Source.Dirs = []string{"src"}
	}

	return &m, nil
}

// FindAndLoad walks up from startDir to find a maggie.toml file,
// then loads and returns the manifest. Returns nil if no manifest is found.
func FindAndLoad(startDir string) (*Manifest, error) {
	dir, err := filepath.Abs(startDir)
	if err != nil {
		return nil, err
	}

	for {
		path := filepath.Join(dir, "maggie.toml")
		if _, err := os.Stat(path); err == nil {
			return Load(dir)
		}

		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached root
			return nil, nil
		}
		dir = parent
	}
}

// SourceDirPaths returns absolute paths for the configured source directories.
func (m *Manifest) SourceDirPaths() []string {
	var paths []string
	for _, d := range m.Source.Dirs {
		paths = append(paths, filepath.Join(m.Dir, d))
	}
	return paths
}

// DepsDir returns the path to the .maggie/deps directory.
func (m *Manifest) DepsDir() string {
	return filepath.Join(m.Dir, ".maggie", "deps")
}

// LockFilePath returns the path to .maggie/lock.toml.
func (m *Manifest) LockFilePath() string {
	return filepath.Join(m.Dir, ".maggie", "lock.toml")
}
