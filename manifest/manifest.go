// Package manifest handles maggie.toml project configuration.
package manifest

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/BurntSushi/toml"
	"github.com/chazu/maggie/version"
)

// RuntimeConfig configures VM runtime parameters.
type RuntimeConfig struct {
	MaxStackDepth   int    `toml:"max-stack-depth"`
	MaxFrameDepth   int    `toml:"max-frame-depth"`
	InitialStack    int    `toml:"initial-stack"`
	InitialFrames   int    `toml:"initial-frames"`
	MailboxCapacity int    `toml:"mailbox-capacity"`
	GCInterval      string `toml:"gc-interval"` // e.g. "30s", "1m"
}

// Manifest represents a maggie.toml project configuration.
type Manifest struct {
	Project         Project                `toml:"project"`
	Source          Source                 `toml:"source"`
	Dependencies    map[string]Dependency  `toml:"dependencies"`
	DevDependencies map[string]Dependency  `toml:"dev-dependencies"`
	Image           ImageConfig            `toml:"image"`
	GoWrap          GoWrapConfig           `toml:"go-wrap"`
	Sync            SyncConfig             `toml:"sync"`
	Trust           TrustConfig            `toml:"trust"`
	Test            TestConfig             `toml:"test"`
	Scripts         ScriptsConfig          `toml:"scripts"`
	Targets         []TargetConfig         `toml:"target"`
	Runtime         RuntimeConfig          `toml:"runtime"`

	// Dir is the directory containing the maggie.toml file (set at load time).
	Dir string `toml:"-"`
}

// Project contains project metadata.
type Project struct {
	Name        string   `toml:"name"`
	Namespace   string   `toml:"namespace"`
	Version     string   `toml:"version"`
	Description string   `toml:"description"`
	License     string   `toml:"license"`
	Authors     []string `toml:"authors"`
	Repository  string   `toml:"repository"`
	Maggie      string   `toml:"maggie"` // version constraint, e.g. ">=0.9.0"
}

// Source configures source file locations.
type Source struct {
	Dirs    []string `toml:"dirs"`
	Entry   string   `toml:"entry"`
	Exclude []string `toml:"exclude"` // glob patterns to exclude from compilation
}

// Dependency represents a single project dependency.
type Dependency struct {
	Git       string `toml:"git"`
	Tag       string `toml:"tag"`
	Branch    string `toml:"branch"`
	Commit    string `toml:"commit"`
	Path      string `toml:"path"`
	Namespace string `toml:"namespace"`
}

// ImageConfig configures image output.
type ImageConfig struct {
	Output        string `toml:"output"`
	IncludeSource bool   `toml:"include-source"`
}

// GoWrapConfig configures Go package wrapping.
type GoWrapConfig struct {
	Output   string         `toml:"output"`
	Packages []GoWrapPackage `toml:"packages"`
}

// GoWrapPackage describes a single Go package to wrap.
type GoWrapPackage struct {
	Import  string   `toml:"import"`
	Include []string `toml:"include"`
}

// SyncConfig configures the content distribution protocol.
type SyncConfig struct {
	Capabilities []string `toml:"capabilities"` // e.g., ["File", "HTTP"]
	Listen       string   `toml:"listen"`       // e.g., ":8081"
	Peers        []string `toml:"peers"`        // e.g., ["localhost:8082"]
}

// TrustConfig configures the node trust model.
type TrustConfig struct {
	Default           string      `toml:"default"`            // default perms for unknown peers
	BanThreshold      int         `toml:"ban-threshold"`      // hash mismatches before auto-ban
	SpawnRestrictions []string    `toml:"spawn-restrictions"` // globals hidden from remote spawns
	Peers             []TrustPeer `toml:"peer"`               // explicitly configured peers
}

// TrustPeer is an explicitly configured peer with permissions.
type TrustPeer struct {
	ID    string `toml:"id"`    // Ed25519 public key, hex-encoded
	Name  string `toml:"name"`  // human-readable label
	Perms string `toml:"perms"` // comma-separated: sync, message, spawn, all
}

// TestConfig configures test execution.
type TestConfig struct {
	Dirs    []string `toml:"dirs"`    // test source directories, e.g. ["test"]
	Entry   string   `toml:"entry"`   // test runner entry point
	Timeout int      `toml:"timeout"` // timeout in milliseconds, 0 = no timeout
	Exclude []string `toml:"exclude"` // glob patterns to exclude from test sources
}

// ScriptsConfig configures lifecycle script hooks.
type ScriptsConfig struct {
	Prebuild  string `toml:"prebuild"`
	Postbuild string `toml:"postbuild"`
	Pretest   string `toml:"pretest"`
	Posttest  string `toml:"posttest"`
}

// TargetConfig represents a named build target.
type TargetConfig struct {
	Name        string       `toml:"name"`
	Entry       string       `toml:"entry"`
	Output      string       `toml:"output"`
	Full        bool         `toml:"full"`
	ExtraDirs   []string     `toml:"extra-dirs"`
	ExcludeDirs []string     `toml:"exclude-dirs"`
	Exclude     []string     `toml:"exclude"`
	GoWrap      GoWrapConfig `toml:"go-wrap"`
	Image       ImageConfig  `toml:"image"`
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

	if err := m.Validate(version.Version); err != nil {
		return nil, fmt.Errorf("%s: %w", path, err)
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

// AllDependencies returns dependencies + dev-dependencies merged.
// Returns error if a name appears in both sections.
func (m *Manifest) AllDependencies() (map[string]Dependency, error) {
	merged := make(map[string]Dependency, len(m.Dependencies)+len(m.DevDependencies))
	for k, v := range m.Dependencies {
		merged[k] = v
	}
	for k, v := range m.DevDependencies {
		if _, exists := merged[k]; exists {
			return nil, fmt.Errorf("dependency %q appears in both [dependencies] and [dev-dependencies]", k)
		}
		merged[k] = v
	}
	return merged, nil
}

// TestDirPaths returns absolute paths for the configured test directories.
func (m *Manifest) TestDirPaths() []string {
	var paths []string
	for _, d := range m.Test.Dirs {
		paths = append(paths, filepath.Join(m.Dir, d))
	}
	return paths
}

// WrapOutputDir returns the absolute path for generated wrapper output.
func (m *Manifest) WrapOutputDir() string {
	if m.GoWrap.Output != "" {
		return filepath.Join(m.Dir, m.GoWrap.Output)
	}
	return filepath.Join(m.Dir, ".maggie", "wrap")
}
