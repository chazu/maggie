// Package pipeline provides the Maggie compilation pipeline: collecting, parsing,
// and two-pass compiling of .mag source files into a VM.
package pipeline

import (
	"fmt"
	"io"
	"path/filepath"
	"sort"
	"strings"

	"github.com/chazu/maggie/manifest"
)

// DeriveNamespace computes a namespace from a file's relative directory path.
// Given filePath="/app/src/myapp/models/User.mag" and basePath="/app/src",
// returns "Myapp::Models".
// Returns empty string for files at the root level (no subdirectory).
func DeriveNamespace(filePath, basePath string) string {
	rel, err := filepath.Rel(basePath, filepath.Dir(filePath))
	if err != nil || rel == "." {
		return ""
	}

	segments := strings.Split(rel, string(filepath.Separator))
	var nsSegments []string
	for _, seg := range segments {
		if seg == "" || seg == "." {
			continue
		}
		nsSegments = append(nsSegments, manifest.ToPascalCase(seg))
	}

	if len(nsSegments) == 0 {
		return ""
	}

	return strings.Join(nsSegments, "::")
}

// RemapImport replaces an import's oldPrefix with newPrefix.
// "Yutani" with old="Yutani" new="ThirdParty::Yutani" -> "ThirdParty::Yutani"
// "Yutani::Events" with old="Yutani" new="ThirdParty::Yutani" -> "ThirdParty::Yutani::Events"
// "OtherDep" (no match) -> "OtherDep" (unchanged)
func RemapImport(imp, oldPrefix, newPrefix string) string {
	if imp == oldPrefix {
		return newPrefix
	}
	if strings.HasPrefix(imp, oldPrefix+"::") {
		return newPrefix + imp[len(oldPrefix):]
	}
	return imp
}

// CheckNamespaceCollisions checks that no two dependencies resolve to the same namespace.
// Reports all collisions at once with a suggested fix.
func CheckNamespaceCollisions(deps []manifest.ResolvedDep) error {
	// Map namespace -> list of dep names
	nsMap := make(map[string][]string)
	for _, dep := range deps {
		nsMap[dep.Namespace] = append(nsMap[dep.Namespace], dep.Name)
	}

	// Collect collisions (sorted for deterministic output)
	var collisions []string
	for ns, names := range nsMap {
		if len(names) > 1 {
			sort.Strings(names)
			collisions = append(collisions, fmt.Sprintf("  namespace %q: [%s]", ns, strings.Join(names, ", ")))
		}
	}

	if len(collisions) == 0 {
		return nil
	}

	sort.Strings(collisions)
	return fmt.Errorf("dependency namespace collision(s):\n%s\n\nFix by adding explicit namespace overrides in [dependencies]",
		strings.Join(collisions, "\n"))
}

// PrefixDepNamespaces prefixes all file namespaces with the dependency's resolved namespace.
// Root-level files (no directory-derived namespace) get dep.Namespace only.
// Subdir files get dep.Namespace + "::" + existing namespace.
func PrefixDepNamespaces(files []ParsedFile, dep manifest.ResolvedDep, verbose io.Writer) {
	if dep.Namespace == "" {
		return
	}

	// Determine the dep's "original" namespace (from its own manifest).
	// This is used for import remapping when consumer overrides differ.
	var originalNS string
	if dep.Manifest != nil && dep.Manifest.Project.Namespace != "" {
		originalNS = dep.Manifest.Project.Namespace
	}

	// Check if import remapping is needed
	needsRemap := originalNS != "" && originalNS != dep.Namespace

	for i := range files {
		if files[i].Namespace == "" {
			files[i].Namespace = dep.Namespace
		} else {
			files[i].Namespace = dep.Namespace + "::" + files[i].Namespace
		}

		// Remap imports if consumer override differs from original namespace
		if needsRemap {
			for j, imp := range files[i].Imports {
				remapped := RemapImport(imp, originalNS, dep.Namespace)
				if remapped != imp {
					if verbose != nil {
						fmt.Fprintf(verbose, "  Remapped import %q -> %q in %s\n", imp, remapped, files[i].Path)
					}
					files[i].Imports[j] = remapped
				}
			}
		}
	}
}
