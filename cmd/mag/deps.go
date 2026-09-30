package main

import (
	"fmt"
	"os"
	"sort"

	"github.com/chazu/maggie/manifest"
)

// handleDepsCommand handles the "mag deps" subcommand.
func handleDepsCommand(args []string, verbose bool) {
	if wantsHelp(args) {
		subcmdUsage("deps [command]",
			"Manage project dependencies declared in maggie.toml.",
			usageSubcommands([][2]string{
				{"resolve", "Resolve and fetch all dependencies (default)"},
				{"update", "Re-resolve ignoring the lock file"},
				{"list", "Show the dependency tree"},
			}),
			usageExamples([][2]string{
				{"mag deps", "Resolve and fetch dependencies"},
				{"mag deps update", "Re-resolve from scratch"},
				{"mag deps list", "Show dependency tree"},
			}),
		)
	}

	m, err := manifest.FindAndLoad(".")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
	if m == nil {
		fmt.Fprintf(os.Stderr, "Error: no maggie.toml found\n")
		os.Exit(1)
	}

	subcmd := ""
	if len(args) > 0 {
		subcmd = args[0]
	}

	// Resolve and lock dev-dependencies too: `mag test` loads them, and a
	// lock written from [dependencies] alone (as `update` did, having just
	// deleted the old lock) left them unpinned.
	allDeps, err := m.AllDependencies()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	switch subcmd {
	case "", "resolve":
		resolver := manifest.NewResolver(m, verbose, allDeps)
		deps, err := resolver.Resolve()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error resolving dependencies: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Resolved %d dependencies:\n", len(deps))
		for _, dep := range deps {
			fmt.Printf("  %s -> %s\n", dep.Name, dep.LocalPath)
		}

	case "update":
		lockPath := m.LockFilePath()
		if err := os.Remove(lockPath); err != nil && !os.IsNotExist(err) {
			fmt.Fprintf(os.Stderr, "Error removing lock file: %v\n", err)
			os.Exit(1)
		}

		resolver := manifest.NewResolver(m, verbose, allDeps)
		deps, err := resolver.Resolve()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error resolving dependencies: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Updated %d dependencies:\n", len(deps))
		for _, dep := range deps {
			fmt.Printf("  %s -> %s\n", dep.Name, dep.LocalPath)
		}

	case "list":
		if len(allDeps) == 0 {
			fmt.Println("No dependencies configured.")
			return
		}
		fmt.Printf("Dependencies for %s:\n", m.Project.Name)
		names := make([]string, 0, len(allDeps))
		for name := range allDeps {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			dep := allDeps[name]
			if _, dev := m.DevDependencies[name]; dev {
				name += " (dev)"
			}
			if dep.Git != "" {
				tag := dep.Tag
				if tag == "" && dep.Branch != "" {
					tag = "branch " + dep.Branch
				} else if tag == "" {
					tag = "(latest)"
				}
				fmt.Printf("  %s: %s @ %s\n", name, dep.Git, tag)
			} else if dep.Path != "" {
				fmt.Printf("  %s: path %s\n", name, dep.Path)
			}
		}

	default:
		fmt.Fprintf(os.Stderr, "Unknown deps subcommand: %s\n", subcmd)
		fmt.Fprintf(os.Stderr, "Usage: %s deps [resolve|update|list]\n", progName())
		os.Exit(1)
	}
}
