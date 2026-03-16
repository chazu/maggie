package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// progName returns the binary name for use in usage strings.
func progName() string {
	return filepath.Base(os.Args[0])
}

// wantsHelp returns true if the first arg is a help request.
func wantsHelp(args []string) bool {
	if len(args) == 0 {
		return false
	}
	switch args[0] {
	case "help", "-h", "--help":
		return true
	}
	return false
}

// subcmdUsage prints cobra-style usage for a subcommand and exits.
func subcmdUsage(name, short string, sections ...string) {
	prog := progName()
	fmt.Fprintf(os.Stderr, "%s\n\n", short)
	fmt.Fprintf(os.Stderr, "Usage:\n  %s %s\n", prog, name)
	for _, s := range sections {
		// Replace "mag " with actual binary name in section text
		fmt.Fprint(os.Stderr, strings.ReplaceAll(s, "mag ", prog+" "))
	}
	fmt.Fprintln(os.Stderr)
	os.Exit(0)
}

// usageAliases formats an "Aliases:" section.
func usageAliases(aliases string) string {
	return fmt.Sprintf("\nAliases:\n  %s\n", aliases)
}

// usageFlags formats a "Flags:" section from name/description pairs.
func usageFlags(flags [][2]string) string {
	s := "\nFlags:\n"
	for _, f := range flags {
		s += fmt.Sprintf("  %-28s %s\n", f[0], f[1])
	}
	return s
}

// usageExamples formats an "Examples:" section.
func usageExamples(examples [][2]string) string {
	prog := progName()
	s := "\nExamples:\n"
	for _, e := range examples {
		line := strings.Replace(e[0], "mag ", prog+" ", 1)
		if e[1] != "" {
			s += fmt.Sprintf("  %s  # %s\n", line, e[1])
		} else {
			s += fmt.Sprintf("  %s\n", line)
		}
	}
	return s
}

// usageSubcommands formats an "Available Commands:" section.
func usageSubcommands(cmds [][2]string) string {
	s := "\nAvailable Commands:\n"
	for _, c := range cmds {
		s += fmt.Sprintf("  %-28s %s\n", c[0], c[1])
	}
	return s
}
