//go:build tools

// Package tools tracks Go build/runtime dependencies that are not yet
// referenced by the main module source tree. Keeping a blank import here
// prevents `go mod tidy` from removing them.
//
// Currently pinning:
//   - github.com/spf13/cobra: will back the forthcoming `Cli` library
//     (see docs/research/2026-04-16-maggie-cli-framework-scout.md).
package tools

import (
	_ "github.com/spf13/cobra"
)
