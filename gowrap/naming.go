package gowrap

import (
	"strings"
	"unicode"
)

// GoPackageToMaggieNamespace converts a Go import path to a Maggie namespace.
// e.g., "encoding/json" → "Go::Json", "net/http" → "Go::Http",
// "strings" → "Go::Strings"
func GoPackageToMaggieNamespace(importPath string) string {
	parts := strings.Split(importPath, "/")
	// Use only the last segment for single-segment stdlib packages
	// For multi-segment, use the last segment capitalized
	last := parts[len(parts)-1]
	return "Go::" + toPascal(last)
}

// GoNameToMaggieSelector converts a Go function/method name to a Maggie selector.
// Go uses PascalCase; Maggie uses camelCase for methods.
// e.g., "ReadAll" → "readAll", "NewDecoder" → "newDecoder"
// For methods with a single parameter, appends ":"
// e.g., "Marshal" with 1 param → "marshal:"
func GoNameToMaggieSelector(name string, paramCount int) string {
	if len(name) == 0 {
		return name
	}
	// Convert first character to lowercase
	sel := strings.ToLower(name[:1]) + name[1:]

	if paramCount == 0 {
		return sel
	}
	if paramCount == 1 {
		return sel + ":"
	}

	// Multi-param: use keyword-style
	// For now, just append the right number of colons
	// e.g., "replace" with 3 params → "replace:with:count:" won't work
	// Without param names in Go, we use positional colons
	return sel + ":" + strings.Repeat("_:", paramCount-1)
}

// GoNameToMaggieClassName converts a Go type name to a Maggie class name
// within a namespace.
// e.g., namespace "Go::Http", type "Server" → "Go::Http::Server"
func GoNameToMaggieClassName(namespace, typeName string) string {
	return namespace + "::" + typeName
}

// toPascal converts a string to PascalCase.
// Handles hyphenated and underscore-separated names.
func toPascal(s string) string {
	if len(s) == 0 {
		return s
	}

	var b strings.Builder
	nextUpper := true
	for _, r := range s {
		if r == '-' || r == '_' {
			nextUpper = true
			continue
		}
		if nextUpper {
			b.WriteRune(unicode.ToUpper(r))
			nextUpper = false
		} else {
			b.WriteRune(r)
		}
	}
	return b.String()
}
