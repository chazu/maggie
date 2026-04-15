package gowrap

import (
	"fmt"
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
// For multi-param functions, parameter names become selector keywords:
// e.g., "Replace" with params (s, old, new, n) → "replace:old:new:n:"
func GoNameToMaggieSelector(name string, paramNames []string) string {
	if len(name) == 0 {
		return name
	}
	// Convert first character to lowercase
	sel := strings.ToLower(name[:1]) + name[1:]

	paramCount := len(paramNames)
	if paramCount == 0 {
		return sel
	}
	if paramCount == 1 {
		return sel + ":"
	}

	// Multi-param: use Go parameter names as keyword parts.
	// First param is implicit (the base selector), remaining become keywords.
	// e.g., Replace(s, old, new, n) → "replace:old:new:n:"
	var b strings.Builder
	b.WriteString(sel)
	b.WriteByte(':')
	for i := 1; i < paramCount; i++ {
		pName := paramNames[i]
		if pName == "" || pName == "_" {
			pName = fmt.Sprintf("p%d", i)
		}
		b.WriteString(lcFirst(pName))
		b.WriteByte(':')
	}
	return b.String()
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
