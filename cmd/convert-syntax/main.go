// Conversion tool for Maggie syntax
// Converts old method-based .mag files to new Trashtalk-style class definitions.
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// superclassMapping maps class names to their superclass
var superclassMapping = map[string]string{
	"Object":          "nil",
	"Boolean":         "Object",
	"True":            "Boolean",
	"False":           "Boolean",
	"UndefinedObject": "Object",
	"SmallInteger":    "Object",
	"Float":           "Object",
	"String":          "Object",
	"Symbol":          "Object",
	"Array":           "Object",
	"Block":           "Object",
	"Channel":         "Object",
	"Process":         "Object",
	"Result":          "Object",
	"Success":         "Result",
	"Failure":         "Result",
	"Dictionary":      "Object",
}

func main() {
	srcDir := flag.String("src", "lib", "Source directory with old-format .mag files")
	dstDir := flag.String("dst", "lib-new", "Destination directory for new-format files")
	flag.Parse()

	files, err := filepath.Glob(filepath.Join(*srcDir, "*.mag"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error finding files: %v\n", err)
		os.Exit(1)
	}

	// Ensure destination directory exists
	if err := os.MkdirAll(*dstDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating directory: %v\n", err)
		os.Exit(1)
	}

	for _, file := range files {
		className := strings.TrimSuffix(filepath.Base(file), ".mag")

		content, err := os.ReadFile(file)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error reading %s: %v\n", file, err)
			continue
		}

		newContent := convertFile(className, string(content))

		dstFile := filepath.Join(*dstDir, filepath.Base(file))
		if err := os.WriteFile(dstFile, []byte(newContent), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "Error writing %s: %v\n", dstFile, err)
			continue
		}

		fmt.Printf("Converted %s\n", className)
	}
}

func convertFile(className, content string) string {
	var result strings.Builder

	// Parse methods from old format
	methods := parseOldFormat(content)

	// Get superclass
	superclass := superclassMapping[className]
	if superclass == "" {
		superclass = "Object"
	}

	// Write header comment (convert -- to #)
	result.WriteString(fmt.Sprintf("# %s.mag\n\n", className))

	// Write class definition
	result.WriteString(fmt.Sprintf("%s subclass: %s\n", className, superclass))

	// Convert each method
	for _, method := range methods {
		result.WriteString("\n")
		result.WriteString(convertMethod(method))
	}

	return result.String()
}

type oldMethod struct {
	signature string
	body      []string
	comment   string // preceding comment
}

func parseOldFormat(content string) []oldMethod {
	var methods []oldMethod
	var current *oldMethod
	var pendingComment string

	lines := strings.Split(content, "\n")
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Skip empty lines
		if trimmed == "" {
			continue
		}

		// Handle comments
		if strings.HasPrefix(trimmed, "--") {
			// Convert to # comment
			pendingComment = strings.TrimPrefix(trimmed, "--")
			pendingComment = strings.TrimSpace(pendingComment)
			continue
		}

		// Check if this line starts a new method (unindented)
		isMethodHeader := len(line) > 0 && line[0] != ' ' && line[0] != '\t'

		if isMethodHeader {
			// Save previous method
			if current != nil {
				methods = append(methods, *current)
			}
			current = &oldMethod{
				signature: trimmed,
				comment:   pendingComment,
			}
			pendingComment = ""
		} else if current != nil {
			// Add body line, preserving indentation relative to 4 spaces
			current.body = append(current.body, line)
		}
	}

	// Don't forget last method
	if current != nil {
		methods = append(methods, *current)
	}

	return methods
}

func convertMethod(m oldMethod) string {
	var result strings.Builder

	// Write comment if present
	if m.comment != "" {
		result.WriteString(fmt.Sprintf("  # %s\n", m.comment))
	}

	// Write method header
	result.WriteString(fmt.Sprintf("  method: %s [\n", m.signature))

	// Write body lines with adjusted indentation
	for _, line := range m.body {
		if strings.TrimSpace(line) == "" {
			result.WriteString("\n")
		} else {
			// Keep relative indentation but add 2 more spaces for being inside the method
			result.WriteString(fmt.Sprintf("  %s\n", line))
		}
	}

	result.WriteString("  ]\n")

	return result.String()
}
