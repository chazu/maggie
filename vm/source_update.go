package vm

import (
	"fmt"
	"os"
	"strings"
)

// UpdateMethodInFile replaces a single method definition in a .mag source file.
// It finds the method by matching "method: <selector>" or "classMethod: <selector>"
// and replaces the entire method block (including leading docstring) with newSource.
// Returns nil on success or an error.
func UpdateMethodInFile(filePath string, selector string, newSource string, isClassMethod bool) error {
	data, err := os.ReadFile(filePath)
	if err != nil {
		return fmt.Errorf("cannot read file: %w", err)
	}
	content := string(data)

	// Normalize selector for matching (strip trailing whitespace)
	selector = strings.TrimSpace(selector)

	// Build the pattern to find: "method: <selector>" or "classMethod: <selector>"
	prefix := "method:"
	if isClassMethod {
		prefix = "classMethod:"
	}

	// Find the method definition in the source
	start, end, found := findMethodRange(content, prefix, selector)
	if !found {
		return fmt.Errorf("method %s %s not found in %s", prefix, selector, filePath)
	}

	// Format replacement: each line gets 2-space indent to match .mag convention
	newSource = strings.TrimSpace(newSource)
	lines := strings.Split(newSource, "\n")
	var sb strings.Builder
	sb.WriteString("\n")
	for _, line := range lines {
		sb.WriteString("  ")
		sb.WriteString(line)
		sb.WriteString("\n")
	}

	// Replace the method range
	replacement := sb.String()
	result := content[:start] + replacement + content[end:]

	err = os.WriteFile(filePath, []byte(result), 0644)
	if err != nil {
		return fmt.Errorf("cannot write file: %w", err)
	}
	return nil
}

// findMethodRange locates a method definition in source text.
// Returns (start, end, found) where start..end is the range to replace,
// including any leading docstring.
func findMethodRange(content string, prefix string, selector string) (int, int, bool) {
	// Extract just the selector name (without parameters) for matching.
	// "foo" matches "method: foo ["
	// "bar:" matches "method: bar: arg ["
	// "baz:quux:" matches "method: baz: a quux: b ["
	selectorParts := extractSelectorParts(selector)

	lines := strings.Split(content, "\n")
	i := 0
	for i < len(lines) {
		line := strings.TrimSpace(lines[i])

		// Check if this line starts the method we're looking for
		if matchesMethodStart(line, prefix, selectorParts) {
			// Found the method start. Now find the range including leading docstring.
			methodStartLine := i

			// Walk backwards to include leading docstring/comments
			docStart := findDocstringStart(lines, methodStartLine)

			// Calculate byte offset for the start of the range
			startOffset := lineOffset(content, docStart)

			// Find the end of the method body by tracking bracket nesting
			endOffset := findMethodEnd(content, lineOffset(content, methodStartLine))

			return startOffset, endOffset, true
		}
		i++
	}
	return 0, 0, false
}

// extractSelectorParts splits a selector into its keyword parts.
// "foo" -> ["foo"]
// "bar:" -> ["bar:"]
// "baz:quux:" -> ["baz:", "quux:"]
// "at:put:" -> ["at:", "put:"]
func extractSelectorParts(selector string) []string {
	if !strings.Contains(selector, ":") {
		return []string{selector}
	}
	var parts []string
	for _, part := range strings.Split(selector, ":") {
		part = strings.TrimSpace(part)
		if part != "" {
			parts = append(parts, part+":")
		}
	}
	return parts
}

// matchesMethodStart checks if a line matches "method: <selector> ..." or
// "classMethod: <selector> ...". For keyword selectors, the parts may be
// interspersed with parameter names.
func matchesMethodStart(line string, prefix string, selectorParts []string) bool {
	if !strings.HasPrefix(line, prefix) {
		return false
	}
	rest := strings.TrimSpace(line[len(prefix):])

	if len(selectorParts) == 1 && !strings.HasSuffix(selectorParts[0], ":") {
		// Unary selector: "foo" should match "foo [" or "foo["
		word := selectorParts[0]
		return strings.HasPrefix(rest, word) &&
			(len(rest) == len(word) ||
				rest[len(word)] == ' ' ||
				rest[len(word)] == '[' ||
				rest[len(word)] == '\t')
	}

	// Keyword selector: check each part appears in order
	pos := 0
	for _, part := range selectorParts {
		idx := strings.Index(rest[pos:], part)
		if idx < 0 {
			return false
		}
		pos += idx + len(part)
	}
	return true
}

// findDocstringStart walks backwards from methodLine to include leading
// docstrings (""" blocks or " comments) and blank lines.
func findDocstringStart(lines []string, methodLine int) int {
	i := methodLine - 1

	// Skip blank lines immediately before
	for i >= 0 && strings.TrimSpace(lines[i]) == "" {
		i--
	}

	// Check for docstring ending here
	if i >= 0 {
		trimmed := strings.TrimSpace(lines[i])
		// Triple-quoted docstring ending with """
		if strings.HasSuffix(trimmed, `"""`) {
			// Find the start of the triple-quoted docstring
			for i >= 0 {
				if strings.Contains(strings.TrimSpace(lines[i]), `"""`) {
					// Check if this line starts the docstring (has opening """)
					line := strings.TrimSpace(lines[i])
					if strings.HasPrefix(line, `"""`) {
						return i
					}
					// If only closing, keep looking
					if i > 0 {
						i--
						continue
					}
				}
				if i == 0 {
					break
				}
				// Check if previous line has opening """
				prevTrimmed := strings.TrimSpace(lines[i])
				if strings.HasPrefix(prevTrimmed, `"""`) {
					return i
				}
				i--
			}
			return i
		}
		// Single-line comment "..."
		if strings.HasPrefix(trimmed, `"`) && strings.HasSuffix(trimmed, `"`) && len(trimmed) > 1 {
			return i
		}
	}

	return methodLine
}

// lineOffset returns the byte offset of the start of line n (0-indexed).
func lineOffset(content string, lineNum int) int {
	offset := 0
	for i := 0; i < lineNum; i++ {
		idx := strings.Index(content[offset:], "\n")
		if idx < 0 {
			return len(content)
		}
		offset += idx + 1
	}
	return offset
}

// findMethodEnd finds the end of a method body starting from offset.
// Tracks bracket nesting to handle nested blocks.
func findMethodEnd(content string, offset int) int {
	// Find the opening bracket
	i := offset
	for i < len(content) && content[i] != '[' {
		i++
	}
	if i >= len(content) {
		return len(content)
	}

	// Track bracket nesting
	depth := 0
	inString := false
	inComment := false
	for i < len(content) {
		ch := content[i]
		if inString {
			if ch == '\'' {
				// Check for escaped quote ''
				if i+1 < len(content) && content[i+1] == '\'' {
					i += 2
					continue
				}
				inString = false
			}
			i++
			continue
		}
		if inComment {
			if ch == '"' {
				inComment = false
			}
			i++
			continue
		}
		switch ch {
		case '\'':
			inString = true
		case '"':
			inComment = true
		case '[':
			depth++
		case ']':
			depth--
			if depth == 0 {
				// Found the matching close bracket
				i++
				// Skip trailing whitespace/newline
				for i < len(content) && (content[i] == ' ' || content[i] == '\t') {
					i++
				}
				if i < len(content) && content[i] == '\n' {
					i++
				}
				return i
			}
		}
		i++
	}
	return len(content)
}

// indentMethod ensures a method source has consistent 2-space indentation.
func indentMethod(source string) string {
	lines := strings.Split(source, "\n")
	var result []string
	for i, line := range lines {
		if i == 0 {
			result = append(result, line)
		} else {
			// Preserve existing indentation within the method
			result = append(result, "  "+strings.TrimLeft(line, " \t"))
		}
	}
	return strings.Join(result, "\n")
}
