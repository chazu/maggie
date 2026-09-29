package vm

import (
	"fmt"
	"strings"
)

// FileOutClass generates .mag source text for a single class.
// It reconstructs the source from runtime class metadata and stored method source.
func FileOutClass(class *Class, selectors *SelectorTable) string {
	var sb strings.Builder

	// Write namespace declaration if present
	if class.Namespace != "" {
		fmt.Fprintf(&sb, "namespace: '%s'\n", class.Namespace)
		// Note: import declarations are not preserved in fileOut.
		// Classes store their namespace but not per-file import lists.
		// After fileOut, you may need to re-add import: declarations manually.
		fmt.Fprintf(&sb, "\"import declarations are not preserved by fileOut\"\n\n")
	}

	// Write class docstring
	if class.DocString != "" {
		fmt.Fprintf(&sb, "\"\"\"\n%s\n\"\"\"\n", class.DocString)
	}

	// Write class header
	superName := "Object"
	if class.Superclass != nil {
		superName = class.Superclass.Name
		// A superclass in another namespace would not resolve by short name.
		if class.Superclass.Namespace != class.Namespace {
			superName = class.Superclass.FullName()
		}
	}
	fmt.Fprintf(&sb, "%s subclass: %s\n", class.Name, superName)

	// Write instance variables
	if len(class.InstVars) > 0 {
		fmt.Fprintf(&sb, "  instanceVars: %s\n", strings.Join(class.InstVars, " "))
	}

	// Write instance methods
	localMethods := class.VTable.LocalMethods()
	for selectorID, method := range localMethods {
		cm, ok := method.(*CompiledMethod)
		if !ok {
			continue
		}

		selectorName := selectors.Name(selectorID)
		if selectorName == "" {
			continue
		}

		if cm.DocString() != "" {
			fmt.Fprintf(&sb, "  \"\"\"\n  %s\n  \"\"\"\n", cm.DocString())
		}
		if cm.Source != "" {
			fmt.Fprintf(&sb, "  %s\n", fileOutMethodSource(cm.Source, selectorName, false))
		} else {
			// Generate a stub with just the selector
			fmt.Fprintf(&sb, "  method: %s [ \"source not available\" ]\n", stubHeader(selectorName))
		}
	}

	// Write class methods
	classLocalMethods := class.ClassVTable.LocalMethods()
	for selectorID, method := range classLocalMethods {
		cm, ok := method.(*CompiledMethod)
		if !ok {
			continue
		}

		selectorName := selectors.Name(selectorID)
		if selectorName == "" {
			continue
		}

		if cm.DocString() != "" {
			fmt.Fprintf(&sb, "  \"\"\"\n  %s\n  \"\"\"\n", cm.DocString())
		}
		if cm.Source != "" {
			fmt.Fprintf(&sb, "  %s\n", fileOutMethodSource(cm.Source, selectorName, true))
		} else {
			fmt.Fprintf(&sb, "  classMethod: %s [ \"source not available\" ]\n", stubHeader(selectorName))
		}
	}

	return sb.String()
}

// fileOutMethodSource renders stored method source as a class-body member
// that fileIn can read back. Sources from .mag files are already
// "method: sel [ body ]" (class-side ones may lack the classMethod: marker when
// written as "class method:"); sources installed via compileAndInstall: are the
// bare "pattern body" form and need the prefix and brackets added.
func fileOutMethodSource(source, selector string, classSide bool) string {
	prefix := "method:"
	if classSide {
		prefix = "classMethod:"
	}
	src := strings.TrimSpace(source)
	for _, p := range []string{"classMethod:", "method:"} {
		if strings.HasPrefix(src, p) {
			return prefix + src[len(p):]
		}
	}
	end := bareHeaderEnd(src, selector)
	header := strings.TrimSpace(src[:end])
	body := strings.TrimSpace(src[end:])
	return prefix + " " + header + " [\n    " + body + "\n  ]"
}

// bareHeaderEnd returns the offset where the message pattern of a bare method
// source ("at: i put: v  ^i") ends: one word for a unary selector, operator
// and argument for a binary one, keyword/argument pairs for a keyword one,
// plus <Type> annotations after arguments and a trailing ^<Type> / ! <Effect>.
func bareHeaderEnd(src, selector string) int {
	words := 1
	if n := strings.Count(selector, ":"); n > 0 {
		words = 2 * n
	} else if selector != "" && !isIdentStart(selector[0]) {
		words = 2
	}
	isSpace := func(c byte) bool { return c == ' ' || c == '\t' || c == '\n' || c == '\r' }
	skipAngle := func(j int) int {
		depth := 0
		for ; j < len(src); j++ {
			if src[j] == '<' {
				depth++
			} else if src[j] == '>' {
				if depth--; depth == 0 {
					return j + 1
				}
			}
		}
		return j
	}
	i, seen := 0, 0
	for {
		j := i
		for j < len(src) && isSpace(src[j]) {
			j++
		}
		if j >= len(src) {
			return j
		}
		afterArg := seen > 1 && seen%2 == 0
		switch {
		case afterArg && src[j] == '<':
			i = skipAngle(j)
			continue
		case seen == words && strings.HasPrefix(src[j:], "^<"):
			i = skipAngle(j + 1)
			continue
		case seen == words && src[j] == '!':
			i = j + 1
			continue
		case seen == words:
			return i
		}
		for j < len(src) && !isSpace(src[j]) && (seen == 0 || src[j] != '<') {
			j++
		}
		seen++
		i = j
	}
}

func isIdentStart(c byte) bool {
	return c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

// stubHeader builds a method pattern for a selector with no source,
// inventing argument names ("at:put:" -> "at: arg1 put: arg2").
func stubHeader(selector string) string {
	if !strings.Contains(selector, ":") {
		if selector != "" && !isIdentStart(selector[0]) {
			return selector + " arg1" // binary
		}
		return selector
	}
	var parts []string
	for i, kw := range strings.SplitAfter(selector, ":") {
		if kw != "" {
			parts = append(parts, fmt.Sprintf("%s arg%d", kw, i+1))
		}
	}
	return strings.Join(parts, " ")
}

// FileOutNamespace generates .mag source text for all classes in a namespace.
// Returns a map of class name -> source text.
func FileOutNamespace(namespace string, classes *ClassTable, selectors *SelectorTable) map[string]string {
	result := make(map[string]string)

	for _, class := range classes.All() {
		if class.Namespace == namespace {
			result[class.Name] = FileOutClass(class, selectors)
		}
	}

	return result
}
