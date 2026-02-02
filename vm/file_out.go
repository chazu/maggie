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
		fmt.Fprintf(&sb, "namespace: '%s'\n\n", class.Namespace)
	}

	// Write class docstring
	if class.DocString != "" {
		fmt.Fprintf(&sb, "\"\"\"\n%s\n\"\"\"\n", class.DocString)
	}

	// Write class header
	superName := "Object"
	if class.Superclass != nil {
		superName = class.Superclass.Name
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
			// Use preserved source text
			fmt.Fprintf(&sb, "  %s\n", cm.Source)
		} else {
			// Generate a stub with just the selector
			fmt.Fprintf(&sb, "  method: %s [ \"source not available\" ]\n", selectorName)
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
			// Use preserved source text (should already include "classMethod:" prefix)
			fmt.Fprintf(&sb, "  %s\n", cm.Source)
		} else {
			fmt.Fprintf(&sb, "  classMethod: %s [ \"source not available\" ]\n", selectorName)
		}
	}

	return sb.String()
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
