package ast

import (
	"encoding/json"
	"fmt"
	"io"
)

// Parse reads AST JSON from a reader and returns a Class.
func Parse(r io.Reader) (*Class, error) {
	var class Class
	decoder := json.NewDecoder(r)
	if err := decoder.Decode(&class); err != nil {
		return nil, fmt.Errorf("failed to parse AST: %w", err)
	}
	return &class, nil
}

// ParseBytes parses AST JSON from a byte slice.
func ParseBytes(data []byte) (*Class, error) {
	var class Class
	if err := json.Unmarshal(data, &class); err != nil {
		return nil, fmt.Errorf("failed to parse AST: %w", err)
	}
	return &class, nil
}

// ParseCompilationUnit parses either a CompilationUnit or plain Class JSON.
// It auto-detects the format based on the presence of a "class" key.
func ParseCompilationUnit(data []byte) (*CompilationUnit, error) {
	// First, try to detect the format by checking for "class" key
	var probe struct {
		Class *Class `json:"class"`
		Type  string `json:"type"`
	}
	if err := json.Unmarshal(data, &probe); err != nil {
		return nil, fmt.Errorf("failed to parse input: %w", err)
	}

	// If "class" key exists, it's a CompilationUnit
	if probe.Class != nil {
		var unit CompilationUnit
		if err := json.Unmarshal(data, &unit); err != nil {
			return nil, fmt.Errorf("failed to parse compilation unit: %w", err)
		}
		return &unit, nil
	}

	// Otherwise it's a plain Class (legacy format)
	var class Class
	if err := json.Unmarshal(data, &class); err != nil {
		return nil, fmt.Errorf("failed to parse class: %w", err)
	}
	return &CompilationUnit{
		Class:  &class,
		Traits: nil,
	}, nil
}

// MergeTraits merges included trait methods into the class.
// Returns lists of merged trait names and missing trait names.
func (cu *CompilationUnit) MergeTraits() (merged []string, missing []string) {
	if len(cu.Class.Traits) == 0 {
		return nil, nil
	}

	for _, traitName := range cu.Class.Traits {
		if cu.Traits == nil {
			missing = append(missing, traitName)
			continue
		}

		trait, ok := cu.Traits[traitName]
		if !ok {
			// Trait not provided, will fall back to Bash
			missing = append(missing, traitName)
			continue
		}

		// Add trait methods to the class
		for _, m := range trait.Methods {
			cu.Class.Methods = append(cu.Class.Methods, m)
		}
		merged = append(merged, traitName)
	}
	return merged, missing
}
