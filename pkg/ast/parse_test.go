package ast

import (
	"strings"
	"testing"
)

func TestParse(t *testing.T) {
	tests := []struct {
		name      string
		json      string
		wantName  string
		wantError bool
	}{
		{
			name:     "simple class",
			json:     `{"type": "class", "name": "Counter", "parent": "Object"}`,
			wantName: "Counter",
		},
		{
			name:     "class with package",
			json:     `{"type": "class", "name": "Counter", "parent": "Object", "package": "MyApp"}`,
			wantName: "Counter",
		},
		{
			name:      "invalid json",
			json:      `{"type": "class", name: invalid}`,
			wantError: true,
		},
		{
			name:      "empty json",
			json:      ``,
			wantError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			class, err := Parse(strings.NewReader(tt.json))
			if (err != nil) != tt.wantError {
				t.Errorf("Parse() error = %v, wantError %v", err, tt.wantError)
				return
			}
			if err == nil && class.Name != tt.wantName {
				t.Errorf("Parse() class.Name = %q, want %q", class.Name, tt.wantName)
			}
		})
	}
}

func TestParseBytes(t *testing.T) {
	tests := []struct {
		name      string
		json      string
		wantName  string
		wantError bool
	}{
		{
			name:     "simple class",
			json:     `{"type": "class", "name": "Counter", "parent": "Object"}`,
			wantName: "Counter",
		},
		{
			name:     "class with methods",
			json:     `{"type": "class", "name": "Counter", "methods": [{"type": "method", "selector": "increment"}]}`,
			wantName: "Counter",
		},
		{
			name:      "invalid json",
			json:      `not json`,
			wantError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			class, err := ParseBytes([]byte(tt.json))
			if (err != nil) != tt.wantError {
				t.Errorf("ParseBytes() error = %v, wantError %v", err, tt.wantError)
				return
			}
			if err == nil && class.Name != tt.wantName {
				t.Errorf("ParseBytes() class.Name = %q, want %q", class.Name, tt.wantName)
			}
		})
	}
}

func TestParseCompilationUnit(t *testing.T) {
	tests := []struct {
		name       string
		json       string
		wantName   string
		wantTraits int
		wantError  bool
	}{
		{
			name:     "legacy format - plain class",
			json:     `{"type": "class", "name": "Counter", "parent": "Object"}`,
			wantName: "Counter",
		},
		{
			name: "compilation unit format",
			json: `{
				"class": {"type": "class", "name": "Counter", "parent": "Object"},
				"traits": {}
			}`,
			wantName: "Counter",
		},
		{
			name: "compilation unit with traits",
			json: `{
				"class": {"type": "class", "name": "Counter", "parent": "Object", "traits": ["Debuggable"]},
				"traits": {
					"Debuggable": {"type": "class", "name": "Debuggable", "isTrait": true, "methods": []}
				}
			}`,
			wantName:   "Counter",
			wantTraits: 1,
		},
		{
			name:      "invalid json",
			json:      `{invalid}`,
			wantError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			unit, err := ParseCompilationUnit([]byte(tt.json))
			if (err != nil) != tt.wantError {
				t.Errorf("ParseCompilationUnit() error = %v, wantError %v", err, tt.wantError)
				return
			}
			if err == nil {
				if unit.Class.Name != tt.wantName {
					t.Errorf("ParseCompilationUnit() class.Name = %q, want %q", unit.Class.Name, tt.wantName)
				}
				if tt.wantTraits > 0 && len(unit.Traits) != tt.wantTraits {
					t.Errorf("ParseCompilationUnit() len(traits) = %d, want %d", len(unit.Traits), tt.wantTraits)
				}
			}
		})
	}
}

func TestMergeTraits(t *testing.T) {
	tests := []struct {
		name        string
		unit        CompilationUnit
		wantMerged  []string
		wantMissing []string
	}{
		{
			name: "no traits to merge",
			unit: CompilationUnit{
				Class: &Class{Name: "Counter"},
			},
			wantMerged:  nil,
			wantMissing: nil,
		},
		{
			name: "merge available trait",
			unit: CompilationUnit{
				Class: &Class{
					Name:   "Counter",
					Traits: []string{"Debuggable"},
				},
				Traits: map[string]*Class{
					"Debuggable": {
						Name:    "Debuggable",
						IsTrait: true,
						Methods: []Method{
							{Selector: "inspect"},
						},
					},
				},
			},
			wantMerged:  []string{"Debuggable"},
			wantMissing: nil,
		},
		{
			name: "missing trait",
			unit: CompilationUnit{
				Class: &Class{
					Name:   "Counter",
					Traits: []string{"Missing"},
				},
				Traits: map[string]*Class{},
			},
			wantMerged:  nil,
			wantMissing: []string{"Missing"},
		},
		{
			name: "nil traits map",
			unit: CompilationUnit{
				Class: &Class{
					Name:   "Counter",
					Traits: []string{"Debuggable"},
				},
				Traits: nil,
			},
			wantMerged:  nil,
			wantMissing: []string{"Debuggable"},
		},
		{
			name: "mixed available and missing traits",
			unit: CompilationUnit{
				Class: &Class{
					Name:   "Counter",
					Traits: []string{"Debuggable", "Missing"},
				},
				Traits: map[string]*Class{
					"Debuggable": {
						Name:    "Debuggable",
						IsTrait: true,
						Methods: []Method{
							{Selector: "inspect"},
						},
					},
				},
			},
			wantMerged:  []string{"Debuggable"},
			wantMissing: []string{"Missing"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			merged, missing := tt.unit.MergeTraits()

			if !stringSliceEqual(merged, tt.wantMerged) {
				t.Errorf("MergeTraits() merged = %v, want %v", merged, tt.wantMerged)
			}
			if !stringSliceEqual(missing, tt.wantMissing) {
				t.Errorf("MergeTraits() missing = %v, want %v", missing, tt.wantMissing)
			}
		})
	}
}

func TestMergeTraitsMethodCount(t *testing.T) {
	unit := CompilationUnit{
		Class: &Class{
			Name:   "Counter",
			Traits: []string{"Debuggable"},
			Methods: []Method{
				{Selector: "increment"},
			},
		},
		Traits: map[string]*Class{
			"Debuggable": {
				Name:    "Debuggable",
				IsTrait: true,
				Methods: []Method{
					{Selector: "inspect"},
					{Selector: "log"},
				},
			},
		},
	}

	initialMethodCount := len(unit.Class.Methods)
	unit.MergeTraits()

	// Should have original methods plus trait methods
	expectedCount := initialMethodCount + 2
	if len(unit.Class.Methods) != expectedCount {
		t.Errorf("MergeTraits() method count = %d, want %d", len(unit.Class.Methods), expectedCount)
	}
}

func stringSliceEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
}
