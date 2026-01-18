package ast

import "testing"

func TestClassQualifiedName(t *testing.T) {
	tests := []struct {
		name     string
		class    Class
		expected string
	}{
		{
			name:     "non-namespaced class",
			class:    Class{Name: "Counter"},
			expected: "Counter",
		},
		{
			name:     "namespaced class",
			class:    Class{Name: "Counter", Package: "MyApp"},
			expected: "MyApp::Counter",
		},
		{
			name:     "empty package treated as non-namespaced",
			class:    Class{Name: "Counter", Package: ""},
			expected: "Counter",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.class.QualifiedName()
			if got != tt.expected {
				t.Errorf("QualifiedName() = %q, want %q", got, tt.expected)
			}
		})
	}
}

func TestClassCompiledName(t *testing.T) {
	tests := []struct {
		name     string
		class    Class
		expected string
	}{
		{
			name:     "non-namespaced class",
			class:    Class{Name: "Counter"},
			expected: "Counter",
		},
		{
			name:     "namespaced class uses double underscore",
			class:    Class{Name: "Counter", Package: "MyApp"},
			expected: "MyApp__Counter",
		},
		{
			name:     "empty package treated as non-namespaced",
			class:    Class{Name: "Counter", Package: ""},
			expected: "Counter",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.class.CompiledName()
			if got != tt.expected {
				t.Errorf("CompiledName() = %q, want %q", got, tt.expected)
			}
		})
	}
}

func TestClassIsNamespaced(t *testing.T) {
	tests := []struct {
		name     string
		class    Class
		expected bool
	}{
		{
			name:     "class with package is namespaced",
			class:    Class{Name: "Counter", Package: "MyApp"},
			expected: true,
		},
		{
			name:     "class without package is not namespaced",
			class:    Class{Name: "Counter"},
			expected: false,
		},
		{
			name:     "empty package is not namespaced",
			class:    Class{Name: "Counter", Package: ""},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.class.IsNamespaced()
			if got != tt.expected {
				t.Errorf("IsNamespaced() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestClassHasClassPragma(t *testing.T) {
	tests := []struct {
		name     string
		class    Class
		pragma   string
		expected bool
	}{
		{
			name:     "has pragma",
			class:    Class{ClassPragmas: []string{"primitiveClass", "other"}},
			pragma:   "primitiveClass",
			expected: true,
		},
		{
			name:     "does not have pragma",
			class:    Class{ClassPragmas: []string{"other"}},
			pragma:   "primitiveClass",
			expected: false,
		},
		{
			name:     "no pragmas",
			class:    Class{},
			pragma:   "primitiveClass",
			expected: false,
		},
		{
			name:     "nil pragmas",
			class:    Class{ClassPragmas: nil},
			pragma:   "primitiveClass",
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.class.HasClassPragma(tt.pragma)
			if got != tt.expected {
				t.Errorf("HasClassPragma(%q) = %v, want %v", tt.pragma, got, tt.expected)
			}
		})
	}
}

func TestClassIsPrimitiveClass(t *testing.T) {
	tests := []struct {
		name     string
		class    Class
		expected bool
	}{
		{
			name:     "has primitiveClass pragma",
			class:    Class{ClassPragmas: []string{"primitiveClass"}},
			expected: true,
		},
		{
			name:     "does not have primitiveClass pragma",
			class:    Class{ClassPragmas: []string{"other"}},
			expected: false,
		},
		{
			name:     "no pragmas",
			class:    Class{},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.class.IsPrimitiveClass()
			if got != tt.expected {
				t.Errorf("IsPrimitiveClass() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestClassValidatePrimitiveClass(t *testing.T) {
	tests := []struct {
		name      string
		class     Class
		wantError bool
	}{
		{
			name:      "non-primitive class with traits is valid",
			class:     Class{Name: "Counter", Traits: []string{"Debuggable"}},
			wantError: false,
		},
		{
			name:      "primitive class without traits is valid",
			class:     Class{Name: "Console", ClassPragmas: []string{"primitiveClass"}},
			wantError: false,
		},
		{
			name: "primitive class with traits is invalid",
			class: Class{
				Name:         "Console",
				ClassPragmas: []string{"primitiveClass"},
				Traits:       []string{"Debuggable"},
			},
			wantError: true,
		},
		{
			name: "primitive class with multiple traits is invalid",
			class: Class{
				Name:         "Console",
				ClassPragmas: []string{"primitiveClass"},
				Traits:       []string{"Debuggable", "Persistable"},
			},
			wantError: true,
		},
		{
			name: "primitive class with only raw methods is valid",
			class: Class{
				Name:         "Console",
				ClassPragmas: []string{"primitiveClass"},
				Methods: []Method{
					{Selector: "print_", Raw: true},
					{Selector: "error_", Raw: true},
				},
			},
			wantError: false,
		},
		{
			name: "primitive class with non-raw method is invalid",
			class: Class{
				Name:         "Console",
				ClassPragmas: []string{"primitiveClass"},
				Methods: []Method{
					{Selector: "print_", Raw: true},
					{Selector: "badMethod", Raw: false},
				},
			},
			wantError: true,
		},
		{
			name: "non-primitive class with non-raw methods is valid",
			class: Class{
				Name: "Counter",
				Methods: []Method{
					{Selector: "increment", Raw: false},
				},
			},
			wantError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := tt.class.ValidatePrimitiveClass()
			if (err != nil) != tt.wantError {
				t.Errorf("ValidatePrimitiveClass() error = %v, wantError %v", err, tt.wantError)
			}
		})
	}
}

func TestMethodHasPragma(t *testing.T) {
	tests := []struct {
		name     string
		method   Method
		pragma   string
		expected bool
	}{
		{
			name:     "has pragma",
			method:   Method{Pragmas: []string{"direct", "bashOnly"}},
			pragma:   "direct",
			expected: true,
		},
		{
			name:     "does not have pragma",
			method:   Method{Pragmas: []string{"bashOnly"}},
			pragma:   "direct",
			expected: false,
		},
		{
			name:     "no pragmas",
			method:   Method{},
			pragma:   "direct",
			expected: false,
		},
		{
			name:     "nil pragmas",
			method:   Method{Pragmas: nil},
			pragma:   "direct",
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.method.HasPragma(tt.pragma)
			if got != tt.expected {
				t.Errorf("HasPragma(%q) = %v, want %v", tt.pragma, got, tt.expected)
			}
		})
	}
}

func TestTokenConstants(t *testing.T) {
	// Verify that token constants are unique and non-empty
	tokens := []string{
		TokenNewline, TokenPipe, TokenIdentifier, TokenAssign,
		TokenSubshell, TokenVariable, TokenCaret, TokenNumber,
		TokenPlus, TokenMinus, TokenStar, TokenSlash,
		TokenDString, TokenSString, TokenTripleString, TokenAt,
		TokenColon, TokenLBracket, TokenRBracket,
		TokenGT, TokenLT, TokenGE, TokenLE, TokenEQ, TokenNE,
		TokenEquals, TokenPercent, TokenDot, TokenComma,
		TokenKeyword, TokenLParen, TokenRParen,
		TokenBlockParam, TokenNamespaceSep,
	}

	seen := make(map[string]bool)
	for _, tok := range tokens {
		if tok == "" {
			t.Error("Token constant should not be empty")
		}
		if seen[tok] {
			t.Errorf("Duplicate token constant: %q", tok)
		}
		seen[tok] = true
	}
}
