package gowrap

import "testing"

func TestGoPackageToMaggieNamespace(t *testing.T) {
	tests := []struct {
		importPath string
		expected   string
	}{
		{"strings", "Go::Strings"},
		{"encoding/json", "Go::Json"},
		{"net/http", "Go::Http"},
		{"crypto/tls", "Go::Tls"},
		{"io", "Go::Io"},
		{"net/http/httptest", "Go::Httptest"},
	}
	for _, tt := range tests {
		t.Run(tt.importPath, func(t *testing.T) {
			got := GoPackageToMaggieNamespace(tt.importPath)
			if got != tt.expected {
				t.Errorf("GoPackageToMaggieNamespace(%q) = %q, want %q", tt.importPath, got, tt.expected)
			}
		})
	}
}

func TestGoNameToMaggieSelector(t *testing.T) {
	tests := []struct {
		name       string
		paramCount int
		expected   string
	}{
		{"Contains", 0, "contains"},
		{"ReadAll", 0, "readAll"},
		{"NewDecoder", 1, "newDecoder:"},
		{"Replace", 0, "replace"},
		{"HasPrefix", 0, "hasPrefix"},
		{"Write", 1, "write:"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := GoNameToMaggieSelector(tt.name, tt.paramCount)
			if got != tt.expected {
				t.Errorf("GoNameToMaggieSelector(%q, %d) = %q, want %q", tt.name, tt.paramCount, got, tt.expected)
			}
		})
	}
}

func TestGoNameToMaggieClassName(t *testing.T) {
	tests := []struct {
		namespace string
		typeName  string
		expected  string
	}{
		{"Go::Http", "Server", "Go::Http::Server"},
		{"Go::Json", "Decoder", "Go::Json::Decoder"},
	}
	for _, tt := range tests {
		t.Run(tt.expected, func(t *testing.T) {
			got := GoNameToMaggieClassName(tt.namespace, tt.typeName)
			if got != tt.expected {
				t.Errorf("got %q, want %q", got, tt.expected)
			}
		})
	}
}

func TestToPascal(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"json", "Json"},
		{"http-server", "HttpServer"},
		{"my_lib", "MyLib"},
		{"strings", "Strings"},
		{"", ""},
	}
	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			got := toPascal(tt.input)
			if got != tt.expected {
				t.Errorf("toPascal(%q) = %q, want %q", tt.input, got, tt.expected)
			}
		})
	}
}
