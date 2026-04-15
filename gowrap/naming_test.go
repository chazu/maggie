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
		paramNames []string
		expected   string
	}{
		{"Contains", nil, "contains"},
		{"ReadAll", nil, "readAll"},
		{"NewDecoder", []string{"r"}, "newDecoder:"},
		{"Replace", nil, "replace"},
		{"HasPrefix", nil, "hasPrefix"},
		{"Write", []string{"p"}, "write:"},
		// Multi-param: Go param names become selector keywords
		{"Contains", []string{"s", "substr"}, "contains:substr:"},
		{"HasPrefix", []string{"s", "prefix"}, "hasPrefix:prefix:"},
		{"Replace", []string{"s", "old", "new", "n"}, "replace:old:new:n:"},
		// 5+ params work (no limit)
		{"SomeFunc", []string{"a", "b", "c", "d", "e"}, "someFunc:b:c:d:e:"},
		{"BigFunc", []string{"x", "y", "z", "w", "v", "u"}, "bigFunc:y:z:w:v:u:"},
		// Unnamed/underscore params get positional fallback
		{"Foo", []string{"x", "", "_"}, "foo:p1:p2:"},
	}
	for _, tt := range tests {
		label := tt.name
		if len(tt.paramNames) > 0 {
			label += "_" + tt.expected
		}
		t.Run(label, func(t *testing.T) {
			got := GoNameToMaggieSelector(tt.name, tt.paramNames)
			if got != tt.expected {
				t.Errorf("GoNameToMaggieSelector(%q, %v) = %q, want %q", tt.name, tt.paramNames, got, tt.expected)
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
