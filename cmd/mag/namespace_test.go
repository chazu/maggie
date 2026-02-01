package main

import "testing"

func TestDeriveNamespace(t *testing.T) {
	tests := []struct {
		filePath string
		basePath string
		want     string
	}{
		{"/app/src/myapp/models/User.mag", "/app/src", "Myapp::Models"},
		{"/app/src/Main.mag", "/app/src", ""},
		{"/app/src/myapp/Main.mag", "/app/src", "Myapp"},
		{"/app/src/my-lib/core/Parser.mag", "/app/src", "MyLib::Core"},
	}

	for _, tc := range tests {
		got := deriveNamespace(tc.filePath, tc.basePath)
		if got != tc.want {
			t.Errorf("deriveNamespace(%q, %q) = %q, want %q", tc.filePath, tc.basePath, got, tc.want)
		}
	}
}

func TestToPascalCase(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"models", "Models"},
		{"my-app", "MyApp"},
		{"my_app", "MyApp"},
		{"myApp", "MyApp"},
		{"UPPER", "Upper"},
		{"a", "A"},
	}

	for _, tc := range tests {
		got := toPascalCase(tc.input)
		if got != tc.want {
			t.Errorf("toPascalCase(%q) = %q, want %q", tc.input, got, tc.want)
		}
	}
}
