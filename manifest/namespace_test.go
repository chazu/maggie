package manifest

import "testing"

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
		{"", ""},
		{"already-PascalCase", "AlreadyPascalCase"},
		{"foo-bar-baz", "FooBarBaz"},
		{"_leading", "Leading"},
	}

	for _, tc := range tests {
		got := ToPascalCase(tc.input)
		if got != tc.want {
			t.Errorf("ToPascalCase(%q) = %q, want %q", tc.input, got, tc.want)
		}
	}
}

func TestIsReservedNamespace(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{"Array", true},
		{"Object", true},
		{"String", true},
		{"Channel", true},
		{"Process", true},
		{"Compiler", true},
		{"Error", true},
		{"StackOverflow", true},
		{"MyApp", false},
		{"Yutani", false},
		{"Widgets", false},
		// Multi-segment: only root checked
		{"ThirdParty::Array", false},
		{"Array::Stuff", true},
		{"MyLib::String", false},
	}

	for _, tc := range tests {
		got := IsReservedNamespace(tc.name)
		if got != tc.want {
			t.Errorf("IsReservedNamespace(%q) = %v, want %v", tc.name, got, tc.want)
		}
	}
}
