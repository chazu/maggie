package vm

import "testing"

func TestSourceMayMutateSchema(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   bool
	}{
		// Pure reads / arithmetic — must stay concurrent.
		{"arithmetic", "1 + 2 * 3", false},
		{"unary send", "myList size", false},
		{"keyword send read", "dict at: #key", false},
		{"string literal", "'hello world'", false},
		{"getGlobal is read-only", "Compiler getGlobal: #Foo", false},

		// Structural mutation — must upgrade to exclusive.
		{"compiler evaluate", "Compiler evaluate: 'Foo subclass: Object'", true},
		{"evaluate in", "Compiler evaluate: 'x' in: ctx", true},
		{"evaluate withLocals", "Compiler evaluate: 'x + y' withLocals: d", true},
		{"setGlobal", "Compiler setGlobal: #X to: 42", true},
		{"compileMethod", "Compiler compileMethod: src", true},
		{"compileAndInstall", "MyClass compileAndInstall: 'foo ^1'", true},
		{"compileAndInstallClassMethod", "MyClass compileAndInstallClassMethod: 'bar ^2'", true},
		{"fileIn", "Compiler fileIn: 'app.mag'", true},
		{"fileInAll", "Compiler fileInAll: 'src'", true},
		{"saveImage", "Compiler saveImage: 'out.image'", true},

		// Boundary: a user selector that merely *contains* a mutating token as a
		// suffix must NOT match (the preceding char is an identifier char).
		{"reevaluate not a match", "widget reevaluate: 3", false},
		{"xsetGlobal not a match", "obj xsetGlobal: #a to: 1", false},

		// A mutating token hidden inside a block still upgrades (textual match).
		{"mutation inside block", "[:x | Compiler setGlobal: #N to: x] value: 5", true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := SourceMayMutateSchema(tc.source); got != tc.want {
				t.Errorf("SourceMayMutateSchema(%q) = %v, want %v", tc.source, got, tc.want)
			}
		})
	}
}
