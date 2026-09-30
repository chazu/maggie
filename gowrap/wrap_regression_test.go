package gowrap

import (
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"

	"github.com/chazu/maggie/compiler"
)

const fixturePkg = "github.com/chazu/maggie/gowrap/testdata/wrapfixture"

func introspectOrFatal(t *testing.T, importPath string, filter map[string]bool) *PackageModel {
	t.Helper()
	model, err := IntrospectPackage(importPath, filter)
	if err != nil {
		t.Fatalf("IntrospectPackage(%s): %v", importPath, err)
	}
	return model
}

// typeCheckGlue type-checks generated wrapper source against the real vm
// package (via a go/packages overlay, nothing is written to disk) and fails
// the test on any compile error.
func typeCheckGlue(t *testing.T, code string) {
	t.Helper()
	dir, err := filepath.Abs(filepath.Join("testdata", "wrapfixture_gen"))
	if err != nil {
		t.Fatal(err)
	}
	cfg := &packages.Config{
		Mode:    packages.NeedName | packages.NeedTypes | packages.NeedSyntax,
		Overlay: map[string][]byte{filepath.Join(dir, "wrap.go"): []byte(code)},
	}
	pkgs, err := packages.Load(cfg, dir)
	if err != nil {
		t.Fatalf("loading generated code: %v", err)
	}
	if len(pkgs) != 1 {
		t.Fatalf("expected 1 package, got %d", len(pkgs))
	}
	for _, e := range pkgs[0].Errors {
		t.Errorf("generated code does not compile: %v", e)
	}
	if t.Failed() {
		t.Logf("generated code:\n%s", code)
	}
}

func TestGeneratedGlueCompiles(t *testing.T) {
	cases := []struct {
		name       string
		importPath string
		filter     map[string]bool
	}{
		{"strings", "strings", map[string]bool{"Contains": true, "HasPrefix": true, "Builder": true}},
		{"fixture", fixturePkg, nil},
		// Every binding skipped: pkg would be an unused import.
		{"only-variadic", fixturePkg, map[string]bool{"Sum": true}},
		// Struct whose methods are all skipped: opaqueClass would be unused.
		{"all-methods-skipped", fixturePkg, map[string]bool{"Opaque": true}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			code, err := GenerateGoGlue(introspectOrFatal(t, tc.importPath, tc.filter))
			if err != nil {
				t.Fatalf("GenerateGoGlue: %v", err)
			}
			typeCheckGlue(t, code)
		})
	}
}

func TestGeneratedGlueFixtureShapes(t *testing.T) {
	model := introspectOrFatal(t, fixturePkg, nil)
	for _, fn := range model.Functions {
		if fn.Name == "Identity" {
			t.Error("generic function Identity should not be introspected")
		}
	}
	for _, tp := range model.Types {
		if tp.Name == "Box" {
			t.Error("generic type Box should not be introspected")
		}
	}

	code, err := GenerateGoGlue(model)
	if err != nil {
		t.Fatalf("GenerateGoGlue: %v", err)
	}
	for _, want := range []string{
		// Checked argument conversion: a wrong-typed argument signals.
		"arg0 := v.GoStringArg(args[0])",
		"arg1 := v.GoBoolArg(args[1])",
		"arg2 := float64(v.GoFloatArg(args[2]))",
		"arg3 := []byte(v.GoStringArg(args[3]))",
		"arg4 := pkg.Label(v.GoStringArg(args[4]))",
		// (T, U, error) keeps the error and wraps the values.
		"r0, r1, err := pkg.Divmod(arg0, arg1)",
		"return v.NewSuccessResult(v.NewArrayWithElements([]vm.Value{v.GoToValue(r0), v.GoToValue(r1)}))",
		// Struct values are registered as *T.
		"return v.GoToValue(&result)",
		"// Skipped: Sum (variadic parameter)",
		"// Skipped: Opaque.Join (variadic parameter)",
		// Alias to another package's named type is not its basic underlying type.
		"// Skipped: ModeBits (unconvertible parameter type:",
		"_ = opaqueClass",
	} {
		if !strings.Contains(code, want) {
			t.Errorf("generated code missing %q", want)
		}
	}
	for _, bad := range []string{".(string)", ".(bool)", ".Float64()"} {
		if strings.Contains(code, bad) {
			t.Errorf("generated code contains unchecked conversion %q", bad)
		}
	}
}

func TestGeneratedStubsParse(t *testing.T) {
	for _, tc := range []struct {
		importPath string
		filter     map[string]bool
	}{
		{"strings", map[string]bool{"Contains": true, "HasPrefix": true, "Builder": true}},
		{fixturePkg, nil},
	} {
		files, err := GenerateMaggieStubs(introspectOrFatal(t, tc.importPath, tc.filter))
		if err != nil {
			t.Fatalf("GenerateMaggieStubs: %v", err)
		}
		for name, src := range files {
			if _, err := compiler.ParseSourceFileFromString(src); err != nil {
				t.Errorf("%s %s does not parse: %v\n%s", tc.importPath, name, err, src)
			}
		}
	}
}
