package pipeline

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/manifest"
)

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
		got := DeriveNamespace(tc.filePath, tc.basePath)
		if got != tc.want {
			t.Errorf("DeriveNamespace(%q, %q) = %q, want %q", tc.filePath, tc.basePath, got, tc.want)
		}
	}
}

func TestRemapImport(t *testing.T) {
	tests := []struct {
		imp    string
		oldPfx string
		newPfx string
		want   string
	}{
		{"Yutani", "Yutani", "ThirdParty::Yutani", "ThirdParty::Yutani"},
		{"Yutani::Events", "Yutani", "ThirdParty::Yutani", "ThirdParty::Yutani::Events"},
		{"OtherDep", "Yutani", "ThirdParty::Yutani", "OtherDep"},
		{"YutaniExtra", "Yutani", "ThirdParty::Yutani", "YutaniExtra"}, // no match (no :: boundary)
	}

	for _, tc := range tests {
		got := RemapImport(tc.imp, tc.oldPfx, tc.newPfx)
		if got != tc.want {
			t.Errorf("RemapImport(%q, %q, %q) = %q, want %q", tc.imp, tc.oldPfx, tc.newPfx, got, tc.want)
		}
	}
}

func TestPrefixDepNamespaces(t *testing.T) {
	dep := manifest.ResolvedDep{
		Name:      "yutani",
		Namespace: "Yutani",
	}

	files := []ParsedFile{
		{Namespace: "", Path: "src/Main.mag"},             // root-level
		{Namespace: "Widgets", Path: "src/widgets/B.mag"}, // subdir
	}

	PrefixDepNamespaces(files, dep, nil)

	if files[0].Namespace != "Yutani" {
		t.Errorf("root file namespace = %q, want %q", files[0].Namespace, "Yutani")
	}
	if files[1].Namespace != "Yutani::Widgets" {
		t.Errorf("subdir file namespace = %q, want %q", files[1].Namespace, "Yutani::Widgets")
	}
}

func TestPrefixDepNamespacesWithRemap(t *testing.T) {
	dep := manifest.ResolvedDep{
		Name:      "yutani",
		Namespace: "ThirdParty::Yutani",
		Manifest: &manifest.Manifest{
			Project: manifest.Project{Namespace: "Yutani"},
		},
	}

	files := []ParsedFile{
		{
			Namespace: "",
			Path:      "src/Main.mag",
			Imports:   []string{"Yutani::Events", "OtherDep"},
		},
	}

	PrefixDepNamespaces(files, dep, nil)

	if files[0].Namespace != "ThirdParty::Yutani" {
		t.Errorf("namespace = %q, want %q", files[0].Namespace, "ThirdParty::Yutani")
	}
	if files[0].Imports[0] != "ThirdParty::Yutani::Events" {
		t.Errorf("import[0] = %q, want %q", files[0].Imports[0], "ThirdParty::Yutani::Events")
	}
	if files[0].Imports[1] != "OtherDep" {
		t.Errorf("import[1] = %q, want %q", files[0].Imports[1], "OtherDep")
	}
}

func TestPrefixDepNamespacesNoRemap(t *testing.T) {
	// When namespace matches producer, no remapping needed
	dep := manifest.ResolvedDep{
		Name:      "yutani",
		Namespace: "Yutani",
		Manifest: &manifest.Manifest{
			Project: manifest.Project{Namespace: "Yutani"},
		},
	}

	files := []ParsedFile{
		{
			Namespace: "",
			Path:      "src/Main.mag",
			Imports:   []string{"Yutani::Events"},
		},
	}

	PrefixDepNamespaces(files, dep, nil)

	// Import should be unchanged (no remap when override == original)
	if files[0].Imports[0] != "Yutani::Events" {
		t.Errorf("import[0] = %q, want %q", files[0].Imports[0], "Yutani::Events")
	}
}

func TestCheckNamespaceCollisions(t *testing.T) {
	t.Run("no collision", func(t *testing.T) {
		deps := []manifest.ResolvedDep{
			{Name: "a", Namespace: "Alpha"},
			{Name: "b", Namespace: "Beta"},
		}
		if err := CheckNamespaceCollisions(deps); err != nil {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("collision detected", func(t *testing.T) {
		deps := []manifest.ResolvedDep{
			{Name: "ui-toolkit", Namespace: "Widgets"},
			{Name: "yutani-widgets", Namespace: "Widgets"},
		}
		err := CheckNamespaceCollisions(deps)
		if err == nil {
			t.Fatal("expected error")
		}
		errMsg := err.Error()
		if !strings.Contains(errMsg, "Widgets") {
			t.Errorf("error should mention namespace, got: %s", errMsg)
		}
		if !strings.Contains(errMsg, "ui-toolkit") || !strings.Contains(errMsg, "yutani-widgets") {
			t.Errorf("error should mention dep names, got: %s", errMsg)
		}
	})

	t.Run("multiple collisions reported", func(t *testing.T) {
		deps := []manifest.ResolvedDep{
			{Name: "a", Namespace: "NS1"},
			{Name: "b", Namespace: "NS1"},
			{Name: "c", Namespace: "NS2"},
			{Name: "d", Namespace: "NS2"},
		}
		err := CheckNamespaceCollisions(deps)
		if err == nil {
			t.Fatal("expected error")
		}
		errMsg := err.Error()
		if !strings.Contains(errMsg, "NS1") || !strings.Contains(errMsg, "NS2") {
			t.Errorf("error should mention both namespaces, got: %s", errMsg)
		}
	})
}

// A dependency file that declares its own namespace under the dep's
// namespace used to be nested again (Yutani::Yutani::Events), so the dep's
// own `import: 'Yutani::Events'` no longer resolved. Declared namespaces are
// absolute: they are remapped onto the resolved namespace like imports.
func TestPrefixDepNamespacesDeclared(t *testing.T) {
	yutaniManifest := &manifest.Manifest{Project: manifest.Project{Namespace: "Yutani"}}
	cases := []struct {
		name string
		dep  manifest.ResolvedDep
		file ParsedFile
		want string
	}{
		{"declared under own namespace",
			manifest.ResolvedDep{Name: "yutani", Namespace: "Yutani", Manifest: yutaniManifest},
			ParsedFile{Namespace: "Yutani::Events", NamespaceDeclared: true}, "Yutani::Events"},
		{"declared as own namespace",
			manifest.ResolvedDep{Name: "yutani", Namespace: "Yutani", Manifest: yutaniManifest},
			ParsedFile{Namespace: "Yutani", NamespaceDeclared: true}, "Yutani"},
		{"declared under own namespace, consumer override",
			manifest.ResolvedDep{Name: "yutani", Namespace: "ThirdParty::Yutani", Manifest: yutaniManifest},
			ParsedFile{Namespace: "Yutani::Events", NamespaceDeclared: true}, "ThirdParty::Yutani::Events"},
		{"declared under fallback namespace, no manifest",
			manifest.ResolvedDep{Name: "my-lib", Namespace: "MyLib"},
			ParsedFile{Namespace: "MyLib::Util", NamespaceDeclared: true}, "MyLib::Util"},
		{"declared elsewhere is still prefixed",
			manifest.ResolvedDep{Name: "yutani", Namespace: "Yutani", Manifest: yutaniManifest},
			ParsedFile{Namespace: "Widgets", NamespaceDeclared: true}, "Yutani::Widgets"},
		{"directory-derived matching the dep name is still nested",
			manifest.ResolvedDep{Name: "yutani", Namespace: "Yutani", Manifest: yutaniManifest},
			ParsedFile{Namespace: "Yutani"}, "Yutani::Yutani"},
	}
	for _, tc := range cases {
		files := []ParsedFile{tc.file}
		PrefixDepNamespaces(files, tc.dep, nil)
		if files[0].Namespace != tc.want {
			t.Errorf("%s: namespace = %q, want %q", tc.name, files[0].Namespace, tc.want)
		}
	}
}
