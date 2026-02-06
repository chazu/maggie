package main

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
		got := deriveNamespace(tc.filePath, tc.basePath)
		if got != tc.want {
			t.Errorf("deriveNamespace(%q, %q) = %q, want %q", tc.filePath, tc.basePath, got, tc.want)
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
		got := remapImport(tc.imp, tc.oldPfx, tc.newPfx)
		if got != tc.want {
			t.Errorf("remapImport(%q, %q, %q) = %q, want %q", tc.imp, tc.oldPfx, tc.newPfx, got, tc.want)
		}
	}
}

func TestPrefixDepNamespaces(t *testing.T) {
	dep := manifest.ResolvedDep{
		Name:      "yutani",
		Namespace: "Yutani",
	}

	files := []parsedFile{
		{namespace: "", path: "src/Main.mag"},          // root-level
		{namespace: "Widgets", path: "src/widgets/B.mag"}, // subdir
	}

	prefixDepNamespaces(files, dep, false)

	if files[0].namespace != "Yutani" {
		t.Errorf("root file namespace = %q, want %q", files[0].namespace, "Yutani")
	}
	if files[1].namespace != "Yutani::Widgets" {
		t.Errorf("subdir file namespace = %q, want %q", files[1].namespace, "Yutani::Widgets")
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

	files := []parsedFile{
		{
			namespace: "",
			path:      "src/Main.mag",
			imports:   []string{"Yutani::Events", "OtherDep"},
		},
	}

	prefixDepNamespaces(files, dep, false)

	if files[0].namespace != "ThirdParty::Yutani" {
		t.Errorf("namespace = %q, want %q", files[0].namespace, "ThirdParty::Yutani")
	}
	if files[0].imports[0] != "ThirdParty::Yutani::Events" {
		t.Errorf("import[0] = %q, want %q", files[0].imports[0], "ThirdParty::Yutani::Events")
	}
	if files[0].imports[1] != "OtherDep" {
		t.Errorf("import[1] = %q, want %q", files[0].imports[1], "OtherDep")
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

	files := []parsedFile{
		{
			namespace: "",
			path:      "src/Main.mag",
			imports:   []string{"Yutani::Events"},
		},
	}

	prefixDepNamespaces(files, dep, false)

	// Import should be unchanged (no remap when override == original)
	if files[0].imports[0] != "Yutani::Events" {
		t.Errorf("import[0] = %q, want %q", files[0].imports[0], "Yutani::Events")
	}
}

func TestCheckNamespaceCollisions(t *testing.T) {
	t.Run("no collision", func(t *testing.T) {
		deps := []manifest.ResolvedDep{
			{Name: "a", Namespace: "Alpha"},
			{Name: "b", Namespace: "Beta"},
		}
		if err := checkNamespaceCollisions(deps); err != nil {
			t.Errorf("unexpected error: %v", err)
		}
	})

	t.Run("collision detected", func(t *testing.T) {
		deps := []manifest.ResolvedDep{
			{Name: "ui-toolkit", Namespace: "Widgets"},
			{Name: "yutani-widgets", Namespace: "Widgets"},
		}
		err := checkNamespaceCollisions(deps)
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
		err := checkNamespaceCollisions(deps)
		if err == nil {
			t.Fatal("expected error")
		}
		errMsg := err.Error()
		if !strings.Contains(errMsg, "NS1") || !strings.Contains(errMsg, "NS2") {
			t.Errorf("error should mention both namespaces, got: %s", errMsg)
		}
	})
}
