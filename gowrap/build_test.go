package gowrap

import (
	"strings"
	"testing"
)

func TestGenerateEmbeddedGoMod(t *testing.T) {
	code := generateEmbeddedGoMod("github.com/chazu/maggie", "/path/to/maggie", "/path/to/project", "", nil)

	if !strings.Contains(code, "module mag-embedded-build") {
		t.Error("expected module declaration")
	}
	if !strings.Contains(code, "github.com/chazu/maggie v0.0.0") {
		t.Error("expected maggie dependency")
	}
	if !strings.Contains(code, "github.com/chazu/maggie => /path/to/maggie") {
		t.Error("expected replace directive")
	}
}

func TestGenerateEmbeddedMain(t *testing.T) {
	code := generateEmbeddedMain("github.com/chazu/maggie", "Main.start", "MyApp", "", "", nil)

	if !strings.Contains(code, "package main") {
		t.Error("expected package main")
	}
	if !strings.Contains(code, `"github.com/chazu/maggie/vm"`) {
		t.Error("expected vm import")
	}
	if !strings.Contains(code, `"github.com/chazu/maggie/compiler"`) {
		t.Error("expected compiler import")
	}
	if !strings.Contains(code, "embeddedImage") {
		t.Error("expected embedded image variable")
	}
	if !strings.Contains(code, "Main.start") {
		t.Error("expected entry point")
	}
}

func TestGenerateEmbeddedMainWithWrappers(t *testing.T) {
	pkgs := []WrapperPackageInfo{
		{ImportPath: "strings", PkgName: "strings"},
	}
	code := generateEmbeddedMain("github.com/chazu/maggie", "Main.start", "MyApp", "/path/to/project", "/path/to/wrap", pkgs)

	if !strings.Contains(code, "wrap_strings") {
		t.Error("expected wrapper import")
	}
	if !strings.Contains(code, "wrap_strings.RegisterPrimitives(v)") {
		t.Error("expected RegisterPrimitives call")
	}
}
