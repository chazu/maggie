package gowrap

import (
	"strings"
	"testing"
)

func TestGenerateMain(t *testing.T) {
	pkgs := []WrapperPackageInfo{
		{ImportPath: "strings", PkgName: "strings"},
		{ImportPath: "encoding/json", PkgName: "json"},
	}

	code := generateMain("github.com/chazu/maggie", ".maggie/wrap", pkgs)

	if !strings.Contains(code, "package main") {
		t.Error("expected package main")
	}
	if !strings.Contains(code, `"github.com/chazu/maggie/vm"`) {
		t.Error("expected vm import")
	}
	if !strings.Contains(code, `wrap_strings "mag-custom-build/wrap/strings"`) {
		t.Error("expected strings wrapper import")
	}
	if !strings.Contains(code, `wrap_json "mag-custom-build/wrap/json"`) {
		t.Error("expected json wrapper import")
	}
	if !strings.Contains(code, "wrap_strings.RegisterPrimitives(v)") {
		t.Error("expected strings RegisterPrimitives call")
	}
	if !strings.Contains(code, "wrap_json.RegisterPrimitives(v)") {
		t.Error("expected json RegisterPrimitives call")
	}
}

func TestGenerateGoMod(t *testing.T) {
	code := generateGoMod("github.com/chazu/maggie", "/path/to/project", "/path/to/wrap")

	if !strings.Contains(code, "module mag-custom-build") {
		t.Error("expected module declaration")
	}
	if !strings.Contains(code, "github.com/chazu/maggie v0.0.0") {
		t.Error("expected project dependency")
	}
	if !strings.Contains(code, "github.com/chazu/maggie => /path/to/project") {
		t.Error("expected replace directive")
	}
}
