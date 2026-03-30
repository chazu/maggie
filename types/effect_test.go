package types

import (
	"testing"

	"github.com/chazu/maggie/compiler"
)

func TestEffectString(t *testing.T) {
	tests := []struct {
		eff  Effect
		want string
	}{
		{EffectNone, "None"},
		{EffectIO, "IO"},
		{EffectNetwork, "Network"},
		{EffectProcess, "Process"},
		{EffectState, "State"},
		{EffectPure, "Pure"},
		{EffectIO | EffectNetwork, "IO, Network"},
		{EffectIO | EffectProcess | EffectState, "IO, Process, State"},
	}
	for _, tt := range tests {
		got := tt.eff.String()
		if got != tt.want {
			t.Errorf("Effect(%d).String() = %q, want %q", tt.eff, got, tt.want)
		}
	}
}

func TestEffectContains(t *testing.T) {
	combined := EffectIO | EffectNetwork
	if !combined.Contains(EffectIO) {
		t.Error("IO|Network should contain IO")
	}
	if !combined.Contains(EffectNetwork) {
		t.Error("IO|Network should contain Network")
	}
	if combined.Contains(EffectProcess) {
		t.Error("IO|Network should not contain Process")
	}
	if !combined.Contains(EffectIO | EffectNetwork) {
		t.Error("IO|Network should contain IO|Network")
	}
}

func TestEffectUnion(t *testing.T) {
	a := EffectIO
	b := EffectNetwork
	got := a.Union(b)
	want := EffectIO | EffectNetwork
	if got != want {
		t.Errorf("IO.Union(Network) = %d, want %d", got, want)
	}
}

func TestEffectIsEmpty(t *testing.T) {
	if !EffectNone.IsEmpty() {
		t.Error("EffectNone should be empty")
	}
	if EffectIO.IsEmpty() {
		t.Error("EffectIO should not be empty")
	}
}

func TestEffectIsPure(t *testing.T) {
	if EffectNone.IsPure() {
		t.Error("EffectNone should not be pure")
	}
	if !EffectPure.IsPure() {
		t.Error("EffectPure should be pure")
	}
	// Pure combined with other effects
	combined := EffectPure | EffectIO
	if !combined.IsPure() {
		t.Error("Pure|IO should still be pure")
	}
}

func TestParseEffects(t *testing.T) {
	tests := []struct {
		name  string
		exprs []*compiler.TypeExpr
		want  Effect
	}{
		{"nil exprs", nil, EffectNone},
		{"empty", []*compiler.TypeExpr{}, EffectNone},
		{"single IO", []*compiler.TypeExpr{{Name: "IO"}}, EffectIO},
		{"Pure", []*compiler.TypeExpr{{Name: "Pure"}}, EffectPure},
		{"IO and Network", []*compiler.TypeExpr{{Name: "IO"}, {Name: "Network"}}, EffectIO | EffectNetwork},
		{"with nil entry", []*compiler.TypeExpr{nil, {Name: "State"}}, EffectState},
		{"unknown ignored", []*compiler.TypeExpr{{Name: "Unknown"}}, EffectNone},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ParseEffects(tt.exprs)
			if got != tt.want {
				t.Errorf("ParseEffects() = %d (%s), want %d (%s)", got, got, tt.want, tt.want)
			}
		})
	}
}

func TestIsValidEffect(t *testing.T) {
	valid := []string{"IO", "Network", "Process", "State", "Pure"}
	for _, name := range valid {
		if !IsValidEffect(name) {
			t.Errorf("IsValidEffect(%q) = false, want true", name)
		}
	}
	invalid := []string{"io", "NETWORK", "FileSystem", "", "Unknown"}
	for _, name := range invalid {
		if IsValidEffect(name) {
			t.Errorf("IsValidEffect(%q) = true, want false", name)
		}
	}
}

func TestGlobalEffectsTable(t *testing.T) {
	// Spot-check a few entries
	if GlobalEffects["File"] != EffectIO {
		t.Error("File should map to IO")
	}
	if GlobalEffects["HTTP"] != EffectNetwork {
		t.Error("HTTP should map to Network")
	}
	if GlobalEffects["Channel"] != EffectProcess {
		t.Error("Channel should map to Process")
	}
}

func TestSelectorEffectsTable(t *testing.T) {
	if SelectorEffects["Compiler"]["evaluate:"] != EffectState {
		t.Error("Compiler>>evaluate: should map to State")
	}
	if SelectorEffects["Compiler"]["setGlobal:to:"] != EffectState {
		t.Error("Compiler>>setGlobal:to: should map to State")
	}
}
