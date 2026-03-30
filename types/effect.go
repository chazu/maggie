package types

import (
	"strings"

	"github.com/chazu/maggie/compiler"
)

// Effect is a bitmask representing side-effect categories.
type Effect uint8

const (
	EffectNone    Effect = 0
	EffectIO      Effect = 1 << 0
	EffectNetwork Effect = 1 << 1
	EffectProcess Effect = 1 << 2
	EffectState   Effect = 1 << 3
	EffectPure    Effect = 1 << 4 // purity assertion
)

// effectNames maps individual effect bits to their human-readable names.
var effectNames = map[Effect]string{
	EffectIO:      "IO",
	EffectNetwork: "Network",
	EffectProcess: "Process",
	EffectState:   "State",
	EffectPure:    "Pure",
}

// String returns a human-readable representation of the effect bitmask.
func (e Effect) String() string {
	if e == EffectNone {
		return "None"
	}
	var parts []string
	for bit := Effect(1); bit <= EffectPure; bit <<= 1 {
		if e&bit != 0 {
			if name, ok := effectNames[bit]; ok {
				parts = append(parts, name)
			}
		}
	}
	if len(parts) == 0 {
		return "None"
	}
	return strings.Join(parts, ", ")
}

// Contains returns true if e contains all bits in other.
func (e Effect) Contains(other Effect) bool { return e&other == other }

// Union returns the union of two effect bitmasks.
func (e Effect) Union(other Effect) Effect { return e | other }

// IsEmpty returns true if no effects are set.
func (e Effect) IsEmpty() bool { return e == EffectNone }

// IsPure returns true if the Pure assertion flag is set.
func (e Effect) IsPure() bool { return e&EffectPure != 0 }

// GlobalEffects maps VM global class names to their effect category.
var GlobalEffects = map[string]Effect{
	"File":            EffectIO,
	"ExternalProcess": EffectIO,
	"SqliteDatabase":  EffectIO,
	"DuckDatabase":    EffectIO,

	"HTTP":             EffectNetwork,
	"HttpClient":       EffectNetwork,
	"HttpServer":       EffectNetwork,
	"UnixSocketClient": EffectNetwork,
	"UnixSocketServer": EffectNetwork,
	"GrpcClient":       EffectNetwork,

	"Process":              EffectProcess,
	"Channel":              EffectProcess,
	"Mutex":                EffectProcess,
	"WaitGroup":            EffectProcess,
	"Semaphore":            EffectProcess,
	"CancellationContext":  EffectProcess,
}

// SelectorEffects maps specific class+selector pairs to effects.
var SelectorEffects = map[string]map[string]Effect{
	"Compiler": {
		"setGlobal:to:": EffectState,
		"evaluate:":     EffectState,
	},
}

// validEffectNames is the set of recognized effect names for validation.
var validEffectNames = map[string]bool{
	"IO": true, "Network": true, "Process": true, "State": true, "Pure": true,
}

// ParseEffects converts parsed TypeExpr effect annotations to an Effect bitmask.
func ParseEffects(exprs []*compiler.TypeExpr) Effect {
	var eff Effect
	for _, expr := range exprs {
		if expr == nil {
			continue
		}
		switch expr.Name {
		case "IO":
			eff |= EffectIO
		case "Network":
			eff |= EffectNetwork
		case "Process":
			eff |= EffectProcess
		case "State":
			eff |= EffectState
		case "Pure":
			eff |= EffectPure
		}
	}
	return eff
}

// IsValidEffect returns true if the name is a recognized effect.
func IsValidEffect(name string) bool {
	return validEffectNames[name]
}
