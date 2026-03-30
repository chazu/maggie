package types

import "github.com/chazu/maggie/compiler"

// EffectTable maps (className, selector) pairs to their declared effects.
// Populated from effect annotations on methods and used during inference
// to propagate callee effects to callers.
type EffectTable struct {
	entries map[returnTypeKey]Effect
}

// NewEffectTable creates an empty effect table.
func NewEffectTable() *EffectTable {
	return &EffectTable{
		entries: make(map[returnTypeKey]Effect),
	}
}

// Register records the effect for a (className, selector) pair.
func (et *EffectTable) Register(className, selector string, eff Effect) {
	et.entries[returnTypeKey{className, selector}] = eff
}

// Lookup returns the declared effect for a (className, selector) pair.
// Returns (effect, true) if found, or (EffectNone, false) if unknown.
func (et *EffectTable) Lookup(className, selector string) (Effect, bool) {
	eff, ok := et.entries[returnTypeKey{className, selector}]
	return eff, ok
}

// HarvestFromMethod extracts effect annotations from a parsed method
// and registers them in the table.
func (et *EffectTable) HarvestFromMethod(className string, md *compiler.MethodDef) {
	if len(md.Effects) == 0 {
		return
	}
	eff := ParseEffects(md.Effects)
	if !eff.IsEmpty() {
		et.Register(className, md.Selector, eff)
	}
}
