package types

// TypeEnv is a scoped type environment for local type inference.
// It maps variable names to their inferred types and supports
// lexical scoping via a parent chain.
//
// This is separate from Env (which handles annotation bindings).
// TypeEnv is used by the Inferrer to track types as assignments
// are processed in statement order.
type TypeEnv struct {
	bindings map[string]MaggieType
	parent   *TypeEnv
}

// NewTypeEnv creates a new type environment with an optional parent scope.
func NewTypeEnv(parent *TypeEnv) *TypeEnv {
	return &TypeEnv{
		bindings: make(map[string]MaggieType),
		parent:   parent,
	}
}

// Set binds a variable name to a type in the current scope.
func (e *TypeEnv) Set(name string, t MaggieType) {
	e.bindings[name] = t
}

// Lookup returns the type bound to a variable name, searching
// up the parent chain. Returns (type, true) if found, or
// (nil, false) if unbound.
func (e *TypeEnv) Lookup(name string) (MaggieType, bool) {
	if t, ok := e.bindings[name]; ok {
		return t, true
	}
	if e.parent != nil {
		return e.parent.Lookup(name)
	}
	return nil, false
}

// Snapshot returns a copy of all bindings visible from this scope,
// including inherited bindings from parent scopes. Used for verbose
// output of inferred types.
func (e *TypeEnv) Snapshot() map[string]MaggieType {
	result := make(map[string]MaggieType)
	// Collect from outermost to innermost so inner scopes shadow outer
	e.collectBindings(result)
	return result
}

func (e *TypeEnv) collectBindings(result map[string]MaggieType) {
	if e.parent != nil {
		e.parent.collectBindings(result)
	}
	for name, t := range e.bindings {
		result[name] = t
	}
}
