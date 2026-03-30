package types

// Env is a type environment that maps variable names to their types.
// Supports lexical scoping via parent chain.
type Env struct {
	bindings map[string]MaggieType
	parent   *Env
}

// NewEnv creates a new empty type environment.
func NewEnv(parent *Env) *Env {
	return &Env{bindings: make(map[string]MaggieType), parent: parent}
}

// Bind sets the type of a variable in the current scope.
func (e *Env) Bind(name string, t MaggieType) {
	e.bindings[name] = t
}

// Lookup returns the type of a variable, searching up the parent chain.
// Returns nil if not found (untyped).
func (e *Env) Lookup(name string) MaggieType {
	if t, ok := e.bindings[name]; ok {
		return t
	}
	if e.parent != nil {
		return e.parent.Lookup(name)
	}
	return nil
}
