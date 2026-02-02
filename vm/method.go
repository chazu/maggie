package vm

// Method represents a callable method in the Maggie VM.
//
// Note: The Method interface is forward-declared in object.go.
// This file contains the arity-specialized implementations that
// avoid slice allocation for common cases (0-3 arguments).

// PrimitiveFunc is a Go function that implements a primitive method.
// The vm parameter will be *VM once that's implemented.
type PrimitiveFunc func(vm interface{}, receiver Value, args []Value) Value

// Method0Func is a primitive taking no arguments.
type Method0Func func(vm interface{}, receiver Value) Value

// Method1Func is a primitive taking one argument.
type Method1Func func(vm interface{}, receiver Value, arg1 Value) Value

// Method2Func is a primitive taking two arguments.
type Method2Func func(vm interface{}, receiver Value, arg1, arg2 Value) Value

// Method3Func is a primitive taking three arguments.
type Method3Func func(vm interface{}, receiver Value, arg1, arg2, arg3 Value) Value

// Method4Func is a primitive taking four arguments.
type Method4Func func(vm interface{}, receiver Value, arg1, arg2, arg3, arg4 Value) Value

// Method8Func is a primitive taking eight arguments.
type Method8Func func(vm interface{}, receiver Value, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 Value) Value

// ---------------------------------------------------------------------------
// Arity-specialized method wrappers
// ---------------------------------------------------------------------------

// PrimitiveMethod wraps a general PrimitiveFunc as a Method.
type PrimitiveMethod struct {
	name      string
	fn        PrimitiveFunc
	docString string
}

func (m *PrimitiveMethod) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver, args)
}

func (m *PrimitiveMethod) Name() string              { return m.name }
func (m *PrimitiveMethod) Arity() int                 { return -1 } // Variable arity
func (m *PrimitiveMethod) DocString() string           { return m.docString }
func (m *PrimitiveMethod) SetDocString(doc string)     { m.docString = doc }

// Method0 wraps a zero-argument primitive.
type Method0 struct {
	name      string
	fn        Method0Func
	docString string
}

func (m *Method0) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver)
}

func (m *Method0) Name() string              { return m.name }
func (m *Method0) Arity() int                 { return 0 }
func (m *Method0) DocString() string           { return m.docString }
func (m *Method0) SetDocString(doc string)     { m.docString = doc }

// Method1 wraps a one-argument primitive.
type Method1 struct {
	name      string
	fn        Method1Func
	docString string
}

func (m *Method1) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver, args[0])
}

func (m *Method1) Name() string              { return m.name }
func (m *Method1) Arity() int                 { return 1 }
func (m *Method1) DocString() string           { return m.docString }
func (m *Method1) SetDocString(doc string)     { m.docString = doc }

// Method2 wraps a two-argument primitive.
type Method2 struct {
	name      string
	fn        Method2Func
	docString string
}

func (m *Method2) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver, args[0], args[1])
}

func (m *Method2) Name() string              { return m.name }
func (m *Method2) Arity() int                 { return 2 }
func (m *Method2) DocString() string           { return m.docString }
func (m *Method2) SetDocString(doc string)     { m.docString = doc }

// Method3 wraps a three-argument primitive.
type Method3 struct {
	name      string
	fn        Method3Func
	docString string
}

func (m *Method3) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver, args[0], args[1], args[2])
}

func (m *Method3) Name() string              { return m.name }
func (m *Method3) Arity() int                 { return 3 }
func (m *Method3) DocString() string           { return m.docString }
func (m *Method3) SetDocString(doc string)     { m.docString = doc }

// Method4 wraps a four-argument primitive.
type Method4 struct {
	name      string
	fn        Method4Func
	docString string
}

func (m *Method4) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver, args[0], args[1], args[2], args[3])
}

func (m *Method4) Name() string              { return m.name }
func (m *Method4) Arity() int                 { return 4 }
func (m *Method4) DocString() string           { return m.docString }
func (m *Method4) SetDocString(doc string)     { m.docString = doc }

// Method8 wraps an eight-argument primitive.
type Method8 struct {
	name      string
	fn        Method8Func
	docString string
}

func (m *Method8) Invoke(vm interface{}, receiver Value, args []Value) Value {
	return m.fn(vm, receiver, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7])
}

func (m *Method8) Name() string              { return m.name }
func (m *Method8) Arity() int                 { return 8 }
func (m *Method8) DocString() string           { return m.docString }
func (m *Method8) SetDocString(doc string)     { m.docString = doc }

// ---------------------------------------------------------------------------
// Factory functions
// ---------------------------------------------------------------------------

// NewPrimitiveMethod creates a new primitive method with variable arity.
func NewPrimitiveMethod(name string, fn PrimitiveFunc) Method {
	return &PrimitiveMethod{name: name, fn: fn}
}

// NewMethod0 creates a new zero-argument primitive method.
func NewMethod0(name string, fn Method0Func) Method {
	return &Method0{name: name, fn: fn}
}

// NewMethod1 creates a new one-argument primitive method.
func NewMethod1(name string, fn Method1Func) Method {
	return &Method1{name: name, fn: fn}
}

// NewMethod2 creates a new two-argument primitive method.
func NewMethod2(name string, fn Method2Func) Method {
	return &Method2{name: name, fn: fn}
}

// NewMethod3 creates a new three-argument primitive method.
func NewMethod3(name string, fn Method3Func) Method {
	return &Method3{name: name, fn: fn}
}

// NewMethod4 creates a new four-argument primitive method.
func NewMethod4(name string, fn Method4Func) Method {
	return &Method4{name: name, fn: fn}
}

// NewMethod8 creates a new eight-argument primitive method.
func NewMethod8(name string, fn Method8Func) Method {
	return &Method8{name: name, fn: fn}
}

// ---------------------------------------------------------------------------
// Method metadata interface (optional)
// ---------------------------------------------------------------------------

// NamedMethod is implemented by methods that have a name.
type NamedMethod interface {
	Method
	Name() string
}

// ArityMethod is implemented by methods that have a fixed arity.
type ArityMethod interface {
	Method
	Arity() int
}

// MethodName returns the name of a method if it implements NamedMethod.
func MethodName(m Method) string {
	if nm, ok := m.(NamedMethod); ok {
		return nm.Name()
	}
	return "<anonymous>"
}

// MethodArity returns the arity of a method if it implements ArityMethod.
// Returns -1 for variable arity methods.
func MethodArity(m Method) int {
	if am, ok := m.(ArityMethod); ok {
		return am.Arity()
	}
	return -1
}

// DocStringable is implemented by methods that support docstrings.
type DocStringable interface {
	DocString() string
	SetDocString(doc string)
}

// MethodDocString returns the docstring of a method if it implements DocStringable.
func MethodDocString(m Method) string {
	if ds, ok := m.(DocStringable); ok {
		return ds.DocString()
	}
	return ""
}
