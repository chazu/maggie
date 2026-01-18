package vm

// ---------------------------------------------------------------------------
// VM: The Maggie Virtual Machine
// ---------------------------------------------------------------------------

// VM is the main Maggie virtual machine.
type VM struct {
	// Global tables
	Selectors *SelectorTable // selector name -> ID
	Symbols   *SymbolTable   // symbol name -> ID
	Classes   *ClassTable    // class name -> Class
	Globals   map[string]Value

	// Well-known classes (for fast-path checks and bootstrapping)
	ObjectClass            *Class
	ClassClass             *Class
	MetaclassClass         *Class
	BlockClass             *Class
	BooleanClass           *Class
	TrueClass              *Class
	FalseClass             *Class
	UndefinedObjectClass   *Class
	SmallIntegerClass      *Class
	FloatClass             *Class
	StringClass            *Class
	SymbolClass            *Class
	ArrayClass             *Class
	ByteArrayClass         *Class
	AssociationClass       *Class
	DictionaryClass        *Class
	CompiledMethodClass    *Class

	// Interpreter
	interpreter *Interpreter
}

// NewVM creates and bootstraps a new VM.
func NewVM() *VM {
	vm := &VM{
		Selectors: NewSelectorTable(),
		Symbols:   NewSymbolTable(),
		Classes:   NewClassTable(),
		Globals:   make(map[string]Value),
	}

	// Bootstrap core classes
	vm.bootstrap()

	// Create interpreter
	vm.interpreter = vm.newInterpreter()

	return vm
}

// newInterpreter creates an interpreter connected to this VM.
func (vm *VM) newInterpreter() *Interpreter {
	interp := NewInterpreter()
	// Share tables with VM
	interp.Selectors = vm.Selectors
	interp.Classes = vm.Classes
	interp.Globals = vm.Globals
	return interp
}

// ---------------------------------------------------------------------------
// Bootstrap: Create core classes
// ---------------------------------------------------------------------------

func (vm *VM) bootstrap() {
	// Phase 1: Create Object and Class (chicken-and-egg problem)
	// Object is the root of all classes
	// Class is the class of all classes
	vm.ObjectClass = vm.createBootstrapClass("Object", nil)
	vm.ClassClass = vm.createBootstrapClass("Class", vm.ObjectClass)

	// Now we can properly set up metaclasses
	// For now, we skip full metaclass support for simplicity

	// Phase 2: Create behavior classes
	vm.BooleanClass = vm.createClass("Boolean", vm.ObjectClass)
	vm.TrueClass = vm.createClass("True", vm.BooleanClass)
	vm.FalseClass = vm.createClass("False", vm.BooleanClass)
	vm.UndefinedObjectClass = vm.createClass("UndefinedObject", vm.ObjectClass)

	// Phase 3: Create magnitude classes
	vm.SmallIntegerClass = vm.createClass("SmallInteger", vm.ObjectClass)
	vm.FloatClass = vm.createClass("Float", vm.ObjectClass)

	// Phase 4: Create collection classes
	vm.StringClass = vm.createClass("String", vm.ObjectClass)
	vm.SymbolClass = vm.createClass("Symbol", vm.StringClass)
	vm.ArrayClass = vm.createClass("Array", vm.ObjectClass)
	vm.ByteArrayClass = vm.createClass("ByteArray", vm.ObjectClass)
	vm.AssociationClass = vm.createClassWithIvars("Association", vm.ObjectClass, []string{"key", "value"})
	vm.DictionaryClass = vm.createClassWithIvars("Dictionary", vm.ObjectClass, []string{"table", "size"})

	// Phase 5: Create method/block classes
	vm.BlockClass = vm.createClass("Block", vm.ObjectClass)
	vm.CompiledMethodClass = vm.createClass("CompiledMethod", vm.ObjectClass)

	// Phase 6: Register primitives on core classes
	vm.registerObjectPrimitives()
	vm.registerBooleanPrimitives()
	vm.registerSmallIntegerPrimitives()
	vm.registerFloatPrimitives()
	vm.registerSymbolPrimitives()
	vm.registerStringPrimitives()
	vm.registerArrayPrimitives()
	vm.registerBlockPrimitives()

	// Phase 7: Set up globals
	vm.Globals["Object"] = vm.classValue(vm.ObjectClass)
	vm.Globals["Class"] = vm.classValue(vm.ClassClass)
	vm.Globals["Boolean"] = vm.classValue(vm.BooleanClass)
	vm.Globals["True"] = vm.classValue(vm.TrueClass)
	vm.Globals["False"] = vm.classValue(vm.FalseClass)
	vm.Globals["UndefinedObject"] = vm.classValue(vm.UndefinedObjectClass)
	vm.Globals["SmallInteger"] = vm.classValue(vm.SmallIntegerClass)
	vm.Globals["Float"] = vm.classValue(vm.FloatClass)
	vm.Globals["String"] = vm.classValue(vm.StringClass)
	vm.Globals["Symbol"] = vm.classValue(vm.SymbolClass)
	vm.Globals["Array"] = vm.classValue(vm.ArrayClass)
	vm.Globals["Block"] = vm.classValue(vm.BlockClass)

	// Well-known symbols
	vm.Globals["nil"] = Nil
	vm.Globals["true"] = True
	vm.Globals["false"] = False
}

// createBootstrapClass creates a class during early bootstrap.
func (vm *VM) createBootstrapClass(name string, superclass *Class) *Class {
	c := NewClass(name, superclass)
	vm.Classes.Register(c)
	return c
}

// createClass creates a regular class.
func (vm *VM) createClass(name string, superclass *Class) *Class {
	c := NewClass(name, superclass)
	vm.Classes.Register(c)
	return c
}

// createClassWithIvars creates a class with instance variables.
func (vm *VM) createClassWithIvars(name string, superclass *Class, ivars []string) *Class {
	c := NewClassWithInstVars(name, superclass, ivars)
	vm.Classes.Register(c)
	return c
}

// classValue returns a Value representing a class.
// For now, we use a symbol as a placeholder.
func (vm *VM) classValue(c *Class) Value {
	return vm.Symbols.SymbolValue(c.Name)
}

// ---------------------------------------------------------------------------
// Primitive registration
// ---------------------------------------------------------------------------

func (vm *VM) registerObjectPrimitives() {
	c := vm.ObjectClass

	// class - return the class of the receiver
	c.AddMethod0(vm.Selectors, "class", func(_ interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// == - identity comparison
	c.AddMethod1(vm.Selectors, "==", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// ~~ - identity non-equality
	c.AddMethod1(vm.Selectors, "~~", func(_ interface{}, recv Value, arg Value) Value {
		if recv != arg {
			return True
		}
		return False
	})

	// isNil
	c.AddMethod0(vm.Selectors, "isNil", func(_ interface{}, recv Value) Value {
		return False
	})

	// notNil
	c.AddMethod0(vm.Selectors, "notNil", func(_ interface{}, recv Value) Value {
		return True
	})

	// yourself
	c.AddMethod0(vm.Selectors, "yourself", func(_ interface{}, recv Value) Value {
		return recv
	})

	// ifNil: - for non-nil objects, don't evaluate block
	c.AddMethod1(vm.Selectors, "ifNil:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	// ifNotNil: - for non-nil objects, evaluate block
	c.AddMethod1(vm.Selectors, "ifNotNil:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	// ifNil:ifNotNil: - for non-nil objects, evaluate second block
	c.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(vmPtr interface{}, recv Value, nilBlock, notNilBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(notNilBlock, nil)
	})

	// perform: - send message by selector
	c.AddMethod1(vm.Selectors, "perform:", func(vmPtr interface{}, recv Value, selector Value) Value {
		v := vmPtr.(*VM)
		if selector.IsSymbol() {
			selName := v.Symbols.Name(selector.SymbolID())
			return v.Send(recv, selName, nil)
		}
		return Nil
	})

	// perform:with: - send message with one argument
	c.AddMethod2(vm.Selectors, "perform:with:", func(vmPtr interface{}, recv Value, selector, arg Value) Value {
		v := vmPtr.(*VM)
		if selector.IsSymbol() {
			selName := v.Symbols.Name(selector.SymbolID())
			return v.Send(recv, selName, []Value{arg})
		}
		return Nil
	})

	// doesNotUnderstand: - default error handler
	c.AddMethod1(vm.Selectors, "doesNotUnderstand:", func(_ interface{}, recv Value, message Value) Value {
		// In a full implementation, this would raise an error
		// For now, return nil
		return Nil
	})

	// = - value equality (default to identity)
	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// ~= - value inequality
	c.AddMethod1(vm.Selectors, "~=", func(_ interface{}, recv Value, arg Value) Value {
		if recv != arg {
			return True
		}
		return False
	})

	// hash - default hash (identity-based)
	c.AddMethod0(vm.Selectors, "hash", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(recv))
	})
}

func (vm *VM) registerBooleanPrimitives() {
	// True class
	vm.TrueClass.AddMethod0(vm.Selectors, "not", func(_ interface{}, recv Value) Value {
		return False
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "&", func(_ interface{}, recv Value, arg Value) Value {
		return arg
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "|", func(_ interface{}, recv Value, arg Value) Value {
		return True
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "ifTrue:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.TrueClass.AddMethod1(vm.Selectors, "ifFalse:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.TrueClass.AddMethod2(vm.Selectors, "ifTrue:ifFalse:", func(vmPtr interface{}, recv Value, trueBlock, falseBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(trueBlock, nil)
	})

	// and: - short-circuit and (evaluate block only if receiver is true)
	vm.TrueClass.AddMethod1(vm.Selectors, "and:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	// or: - short-circuit or (don't evaluate block since receiver is true)
	vm.TrueClass.AddMethod1(vm.Selectors, "or:", func(_ interface{}, recv Value, block Value) Value {
		return True
	})

	// xor: - exclusive or
	vm.TrueClass.AddMethod1(vm.Selectors, "xor:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == True {
			return False
		}
		return True
	})

	// eqv: - equivalence (same as =)
	vm.TrueClass.AddMethod1(vm.Selectors, "eqv:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == True {
			return True
		}
		return False
	})

	// False class
	vm.FalseClass.AddMethod0(vm.Selectors, "not", func(_ interface{}, recv Value) Value {
		return True
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "&", func(_ interface{}, recv Value, arg Value) Value {
		return False
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "|", func(_ interface{}, recv Value, arg Value) Value {
		return arg
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "ifTrue:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	vm.FalseClass.AddMethod1(vm.Selectors, "ifFalse:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.FalseClass.AddMethod2(vm.Selectors, "ifTrue:ifFalse:", func(vmPtr interface{}, recv Value, trueBlock, falseBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(falseBlock, nil)
	})

	// and: - short-circuit and (don't evaluate block since receiver is false)
	vm.FalseClass.AddMethod1(vm.Selectors, "and:", func(_ interface{}, recv Value, block Value) Value {
		return False
	})

	// or: - short-circuit or (evaluate block since receiver is false)
	vm.FalseClass.AddMethod1(vm.Selectors, "or:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	// xor: - exclusive or
	vm.FalseClass.AddMethod1(vm.Selectors, "xor:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == False {
			return False
		}
		return True
	})

	// eqv: - equivalence
	vm.FalseClass.AddMethod1(vm.Selectors, "eqv:", func(_ interface{}, recv Value, arg Value) Value {
		if arg == False {
			return True
		}
		return False
	})

	// UndefinedObject (nil)
	vm.UndefinedObjectClass.AddMethod0(vm.Selectors, "isNil", func(_ interface{}, recv Value) Value {
		return True
	})

	vm.UndefinedObjectClass.AddMethod0(vm.Selectors, "notNil", func(_ interface{}, recv Value) Value {
		return False
	})

	vm.UndefinedObjectClass.AddMethod1(vm.Selectors, "ifNil:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(block, nil)
	})

	vm.UndefinedObjectClass.AddMethod1(vm.Selectors, "ifNotNil:", func(_ interface{}, recv Value, block Value) Value {
		return Nil
	})

	// ifNil:ifNotNil: - evaluate first block
	vm.UndefinedObjectClass.AddMethod2(vm.Selectors, "ifNil:ifNotNil:", func(vmPtr interface{}, recv Value, nilBlock, notNilBlock Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(nilBlock, nil)
	})
}

func (vm *VM) registerSmallIntegerPrimitives() {
	c := vm.SmallIntegerClass

	// Arithmetic
	c.AddMethod1(vm.Selectors, "+", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() + arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) + arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "-", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() - arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) - arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "*", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() * arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) * arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "/", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil // Division by zero
			}
			return FromSmallInt(recv.SmallInt() / arg.SmallInt())
		}
		if arg.IsFloat() {
			return FromFloat64(float64(recv.SmallInt()) / arg.Float64())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "\\\\", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil
			}
			return FromSmallInt(recv.SmallInt() % arg.SmallInt())
		}
		return Nil
	})

	// // - truncated integer division (floor division)
	c.AddMethod1(vm.Selectors, "//", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if arg.SmallInt() == 0 {
				return Nil
			}
			// Smalltalk // is floor division
			a, b := recv.SmallInt(), arg.SmallInt()
			result := a / b
			// Adjust for floor behavior with negative numbers
			if (a < 0) != (b < 0) && a%b != 0 {
				result--
			}
			return FromSmallInt(result)
		}
		return Nil
	})

	// Comparisons
	c.AddMethod1(vm.Selectors, "<", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() < arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, ">", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() > arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "<=", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() <= arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, ">=", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() >= arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			if recv.SmallInt() == arg.SmallInt() {
				return True
			}
			return False
		}
		return Nil
	})

	// Bit operations
	c.AddMethod1(vm.Selectors, "bitAnd:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() & arg.SmallInt())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "bitOr:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() | arg.SmallInt())
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "bitXor:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			return FromSmallInt(recv.SmallInt() ^ arg.SmallInt())
		}
		return Nil
	})

	c.AddMethod0(vm.Selectors, "negated", func(_ interface{}, recv Value) Value {
		return FromSmallInt(-recv.SmallInt())
	})

	c.AddMethod0(vm.Selectors, "abs", func(_ interface{}, recv Value) Value {
		n := recv.SmallInt()
		if n < 0 {
			return FromSmallInt(-n)
		}
		return recv
	})

	// Iteration
	c.AddMethod1(vm.Selectors, "timesRepeat:", func(vmPtr interface{}, recv Value, block Value) Value {
		v := vmPtr.(*VM)
		n := recv.SmallInt()
		for i := int64(0); i < n; i++ {
			v.evaluateBlock(block, nil)
		}
		return recv
	})

	c.AddMethod1(vm.Selectors, "to:do:", func(vmPtr interface{}, recv Value, end Value) Value {
		// This is a 2-arg method, but we're defining it as 1-arg for simplicity
		// Full implementation would use to:do: properly
		return recv
	})
}

func (vm *VM) registerFloatPrimitives() {
	c := vm.FloatClass

	c.AddMethod1(vm.Selectors, "+", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() + arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() + float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "-", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() - arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() - float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "*", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() * arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() * float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "/", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsFloat() {
			return FromFloat64(recv.Float64() / arg.Float64())
		}
		if arg.IsSmallInt() {
			return FromFloat64(recv.Float64() / float64(arg.SmallInt()))
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "<", func(_ interface{}, recv Value, arg Value) Value {
		var other float64
		if arg.IsFloat() {
			other = arg.Float64()
		} else if arg.IsSmallInt() {
			other = float64(arg.SmallInt())
		} else {
			return Nil
		}
		if recv.Float64() < other {
			return True
		}
		return False
	})

	c.AddMethod1(vm.Selectors, ">", func(_ interface{}, recv Value, arg Value) Value {
		var other float64
		if arg.IsFloat() {
			other = arg.Float64()
		} else if arg.IsSmallInt() {
			other = float64(arg.SmallInt())
		} else {
			return Nil
		}
		if recv.Float64() > other {
			return True
		}
		return False
	})

	c.AddMethod0(vm.Selectors, "negated", func(_ interface{}, recv Value) Value {
		return FromFloat64(-recv.Float64())
	})

	c.AddMethod0(vm.Selectors, "abs", func(_ interface{}, recv Value) Value {
		f := recv.Float64()
		if f < 0 {
			return FromFloat64(-f)
		}
		return recv
	})

	c.AddMethod0(vm.Selectors, "truncated", func(_ interface{}, recv Value) Value {
		return FromSmallInt(int64(recv.Float64()))
	})

	c.AddMethod0(vm.Selectors, "rounded", func(_ interface{}, recv Value) Value {
		f := recv.Float64()
		if f >= 0 {
			return FromSmallInt(int64(f + 0.5))
		}
		return FromSmallInt(int64(f - 0.5))
	})
}

func (vm *VM) registerSymbolPrimitives() {
	c := vm.SymbolClass

	// asString - convert symbol to string
	c.AddMethod0(vm.Selectors, "asString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			// Return a symbol representing the string for now
			// Full implementation would return a String object
			return v.Symbols.SymbolValue(name)
		}
		return Nil
	})

	// = - symbol equality (identity since symbols are interned)
	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})

	// hash - symbol hash (use symbol ID)
	c.AddMethod0(vm.Selectors, "hash", func(_ interface{}, recv Value) Value {
		if recv.IsSymbol() {
			return FromSmallInt(int64(recv.SymbolID()))
		}
		return FromSmallInt(0)
	})

	// size - length of symbol name
	c.AddMethod0(vm.Selectors, "size", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			return FromSmallInt(int64(len(name)))
		}
		return FromSmallInt(0)
	})

	// asSymbol - return self
	c.AddMethod0(vm.Selectors, "asSymbol", func(_ interface{}, recv Value) Value {
		return recv
	})
}

func (vm *VM) registerStringPrimitives() {
	// String primitives
	// For Phase 1, strings are represented as symbols
	// Full String implementation requires heap-allocated string objects
	c := vm.StringClass

	// size - return length (placeholder)
	c.AddMethod0(vm.Selectors, "size", func(_ interface{}, recv Value) Value {
		// For now, strings aren't fully implemented
		return FromSmallInt(0)
	})

	// asSymbol - convert to symbol (placeholder)
	c.AddMethod0(vm.Selectors, "asSymbol", func(_ interface{}, recv Value) Value {
		return recv
	})

	// = - string equality (placeholder)
	c.AddMethod1(vm.Selectors, "=", func(_ interface{}, recv Value, arg Value) Value {
		if recv == arg {
			return True
		}
		return False
	})
}

func (vm *VM) registerArrayPrimitives() {
	// Array primitives
	// For Phase 1, arrays require heap-allocated objects
	// Placeholder implementations for now
	c := vm.ArrayClass

	// size - return array size (placeholder)
	c.AddMethod0(vm.Selectors, "size", func(_ interface{}, recv Value) Value {
		return FromSmallInt(0)
	})

	// at: - array access (placeholder)
	c.AddMethod1(vm.Selectors, "at:", func(_ interface{}, recv Value, index Value) Value {
		return Nil
	})

	// at:put: - array modification (placeholder)
	c.AddMethod2(vm.Selectors, "at:put:", func(_ interface{}, recv Value, index, value Value) Value {
		return recv
	})
}

func (vm *VM) registerBlockPrimitives() {
	c := vm.BlockClass

	c.AddMethod0(vm.Selectors, "value", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, nil)
	})

	c.AddMethod1(vm.Selectors, "value:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg})
	})

	c.AddMethod2(vm.Selectors, "value:value:", func(vmPtr interface{}, recv Value, arg1, arg2 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2})
	})

	c.AddMethod0(vm.Selectors, "whileTrue", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		for {
			result := v.evaluateBlock(recv, nil)
			if result != True {
				break
			}
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "whileTrue:", func(vmPtr interface{}, recv Value, body Value) Value {
		v := vmPtr.(*VM)
		for {
			cond := v.evaluateBlock(recv, nil)
			if cond != True {
				break
			}
			v.evaluateBlock(body, nil)
		}
		return Nil
	})

	c.AddMethod1(vm.Selectors, "whileFalse:", func(vmPtr interface{}, recv Value, body Value) Value {
		v := vmPtr.(*VM)
		for {
			cond := v.evaluateBlock(recv, nil)
			if cond == True {
				break
			}
			v.evaluateBlock(body, nil)
		}
		return Nil
	})
}

// ---------------------------------------------------------------------------
// Block evaluation helper
// ---------------------------------------------------------------------------

func (vm *VM) evaluateBlock(blockVal Value, args []Value) Value {
	// Get block from registry
	if bv := vm.interpreter.getBlockValue(blockVal); bv != nil {
		return vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, args)
	}
	return Nil
}

// ---------------------------------------------------------------------------
// Class lookup for values
// ---------------------------------------------------------------------------

func (vm *VM) primitiveClass(v Value) Value {
	var className string
	switch {
	case v == Nil:
		className = "UndefinedObject"
	case v == True:
		className = "True"
	case v == False:
		className = "False"
	case v.IsSmallInt():
		className = "SmallInteger"
	case v.IsFloat():
		className = "Float"
	case v.IsSymbol():
		className = "Symbol"
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			className = obj.VTablePtr().Class().Name
		} else {
			className = "Object"
		}
	default:
		className = "Object"
	}
	return vm.Symbols.SymbolValue(className)
}

// ClassFor returns the class for a value.
func (vm *VM) ClassFor(v Value) *Class {
	switch {
	case v == Nil:
		return vm.UndefinedObjectClass
	case v == True:
		return vm.TrueClass
	case v == False:
		return vm.FalseClass
	case v.IsSmallInt():
		return vm.SmallIntegerClass
	case v.IsFloat():
		return vm.FloatClass
	case v.IsSymbol():
		return vm.SymbolClass
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			return obj.VTablePtr().Class()
		}
		return vm.ObjectClass
	default:
		return vm.ObjectClass
	}
}

// ---------------------------------------------------------------------------
// Public execution API
// ---------------------------------------------------------------------------

// Execute runs a compiled method with the given receiver and arguments.
func (vm *VM) Execute(method *CompiledMethod, receiver Value, args []Value) Value {
	return vm.interpreter.Execute(method, receiver, args)
}

// Send sends a message to a receiver.
func (vm *VM) Send(receiver Value, selector string, args []Value) Value {
	selectorID := vm.Selectors.Intern(selector)
	class := vm.ClassFor(receiver)
	if class == nil {
		return Nil
	}

	method := class.VTable.Lookup(selectorID)
	if method == nil {
		return Nil // Would trigger doesNotUnderstand:
	}

	// Check if it's a compiled method or primitive
	if cm, ok := method.(*CompiledMethod); ok {
		return vm.interpreter.Execute(cm, receiver, args)
	}

	// Primitive method - pass VM as the vm parameter
	return method.Invoke(vm, receiver, args)
}

// Intern returns the symbol ID for a name.
func (vm *VM) Intern(name string) uint32 {
	return vm.Symbols.Intern(name)
}

// SymbolName returns the name for a symbol ID.
func (vm *VM) SymbolName(id uint32) string {
	return vm.Symbols.Name(id)
}

// LookupGlobal returns a global value by name.
func (vm *VM) LookupGlobal(name string) (Value, bool) {
	v, ok := vm.Globals[name]
	return v, ok
}

// SetGlobal sets a global value.
func (vm *VM) SetGlobal(name string, value Value) {
	vm.Globals[name] = value
}

// LookupClass returns a class by name.
func (vm *VM) LookupClass(name string) *Class {
	return vm.Classes.Lookup(name)
}
