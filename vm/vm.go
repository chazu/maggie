package vm

import (
	"strconv"
)

// ---------------------------------------------------------------------------
// VM: The Maggie Virtual Machine
// ---------------------------------------------------------------------------

// VM is the main Maggie virtual machine.
type VM struct {
	// Global tables
	Selectors *SelectorTable // selector name -> ID
	Symbols   *SymbolTable   // symbol name -> ID
	Classes   *ClassTable    // class name -> Class
	Traits    *TraitTable    // trait name -> Trait
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
	ChannelClass           *Class
	ProcessClass           *Class
	ResultClass            *Class
	SuccessClass           *Class
	FailureClass           *Class

	// Interpreter
	interpreter *Interpreter

	// Compiler backend (Go or Maggie)
	compilerBackend CompilerBackend

	// keepAlive holds references to objects to prevent GC
	keepAlive []*Object
}

// NewVM creates and bootstraps a new VM.
func NewVM() *VM {
	vm := &VM{
		Selectors: NewSelectorTable(),
		Symbols:   NewSymbolTable(),
		Classes:   NewClassTable(),
		Traits:    NewTraitTable(),
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
	interp.Symbols = vm.Symbols
	interp.Classes = vm.Classes
	interp.Globals = vm.Globals
	interp.vm = vm // Back-reference for primitives
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

	// Phase 5b: Create concurrency classes
	vm.ChannelClass = vm.createClass("Channel", vm.ObjectClass)
	vm.ProcessClass = vm.createClass("Process", vm.ObjectClass)

	// Phase 5c: Create Result pattern classes
	vm.ResultClass = vm.createClass("Result", vm.ObjectClass)
	vm.SuccessClass = vm.createClass("Success", vm.ResultClass)
	vm.FailureClass = vm.createClass("Failure", vm.ResultClass)

	// Phase 6: Register primitives on core classes
	vm.registerObjectPrimitives()
	vm.registerBooleanPrimitives()
	vm.registerSmallIntegerPrimitives()
	vm.registerFloatPrimitives()
	vm.registerSymbolPrimitives()
	vm.registerStringPrimitives()
	vm.registerArrayPrimitives()
	vm.registerBlockPrimitives()
	vm.registerChannelPrimitives()
	vm.registerProcessPrimitives()
	vm.registerResultPrimitives()
	vm.registerDictionaryPrimitives()

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
	vm.Globals["Channel"] = vm.classValue(vm.ChannelClass)
	vm.Globals["Process"] = vm.classValue(vm.ProcessClass)
	vm.Globals["Result"] = vm.classValue(vm.ResultClass)
	vm.Globals["Success"] = vm.classValue(vm.SuccessClass)
	vm.Globals["Failure"] = vm.classValue(vm.FailureClass)
	vm.Globals["Dictionary"] = vm.classValue(vm.DictionaryClass)

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

	// new - create a new instance (class-side primitive)
	// This is a class method - registered on ClassVTable
	// When sent to a symbol representing a class, creates an instance of that class
	_ = vm.Selectors.Intern("new") // Ensure "new" selector is interned
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// If receiver is a symbol representing a class, create an instance
		if recv.IsSymbol() {
			symName := v.Symbols.Name(recv.SymbolID())
			if cls := v.Classes.Lookup(symName); cls != nil {
				instance := cls.NewInstance()
				// Keep a reference to prevent GC - Value is just uint64 to Go
				v.keepAlive = append(v.keepAlive, instance)
				return instance.ToValue()
			}
		}
		// Otherwise, just return self (for instance-side call)
		return recv
	})

	// basicNew - create a new instance without initialization (class-side primitive)
	// Same as new, but in Smalltalk convention, basicNew is the raw allocator
	c.AddClassMethod0(vm.Selectors, "basicNew", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		// If receiver is a symbol representing a class, create an instance
		if recv.IsSymbol() {
			symName := v.Symbols.Name(recv.SymbolID())
			if cls := v.Classes.Lookup(symName); cls != nil {
				instance := cls.NewInstance()
				v.keepAlive = append(v.keepAlive, instance)
				return instance.ToValue()
			}
		}
		return recv
	})

	// class - return the class of the receiver
	c.AddMethod0(vm.Selectors, "class", func(_ interface{}, recv Value) Value {
		return vm.primitiveClass(recv)
	})

	// primClass - same as class, but a primitive that Maggie code can call
	c.AddMethod0(vm.Selectors, "primClass", func(_ interface{}, recv Value) Value {
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

// ReRegisterNilPrimitives forces re-registration of nil-related primitives.
// Call this after loading an image to ensure primitives override any compiled methods.
func (vm *VM) ReRegisterNilPrimitives() {
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

	c.AddMethod1(vm.Selectors, "bitShift:", func(_ interface{}, recv Value, arg Value) Value {
		if arg.IsSmallInt() {
			shift := arg.SmallInt()
			if shift >= 0 {
				return FromSmallInt(recv.SmallInt() << uint(shift))
			}
			return FromSmallInt(recv.SmallInt() >> uint(-shift))
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

	// Printing
	c.AddMethod0(vm.Selectors, "primPrintString", func(_ interface{}, recv Value) Value {
		return NewStringValue(strconv.FormatInt(recv.SmallInt(), 10))
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

	c.AddMethod2(vm.Selectors, "to:do:", func(vmPtr interface{}, recv Value, stop Value, block Value) Value {
		v := vmPtr.(*VM)
		start := recv.SmallInt()
		end := stop.SmallInt()
		for i := start; i <= end; i++ {
			v.evaluateBlock(block, []Value{FromSmallInt(i)})
		}
		return recv
	})

	c.AddMethod3(vm.Selectors, "to:by:do:", func(vmPtr interface{}, recv Value, stop Value, step Value, block Value) Value {
		v := vmPtr.(*VM)
		start := recv.SmallInt()
		end := stop.SmallInt()
		stepVal := step.SmallInt()
		if stepVal > 0 {
			for i := start; i <= end; i += stepVal {
				v.evaluateBlock(block, []Value{FromSmallInt(i)})
			}
		} else if stepVal < 0 {
			for i := start; i >= end; i += stepVal {
				v.evaluateBlock(block, []Value{FromSmallInt(i)})
			}
		}
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

	// asString - convert symbol to string (returns actual string)
	c.AddMethod0(vm.Selectors, "asString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			return NewStringValue(name)
		}
		return Nil
	})

	// primAsString - same as asString, for compatibility with Symbol.mag
	c.AddMethod0(vm.Selectors, "primAsString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		if recv.IsSymbol() {
			name := v.Symbols.Name(recv.SymbolID())
			return NewStringValue(name)
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
	// Register the extended string primitives from string_primitives.go
	vm.registerStringPrimitivesExtended()
}

func (vm *VM) registerArrayPrimitives() {
	c := vm.ArrayClass

	// primSize - return array size
	c.AddMethod0(vm.Selectors, "primSize", func(_ interface{}, recv Value) Value {
		if recv.IsObject() {
			obj := ObjectFromValue(recv)
			if obj != nil {
				return FromSmallInt(int64(obj.NumSlots()))
			}
		}
		return FromSmallInt(0)
	})

	// size - alias for primSize for direct calls
	c.AddMethod0(vm.Selectors, "size", func(_ interface{}, recv Value) Value {
		if recv.IsObject() {
			obj := ObjectFromValue(recv)
			if obj != nil {
				return FromSmallInt(int64(obj.NumSlots()))
			}
		}
		return FromSmallInt(0)
	})

	// primAt: - array access (0-based indexing)
	c.AddMethod1(vm.Selectors, "primAt:", func(_ interface{}, recv Value, index Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return Nil
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return Nil // Bounds error - would raise in full implementation
		}
		return obj.GetSlot(int(idx))
	})

	// at: - alias for primAt: for direct calls
	c.AddMethod1(vm.Selectors, "at:", func(_ interface{}, recv Value, index Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return Nil
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return Nil
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return Nil // Bounds error - would raise in full implementation
		}
		return obj.GetSlot(int(idx))
	})

	// primAt:put: - array modification (0-based indexing)
	c.AddMethod2(vm.Selectors, "primAt:put:", func(_ interface{}, recv Value, index, value Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return value
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return value
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return value // Bounds error - would raise in full implementation
		}
		obj.SetSlot(int(idx), value)
		return value
	})

	// at:put: - alias for primAt:put: for direct calls
	c.AddMethod2(vm.Selectors, "at:put:", func(_ interface{}, recv Value, index, value Value) Value {
		if !recv.IsObject() || !index.IsSmallInt() {
			return value
		}
		obj := ObjectFromValue(recv)
		if obj == nil {
			return value
		}
		idx := index.SmallInt()
		if idx < 0 || idx >= int64(obj.NumSlots()) {
			return value // Bounds error - would raise in full implementation
		}
		obj.SetSlot(int(idx), value)
		return value
	})

	// Class-side new - create empty array
	c.AddClassMethod0(vm.Selectors, "new", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.NewArray(0)
	})

	// Class-side new: - create array of given size
	// This is a class method - registered on ClassVTable
	c.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, recv Value, size Value) Value {
		v := vmPtr.(*VM)
		if !size.IsSmallInt() {
			return Nil
		}
		n := size.SmallInt()
		if n < 0 {
			return Nil
		}
		// Create array object with n slots
		return v.NewArray(int(n))
	})

	// with: - create single-element array (class side)
	c.AddClassMethod1(vm.Selectors, "with:", func(vmPtr interface{}, recv Value, elem Value) Value {
		v := vmPtr.(*VM)
		arr := v.NewArray(1)
		if arr.IsObject() {
			obj := ObjectFromValue(arr)
			obj.SetSlot(0, elem)
		}
		return arr
	})

	// with:with: - create two-element array (class side)
	c.AddClassMethod2(vm.Selectors, "with:with:", func(vmPtr interface{}, recv Value, elem1, elem2 Value) Value {
		v := vmPtr.(*VM)
		arr := v.NewArray(2)
		if arr.IsObject() {
			obj := ObjectFromValue(arr)
			obj.SetSlot(0, elem1)
			obj.SetSlot(1, elem2)
		}
		return arr
	})
}

// NewArray creates a new Array object with the given number of slots.
// All slots are initialized to Nil.
func (vm *VM) NewArray(size int) Value {
	if vm.ArrayClass == nil || vm.ArrayClass.VTable == nil {
		return Nil
	}
	obj := NewObject(vm.ArrayClass.VTable, size)
	obj.SetSize(size) // Set the logical size for arrays
	val := obj.ToValue()
	// Keep a reference to prevent GC
	vm.keepAlive = append(vm.keepAlive, obj)
	return val
}

// NewArrayWithElements creates a new Array object with the given elements.
func (vm *VM) NewArrayWithElements(elements []Value) Value {
	if vm.ArrayClass == nil || vm.ArrayClass.VTable == nil {
		return Nil
	}
	obj := NewObjectWithSlots(vm.ArrayClass.VTable, elements)
	obj.SetSize(len(elements)) // Set the logical size for arrays
	val := obj.ToValue()
	// Keep a reference to prevent GC
	vm.keepAlive = append(vm.keepAlive, obj)
	return val
}

// NewDictionary creates a new empty Dictionary.
func (vm *VM) NewDictionary() Value {
	return NewDictionaryValue()
}

// DictionaryAtPut sets a key-value pair in a dictionary.
func (vm *VM) DictionaryAtPut(dict Value, key Value, value Value) {
	d := GetDictionaryObject(dict)
	if d == nil {
		return
	}
	h := hashValue(key)
	d.Data[h] = value
	d.Keys[h] = key
}

// DictionaryAt gets a value from a dictionary by key.
func (vm *VM) DictionaryAt(dict Value, key Value) Value {
	d := GetDictionaryObject(dict)
	if d == nil {
		return Nil
	}
	h := hashValue(key)
	if val, ok := d.Data[h]; ok {
		return val
	}
	return Nil
}

func (vm *VM) registerBlockPrimitives() {
	c := vm.BlockClass

	// Primitive evaluation methods (called by Block.mag's value, value:, etc.)
	c.AddMethod0(vm.Selectors, "primValue", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, nil)
	})

	c.AddMethod1(vm.Selectors, "primValue:", func(vmPtr interface{}, recv Value, arg Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg})
	})

	c.AddMethod2(vm.Selectors, "primValue:value:", func(vmPtr interface{}, recv Value, arg1, arg2 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2})
	})

	c.AddMethod3(vm.Selectors, "primValue:value:value:", func(vmPtr interface{}, recv Value, arg1, arg2, arg3 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2, arg3})
	})

	// Direct evaluation methods (for Go code calling blocks directly)
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

	c.AddMethod3(vm.Selectors, "value:value:value:", func(vmPtr interface{}, recv Value, arg1, arg2, arg3 Value) Value {
		v := vmPtr.(*VM)
		return v.evaluateBlock(recv, []Value{arg1, arg2, arg3})
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

	c.AddMethod0(vm.Selectors, "whileFalse", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		for {
			result := v.evaluateBlock(recv, nil)
			if result == True {
				break
			}
		}
		return Nil
	})
}

// ---------------------------------------------------------------------------
// Block evaluation helper
// ---------------------------------------------------------------------------

func (vm *VM) evaluateBlock(blockVal Value, args []Value) Value {
	// Get block from registry
	bv := vm.interpreter.getBlockValue(blockVal)
	if bv == nil {
// 		fmt.Printf("DEBUG evaluateBlock: blockVal=%v is not a valid block (IsBlock=%v)\n", blockVal, blockVal.IsBlock())
		return Nil
	}
	return vm.interpreter.ExecuteBlock(bv.Block, bv.Captures, args, bv.HomeFrame, bv.HomeSelf)
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
	case v.IsBlock():
		className = "Block"
	case IsStringValue(v):
		className = "String"
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
	case v.IsBlock():
		return vm.BlockClass
	case IsStringValue(v):
		return vm.StringClass
	case IsDictionaryValue(v):
		return vm.DictionaryClass
	case v.IsSymbol():
		return vm.SymbolClass
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil {
			vt := obj.VTablePtr()
			if vt != nil && vt.Class() != nil {
				return vt.Class()
			}
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

	// Determine the class for method dispatch
	var class *Class
	isClassSide := false // Track if this is a class-side dispatch
	if receiver.IsSymbol() {
		// Check for string values first (they use the symbol tag but with high IDs)
		if IsStringValue(receiver) {
			class = vm.StringClass
		} else if IsDictionaryValue(receiver) {
			class = vm.DictionaryClass
		} else if isChannelValue(receiver) {
			// Check for special symbol-encoded values (channels, processes, results)
			class = vm.ChannelClass
		} else if isProcessValue(receiver) {
			class = vm.ProcessClass
		} else if isResultValue(receiver) {
			// Determine if it's a Success or Failure
			r := getResult(receiver)
			if r != nil && r.resultType == ResultSuccess {
				class = vm.SuccessClass
			} else {
				class = vm.FailureClass
			}
		} else {
			// Check if this symbol represents a class name (for class-side messages)
			// This handles cases like: Channel new, Process sleep: 100
			symName := vm.Symbols.Name(receiver.SymbolID())
			if cls := vm.Classes.Lookup(symName); cls != nil {
				class = cls
				isClassSide = true // Use ClassVTable for class-side dispatch
			} else {
				class = vm.SymbolClass
			}
		}
	} else {
		class = vm.ClassFor(receiver)
	}

	if class == nil {
		return Nil
	}

	// Use ClassVTable for class-side dispatch, VTable for instance-side
	var method Method
	if isClassSide && class.ClassVTable != nil {
		method = class.ClassVTable.Lookup(selectorID)
	} else {
		method = class.VTable.Lookup(selectorID)
	}

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
