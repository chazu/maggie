package vm

import (
	"os"
	"strconv"
)

// ---------------------------------------------------------------------------
// System Primitives: pid, env, args, exit
// ---------------------------------------------------------------------------

func (vm *VM) registerSystemPrimitives() {
	sysClass := vm.createClass("System", vm.ObjectClass)
	vm.Globals["System"] = vm.classValue(sysClass)

	// System pid — returns os.Getpid() as SmallInt
	sysClass.AddClassMethod0(vm.Selectors, "pid", func(vmPtr interface{}, recv Value) Value {
		return FromSmallInt(int64(os.Getpid()))
	})

	// System env: key — returns os.Getenv(key) as String (empty string if unset)
	sysClass.AddClassMethod1(vm.Selectors, "env:", func(vmPtr interface{}, recv Value, keyVal Value) Value {
		v := vmPtr.(*VM)
		key := v.valueToString(keyVal)
		if key == "" {
			return v.newFailureResult("System env: requires a non-empty key string")
		}
		return v.registry.NewStringValue(os.Getenv(key))
	})

	// System args — returns os.Args[1:] as Array of Strings (skip binary name)
	sysClass.AddClassMethod0(vm.Selectors, "args", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		args := os.Args
		if len(args) > 1 {
			args = args[1:]
		} else {
			args = nil
		}
		elements := make([]Value, len(args))
		for i, a := range args {
			elements[i] = v.registry.NewStringValue(a)
		}
		return v.NewArrayWithElements(elements)
	})

	// System exit: code — calls os.Exit(code)
	sysClass.AddClassMethod1(vm.Selectors, "exit:", func(vmPtr interface{}, recv Value, codeVal Value) Value {
		if !codeVal.IsSmallInt() {
			os.Exit(1)
		}
		os.Exit(int(codeVal.SmallInt()))
		return Nil // unreachable
	})

	// System hostname — returns os.Hostname() as String
	sysClass.AddClassMethod0(vm.Selectors, "hostname", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		name, err := os.Hostname()
		if err != nil {
			return v.newFailureResult("System hostname: " + err.Error())
		}
		return v.registry.NewStringValue(name)
	})

	// System pidString — returns pid as a String (useful for file writing)
	sysClass.AddClassMethod0(vm.Selectors, "pidString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		return v.registry.NewStringValue(strconv.Itoa(os.Getpid()))
	})
}
