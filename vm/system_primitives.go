package vm

import (
	"os"
	"strconv"
	"time"
)

// ---------------------------------------------------------------------------
// System Primitives: pid, env, args, exit
// ---------------------------------------------------------------------------

func (vm *VM) registerSystemPrimitives() {
	sysClass := vm.createClass("System", vm.ObjectClass)
	vm.globals["System"] = vm.classValue(sysClass)

	// System pid — returns os.Getpid() as SmallInt
	sysClass.AddClassMethod0(vm.Selectors, "pid", func(v *VM, recv Value) Value {
		return FromSmallInt(int64(os.Getpid()))
	})

	// System env: key — returns os.Getenv(key) as String (empty string if unset)
	sysClass.AddClassMethod1(vm.Selectors, "env:", func(v *VM, recv Value, keyVal Value) Value {
		key := v.valueToString(keyVal)
		if key == "" {
			return v.newFailureResult("System env: requires a non-empty key string")
		}
		return v.registry.NewStringValue(os.Getenv(key))
	})

	// System args — returns os.Args[1:] as Array of Strings (skip binary name)
	sysClass.AddClassMethod0(vm.Selectors, "args", func(v *VM, recv Value) Value {
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
	sysClass.AddClassMethod1(vm.Selectors, "exit:", func(v *VM, recv Value, codeVal Value) Value {
		if !codeVal.IsSmallInt() {
			os.Exit(1)
		}
		os.Exit(int(codeVal.SmallInt()))
		return Nil // unreachable
	})

	// System hostname — returns os.Hostname() as String
	sysClass.AddClassMethod0(vm.Selectors, "hostname", func(v *VM, recv Value) Value {
		name, err := os.Hostname()
		if err != nil {
			return v.newFailureResult("System hostname: " + err.Error())
		}
		return v.registry.NewStringValue(name)
	})

	// System pidString — returns pid as a String (useful for file writing)
	sysClass.AddClassMethod0(vm.Selectors, "pidString", func(v *VM, recv Value) Value {
		return v.registry.NewStringValue(strconv.Itoa(os.Getpid()))
	})

	// System executable — returns os.Args[0] (the binary path)
	sysClass.AddClassMethod0(vm.Selectors, "executable", func(v *VM, recv Value) Value {
		if len(os.Args) > 0 {
			return v.registry.NewStringValue(os.Args[0])
		}
		return v.registry.NewStringValue("pp")
	})

	// System epochSeconds — returns current Unix timestamp as SmallInt
	sysClass.AddClassMethod0(vm.Selectors, "epochSeconds", func(v *VM, recv Value) Value {
		return FromSmallInt(time.Now().Unix())
	})
}
