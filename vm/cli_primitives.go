package vm

import (
	"fmt"
	"io"

	"github.com/spf13/cobra"
)

// ---------------------------------------------------------------------------
// Cli::Command — Maggie bridge for spf13/cobra
//
// This file exposes cobra's *Command type to Maggie code so applications can
// build Unix-style CLI tools (subcommands, flags, help text) directly from
// Maggie. The Go side owns the wrapper struct; Maggie holds a NaN-boxed
// handle (cliCommandMarker, see vm/markers.go).
//
// The wrapper retains a back-pointer to the VM so the cobra.Run callback can
// evaluate the Maggie run-block synchronously on the goroutine that invoked
// Execute. Arguments are marshaled to Array<String> using the same shape
// System args uses (vm/system_primitives.go).
//
// Error / exit-code convention (see story:maggie-cli-framework):
//   - SmallInteger return from run block → exit code
//   - Unhandled exception in run block   → print "Error: <reason>" to stderr,
//                                          caller sees exit code 1 from execute
//   - Otherwise                           → exit code 0
// ---------------------------------------------------------------------------

// CliCommandWrapper bundles a *cobra.Command with the VM state needed to run
// its callback block from Go.
type CliCommandWrapper struct {
	Cobra    *cobra.Command
	RunBlock Value // Maggie block, evaluated when cobra Run fires (Nil if unset)
	VM       *VM   // back-pointer used to evaluate RunBlock
	exitCode int   // set by run block, consumed by execute
}

// ---------------------------------------------------------------------------
// NaN-boxing / registry helpers
// ---------------------------------------------------------------------------

func cliCommandToValue(id uint32) Value {
	return FromSymbolID(id | cliCommandMarker)
}

func isCliCommandValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	return (v.SymbolID() & markerMask) == cliCommandMarker
}

func cliCommandIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^uint32(0xFF<<24)
}

func (vm *VM) vmGetCliCommand(v Value) *CliCommandWrapper {
	if !isCliCommandValue(v) {
		return nil
	}
	return vm.registry.GetCliCommand(cliCommandIDFromValue(v))
}

func (vm *VM) vmRegisterCliCommand(w *CliCommandWrapper) Value {
	id := vm.registry.RegisterCliCommand(w)
	return cliCommandToValue(id)
}

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerCliPrimitives() {
	// Cli::Command — namespaced class, matches lib/Cli/Command.mag
	cliCommandClass := NewClassInNamespace("Cli", "Command", vm.ObjectClass)
	vm.Classes.Register(cliCommandClass)
	vm.Globals["Cli::Command"] = vm.classValue(cliCommandClass)
	vm.symbolDispatch.Register(cliCommandMarker, &SymbolTypeEntry{Class: cliCommandClass})

	// Cli::CliError exception class — raised by execute if cobra returns an error
	cliErrorClass := NewClassInNamespace("Cli", "CliError", vm.ErrorClass)
	vm.Classes.Register(cliErrorClass)
	vm.Globals["Cli::CliError"] = vm.classValue(cliErrorClass)

	// ---------------------------------------------------------------------
	// Class-side constructor: Cli::Command new: useString
	// ---------------------------------------------------------------------
	cliCommandClass.AddClassMethod1(vm.Selectors, "new:", func(vmPtr interface{}, recv Value, useVal Value) Value {
		v := vmPtr.(*VM)
		use := v.valueToString(useVal)
		w := &CliCommandWrapper{
			Cobra:    &cobra.Command{Use: use},
			RunBlock: Nil,
			VM:       v,
		}
		return v.vmRegisterCliCommand(w)
	})

	// ---------------------------------------------------------------------
	// Documentation setters
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod1(vm.Selectors, "shortDoc:", func(vmPtr interface{}, recv Value, sVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		w.Cobra.Short = v.valueToString(sVal)
		return recv
	})

	cliCommandClass.AddMethod1(vm.Selectors, "longDoc:", func(vmPtr interface{}, recv Value, sVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		w.Cobra.Long = v.valueToString(sVal)
		return recv
	})

	// ---------------------------------------------------------------------
	// run: aBlock — stores the block; wires cobra.Run to call it.
	// Runs synchronously on the goroutine that invoked Execute.
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod1(vm.Selectors, "run:", func(vmPtr interface{}, recv Value, blockVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		w.RunBlock = blockVal

		// Capture wrapper so the closure always sees the latest run block.
		w.Cobra.Run = func(cmd *cobra.Command, args []string) {
			v.runCliBlock(w, args)
		}
		return recv
	})

	// ---------------------------------------------------------------------
	// addSubcommand: aCommand
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod1(vm.Selectors, "addSubcommand:", func(vmPtr interface{}, recv Value, childVal Value) Value {
		v := vmPtr.(*VM)
		parent := v.vmGetCliCommand(recv)
		child := v.vmGetCliCommand(childVal)
		if parent == nil || child == nil {
			return Nil
		}
		parent.Cobra.AddCommand(child.Cobra)
		return recv
	})

	// ---------------------------------------------------------------------
	// Flag declarations: addStringFlag:default:doc:, addBoolFlag:default:doc:,
	// addIntFlag:default:doc:
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod3(vm.Selectors, "addStringFlag:default:doc:", func(vmPtr interface{}, recv Value, nameVal, defVal, docVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		name := v.valueToString(nameVal)
		if name == "" {
			return v.signalException(cliErrorClass, v.registry.NewStringValue("addStringFlag: requires a non-empty name"))
		}
		w.Cobra.Flags().String(name, v.valueToString(defVal), v.valueToString(docVal))
		return recv
	})

	cliCommandClass.AddMethod3(vm.Selectors, "addBoolFlag:default:doc:", func(vmPtr interface{}, recv Value, nameVal, defVal, docVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		name := v.valueToString(nameVal)
		if name == "" {
			return v.signalException(cliErrorClass, v.registry.NewStringValue("addBoolFlag: requires a non-empty name"))
		}
		def := defVal == True
		w.Cobra.Flags().Bool(name, def, v.valueToString(docVal))
		return recv
	})

	cliCommandClass.AddMethod3(vm.Selectors, "addIntFlag:default:doc:", func(vmPtr interface{}, recv Value, nameVal, defVal, docVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		name := v.valueToString(nameVal)
		if name == "" {
			return v.signalException(cliErrorClass, v.registry.NewStringValue("addIntFlag: requires a non-empty name"))
		}
		def := 0
		if defVal.IsSmallInt() {
			def = int(defVal.SmallInt())
		}
		w.Cobra.Flags().Int(name, def, v.valueToString(docVal))
		return recv
	})

	// ---------------------------------------------------------------------
	// Flag accessors: stringFlag:, boolFlag:, intFlag:
	// Intended to be called from within the run block.
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod1(vm.Selectors, "stringFlag:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		name := v.valueToString(nameVal)
		val, err := w.Cobra.Flags().GetString(name)
		if err != nil {
			return v.signalException(cliErrorClass, v.registry.NewStringValue("stringFlag: "+err.Error()))
		}
		return v.registry.NewStringValue(val)
	})

	cliCommandClass.AddMethod1(vm.Selectors, "boolFlag:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		name := v.valueToString(nameVal)
		val, err := w.Cobra.Flags().GetBool(name)
		if err != nil {
			return v.signalException(cliErrorClass, v.registry.NewStringValue("boolFlag: "+err.Error()))
		}
		if val {
			return True
		}
		return False
	})

	cliCommandClass.AddMethod1(vm.Selectors, "intFlag:", func(vmPtr interface{}, recv Value, nameVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		name := v.valueToString(nameVal)
		val, err := w.Cobra.Flags().GetInt(name)
		if err != nil {
			return v.signalException(cliErrorClass, v.registry.NewStringValue("intFlag: "+err.Error()))
		}
		return FromSmallInt(int64(val))
	})

	// ---------------------------------------------------------------------
	// execute — invoke cobra.Execute and convert the return to an exit code.
	// Returns the exit code as a SmallInteger. Raises Cli::CliError on
	// cobra parsing/routing errors.
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod0(vm.Selectors, "execute", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		code, err := v.executeCliCommand(w)
		if err != nil {
			return v.signalException(cliErrorClass, v.registry.NewStringValue(err.Error()))
		}
		return FromSmallInt(int64(code))
	})

	// ---------------------------------------------------------------------
	// setOutput: aStream — redirects cobra's Out and Err writers.
	// Accepts a GoObject wrapping an io.Writer (e.g. a *bytes.Buffer) for
	// tests. Passing Nil restores the defaults.
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod1(vm.Selectors, "setOutput:", func(vmPtr interface{}, recv Value, streamVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		if streamVal == Nil {
			w.Cobra.SetOut(nil)
			w.Cobra.SetErr(nil)
			return recv
		}
		writer, ok := v.cliResolveWriter(streamVal)
		if !ok {
			return v.signalException(cliErrorClass,
				v.registry.NewStringValue("setOutput: expects a GoObject wrapping an io.Writer"))
		}
		w.Cobra.SetOut(writer)
		w.Cobra.SetErr(writer)
		return recv
	})

	// ---------------------------------------------------------------------
	// setArgs: anArray — primarily for tests to simulate command-line
	// invocation without touching os.Args.
	// ---------------------------------------------------------------------
	cliCommandClass.AddMethod1(vm.Selectors, "setArgs:", func(vmPtr interface{}, recv Value, arrVal Value) Value {
		v := vmPtr.(*VM)
		w := v.vmGetCliCommand(recv)
		if w == nil {
			return Nil
		}
		w.Cobra.SetArgs(v.valueToStringArray(arrVal))
		return recv
	})
}

// ---------------------------------------------------------------------------
// Block invocation + exit-code convention
// ---------------------------------------------------------------------------

// runCliBlock evaluates the wrapper's run block with the cobra args marshaled
// to an Array<String>. Exit-code handling lives here because cobra's Run
// signature returns no error — we must stash the outcome on the wrapper for
// executeCliCommand to read.
func (vm *VM) runCliBlock(w *CliCommandWrapper, args []string) {
	if w.RunBlock == Nil {
		w.exitCode = 0
		return
	}

	// Marshal args to Maggie Array<String> — same shape as System args.
	elements := make([]Value, len(args))
	for i, a := range args {
		elements[i] = vm.registry.NewStringValue(a)
	}
	argsVal := vm.NewArrayWithElements(elements)

	// Evaluate the block inside a defer/recover so unhandled exceptions
	// become exit code 1 instead of taking down the process.
	defer func() {
		if r := recover(); r != nil {
			if sigEx, ok := r.(SignaledException); ok {
				reason := vm.valueToString(sigEx.Object.MessageText)
				if reason == "" {
					reason = "unhandled exception"
				}
				fmt.Fprintln(w.Cobra.ErrOrStderr(), "Error: "+reason)
				w.exitCode = 1
				return
			}
			// Raw panics surface as stderr messages too.
			fmt.Fprintf(w.Cobra.ErrOrStderr(), "Error: %v\n", r)
			w.exitCode = 1
		}
	}()

	result := vm.evaluateBlock(w.RunBlock, []Value{argsVal})

	// If the block returns a SmallInteger, treat as the exit code.
	// Otherwise exit code 0.
	if result.IsSmallInt() {
		w.exitCode = int(result.SmallInt())
	} else {
		w.exitCode = 0
	}
}

// executeCliCommand invokes cobra.Command.Execute and returns the combined
// exit code. Cobra errors (bad flags, unknown subcommand) are surfaced as a
// Go error so the caller can raise Cli::CliError.
//
// We use cobra.ExecuteC() to get the matched *cobra.Command back so a parent
// command can report the exit code set by the child whose Run fired. Wrappers
// are found via the VM's lookup map (cobra.Command → CliCommandWrapper).
func (vm *VM) executeCliCommand(w *CliCommandWrapper) (int, error) {
	// Reset exit code in case execute is called repeatedly.
	w.exitCode = 0

	ran, err := w.Cobra.ExecuteC()
	if err != nil {
		return 1, err
	}
	// Look up the wrapper that actually ran and surface its exit code.
	if effective := vm.cliWrapperForCobra(ran); effective != nil {
		return effective.exitCode, nil
	}
	return w.exitCode, nil
}

// cliWrapperForCobra finds the CliCommandWrapper whose Cobra field matches
// the given *cobra.Command. Scans the registry linearly — fine because CLI
// apps have tens, not thousands, of commands.
func (vm *VM) cliWrapperForCobra(c *cobra.Command) *CliCommandWrapper {
	if c == nil {
		return nil
	}
	var match *CliCommandWrapper
	vm.registry.cliCommands.ForEach(func(_ uint32, w *CliCommandWrapper) {
		if w.Cobra == c {
			match = w
		}
	})
	return match
}

// cliResolveWriter unpacks an io.Writer from a GoObject-wrapped Maggie value.
// Accepts any Go value that satisfies io.Writer (e.g. *bytes.Buffer,
// *os.File). Returns (writer, true) on success.
func (vm *VM) cliResolveWriter(v Value) (io.Writer, bool) {
	goVal, ok := vm.GetGoObject(v)
	if !ok {
		return nil, false
	}
	w, ok := goVal.(io.Writer)
	return w, ok
}

