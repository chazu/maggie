package vm

import (
	"os"
	"os/signal"
	"strings"
	"syscall"
)

// ---------------------------------------------------------------------------
// Signal Primitives: trap OS signals and deliver to Maggie channels
// ---------------------------------------------------------------------------

func (vm *VM) registerSignalPrimitives() {
	sigClass := vm.createClass("Signal", vm.ObjectClass)
	vm.globals["Signal"] = vm.classValue(sigClass)

	// Signal trap: signalName do: aChannel
	// Registers a handler that sends the signal name (as a Symbol) to the channel.
	// signalName is a String like 'SIGTERM', 'SIGINT', 'SIGHUP'.
	// Returns the receiver (Signal class).
	sigClass.AddClassMethod2(vm.Selectors, "trap:toChannel:", func(v *VM, recv Value, nameVal, chVal Value) Value {
		name := v.valueToString(nameVal)
		if name == "" {
			return v.newFailureResult("Signal trap:toChannel: requires a signal name string")
		}
		ch := v.vmGetChannel(chVal)
		if ch == nil {
			return v.newFailureResult("Signal trap:toChannel: requires a Channel")
		}

		sig := parseSignalName(name)
		if sig == nil {
			return v.newFailureResult("Signal trap:toChannel: unknown signal: " + name)
		}

		goCh := make(chan os.Signal, 1)
		signal.Notify(goCh, sig)

		sigSymVal := v.Symbols.SymbolValue(strings.ToUpper(name))

		go func() {
			for range goCh {
				// safeSend instead of a raw send: a closed channel stops
				// delivery instead of panicking this goroutine.
				if !ch.safeSend(sigSymVal) {
					return
				}
			}
		}()

		return recv
	})

	// Signal trapAll: anArray toChannel: aChannel
	// Like trap:toChannel: but for multiple signals at once.
	// anArray is an Array of signal name Strings.
	sigClass.AddClassMethod2(vm.Selectors, "trapAll:toChannel:", func(v *VM, recv Value, arrayVal, chVal Value) Value {
		ch := v.vmGetChannel(chVal)
		if ch == nil {
			return v.newFailureResult("Signal trapAll:toChannel: requires a Channel")
		}

		names := v.valueToStringArray(arrayVal)
		if len(names) == 0 {
			return v.newFailureResult("Signal trapAll:toChannel: requires a non-empty array of signal names")
		}

		var sigs []os.Signal
		for _, name := range names {
			sig := parseSignalName(name)
			if sig == nil {
				return v.newFailureResult("Signal trapAll:toChannel: unknown signal: " + name)
			}
			sigs = append(sigs, sig)
		}

		goCh := make(chan os.Signal, 1)
		signal.Notify(goCh, sigs...)

		go func() {
			for s := range goCh {
				sigName := strings.ToUpper(s.String())
				// Normalize to match input format
				if !strings.HasPrefix(sigName, "SIG") {
					sigName = "SIG" + sigName
				}
				symVal := v.Symbols.SymbolValue(sigName)
				if !ch.safeSend(symVal) {
					return
				}
			}
		}()

		return recv
	})
}

// parseSignalName converts a signal name string to an os.Signal.
func parseSignalName(name string) os.Signal {
	upper := strings.ToUpper(strings.TrimSpace(name))
	// Strip "SIG" prefix if present for matching
	bare := strings.TrimPrefix(upper, "SIG")

	switch bare {
	case "TERM":
		return syscall.SIGTERM
	case "INT":
		return syscall.SIGINT
	case "HUP":
		return syscall.SIGHUP
	case "QUIT":
		return syscall.SIGQUIT
	case "USR1":
		return syscall.SIGUSR1
	case "USR2":
		return syscall.SIGUSR2
	case "PIPE":
		return syscall.SIGPIPE
	default:
		return nil
	}
}
