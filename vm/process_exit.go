package vm

// ExitReason describes why a process terminated.
type ExitReason struct {
	Normal         bool   // true if the process returned normally
	Result         Value  // the return value (meaningful when Normal==true)
	Error          error  // Go-level error (panics, NLR escapes)
	Signal         string // "kill", "shutdown", "linked" for exit signals
	ExceptionValue Value  // the exception Value if the process died from a SignaledException
}

// ExitNormal creates a normal exit reason.
func ExitNormal(result Value) ExitReason {
	return ExitReason{Normal: true, Result: result}
}

// ExitError creates an error exit reason.
func ExitError(err error) ExitReason {
	return ExitReason{Normal: false, Error: err}
}

// ExitException creates an exit reason from a Maggie exception.
func ExitException(err error, exVal Value) ExitReason {
	return ExitReason{Normal: false, Error: err, ExceptionValue: exVal}
}

// ExitSignal creates a signal-based exit reason (for link propagation).
func ExitSignal(signal string, originResult Value) ExitReason {
	return ExitReason{Normal: false, Signal: signal, Result: originResult}
}
