package vm

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"sync"
	"time"
)

// ---------------------------------------------------------------------------
// ExternalProcess: os/exec wrapper for Maggie
// ---------------------------------------------------------------------------

// ExternalProcessObject wraps a Go os/exec.Cmd for use in Maggie.
type ExternalProcessObject struct {
	command string
	args    []string
	env     map[string]string
	dir     string

	// Captured output after run/wait
	stdout   string
	stderr   string
	exitCode int
	err      error

	// For async processes
	cmd      *exec.Cmd
	started  bool
	done     bool
	finished chan struct{} // closed by finish(); non-nil once started
	mu       sync.Mutex    // guards every field above; never held across the child's run

	// For cancellation/timeout
	cancel context.CancelFunc
}

// begin launches the child described by p and marks p started. p.mu is held
// only while launching — never across the child's run — so kill and the
// accessors stay responsive. ctx/cancel are non-nil for runWithTimeout:.
// Answers a nil cmd if p was already started or the child failed to start
// (the failure is already recorded); otherwise the caller must c.Wait() and
// then finish().
func (p *ExternalProcessObject) begin(ctx context.Context, cancel context.CancelFunc) (c *exec.Cmd, stdout, stderr *bytes.Buffer) {
	p.mu.Lock()
	if p.started {
		p.mu.Unlock()
		return nil, nil, nil
	}
	if ctx != nil {
		c = exec.CommandContext(ctx, p.command, p.args...)
	} else {
		c = exec.Command(p.command, p.args...)
	}
	c.Env = buildEnv(p.env)
	if p.dir != "" {
		c.Dir = p.dir
	}
	stdout, stderr = &bytes.Buffer{}, &bytes.Buffer{}
	c.Stdout = stdout
	c.Stderr = stderr

	p.started = true
	p.finished = make(chan struct{})
	p.cancel = cancel
	if err := c.Start(); err != nil {
		p.mu.Unlock()
		p.finish("", "", err, false)
		return nil, nil, nil
	}
	p.cmd = c
	p.mu.Unlock()
	return c, stdout, stderr
}

// finish records the child's outcome and wakes wait. Caller must not hold p.mu.
func (p *ExternalProcessObject) finish(stdout, stderr string, err error, timedOut bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.stdout = stdout
	p.stderr = stderr
	p.done = true
	p.err = err
	switch {
	case err == nil:
		p.exitCode = 0
	case timedOut:
		p.exitCode = -1
		p.err = context.DeadlineExceeded
	default:
		p.exitCode = -1
		if exitErr, ok := err.(*exec.ExitError); ok {
			p.exitCode = exitErr.ExitCode()
		}
	}
	close(p.finished)
}

// ---------------------------------------------------------------------------
// Value encoding helpers
// ---------------------------------------------------------------------------

func isExtProcessValue(v Value) bool {
	return isExtensionValue(v, externalProcessMarker)
}

func (vm *VM) vmGetExtProcess(v Value) *ExternalProcessObject {
	if o := ExtensionObject(v, externalProcessMarker); o != nil {
		return o.(*ExternalProcessObject)
	}
	return nil
}

func (vm *VM) vmRegisterExtProcess(p *ExternalProcessObject) Value {
	return makeExtensionValue(externalProcessMarker, p)
}

// ---------------------------------------------------------------------------
// Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerExecPrimitives() {
	epClass := vm.createClass("ExternalProcess", vm.ObjectClass)
	vm.globals["ExternalProcess"] = vm.classValue(epClass)
	vm.symbolDispatch.Register(externalProcessMarker, &SymbolTypeEntry{Class: epClass})

	// -----------------------------------------------------------------------
	// Class methods (constructors)
	// -----------------------------------------------------------------------

	// command: cmdString — Create a process with a command string (no args)
	epClass.AddClassMethod1(vm.Selectors, "command:", func(v *VM, recv Value, cmdVal Value) Value {
		cmd := v.valueToString(cmdVal)
		if cmd == "" {
			return v.newFailureResult("command: requires a non-empty string")
		}
		p := &ExternalProcessObject{
			command:  cmd,
			exitCode: -1,
		}
		return v.vmRegisterExtProcess(p)
	})

	// command:args: — Create a process with command and arguments array
	epClass.AddClassMethod2(vm.Selectors, "command:args:", func(v *VM, recv Value, cmdVal, argsVal Value) Value {
		cmd := v.valueToString(cmdVal)
		if cmd == "" {
			return v.newFailureResult("command:args: requires a non-empty command string")
		}
		args := v.valueToStringArray(argsVal)
		p := &ExternalProcessObject{
			command:  cmd,
			args:     args,
			exitCode: -1,
		}
		return v.vmRegisterExtProcess(p)
	})

	// run:args: — Convenience: run command synchronously, return stdout string or Failure
	epClass.AddClassMethod2(vm.Selectors, "run:args:", func(v *VM, recv Value, cmdVal, argsVal Value) Value {
		cmd := v.valueToString(cmdVal)
		if cmd == "" {
			return v.newFailureResult("run:args: requires a non-empty command string")
		}
		args := v.valueToStringArray(argsVal)

		c := exec.Command(cmd, args...)
		c.Env = os.Environ()
		var stdout, stderr bytes.Buffer
		c.Stdout = &stdout
		c.Stderr = &stderr

		err := c.Run()
		if err != nil {
			exitCode := -1
			if exitErr, ok := err.(*exec.ExitError); ok {
				exitCode = exitErr.ExitCode()
			}
			reason := strings.TrimRight(stderr.String(), "\n")
			if reason == "" {
				reason = err.Error()
			}
			// Documented contract: stdout String or a Failure. The Failure
			// carries the exit code and stderr; callers needing structured
			// output use command:args: + run + exitCode/stdout/stderr.
			return v.newFailureResult(fmt.Sprintf("Process exited with code %d: %s", exitCode, reason))
		}

		return v.registry.NewStringValue(stdout.String())
	})

	// -----------------------------------------------------------------------
	// Instance methods (configuration — return self for chaining)
	// -----------------------------------------------------------------------

	// args: anArray — Set arguments
	epClass.AddMethod1(vm.Selectors, "args:", func(v *VM, recv Value, argsVal Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.args = v.valueToStringArray(argsVal)
		return recv
	})

	// env: aDictionary — Set environment variables (merged with inherited env)
	epClass.AddMethod1(vm.Selectors, "env:", func(v *VM, recv Value, envVal Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.env = v.valueToDictStringMap(envVal)
		return recv
	})

	// dir: aString — Set working directory
	epClass.AddMethod1(vm.Selectors, "dir:", func(v *VM, recv Value, dirVal Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.dir = v.valueToString(dirVal)
		return recv
	})

	// -----------------------------------------------------------------------
	// Instance methods (execution)
	// -----------------------------------------------------------------------

	// run — Run synchronously, return self (stdout/stderr/exitCode available after)
	epClass.AddMethod0(vm.Selectors, "run", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		if c, stdout, stderr := p.begin(nil, nil); c != nil {
			err := c.Wait()
			p.finish(stdout.String(), stderr.String(), err, false)
		}
		return recv
	})

	// runWithTimeout: milliseconds — Run synchronously with timeout
	epClass.AddMethod1(vm.Selectors, "runWithTimeout:", func(v *VM, recv Value, msVal Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		if !msVal.IsSmallInt() {
			return v.newFailureResult("runWithTimeout: requires an integer (milliseconds)")
		}
		ms := msVal.SmallInt()
		if ms <= 0 {
			return v.newFailureResult("runWithTimeout: requires a positive timeout")
		}

		ctx, cancel := context.WithTimeout(context.Background(), time.Duration(ms)*time.Millisecond)
		defer cancel()
		if c, stdout, stderr := p.begin(ctx, cancel); c != nil {
			err := c.Wait()
			p.finish(stdout.String(), stderr.String(), err, ctx.Err() == context.DeadlineExceeded)
		}
		return recv
	})

	// start — Start asynchronously, return self
	epClass.AddMethod0(vm.Selectors, "start", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		if c, stdout, stderr := p.begin(nil, nil); c != nil {
			// Wait in a goroutine to capture output
			go func() {
				err := c.Wait()
				p.finish(stdout.String(), stderr.String(), err, false)
			}()
		}
		return recv
	})

	// wait — Block until async process completes, return self. Waiting on a
	// process that was never started is a programmer error (it would block
	// forever), so it signals.
	epClass.AddMethod0(vm.Selectors, "wait", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		started, finished := p.started, p.finished
		p.mu.Unlock()
		if !started {
			return v.SignalPrimitiveError("wait", "process was never started (send start first)")
		}
		<-finished
		return recv
	})

	// kill — Kill the running process
	epClass.AddMethod0(vm.Selectors, "kill", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		defer p.mu.Unlock()

		if p.cancel != nil {
			p.cancel()
		}
		if p.cmd != nil && p.cmd.Process != nil && !p.done {
			p.cmd.Process.Kill()
		}
		return recv
	})

	// -----------------------------------------------------------------------
	// Instance methods (result accessors)
	// -----------------------------------------------------------------------

	// stdout — Return captured stdout as string
	epClass.AddMethod0(vm.Selectors, "stdout", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		out := p.stdout
		p.mu.Unlock()
		return v.registry.NewStringValue(out)
	})

	// stderr — Return captured stderr as string
	epClass.AddMethod0(vm.Selectors, "stderr", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		errOut := p.stderr
		p.mu.Unlock()
		return v.registry.NewStringValue(errOut)
	})

	// exitCode — Return exit code (integer, -1 if not yet run or error)
	epClass.AddMethod0(vm.Selectors, "exitCode", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		code := p.exitCode
		p.mu.Unlock()
		return FromSmallInt(int64(code))
	})

	// isSuccess — Return true if exit code is 0
	epClass.AddMethod0(vm.Selectors, "isSuccess", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return False
		}
		p.mu.Lock()
		ok := p.done && p.exitCode == 0
		p.mu.Unlock()
		if ok {
			return True
		}
		return False
	})

	// isDone — Return true if process has completed
	epClass.AddMethod0(vm.Selectors, "isDone", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return False
		}
		p.mu.Lock()
		done := p.done
		p.mu.Unlock()
		if done {
			return True
		}
		return False
	})

	// command — Return the command string
	epClass.AddMethod0(vm.Selectors, "command", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		return v.registry.NewStringValue(p.command)
	})

	// printString — Return a string representation
	epClass.AddMethod0(vm.Selectors, "printString", func(v *VM, recv Value) Value {
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return v.registry.NewStringValue("an ExternalProcess")
		}
		desc := "an ExternalProcess(" + p.command + ")"
		return v.registry.NewStringValue(desc)
	})
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// buildEnv merges the parent environment with overrides.
func buildEnv(overrides map[string]string) []string {
	if len(overrides) == 0 {
		return os.Environ()
	}
	env := os.Environ()
	// Build map of existing env for override
	existing := make(map[string]int, len(env))
	for i, e := range env {
		if idx := strings.IndexByte(e, '='); idx >= 0 {
			existing[e[:idx]] = i
		}
	}
	for k, val := range overrides {
		if idx, ok := existing[k]; ok {
			env[idx] = k + "=" + val
		} else {
			env = append(env, k+"="+val)
		}
	}
	return env
}

// valueToStringArray converts an Array Value to a []string. String and Symbol
// elements are kept even when empty — the empty string is a legitimate argv entry, and
// dropping it would silently shift the remaining arguments. Elements of any
// other class are skipped.
func (vm *VM) valueToStringArray(v Value) []string {
	arr := vm.getArrayValue(v)
	if arr == nil {
		return nil
	}
	result := make([]string, 0, len(arr))
	for _, elem := range arr {
		if IsStringValue(elem) || elem.IsSymbol() {
			result = append(result, vm.valueToString(elem))
		}
	}
	return result
}

// valueToDictStringMap converts a Dictionary Value to a map[string]string.
func (vm *VM) valueToDictStringMap(v Value) map[string]string {
	dictObj := vm.registry.GetDictionaryObject(v)
	if dictObj == nil {
		return nil
	}
	entries := dictObj.Entries()
	result := make(map[string]string, len(entries))
	for _, e := range entries {
		k := vm.valueToString(e.Key)
		vStr := vm.valueToString(e.Value)
		if k != "" {
			result[k] = vStr
		}
	}
	return result
}
