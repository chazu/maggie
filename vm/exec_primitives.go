package vm

import (
	"bytes"
	"context"
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
	cmd     *exec.Cmd
	started bool
	done    bool
	mu      sync.Mutex

	// For cancellation/timeout
	cancel context.CancelFunc
}

// ---------------------------------------------------------------------------
// Value encoding helpers
// ---------------------------------------------------------------------------

func extProcessToValue(id uint32) Value {
	return FromSymbolID(id | externalProcessMarker)
}

func isExtProcessValue(v Value) bool {
	if !v.IsSymbol() {
		return false
	}
	id := v.SymbolID()
	return (id & (0xFF << 24)) == externalProcessMarker
}

func extProcessIDFromValue(v Value) uint32 {
	return v.SymbolID() & ^uint32(0xFF<<24)
}

func (vm *VM) vmGetExtProcess(v Value) *ExternalProcessObject {
	if !isExtProcessValue(v) {
		return nil
	}
	return vm.registry.GetExternalProcess(extProcessIDFromValue(v))
}

func (vm *VM) vmRegisterExtProcess(p *ExternalProcessObject) Value {
	id := vm.registry.RegisterExternalProcess(p)
	return extProcessToValue(id)
}

// ---------------------------------------------------------------------------
// Primitives Registration
// ---------------------------------------------------------------------------

func (vm *VM) registerExecPrimitives() {
	epClass := vm.createClass("ExternalProcess", vm.ObjectClass)
	vm.Globals["ExternalProcess"] = vm.classValue(epClass)
	vm.symbolDispatch.Register(externalProcessMarker, &SymbolTypeEntry{Class: epClass})

	// -----------------------------------------------------------------------
	// Class methods (constructors)
	// -----------------------------------------------------------------------

	// command: cmdString — Create a process with a command string (no args)
	epClass.AddClassMethod1(vm.Selectors, "command:", func(vmPtr interface{}, recv Value, cmdVal Value) Value {
		v := vmPtr.(*VM)
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
	epClass.AddClassMethod2(vm.Selectors, "command:args:", func(vmPtr interface{}, recv Value, cmdVal, argsVal Value) Value {
		v := vmPtr.(*VM)
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
	epClass.AddClassMethod2(vm.Selectors, "run:args:", func(vmPtr interface{}, recv Value, cmdVal, argsVal Value) Value {
		v := vmPtr.(*VM)
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
			reason := stderr.String()
			if reason == "" {
				reason = err.Error()
			}
			// Return a Dictionary with exit code, stdout, stderr for non-zero exits
			dict := v.registry.NewDictionaryValue()
			dictObj := v.registry.GetDictionaryObject(dict)
			if dictObj != nil {
				exitKey := v.registry.NewStringValue("exitCode")
				stdoutKey := v.registry.NewStringValue("stdout")
				stderrKey := v.registry.NewStringValue("stderr")
				dictObj.Keys[hashValue(v.registry, exitKey)] = exitKey
				dictObj.Data[hashValue(v.registry, exitKey)] = FromSmallInt(int64(exitCode))
				dictObj.Keys[hashValue(v.registry, stdoutKey)] = stdoutKey
				dictObj.Data[hashValue(v.registry, stdoutKey)] = v.registry.NewStringValue(stdout.String())
				dictObj.Keys[hashValue(v.registry, stderrKey)] = stderrKey
				dictObj.Data[hashValue(v.registry, stderrKey)] = v.registry.NewStringValue(strings.TrimRight(reason, "\n"))
			}
			return v.newFailureResult("Process exited with code " + strings.TrimRight(err.Error(), "\n"))
		}

		return v.registry.NewStringValue(stdout.String())
	})

	// -----------------------------------------------------------------------
	// Instance methods (configuration — return self for chaining)
	// -----------------------------------------------------------------------

	// args: anArray — Set arguments
	epClass.AddMethod1(vm.Selectors, "args:", func(vmPtr interface{}, recv Value, argsVal Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.args = v.valueToStringArray(argsVal)
		return recv
	})

	// env: aDictionary — Set environment variables (merged with inherited env)
	epClass.AddMethod1(vm.Selectors, "env:", func(vmPtr interface{}, recv Value, envVal Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.env = v.valueToDictStringMap(envVal)
		return recv
	})

	// dir: aString — Set working directory
	epClass.AddMethod1(vm.Selectors, "dir:", func(vmPtr interface{}, recv Value, dirVal Value) Value {
		v := vmPtr.(*VM)
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
	epClass.AddMethod0(vm.Selectors, "run", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		defer p.mu.Unlock()

		if p.started {
			return recv
		}

		c := exec.Command(p.command, p.args...)
		c.Env = buildEnv(p.env)
		if p.dir != "" {
			c.Dir = p.dir
		}

		var stdout, stderr bytes.Buffer
		c.Stdout = &stdout
		c.Stderr = &stderr

		p.started = true
		err := c.Run()
		p.stdout = stdout.String()
		p.stderr = stderr.String()
		p.done = true

		if err != nil {
			p.err = err
			if exitErr, ok := err.(*exec.ExitError); ok {
				p.exitCode = exitErr.ExitCode()
			} else {
				p.exitCode = -1
			}
		} else {
			p.exitCode = 0
		}

		return recv
	})

	// runWithTimeout: milliseconds — Run synchronously with timeout
	epClass.AddMethod1(vm.Selectors, "runWithTimeout:", func(vmPtr interface{}, recv Value, msVal Value) Value {
		v := vmPtr.(*VM)
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

		p.mu.Lock()
		defer p.mu.Unlock()

		if p.started {
			return recv
		}

		ctx, cancel := context.WithTimeout(context.Background(), time.Duration(ms)*time.Millisecond)
		defer cancel()
		p.cancel = cancel

		c := exec.CommandContext(ctx, p.command, p.args...)
		c.Env = buildEnv(p.env)
		if p.dir != "" {
			c.Dir = p.dir
		}

		var stdout, stderr bytes.Buffer
		c.Stdout = &stdout
		c.Stderr = &stderr

		p.started = true
		err := c.Run()
		p.stdout = stdout.String()
		p.stderr = stderr.String()
		p.done = true

		if err != nil {
			p.err = err
			if ctx.Err() == context.DeadlineExceeded {
				p.exitCode = -1
				p.err = ctx.Err()
			} else if exitErr, ok := err.(*exec.ExitError); ok {
				p.exitCode = exitErr.ExitCode()
			} else {
				p.exitCode = -1
			}
		} else {
			p.exitCode = 0
		}

		return recv
	})

	// start — Start asynchronously, return self
	epClass.AddMethod0(vm.Selectors, "start", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		defer p.mu.Unlock()

		if p.started {
			return recv
		}

		c := exec.Command(p.command, p.args...)
		c.Env = buildEnv(p.env)
		if p.dir != "" {
			c.Dir = p.dir
		}

		var stdout, stderr bytes.Buffer
		c.Stdout = &stdout
		c.Stderr = &stderr

		err := c.Start()
		if err != nil {
			p.err = err
			p.exitCode = -1
			p.done = true
			p.started = true
			return recv
		}

		p.cmd = c
		p.started = true

		// Wait in a goroutine to capture output
		go func() {
			waitErr := c.Wait()
			p.mu.Lock()
			defer p.mu.Unlock()
			p.stdout = stdout.String()
			p.stderr = stderr.String()
			p.done = true
			if waitErr != nil {
				p.err = waitErr
				if exitErr, ok := waitErr.(*exec.ExitError); ok {
					p.exitCode = exitErr.ExitCode()
				} else {
					p.exitCode = -1
				}
			} else {
				p.exitCode = 0
			}
		}()

		return recv
	})

	// wait — Block until async process completes, return self
	epClass.AddMethod0(vm.Selectors, "wait", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}

		// Spin-wait with short sleeps (process goroutine sets done)
		for {
			p.mu.Lock()
			done := p.done
			p.mu.Unlock()
			if done {
				break
			}
			time.Sleep(1 * time.Millisecond)
		}

		return recv
	})

	// kill — Kill the running process
	epClass.AddMethod0(vm.Selectors, "kill", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		p.mu.Lock()
		defer p.mu.Unlock()

		if p.cancel != nil {
			p.cancel()
		}
		if p.cmd != nil && p.cmd.Process != nil {
			p.cmd.Process.Kill()
		}
		return recv
	})

	// -----------------------------------------------------------------------
	// Instance methods (result accessors)
	// -----------------------------------------------------------------------

	// stdout — Return captured stdout as string
	epClass.AddMethod0(vm.Selectors, "stdout", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		return v.registry.NewStringValue(p.stdout)
	})

	// stderr — Return captured stderr as string
	epClass.AddMethod0(vm.Selectors, "stderr", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		return v.registry.NewStringValue(p.stderr)
	})

	// exitCode — Return exit code (integer, -1 if not yet run or error)
	epClass.AddMethod0(vm.Selectors, "exitCode", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		return FromSmallInt(int64(p.exitCode))
	})

	// isSuccess — Return true if exit code is 0
	epClass.AddMethod0(vm.Selectors, "isSuccess", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return False
		}
		if p.done && p.exitCode == 0 {
			return True
		}
		return False
	})

	// isDone — Return true if process has completed
	epClass.AddMethod0(vm.Selectors, "isDone", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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
	epClass.AddMethod0(vm.Selectors, "command", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
		p := v.vmGetExtProcess(recv)
		if p == nil {
			return Nil
		}
		return v.registry.NewStringValue(p.command)
	})

	// printString — Return a string representation
	epClass.AddMethod0(vm.Selectors, "printString", func(vmPtr interface{}, recv Value) Value {
		v := vmPtr.(*VM)
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

// valueToStringArray converts an Array Value to a []string.
func (vm *VM) valueToStringArray(v Value) []string {
	arr := vm.getArrayValue(v)
	if arr == nil {
		return nil
	}
	result := make([]string, 0, len(arr))
	for _, elem := range arr {
		s := vm.valueToString(elem)
		if s != "" {
			result = append(result, s)
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
	result := make(map[string]string, len(dictObj.Data))
	for h, key := range dictObj.Keys {
		k := vm.valueToString(key)
		val := dictObj.Data[h]
		vStr := vm.valueToString(val)
		if k != "" {
			result[k] = vStr
		}
	}
	return result
}
