package integration_test

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/pipeline"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// TestCliFramework — end-to-end coverage of the Cli stack.
//
// The test loads the bootstrapped image (so lib/Cli/*.mag is available),
// compiles examples/cli/demo.mag on top, and then invokes the demo command
// via a Maggie class method with a table of argv arrays. For each case we
// capture os.Stdout / os.Stderr via pipes and assert on exit code and the
// captured text.
//
// This exercises the entire path:
//
//     cobra.ExecuteC
//       -> cli_primitives.go Run callback
//       -> runCliBlock
//       -> evaluateBlock (Maggie bytecode)
//       -> println / Error signal: / flag accessors
//       -> recover into exit code
//
// The goal is to catch regressions that unit tests on individual primitives
// would miss — cobra wiring, argument marshaling, env-var propagation,
// subcommand dispatch, and the panic/exit-code convention when a run block
// raises an exception.
// ---------------------------------------------------------------------------

// repoRoot walks up from the test file's directory until it finds go.mod.
func repoRoot(t *testing.T) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	dir := filepath.Dir(file)
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("could not find repo root (go.mod)")
	return ""
}

// newCliFrameworkVM loads the bootstrapped image (which already contains
// lib/Cli/*.mag) and then compiles examples/cli/demo.mag on top.
func newCliFrameworkVM(t *testing.T) *vm.VM {
	t.Helper()
	root := repoRoot(t)
	imagePath := filepath.Join(root, "cmd", "mag", "maggie.image")
	if _, err := os.Stat(imagePath); err != nil {
		t.Skipf("bootstrap image missing at %s: %v", imagePath, err)
	}

	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("LoadImage(%s): %v", imagePath, err)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)

	demoPath := filepath.Join(root, "examples", "cli", "demo.mag")
	pipe := &pipeline.Pipeline{VM: vmInst}
	if _, err := pipe.CompileFile(demoPath); err != nil {
		t.Fatalf("CompileFile(%s): %v", demoPath, err)
	}

	if _, ok := vmInst.Globals["CliDemo::Main"]; !ok {
		t.Fatal("CliDemo::Main not registered after compiling demo.mag")
	}
	return vmInst
}

// captured bundles the stdout/stderr text produced during a run.
type captured struct {
	stdout string
	stderr string
}

// runDemo redirects os.Stdout / os.Stderr, invokes CliDemo::Main>>runWithArgs:
// with the provided argv, and returns the exit code plus captured output.
//
// The pipe reader goroutines drain the pipes into buffers so the writing
// goroutine (the Maggie run block) never blocks on a full pipe buffer.
func runDemo(t *testing.T, vmInst *vm.VM, argv []string) (int, captured) {
	t.Helper()

	oldStdout, oldStderr := os.Stdout, os.Stderr
	rOut, wOut, err := os.Pipe()
	if err != nil {
		t.Fatalf("os.Pipe stdout: %v", err)
	}
	rErr, wErr, err := os.Pipe()
	if err != nil {
		t.Fatalf("os.Pipe stderr: %v", err)
	}
	os.Stdout = wOut
	os.Stderr = wErr

	var outBuf, errBuf bytes.Buffer
	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		io.Copy(&outBuf, rOut)
	}()
	go func() {
		defer wg.Done()
		io.Copy(&errBuf, rErr)
	}()

	restore := func() {
		wOut.Close()
		wErr.Close()
		os.Stdout, os.Stderr = oldStdout, oldStderr
		wg.Wait()
	}

	// Marshal argv → Array<String>
	elems := make([]vm.Value, len(argv))
	for i, a := range argv {
		elems[i] = vmInst.Registry().NewStringValue(a)
	}
	argsVal := vmInst.NewArrayWithElements(elems)

	mainVal, ok := vmInst.Globals["CliDemo::Main"]
	if !ok {
		restore()
		t.Fatal("CliDemo::Main missing from globals")
	}

	// Panics propagating out of Send would leave os.Stdout/Stderr swapped; make
	// sure we always restore them.
	var result vm.Value
	func() {
		defer func() {
			if r := recover(); r != nil {
				restore()
				if sig, ok := r.(vm.SignaledException); ok {
					reason := vmInst.Registry().GetStringContent(sig.Object.MessageText)
					t.Fatalf("unhandled exception from CliDemo::Main>>runWithArgs:: %q", reason)
				}
				t.Fatalf("panic from CliDemo::Main>>runWithArgs:: %v", r)
			}
		}()
		result = vmInst.Send(mainVal, "runWithArgs:", []vm.Value{argsVal})
	}()
	restore()

	if !result.IsSmallInt() {
		t.Fatalf("runWithArgs: must return SmallInteger, got %v", result)
	}
	return int(result.SmallInt()), captured{stdout: outBuf.String(), stderr: errBuf.String()}
}

func TestCliFramework(t *testing.T) {
	vmInst := newCliFrameworkVM(t)

	// Guard test 7: the PP_DEMO_GREETING env var must be unset for the other
	// cases so the default greeting ("Hello") surfaces. Snapshot the value and
	// clear it while the table runs.
	prevGreeting, hadGreeting := os.LookupEnv("PP_DEMO_GREETING")
	os.Unsetenv("PP_DEMO_GREETING")
	t.Cleanup(func() {
		if hadGreeting {
			os.Setenv("PP_DEMO_GREETING", prevGreeting)
		} else {
			os.Unsetenv("PP_DEMO_GREETING")
		}
	})

	type stdCheck struct {
		contains   []string
		noContains []string
	}

	tests := []struct {
		name     string
		env      map[string]string // set before invocation, cleared after
		args     []string
		wantCode int
		// wantCodeNonzero = true allows any non-zero exit code (since cobra's
		// exit code for unknown-command is consistent but we don't want to
		// over-specify).
		wantCodeNonzero bool
		stdout          stdCheck
		stderr          stdCheck
	}{
		{
			name:     "no args prints help",
			args:     nil,
			wantCode: 0,
			stdout: stdCheck{
				contains: []string{"Usage:", "Available Commands:", "greet", "count"},
			},
		},
		{
			name:     "greet with name",
			args:     []string{"greet", "--name", "World"},
			wantCode: 0,
			stdout:   stdCheck{contains: []string{"Hello, World"}},
		},
		{
			name:     "greet loud",
			args:     []string{"greet", "--name", "World", "--loud"},
			wantCode: 0,
			stdout:   stdCheck{contains: []string{"HELLO, WORLD"}},
		},
		{
			name:            "required flag missing",
			args:            []string{"greet-strict"},
			wantCodeNonzero: true,
			stderr:          stdCheck{contains: []string{"required flag"}},
			stdout:          stdCheck{noContains: []string{"Hello,"}},
		},
		{
			name:     "count prints N lines",
			args:     []string{"count", "--n", "3"},
			wantCode: 0,
			stdout:   stdCheck{contains: []string{"line 1", "line 2", "line 3"}},
		},
		{
			name:            "unknown subcommand",
			args:            []string{"bogus"},
			wantCodeNonzero: true,
			stderr:          stdCheck{contains: []string{"unknown command"}},
		},
		{
			name:     "env binding overrides greeting",
			env:      map[string]string{"PP_DEMO_GREETING": "Hi"},
			args:     []string{"greet"},
			wantCode: 0,
			stdout:   stdCheck{contains: []string{"Hi, World"}},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Apply env vars, restoring them when the case returns.
			for k, v := range tc.env {
				prev, had := os.LookupEnv(k)
				os.Setenv(k, v)
				t.Cleanup(func() {
					if had {
						os.Setenv(k, prev)
					} else {
						os.Unsetenv(k)
					}
				})
			}

			code, out := runDemo(t, vmInst, tc.args)

			if tc.wantCodeNonzero {
				if code == 0 {
					t.Errorf("expected non-zero exit code, got 0 (stdout=%q, stderr=%q)", out.stdout, out.stderr)
				}
			} else if code != tc.wantCode {
				t.Errorf("exit code = %d, want %d (stdout=%q, stderr=%q)", code, tc.wantCode, out.stdout, out.stderr)
			}

			for _, needle := range tc.stdout.contains {
				if !strings.Contains(out.stdout, needle) {
					t.Errorf("stdout missing %q\nstdout=%q\nstderr=%q", needle, out.stdout, out.stderr)
				}
			}
			for _, forbidden := range tc.stdout.noContains {
				if strings.Contains(out.stdout, forbidden) {
					t.Errorf("stdout unexpectedly contains %q\nstdout=%q", forbidden, out.stdout)
				}
			}
			for _, needle := range tc.stderr.contains {
				if !strings.Contains(out.stderr, needle) {
					t.Errorf("stderr missing %q\nstdout=%q\nstderr=%q", needle, out.stdout, out.stderr)
				}
			}
			for _, forbidden := range tc.stderr.noContains {
				if strings.Contains(out.stderr, forbidden) {
					t.Errorf("stderr unexpectedly contains %q\nstderr=%q", forbidden, out.stderr)
				}
			}
		})
	}
}
