package runtime

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
)

// BashBridge handles fallback to Bash runtime for methods not implemented natively.
// It maintains a persistent bash subprocess with trash.bash already sourced,
// avoiding the overhead of forking and sourcing on every call.
type BashBridge struct {
	trashDir  string // Path to ~/.trashtalk
	debug     bool
	sessionID string // Current client session ID for _ENV_DIR consistency

	// Persistent bash process
	cmd    *exec.Cmd
	stdin  io.WriteCloser
	stdout *bufio.Reader
	mu     sync.Mutex // Protects access to the persistent process
}

// NewBashBridge creates a new bash bridge
func NewBashBridge(trashDir string) *BashBridge {
	return &BashBridge{
		trashDir: trashDir,
		debug:    os.Getenv("TRASHTALK_DEBUG") != "",
	}
}

// NewBashBridgeDefault creates a bash bridge with default paths
func NewBashBridgeDefault() (*BashBridge, error) {
	trashDir := os.Getenv("TRASHTALK_ROOT")
	if trashDir == "" {
		home, err := os.UserHomeDir()
		if err != nil {
			return nil, fmt.Errorf("getting home dir: %w", err)
		}
		trashDir = filepath.Join(home, ".trashtalk")
	}
	return NewBashBridge(trashDir), nil
}

// SetDebug enables or disables debug output
func (bb *BashBridge) SetDebug(debug bool) {
	bb.debug = debug
}

// SetSessionID sets the client session ID for _ENV_DIR consistency.
// This ensures BashBridge uses the same temp directory as the client.
func (bb *BashBridge) SetSessionID(sessionID string) {
	bb.mu.Lock()
	defer bb.mu.Unlock()
	bb.sessionID = sessionID
}

// Close shuts down the persistent bash process
func (bb *BashBridge) Close() error {
	bb.mu.Lock()
	defer bb.mu.Unlock()

	if bb.cmd != nil {
		bb.stdin.Close()
		bb.cmd.Wait()
		bb.cmd = nil
	}
	return nil
}

// ensureProcess starts the persistent bash process if not already running
func (bb *BashBridge) ensureProcess() error {
	if bb.cmd != nil {
		return nil
	}

	trashBash := filepath.Join(bb.trashDir, "lib", "trash.bash")

	// Start bash in interactive-like mode, sourcing trash.bash
	// CRITICAL: Set TRASHTALK_DISABLE_NATIVE=1 to prevent bash from trying to
	// call back to the daemon, which would cause deadlock since we're already
	// inside a daemon request handler.
	bb.cmd = exec.Command("bash")
	bb.cmd.Env = append(os.Environ(), "TRASHTALK_DISABLE_NATIVE=1")
	bb.cmd.Stderr = os.Stderr

	var err error
	bb.stdin, err = bb.cmd.StdinPipe()
	if err != nil {
		return fmt.Errorf("creating stdin pipe: %w", err)
	}

	stdout, err := bb.cmd.StdoutPipe()
	if err != nil {
		return fmt.Errorf("creating stdout pipe: %w", err)
	}
	bb.stdout = bufio.NewReader(stdout)

	if err := bb.cmd.Start(); err != nil {
		return fmt.Errorf("starting bash: %w", err)
	}

	// Source trash.bash once
	initCmd := fmt.Sprintf("source %q\n", trashBash)
	if _, err := bb.stdin.Write([]byte(initCmd)); err != nil {
		bb.cmd.Process.Kill()
		bb.cmd = nil
		return fmt.Errorf("sourcing trash.bash: %w", err)
	}

	// Send a marker to confirm initialization
	marker := "__BASH_BRIDGE_READY__"
	if _, err := bb.stdin.Write([]byte(fmt.Sprintf("echo %s\n", marker))); err != nil {
		bb.cmd.Process.Kill()
		bb.cmd = nil
		return fmt.Errorf("sending ready marker: %w", err)
	}

	// Read until we see the marker
	for {
		line, err := bb.stdout.ReadString('\n')
		if err != nil {
			bb.cmd.Process.Kill()
			bb.cmd = nil
			return fmt.Errorf("waiting for ready marker: %w", err)
		}
		if strings.TrimSpace(line) == marker {
			break
		}
	}

	if bb.debug {
		fmt.Fprintf(os.Stderr, "BashBridge: persistent process started (pid %d)\n", bb.cmd.Process.Pid)
	}

	return nil
}

// execCommand runs a command in the persistent bash process and returns the output
func (bb *BashBridge) execCommand(bashCmd string) (string, int, error) {
	bb.mu.Lock()
	defer bb.mu.Unlock()

	if err := bb.ensureProcess(); err != nil {
		return "", 1, err
	}

	// Use a unique marker to detect end of output
	// Format: __END_MARKER_<exitcode>__
	marker := "__BASH_BRIDGE_END__"

	// If session ID is set, prepend environment setup to use client's _ENV_DIR
	// This ensures instances created here are visible to the client's bash
	sessionSetup := ""
	if bb.sessionID != "" {
		sessionSetup = fmt.Sprintf("export TRASH_SESSION_ID=%q; export _ENV_DIR=/tmp/trashtalk_%s; ", bb.sessionID, bb.sessionID)
	}

	// Run the command, capture exit code, then echo marker with exit code
	fullCmd := fmt.Sprintf("{ %s%s; }; __bb_exit=$?; echo \"%s${__bb_exit}__\"\n", sessionSetup, bashCmd, marker)

	if bb.debug {
		fmt.Fprintf(os.Stderr, "BashBridge: exec: %s", fullCmd)
	}

	if _, err := bb.stdin.Write([]byte(fullCmd)); err != nil {
		// Process may have died, try to restart on next call
		bb.cmd = nil
		return "", 1, fmt.Errorf("writing command: %w", err)
	}

	// Read output until we see the marker
	var output strings.Builder
	var exitCode int
	for {
		line, err := bb.stdout.ReadString('\n')
		if err != nil {
			bb.cmd = nil
			return "", 1, fmt.Errorf("reading output: %w", err)
		}

		line = strings.TrimSuffix(line, "\n")

		// Check for end marker
		if strings.HasPrefix(line, marker) {
			// Parse exit code from marker
			exitStr := strings.TrimPrefix(line, marker)
			exitStr = strings.TrimSuffix(exitStr, "__")
			fmt.Sscanf(exitStr, "%d", &exitCode)
			break
		}

		if output.Len() > 0 {
			output.WriteString("\n")
		}
		output.WriteString(line)
	}

	return output.String(), exitCode, nil
}

// Fallback dispatches a message through the Bash runtime
// This is used when:
// - A method is marked bashOnly
// - A class isn't registered natively
// - A method isn't found in the native dispatch table
func (bb *BashBridge) Fallback(receiver string, selector string, args []Value) Value {
	// Quote each argument to handle spaces and special characters
	quotedArgs := make([]string, len(args))
	for i, arg := range args {
		quotedArgs[i] = fmt.Sprintf("%q", arg.AsString())
	}

	argsStr := ""
	if len(quotedArgs) > 0 {
		argsStr = " " + strings.Join(quotedArgs, " ")
	}

	// Build the bash command (trash.bash is already sourced)
	bashCmd := fmt.Sprintf("@ %s %s%s", receiver, selector, argsStr)

	if bb.debug {
		fmt.Fprintf(os.Stderr, "BashBridge: %s\n", bashCmd)
	}

	result, exitCode, err := bb.execCommand(bashCmd)
	if err != nil {
		return ErrorValue(fmt.Sprintf("bash bridge error: %v", err))
	}

	// Exit code 200 means "method not found"
	if exitCode == 200 {
		return ErrorValue(fmt.Sprintf("unknown method: %s %s", receiver, selector))
	}
	if exitCode != 0 {
		return ErrorValue(fmt.Sprintf("bash error (exit %d)", exitCode))
	}

	if bb.debug {
		fmt.Fprintf(os.Stderr, "BashBridge: result=%q\n", result)
	}

	return StringValue(result)
}

// FallbackClassMethod dispatches a class method through Bash
func (bb *BashBridge) FallbackClassMethod(className string, selector string, args []Value) Value {
	return bb.Fallback(className, selector, args)
}

// FallbackInstanceMethod dispatches an instance method through Bash
func (bb *BashBridge) FallbackInstanceMethod(instanceID string, selector string, args []Value) Value {
	return bb.Fallback(instanceID, selector, args)
}

// InvokeBashBlock invokes a Bash-based block by its ID
func (bb *BashBridge) InvokeBashBlock(blockID string, args []Value) Value {
	// Quote arguments
	quotedArgs := make([]string, len(args))
	for i, arg := range args {
		quotedArgs[i] = fmt.Sprintf("%q", arg.AsString())
	}

	argsStr := ""
	if len(quotedArgs) > 0 {
		argsStr = " " + strings.Join(quotedArgs, " ")
	}

	// Invoke block via @ blockID value args...
	bashCmd := fmt.Sprintf("@ %s value%s", blockID, argsStr)

	if bb.debug {
		fmt.Fprintf(os.Stderr, "BashBridge: block invoke: %s\n", bashCmd)
	}

	result, exitCode, err := bb.execCommand(bashCmd)
	if err != nil {
		return ErrorValue(fmt.Sprintf("block invocation failed: %v", err))
	}
	if exitCode != 0 {
		return ErrorValue(fmt.Sprintf("block invocation failed (exit %d)", exitCode))
	}

	return StringValue(result)
}

// Eval evaluates a Trashtalk expression through the Bash runtime
func (bb *BashBridge) Eval(code string) Value {
	bashCmd := fmt.Sprintf("@ Trash eval: %q", code)

	if bb.debug {
		fmt.Fprintf(os.Stderr, "BashBridge: eval: %s\n", bashCmd)
	}

	result, exitCode, err := bb.execCommand(bashCmd)
	if err != nil {
		return ErrorValue(fmt.Sprintf("eval failed: %v", err))
	}
	if exitCode != 0 {
		return ErrorValue(fmt.Sprintf("eval failed (exit %d)", exitCode))
	}

	return StringValue(result)
}
