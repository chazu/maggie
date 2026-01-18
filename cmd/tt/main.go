// tt - Trashtalk daemon: dynamic plugin loader and native runtime
//
// This daemon initializes the shared runtime (libtrashtalk) and handles
// method dispatch via Unix socket or stdin/stdout JSON protocol.
//
// Build: go build ./cmd/tt
// Usage:
//   tt [--plugin-dir DIR]                    # stdin/stdout mode
//   tt --socket /tmp/tt.sock                 # socket mode
//   tt --socket /tmp/tt.sock --idle-timeout 300
package main

/*
#cgo CFLAGS: -I${SRCDIR}/../../lib/runtime
#cgo LDFLAGS: -L${SRCDIR}/../../bin -ltrashtalk
#cgo LDFLAGS: -ldl

#include <libtrashtalk.h>
#include <stdlib.h>
#include <dlfcn.h>

// Helper to load a plugin (triggers its init() which registers with runtime)
static void* load_plugin(const char* path) {
    return dlopen(path, RTLD_NOW | RTLD_GLOBAL);
}

static const char* load_error() {
    return dlerror();
}

// Get a symbol from a loaded plugin
static void* get_symbol(void* handle, const char* name) {
    return dlsym(handle, name);
}

// Typedef for the dispatch function signature: char* ClassName_Dispatch(char* instanceJSON, char* selector, char* argsJSON)
typedef char* (*dispatch_func)(char*, char*, char*);

// Call a dispatch function pointer
static char* call_dispatch(void* fn_ptr, char* instanceJSON, char* selector, char* argsJSON) {
    dispatch_func fn = (dispatch_func)fn_ptr;
    return fn(instanceJSON, selector, argsJSON);
}
*/
import "C"

import (
	"bufio"
	"database/sql"
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"net"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"
	"unsafe"

	"github.com/chazu/procyon/pkg/bytecode"
	_ "github.com/mattn/go-sqlite3"
)

// Request is the JSON request from Bash
type Request struct {
	Class     string   `json:"class"`
	Instance  string   `json:"instance"`
	Selector  string   `json:"selector"`
	Args      []string `json:"args"`
	SessionID string   `json:"session_id,omitempty"` // TRASH_SESSION_ID for BashBridge fallback

	// For bytecode block operations
	BlockOp       string `json:"block_op,omitempty"`       // "register", "invoke", "serialize"
	BlockID       string `json:"block_id,omitempty"`       // Block identifier
	BlockData     string `json:"block_data,omitempty"`     // Hex-encoded bytecode or JSON args
	BlockCaptures string `json:"block_captures,omitempty"` // JSON capture cell data
}

// Response is the JSON response to Bash
type Response struct {
	Instance      string `json:"instance,omitempty"`
	Result        string `json:"result,omitempty"`
	ExitCode      int    `json:"exit_code"`
	Error         string `json:"error,omitempty"`
	ExecutionMode string `json:"execution_mode,omitempty"` // "native", "fallback", or empty

	// For bytecode block operations
	BlockID   string `json:"block_id,omitempty"`
	BlockData string `json:"block_data,omitempty"`
}

// Daemon manages plugin loading and dispatch via the shared runtime
type Daemon struct {
	pluginDir      string
	loadedPlugins  map[string]bool      // Track which plugins we've loaded
	pluginHandles  map[string]unsafe.Pointer // Store dlopen handles for dlsym
	dispatchFuncs  map[string]unsafe.Pointer // Cache dispatch function pointers
	mu             sync.RWMutex
	idleTimeout    time.Duration
	idleTimer      *time.Timer
	timerMu        sync.Mutex
}

var (
	pluginDir   = flag.String("plugin-dir", "", "Directory containing .dylib/.so plugins")
	socketPath  = flag.String("socket", "", "Unix socket path (enables socket mode)")
	idleTimeout = flag.Int("idle-timeout", 0, "Idle timeout in seconds (socket mode only, 0 = no timeout, default)")
	debug       = flag.Bool("debug", false, "Enable debug output to stderr")
	dbPath      = flag.String("db", "", "SQLite database path (default: ~/.trashtalk/instances.db)")
	killDaemon     = flag.Bool("kill", false, "Kill the running daemon and exit")
	showStatus     = flag.Bool("status", false, "Show daemon status (running/stopped, PID) and exit")
	restartDaemon  = flag.Bool("restart", false, "Restart the daemon (kill existing + start new)")
	tracePath      = flag.String("trace", "", "Enable execution tracing (path to file, or 'stderr' for stderr output)")
	inspectTarget  = flag.String("inspect", "", "Inspect state: instance ID, 'plugins', or 'classes'")
	profilePath    = flag.String("profile", "", "Enable performance profiling (path to file, or 'stderr' for stderr output)")
)

// Trace output writer (nil if tracing disabled)
var traceWriter *os.File

// Profile output writer (nil if profiling disabled)
var profileWriter *os.File

// trace logs execution path info when tracing is enabled
func trace(format string, args ...interface{}) {
	if traceWriter != nil {
		timestamp := time.Now().Format("15:04:05.000")
		msg := fmt.Sprintf(format, args...)
		fmt.Fprintf(traceWriter, "[%s] %s\n", timestamp, msg)
	}
}

// profile logs performance timing data when profiling is enabled
func profile(operation string, duration time.Duration, details string) {
	if profileWriter != nil {
		timestamp := time.Now().Format("15:04:05.000")
		µs := float64(duration.Nanoseconds()) / 1000.0
		if details != "" {
			fmt.Fprintf(profileWriter, "[%s] %s: %.2fµs (%s)\n", timestamp, operation, µs, details)
		} else {
			fmt.Fprintf(profileWriter, "[%s] %s: %.2fµs\n", timestamp, operation, µs)
		}
	}
}

// profileEnabled returns true if profiling is active
func profileEnabled() bool {
	return profileWriter != nil
}

// Default socket path when not specified
const defaultSocketPath = "/tmp/tt.sock"

// handleStatus shows daemon status and exits
func handleStatus(socketPath, pidFile string) {
	// Check if PID file exists
	pidBytes, err := os.ReadFile(pidFile)
	if err != nil {
		fmt.Println("Status: stopped")
		fmt.Println("PID: none")
		fmt.Printf("Socket: %s (not found)\n", socketPath)
		return
	}

	pid := strings.TrimSpace(string(pidBytes))
	pidInt, err := strconv.Atoi(pid)
	if err != nil {
		fmt.Println("Status: stopped (invalid PID file)")
		fmt.Printf("PID file: %s\n", pidFile)
		return
	}

	// Check if process is running
	process, err := os.FindProcess(pidInt)
	if err != nil {
		fmt.Println("Status: stopped")
		fmt.Printf("PID: %d (not found)\n", pidInt)
		return
	}

	// On Unix, FindProcess always succeeds - need to send signal 0 to check
	err = process.Signal(syscall.Signal(0))
	if err != nil {
		fmt.Println("Status: stopped")
		fmt.Printf("PID: %d (not running)\n", pidInt)
		return
	}

	// Check if socket exists
	socketExists := false
	if _, err := os.Stat(socketPath); err == nil {
		socketExists = true
	}

	fmt.Println("Status: running")
	fmt.Printf("PID: %d\n", pidInt)
	if socketExists {
		fmt.Printf("Socket: %s\n", socketPath)
	} else {
		fmt.Printf("Socket: %s (missing)\n", socketPath)
	}
}

// handleKill stops the running daemon and exits
func handleKill(socketPath, pidFile string) {
	// Read PID from file
	pidBytes, err := os.ReadFile(pidFile)
	if err != nil {
		fmt.Println("No daemon running (PID file not found)")
		return
	}

	pid := strings.TrimSpace(string(pidBytes))
	pidInt, err := strconv.Atoi(pid)
	if err != nil {
		fmt.Printf("Invalid PID file: %s\n", pidFile)
		os.Remove(pidFile)
		return
	}

	// Find and kill the process
	process, err := os.FindProcess(pidInt)
	if err != nil {
		fmt.Printf("Process %d not found\n", pidInt)
		os.Remove(pidFile)
		os.Remove(socketPath)
		return
	}

	// Send SIGTERM
	err = process.Signal(syscall.SIGTERM)
	if err != nil {
		fmt.Printf("Failed to kill process %d: %v\n", pidInt, err)
		// Clean up stale files anyway
		os.Remove(pidFile)
		os.Remove(socketPath)
		return
	}

	// Wait briefly for process to exit
	time.Sleep(100 * time.Millisecond)

	// Clean up files
	os.Remove(pidFile)
	os.Remove(socketPath)

	fmt.Printf("Killed daemon (PID %d)\n", pidInt)
}

// handleKillSilent stops the running daemon without output (for --restart)
func handleKillSilent(socketPath, pidFile string) {
	// Read PID from file
	pidBytes, err := os.ReadFile(pidFile)
	if err != nil {
		// No daemon running, nothing to kill
		if *debug {
			fmt.Fprintf(os.Stderr, "tt: no existing daemon to kill\n")
		}
		return
	}

	pid := strings.TrimSpace(string(pidBytes))
	pidInt, err := strconv.Atoi(pid)
	if err != nil {
		os.Remove(pidFile)
		return
	}

	// Find and kill the process
	process, err := os.FindProcess(pidInt)
	if err != nil {
		os.Remove(pidFile)
		os.Remove(socketPath)
		return
	}

	// Send SIGTERM
	err = process.Signal(syscall.SIGTERM)
	if err != nil {
		// Process may already be dead
		os.Remove(pidFile)
		os.Remove(socketPath)
		return
	}

	// Wait for process to exit
	for i := 0; i < 20; i++ { // Wait up to 2 seconds
		time.Sleep(100 * time.Millisecond)
		if err := process.Signal(syscall.Signal(0)); err != nil {
			// Process is gone
			break
		}
	}

	// Clean up files
	os.Remove(pidFile)
	os.Remove(socketPath)

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: killed existing daemon (PID %d)\n", pidInt)
	}
}

// handleInspect inspects runtime state (instances, plugins, classes)
func handleInspect(target, dbFile, pluginDirectory string) {
	switch target {
	case "plugins":
		handleInspectPlugins(pluginDirectory)
	case "classes":
		handleInspectClasses(pluginDirectory)
	default:
		// Assume it's an instance ID
		handleInspectInstance(target, dbFile)
	}
}

// handleInspectPlugins lists available plugin files
func handleInspectPlugins(pluginDirectory string) {
	ext := ".so"
	if runtime.GOOS == "darwin" {
		ext = ".dylib"
	}

	files, err := os.ReadDir(pluginDirectory)
	if err != nil {
		fmt.Printf("Error reading plugin directory %s: %v\n", pluginDirectory, err)
		return
	}

	fmt.Println("Available Plugins:")
	fmt.Println("==================")
	count := 0
	for _, f := range files {
		if !f.IsDir() && strings.HasSuffix(f.Name(), ext) {
			className := strings.TrimSuffix(f.Name(), ext)
			// Convert MyApp__Counter back to MyApp::Counter
			className = strings.ReplaceAll(className, "__", "::")
			info, _ := f.Info()
			size := "unknown"
			if info != nil {
				size = fmt.Sprintf("%d bytes", info.Size())
			}
			fmt.Printf("  %s (%s)\n", className, size)
			count++
		}
	}
	fmt.Printf("\nTotal: %d plugins\n", count)
}

// handleInspectClasses lists classes with their method counts
func handleInspectClasses(pluginDirectory string) {
	ext := ".so"
	if runtime.GOOS == "darwin" {
		ext = ".dylib"
	}

	files, err := os.ReadDir(pluginDirectory)
	if err != nil {
		fmt.Printf("Error reading plugin directory %s: %v\n", pluginDirectory, err)
		return
	}

	fmt.Println("Native Classes:")
	fmt.Println("===============")
	for _, f := range files {
		if !f.IsDir() && strings.HasSuffix(f.Name(), ext) {
			className := strings.TrimSuffix(f.Name(), ext)
			className = strings.ReplaceAll(className, "__", "::")
			fmt.Printf("  %s\n", className)
		}
	}
}

// handleInspectInstance inspects an instance from the database
func handleInspectInstance(instanceID, dbFile string) {
	db, err := sql.Open("sqlite3", dbFile)
	if err != nil {
		fmt.Printf("Error opening database %s: %v\n", dbFile, err)
		return
	}
	defer db.Close()

	// Query the instance - just get the data column, extract fields from JSON
	var data string
	err = db.QueryRow(`SELECT data FROM instances WHERE id = ?`, instanceID).Scan(&data)

	if err == sql.ErrNoRows {
		fmt.Printf("Instance not found: %s\n", instanceID)
		return
	} else if err != nil {
		fmt.Printf("Error querying instance: %v\n", err)
		return
	}

	// Parse the JSON data
	var instanceData map[string]interface{}
	if err := json.Unmarshal([]byte(data), &instanceData); err != nil {
		fmt.Printf("Error parsing instance data: %v\n", err)
		fmt.Printf("Raw data: %s\n", data)
		return
	}

	// Extract class and created_at from the data
	class, _ := instanceData["class"].(string)
	createdAt, _ := instanceData["created_at"].(string)

	fmt.Printf("Instance: %s\n", instanceID)
	fmt.Printf("Class: %s\n", class)
	if createdAt != "" {
		fmt.Printf("Created: %s\n", createdAt)
	}

	fmt.Println("\nInstance Variables:")
	fmt.Println("-------------------")

	// Print all fields except metadata
	metaFields := map[string]bool{"class": true, "created_at": true, "id": true}
	for key, value := range instanceData {
		if !metaFields[key] {
			fmt.Printf("  %s: %v\n", key, value)
		}
	}
}

func main() {
	flag.Parse()

	// Determine socket path for management commands
	sock := *socketPath
	if sock == "" {
		sock = defaultSocketPath
	}
	pidFile := sock + ".pid"

	// Handle --status and --kill commands (don't start daemon)
	if *showStatus {
		handleStatus(sock, pidFile)
		return
	}
	if *killDaemon {
		handleKill(sock, pidFile)
		return
	}

	// Handle --inspect command
	if *inspectTarget != "" {
		// Determine paths for inspection
		dir := *pluginDir
		if dir == "" {
			home, _ := os.UserHomeDir()
			dir = filepath.Join(home, ".trashtalk", "trash", ".compiled")
		}
		db := *dbPath
		if db == "" {
			home, _ := os.UserHomeDir()
			db = filepath.Join(home, ".trashtalk", "instances.db")
		}
		handleInspect(*inspectTarget, db, dir)
		return
	}

	// Handle --restart: kill existing daemon silently, then continue to start new one
	if *restartDaemon {
		// Kill existing daemon if running (don't print messages unless debug)
		handleKillSilent(sock, pidFile)
	}

	// Initialize trace output if --trace is specified
	if *tracePath != "" {
		if *tracePath == "stderr" {
			traceWriter = os.Stderr
		} else {
			f, err := os.OpenFile(*tracePath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil {
				fmt.Fprintf(os.Stderr, "tt: failed to open trace file %s: %v\n", *tracePath, err)
				os.Exit(1)
			}
			traceWriter = f
			defer f.Close()
		}
		trace("tt daemon starting (trace enabled)")
	}

	// Initialize profile output if --profile is specified
	if *profilePath != "" {
		if *profilePath == "stderr" {
			profileWriter = os.Stderr
		} else {
			f, err := os.OpenFile(*profilePath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644)
			if err != nil {
				fmt.Fprintf(os.Stderr, "tt: failed to open profile file %s: %v\n", *profilePath, err)
				os.Exit(1)
			}
			profileWriter = f
			defer f.Close()
		}
		profile("startup", 0, "profiling enabled")
	}

	// Determine plugin directory
	dir := *pluginDir
	if dir == "" {
		home, _ := os.UserHomeDir()
		dir = filepath.Join(home, ".trashtalk", "trash", ".compiled")
	}

	// Initialize the shared runtime
	var cDbPath *C.char
	if *dbPath != "" {
		cDbPath = C.CString(*dbPath)
		defer C.free(unsafe.Pointer(cDbPath))
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: initializing shared runtime\n")
	}

	if C.TT_Init(cDbPath, nil) != 0 {
		fmt.Fprintf(os.Stderr, "tt: failed to initialize shared runtime\n")
		os.Exit(1)
	}
	defer C.TT_Close()

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: shared runtime initialized\n")
		fmt.Fprintf(os.Stderr, "tt: plugin-dir=%s\n", dir)
	}

	d := &Daemon{
		pluginDir:      dir,
		loadedPlugins:  make(map[string]bool),
		pluginHandles:  make(map[string]unsafe.Pointer),
		dispatchFuncs:  make(map[string]unsafe.Pointer),
		idleTimeout:    time.Duration(*idleTimeout) * time.Second,
	}

	if *socketPath != "" {
		d.RunSocket(*socketPath)
	} else {
		d.RunStdin()
	}
}

// RunStdin processes JSON requests from stdin (original mode)
func (d *Daemon) RunStdin() {
	scanner := bufio.NewScanner(os.Stdin)
	// Increase buffer for large instance JSON
	buf := make([]byte, 1024*1024) // 1MB
	scanner.Buffer(buf, len(buf))

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}

		var req Request
		if err := json.Unmarshal([]byte(line), &req); err != nil {
			d.respond(os.Stdout, Response{ExitCode: 1, Error: "invalid JSON: " + err.Error()})
			continue
		}

		if *debug {
			fmt.Fprintf(os.Stderr, "tt: request class=%s selector=%s args=%v\n", req.Class, req.Selector, req.Args)
		}

		resp := d.HandleRequest(req)
		d.respond(os.Stdout, resp)
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "tt: scanner error: %v\n", err)
	}
}

// RunSocket runs the daemon in Unix socket mode
func (d *Daemon) RunSocket(path string) {
	// Remove existing socket file
	os.Remove(path)

	listener, err := net.Listen("unix", path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "tt: failed to listen on %s: %v\n", path, err)
		os.Exit(1)
	}
	defer listener.Close()
	defer os.Remove(path)

	// Make socket world-writable so any process can connect
	os.Chmod(path, 0777)

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: listening on %s (idle-timeout=%v)\n", path, d.idleTimeout)
	}

	// Write PID file
	pidPath := path + ".pid"
	os.WriteFile(pidPath, []byte(fmt.Sprintf("%d", os.Getpid())), 0644)
	defer os.Remove(pidPath)

	// Handle shutdown signals
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigChan
		if *debug {
			fmt.Fprintf(os.Stderr, "tt: shutting down on signal\n")
		}
		listener.Close()
	}()

	// Start idle timer if timeout is set
	if d.idleTimeout > 0 {
		d.startIdleTimer(listener)
	}

	// Accept connections
	for {
		conn, err := listener.Accept()
		if err != nil {
			// Check if it's because we're shutting down
			if opErr, ok := err.(*net.OpError); ok && opErr.Err.Error() == "use of closed network connection" {
				break
			}
			if *debug {
				fmt.Fprintf(os.Stderr, "tt: accept error: %v\n", err)
			}
			continue
		}

		// Reset idle timer on each connection
		d.resetIdleTimer(listener)

		// Handle connection (one request per connection)
		d.handleConnection(conn)
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: exiting\n")
	}
}

// handleConnection handles a single request on a connection
func (d *Daemon) handleConnection(conn net.Conn) {
	defer conn.Close()

	// Set read deadline to prevent hanging connections
	conn.SetReadDeadline(time.Now().Add(30 * time.Second))

	reader := bufio.NewReader(conn)
	line, err := reader.ReadString('\n')
	if err != nil {
		if *debug {
			fmt.Fprintf(os.Stderr, "tt: read error: %v\n", err)
		}
		return
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: raw request: %s\n", strings.TrimSpace(line))
	}

	var req Request
	if err := json.Unmarshal([]byte(line), &req); err != nil {
		d.respond(conn, Response{ExitCode: 1, Error: "invalid JSON: " + err.Error()})
		return
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: parsed class=%s selector=%s args=%v\n", req.Class, req.Selector, req.Args)
	}

	resp := d.HandleRequest(req)
	d.respond(conn, resp)
}

// startIdleTimer starts the idle timeout timer
func (d *Daemon) startIdleTimer(listener net.Listener) {
	d.timerMu.Lock()
	defer d.timerMu.Unlock()

	d.idleTimer = time.AfterFunc(d.idleTimeout, func() {
		if *debug {
			fmt.Fprintf(os.Stderr, "tt: idle timeout reached, shutting down\n")
		}
		listener.Close()
	})
}

// resetIdleTimer resets the idle timeout timer
func (d *Daemon) resetIdleTimer(listener net.Listener) {
	d.timerMu.Lock()
	defer d.timerMu.Unlock()

	if d.idleTimer != nil {
		d.idleTimer.Stop()
		d.idleTimer = time.AfterFunc(d.idleTimeout, func() {
			if *debug {
				fmt.Fprintf(os.Stderr, "tt: idle timeout reached, shutting down\n")
			}
			listener.Close()
		})
	}
}

func (d *Daemon) respond(w interface{ Write([]byte) (int, error) }, resp Response) {
	output, _ := json.Marshal(resp)
	w.Write(append(output, '\n'))
}

// HandleRequest processes a single dispatch request
func (d *Daemon) HandleRequest(req Request) Response {
	var requestStart time.Time
	if profileEnabled() {
		requestStart = time.Now()
	}

	// Determine receiver for trace output
	receiver := req.Class
	if req.Instance != "" {
		receiver = req.Instance
	}
	trace("DISPATCH %s >> %s", receiver, req.Selector)

	// Set session ID for BashBridge to ensure instances are created in the right _ENV_DIR
	if req.SessionID != "" {
		cSessionID := C.CString(req.SessionID)
		defer C.free(unsafe.Pointer(cSessionID))
		C.TT_SetSessionID(cSessionID)
	}

	// Handle bytecode block operations
	if req.BlockOp != "" {
		trace("  BLOCK_OP %s (id=%s)", req.BlockOp, req.BlockID)
		resp := d.handleBlockOp(req)
		if profileEnabled() {
			profile("request_total", time.Since(requestStart), fmt.Sprintf("block_op=%s", req.BlockOp))
		}
		return resp
	}

	// Ensure the plugin for this class is loaded
	var pluginLoadStart time.Time
	if profileEnabled() {
		pluginLoadStart = time.Now()
	}
	if err := d.ensurePluginLoaded(req.Class); err != nil {
		if *debug {
			fmt.Fprintf(os.Stderr, "tt: no plugin for %s: %v\n", req.Class, err)
		}
		// No native plugin, signal fallback to Bash
		trace("  FALLBACK (no plugin for %s)", req.Class)
		if profileEnabled() {
			profile("request_total", time.Since(requestStart), fmt.Sprintf("%s.%s -> fallback", req.Class, req.Selector))
		}
		return Response{ExitCode: 200, ExecutionMode: "fallback"}
	}
	if profileEnabled() {
		profile("plugin_ensure", time.Since(pluginLoadStart), req.Class)
	}

	// Check if we have a dispatch function for this class
	d.mu.RLock()
	dispatchFunc := d.dispatchFuncs[req.Class]
	d.mu.RUnlock()

	var resp Response
	if dispatchFunc != nil {
		// Call the plugin's dispatch function directly
		resp = d.callDispatchFunc(req, dispatchFunc)
	} else {
		// Fall back to TT_Send (for primitive classes that register methods directly)
		resp = d.callTTSend(req)
	}

	if profileEnabled() {
		profile("request_total", time.Since(requestStart), fmt.Sprintf("%s.%s", req.Class, req.Selector))
	}
	return resp
}

// callDispatchFunc calls the plugin's exported dispatch function directly
func (d *Daemon) callDispatchFunc(req Request, dispatchFunc unsafe.Pointer) Response {
	// Prepare instanceJSON - either the instance ID or the class name for class methods
	instanceJSON := req.Instance
	if instanceJSON == "" {
		instanceJSON = req.Class
	}

	// Convert args to JSON array
	var marshalStart time.Time
	if profileEnabled() {
		marshalStart = time.Now()
	}
	argsJSON, _ := json.Marshal(req.Args)
	if profileEnabled() {
		profile("json_marshal_args", time.Since(marshalStart), fmt.Sprintf("%d args", len(req.Args)))
	}

	// Create C strings
	cInstanceJSON := C.CString(instanceJSON)
	defer C.free(unsafe.Pointer(cInstanceJSON))

	cSelector := C.CString(req.Selector)
	defer C.free(unsafe.Pointer(cSelector))

	cArgsJSON := C.CString(string(argsJSON))
	defer C.free(unsafe.Pointer(cArgsJSON))

	// Call the dispatch function via C helper: char* dispatch(char* instanceJSON, char* selector, char* argsJSON)
	var dispatchStart time.Time
	if profileEnabled() {
		dispatchStart = time.Now()
	}
	cResult := C.call_dispatch(dispatchFunc, cInstanceJSON, cSelector, cArgsJSON)
	if profileEnabled() {
		profile("ffi_dispatch", time.Since(dispatchStart), fmt.Sprintf("%s.%s", req.Class, req.Selector))
	}
	if cResult == nil {
		return Response{ExitCode: 1, Error: "dispatch returned nil"}
	}
	defer C.free(unsafe.Pointer(cResult))

	resultJSON := C.GoString(cResult)

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: dispatch result: %s\n", resultJSON)
	}

	// Parse the JSON response from the dispatch function
	var unmarshalStart time.Time
	if profileEnabled() {
		unmarshalStart = time.Now()
	}
	// Instance can be either a string or an object (plugins return object for updated state)
	var dispatchResp struct {
		Instance json.RawMessage `json:"instance,omitempty"`
		Result   string          `json:"result,omitempty"`
		ExitCode int             `json:"exit_code"`
		Error    string          `json:"error,omitempty"`
	}
	if err := json.Unmarshal([]byte(resultJSON), &dispatchResp); err != nil {
		return Response{ExitCode: 1, Error: "invalid dispatch response: " + err.Error()}
	}
	if profileEnabled() {
		profile("json_unmarshal_resp", time.Since(unmarshalStart), fmt.Sprintf("%d bytes", len(resultJSON)))
	}

	// Convert instance to string - it may be a JSON object or a JSON string
	instanceStr := ""
	if len(dispatchResp.Instance) > 0 {
		// Check if it's a JSON string (starts with quote) or an object
		if dispatchResp.Instance[0] == '"' {
			// It's a JSON string, unmarshal it
			json.Unmarshal(dispatchResp.Instance, &instanceStr)
		} else {
			// It's a JSON object, use as-is
			instanceStr = string(dispatchResp.Instance)
		}
	}

	trace("  NATIVE (plugin dispatch) -> %s", truncateResult(dispatchResp.Result))
	return Response{
		Instance:      instanceStr,
		Result:        dispatchResp.Result,
		ExitCode:      dispatchResp.ExitCode,
		Error:         dispatchResp.Error,
		ExecutionMode: "native",
	}
}

// truncateResult truncates long results for trace output
func truncateResult(s string) string {
	if len(s) > 50 {
		return s[:50] + "..."
	}
	return s
}

// callTTSend calls TT_Send through the shared runtime (for primitive classes)
func (d *Daemon) callTTSend(req Request) Response {
	// Determine the receiver - either instance ID or class name
	receiver := req.Instance
	if receiver == "" {
		receiver = req.Class
	}

	// Build arguments for TT_Send
	cReceiver := C.CString(receiver)
	defer C.free(unsafe.Pointer(cReceiver))

	cSelector := C.CString(req.Selector)
	defer C.free(unsafe.Pointer(cSelector))

	// Convert args to TTValue array
	var argsPtr *C.TTValue
	cArgs := make([]C.TTValue, len(req.Args))
	cArgStrings := make([]*C.char, len(req.Args)) // Keep references to free later

	for i, arg := range req.Args {
		cArgStrings[i] = C.CString(arg)
		cArgs[i] = C.TT_MakeString(cArgStrings[i])
	}
	defer func() {
		for _, cStr := range cArgStrings {
			C.free(unsafe.Pointer(cStr))
		}
	}()

	if len(cArgs) > 0 {
		argsPtr = &cArgs[0]
	}

	// Call TT_Send through the shared runtime
	var sendStart time.Time
	if profileEnabled() {
		sendStart = time.Now()
	}
	result := C.TT_Send(cReceiver, cSelector, argsPtr, C.int(len(req.Args)))
	if profileEnabled() {
		profile("tt_send", time.Since(sendStart), fmt.Sprintf("%s.%s", req.Class, req.Selector))
	}

	// Check result type for errors
	if result._type == C.TT_TYPE_ERROR {
		// Extract error message
		cErrMsg := C.TT_ValueAsString(result)
		if cErrMsg != nil {
			errMsg := C.GoString(cErrMsg)
			C.free(unsafe.Pointer(cErrMsg))
			if strings.Contains(errMsg, "unknown selector") {
				trace("  FALLBACK (unknown selector: %s)", req.Selector)
				return Response{ExitCode: 200, ExecutionMode: "fallback"} // Signal fallback to Bash
			}
			return Response{ExitCode: 1, Error: errMsg}
		}
		return Response{ExitCode: 1, Error: "unknown error"}
	}

	// Convert result to string
	cResultStr := C.TT_ValueAsString(result)
	var resultStr string
	if cResultStr != nil {
		resultStr = C.GoString(cResultStr)
		C.free(unsafe.Pointer(cResultStr))
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: TT_Send result: %q\n", resultStr)
	}

	trace("  NATIVE (TT_Send) -> %s", truncateResult(resultStr))
	return Response{
		Result:        resultStr,
		ExitCode:      0,
		ExecutionMode: "native",
	}
}

// ensurePluginLoaded loads the plugin for a class if not already loaded
func (d *Daemon) ensurePluginLoaded(className string) error {
	d.mu.Lock()
	defer d.mu.Unlock()

	// Check if already loaded
	if d.loadedPlugins[className] {
		return nil
	}

	// Determine shared library extension
	ext := ".so"
	if runtime.GOOS == "darwin" {
		ext = ".dylib"
	}

	// Try to find and load the plugin
	// Handle namespaced classes: MyApp::Counter -> MyApp__Counter
	pluginName := strings.ReplaceAll(className, "::", "__")
	soPath := filepath.Join(d.pluginDir, pluginName+ext)

	if _, err := os.Stat(soPath); os.IsNotExist(err) {
		return fmt.Errorf("plugin not found: %s", soPath)
	}

	// Load the plugin via dlopen - this triggers its init() which registers with the runtime
	cPath := C.CString(soPath)
	defer C.free(unsafe.Pointer(cPath))

	handle := C.load_plugin(cPath)
	if handle == nil {
		errMsg := C.GoString(C.load_error())
		return fmt.Errorf("failed to load %s: %s", soPath, errMsg)
	}

	// Store the handle for later dlsym calls
	d.pluginHandles[className] = handle

	// Look up the dispatch function: <ShortClassName>_Dispatch
	// For namespaced classes: Yutani::IDE -> IDE_Dispatch (uses just the class name part)
	shortClassName := className
	if idx := strings.LastIndex(className, "::"); idx != -1 {
		shortClassName = className[idx+2:]
	}
	dispatchName := shortClassName + "_Dispatch"
	cDispatchName := C.CString(dispatchName)
	defer C.free(unsafe.Pointer(cDispatchName))

	dispatchFunc := C.get_symbol(handle, cDispatchName)
	if dispatchFunc != nil {
		d.dispatchFuncs[className] = dispatchFunc
		if *debug {
			fmt.Fprintf(os.Stderr, "tt: found dispatch func %s\n", dispatchName)
		}
	} else if *debug {
		fmt.Fprintf(os.Stderr, "tt: no dispatch func %s, will use TT_Send\n", dispatchName)
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: loaded plugin %s\n", soPath)
	}

	d.loadedPlugins[className] = true
	return nil
}

// ============ Bytecode Block Operations ============

// handleBlockOp processes bytecode block operations via the shared runtime
func (d *Daemon) handleBlockOp(req Request) Response {
	switch req.BlockOp {
	case "register":
		return d.handleBlockRegister(req)
	case "invoke":
		return d.handleBlockInvoke(req)
	case "serialize":
		return d.handleBlockSerialize(req)
	default:
		return Response{ExitCode: 1, Error: "unknown block operation: " + req.BlockOp}
	}
}

// handleBlockRegister registers a new bytecode block with the shared runtime
func (d *Daemon) handleBlockRegister(req Request) Response {
	// Decode hex-encoded bytecode
	bytecodeBytes, err := hex.DecodeString(req.BlockData)
	if err != nil {
		return Response{ExitCode: 1, Error: "invalid hex bytecode: " + err.Error()}
	}

	// Deserialize bytecode into a Chunk
	chunk, err := bytecode.Deserialize(bytecodeBytes)
	if err != nil {
		return Response{ExitCode: 1, Error: "failed to deserialize bytecode: " + err.Error()}
	}

	// Parse captures JSON if provided
	var numCaptures C.int
	var capturesPtr **C.TTValue

	if req.BlockCaptures != "" && req.BlockCaptures != "[]" {
		var captures []map[string]interface{}
		if err := json.Unmarshal([]byte(req.BlockCaptures), &captures); err != nil {
			return Response{ExitCode: 1, Error: "invalid captures JSON: " + err.Error()}
		}

		if len(captures) > 0 {
			// Allocate C values for captures
			cCaptures := make([]*C.TTValue, len(captures))
			for i, cap := range captures {
				val := C.TTValue{}
				// Extract value from capture
				if v, ok := cap["value"]; ok {
					switch tv := v.(type) {
					case string:
						val._type = C.TT_TYPE_STRING
						cStr := C.CString(tv)
						*(**C.char)(unsafe.Pointer(&val.anon0)) = cStr
					case float64:
						if tv == float64(int64(tv)) {
							val._type = C.TT_TYPE_INT
							*(*C.int64_t)(unsafe.Pointer(&val.anon0)) = C.int64_t(int64(tv))
						} else {
							val._type = C.TT_TYPE_FLOAT
							*(*C.double)(unsafe.Pointer(&val.anon0)) = C.double(tv)
						}
					case bool:
						val._type = C.TT_TYPE_BOOL
						if tv {
							*(*C.int64_t)(unsafe.Pointer(&val.anon0)) = 1
						} else {
							*(*C.int64_t)(unsafe.Pointer(&val.anon0)) = 0
						}
					default:
						val._type = C.TT_TYPE_NIL
					}
				} else {
					val._type = C.TT_TYPE_NIL
				}
				cCaptures[i] = &val
			}
			numCaptures = C.int(len(captures))
			capturesPtr = &cCaptures[0]
		}
	}

	// Serialize the chunk for the C API
	serialized, err := chunk.Serialize()
	if err != nil {
		return Response{ExitCode: 1, Error: "failed to serialize chunk: " + err.Error()}
	}

	// Call TT_RegisterBlock
	cBlockID := C.TT_RegisterBlock(
		(*C.uint8_t)(unsafe.Pointer(&serialized[0])),
		C.size_t(len(serialized)),
		capturesPtr,
		numCaptures,
	)

	if cBlockID == nil {
		return Response{ExitCode: 1, Error: "TT_RegisterBlock returned nil"}
	}

	blockID := C.GoString(cBlockID)
	C.free(unsafe.Pointer(cBlockID))

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: registered block %s (%d bytes, %d captures)\n",
			blockID, len(serialized), numCaptures)
	}

	return Response{ExitCode: 0, BlockID: blockID}
}

// handleBlockInvoke invokes a registered bytecode block via TT_InvokeBlock
func (d *Daemon) handleBlockInvoke(req Request) Response {
	// Parse arguments from BlockData
	var args []string
	if req.BlockData != "" {
		if err := json.Unmarshal([]byte(req.BlockData), &args); err != nil {
			return Response{ExitCode: 1, Error: "invalid args JSON: " + err.Error()}
		}
	}

	// Convert block ID to C string
	cBlockID := C.CString(req.BlockID)
	defer C.free(unsafe.Pointer(cBlockID))

	// Build args array
	var argsPtr *C.TTValue
	cArgs := make([]C.TTValue, len(args))
	cArgStrings := make([]*C.char, len(args))

	for i, arg := range args {
		cArgStrings[i] = C.CString(arg)
		cArgs[i] = C.TT_MakeString(cArgStrings[i])
	}
	defer func() {
		for _, cStr := range cArgStrings {
			C.free(unsafe.Pointer(cStr))
		}
	}()

	if len(cArgs) > 0 {
		argsPtr = &cArgs[0]
	}

	// Call TT_InvokeBlock
	result := C.TT_InvokeBlock(cBlockID, argsPtr, C.int(len(args)))

	// Check for errors
	if result._type == C.TT_TYPE_ERROR {
		cErrMsg := C.TT_ValueAsString(result)
		if cErrMsg != nil {
			errMsg := C.GoString(cErrMsg)
			C.free(unsafe.Pointer(cErrMsg))
			// Check if block not found - signal fallback
			if strings.Contains(errMsg, "not found") {
				return Response{ExitCode: 200, ExecutionMode: "fallback"}
			}
			return Response{ExitCode: 1, Error: errMsg}
		}
		return Response{ExitCode: 200, ExecutionMode: "fallback"} // Fallback if unknown error
	}

	// Convert result to string
	cResultStr := C.TT_ValueAsString(result)
	var resultStr string
	if cResultStr != nil {
		resultStr = C.GoString(cResultStr)
		C.free(unsafe.Pointer(cResultStr))
	}

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: invoked block %s with %d args -> %q\n",
			req.BlockID, len(args), resultStr)
	}

	return Response{ExitCode: 0, Result: resultStr, ExecutionMode: "native"}
}

// handleBlockSerialize serializes a block for cross-process transfer
func (d *Daemon) handleBlockSerialize(req Request) Response {
	if req.BlockID == "" {
		return Response{ExitCode: 1, Error: "block_id is required"}
	}

	// Look up the block via C API
	cBlockID := C.CString(req.BlockID)
	defer C.free(unsafe.Pointer(cBlockID))

	cBlock := C.TT_LookupBlock(cBlockID)
	if cBlock == nil {
		return Response{ExitCode: 1, Error: "block not found: " + req.BlockID}
	}

	// Cast to Go Block type to access fields
	// Note: This works because TTBlock is typedef'd from the Go Block struct
	type goBlock struct {
		ID         string
		Chunk      *bytecode.Chunk
		Captures   []interface{} // Actually []*CaptureCell but we just need the data
		InstanceID string
		ClassName  string
	}
	block := (*goBlock)(unsafe.Pointer(cBlock))

	if block.Chunk == nil {
		return Response{ExitCode: 1, Error: "block has no bytecode chunk"}
	}

	// Serialize the bytecode
	bytecodeBytes, err := block.Chunk.Serialize()
	if err != nil {
		return Response{ExitCode: 1, Error: "failed to serialize bytecode: " + err.Error()}
	}

	// Hex-encode the bytecode
	hexBytecode := hex.EncodeToString(bytecodeBytes)

	// Serialize captures to JSON
	// For now, we'll serialize capture values as a simple array
	capturesJSON := "[]"
	// TODO: Implement full capture serialization if needed

	if *debug {
		fmt.Fprintf(os.Stderr, "tt: serialized block %s (%d bytes)\n",
			req.BlockID, len(bytecodeBytes))
	}

	return Response{
		ExitCode:  0,
		BlockID:   req.BlockID,
		BlockData: hexBytecode,
		Result:    capturesJSON,
	}
}
