package vm

import (
	"fmt"
	"sync"
)

// ---------------------------------------------------------------------------
// Debugger: IDE-integrated debug server for the Maggie VM
// ---------------------------------------------------------------------------

// DebugServer provides debugging capabilities for IDE integration.
// It supports breakpoints, pause/resume, stepping, and variable inspection.
type DebugServer struct {
	vm          *VM
	active      bool
	breakpoints map[breakpointKey]bool
	pauseChan   chan pauseRequest
	resumeChan  chan struct{}
	eventChan   chan DebugEvent
	mu          sync.Mutex

	// Stepping state
	stepMode    StepMode
	stepFrame   int // frame pointer when step was initiated
	stepLine    int // line number when step was initiated

	// Current pause state
	paused      bool
	pauseReason string
}

// breakpointKey uniquely identifies a breakpoint location.
type breakpointKey struct {
	className  string
	methodName string
	line       int
}

// pauseRequest signals a pause request to the interpreter.
type pauseRequest struct {
	goroutineID uint64
	reason      string
}

// StepMode indicates the current stepping mode.
type StepMode int

const (
	StepNone StepMode = iota
	StepOver
	StepInto
	StepOut
)

// ---------------------------------------------------------------------------
// Debug events for clients
// ---------------------------------------------------------------------------

// DebugEvent represents a debugging event sent to clients.
type DebugEvent struct {
	Type     string          // "stopped", "continued", "breakpointHit", "exception"
	Reason   string          // Additional context about the event
	Location *SourceLocation // Current source location (if applicable)
}

// SourceLocation represents a position in source code.
type SourceLocation struct {
	Class  string // Class name
	Method string // Method name
	Line   int    // 1-based line number
	Column int    // 1-based column number
	Source string // Optional source snippet
}

// StackFrame represents a single frame in the call stack.
type StackFrame struct {
	ID       int    // Unique frame identifier
	Method   string // Method name
	Class    string // Class name
	Line     int    // 1-based line number
	Column   int    // 1-based column number
	IsBlock  bool   // True if this is a block frame
}

// Variable represents a variable for inspection.
type Variable struct {
	Name  string // Variable name
	Value string // String representation of value
	Type  string // Type name
}

// Breakpoint represents a breakpoint for external clients.
type Breakpoint struct {
	ID     int    // Unique breakpoint identifier
	Class  string // Class name
	Method string // Method name
	Line   int    // Line number
	Active bool   // Whether breakpoint is enabled
}

// ---------------------------------------------------------------------------
// DebugServer creation and lifecycle
// ---------------------------------------------------------------------------

// NewDebugServer creates a new debug server attached to the given VM.
func NewDebugServer(vm *VM) *DebugServer {
	return &DebugServer{
		vm:          vm,
		active:      false,
		breakpoints: make(map[breakpointKey]bool),
		pauseChan:   make(chan pauseRequest, 1),
		resumeChan:  make(chan struct{}, 1),
		eventChan:   make(chan DebugEvent, 10),
		stepMode:    StepNone,
	}
}

// Activate enables the debug server.
func (d *DebugServer) Activate() {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.active = true
}

// Deactivate disables the debug server.
func (d *DebugServer) Deactivate() {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.active = false
	// Clear all breakpoints
	d.breakpoints = make(map[breakpointKey]bool)
}

// IsActive returns whether the debug server is enabled.
func (d *DebugServer) IsActive() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.active
}

// Events returns the event channel for receiving debug events.
func (d *DebugServer) Events() <-chan DebugEvent {
	return d.eventChan
}

// ---------------------------------------------------------------------------
// Breakpoint management
// ---------------------------------------------------------------------------

// SetBreakpoint sets a breakpoint at the specified location.
// Returns an error if the method cannot be found.
func (d *DebugServer) SetBreakpoint(class, method string, line int) error {
	d.mu.Lock()
	defer d.mu.Unlock()

	// Validate that the class exists
	if d.vm != nil {
		cls := d.vm.Classes.Lookup(class)
		if cls == nil {
			return fmt.Errorf("class not found: %s", class)
		}
		// Note: We don't validate the method here because it might not
		// be loaded yet. The breakpoint will be validated at hit time.
	}

	key := breakpointKey{
		className:  class,
		methodName: method,
		line:       line,
	}
	d.breakpoints[key] = true
	return nil
}

// RemoveBreakpoint removes a breakpoint at the specified location.
// Returns an error if no breakpoint exists at that location.
func (d *DebugServer) RemoveBreakpoint(class, method string, line int) error {
	d.mu.Lock()
	defer d.mu.Unlock()

	key := breakpointKey{
		className:  class,
		methodName: method,
		line:       line,
	}
	if _, exists := d.breakpoints[key]; !exists {
		return fmt.Errorf("no breakpoint at %s>>%s line %d", class, method, line)
	}
	delete(d.breakpoints, key)
	return nil
}

// ListBreakpoints returns all currently set breakpoints.
func (d *DebugServer) ListBreakpoints() []Breakpoint {
	d.mu.Lock()
	defer d.mu.Unlock()

	result := make([]Breakpoint, 0, len(d.breakpoints))
	id := 1
	for key, active := range d.breakpoints {
		result = append(result, Breakpoint{
			ID:     id,
			Class:  key.className,
			Method: key.methodName,
			Line:   key.line,
			Active: active,
		})
		id++
	}
	return result
}

// HasBreakpoint checks if there's a breakpoint at the given location.
func (d *DebugServer) HasBreakpoint(class, method string, line int) bool {
	d.mu.Lock()
	defer d.mu.Unlock()

	key := breakpointKey{
		className:  class,
		methodName: method,
		line:       line,
	}
	active, exists := d.breakpoints[key]
	return exists && active
}

// ClearAllBreakpoints removes all breakpoints.
func (d *DebugServer) ClearAllBreakpoints() {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.breakpoints = make(map[breakpointKey]bool)
}

// EnableBreakpoint enables a previously disabled breakpoint.
func (d *DebugServer) EnableBreakpoint(class, method string, line int) error {
	d.mu.Lock()
	defer d.mu.Unlock()

	key := breakpointKey{
		className:  class,
		methodName: method,
		line:       line,
	}
	if _, exists := d.breakpoints[key]; !exists {
		return fmt.Errorf("no breakpoint at %s>>%s line %d", class, method, line)
	}
	d.breakpoints[key] = true
	return nil
}

// DisableBreakpoint temporarily disables a breakpoint without removing it.
func (d *DebugServer) DisableBreakpoint(class, method string, line int) error {
	d.mu.Lock()
	defer d.mu.Unlock()

	key := breakpointKey{
		className:  class,
		methodName: method,
		line:       line,
	}
	if _, exists := d.breakpoints[key]; !exists {
		return fmt.Errorf("no breakpoint at %s>>%s line %d", class, method, line)
	}
	d.breakpoints[key] = false
	return nil
}

// ---------------------------------------------------------------------------
// Execution control
// ---------------------------------------------------------------------------

// Pause requests the interpreter to pause execution.
func (d *DebugServer) Pause() {
	d.mu.Lock()
	defer d.mu.Unlock()

	if !d.active {
		return
	}

	select {
	case d.pauseChan <- pauseRequest{reason: "user request"}:
	default:
		// Channel full, pause already requested
	}
}

// Resume continues execution after a pause.
func (d *DebugServer) Resume() {
	d.mu.Lock()
	d.paused = false
	d.stepMode = StepNone
	d.mu.Unlock()

	select {
	case d.resumeChan <- struct{}{}:
	default:
		// Channel full
	}

	d.sendEvent(DebugEvent{
		Type:   "continued",
		Reason: "resume",
	})
}

// StepOver steps to the next line, stepping over method calls.
func (d *DebugServer) StepOver(currentFrame int, currentLine int) {
	d.mu.Lock()
	d.stepMode = StepOver
	d.stepFrame = currentFrame
	d.stepLine = currentLine
	d.paused = false
	d.mu.Unlock()

	select {
	case d.resumeChan <- struct{}{}:
	default:
	}
}

// StepInto steps into the next method call.
func (d *DebugServer) StepInto() {
	d.mu.Lock()
	d.stepMode = StepInto
	d.paused = false
	d.mu.Unlock()

	select {
	case d.resumeChan <- struct{}{}:
	default:
	}
}

// StepOut steps out of the current method.
func (d *DebugServer) StepOut(currentFrame int) {
	d.mu.Lock()
	d.stepMode = StepOut
	d.stepFrame = currentFrame
	d.paused = false
	d.mu.Unlock()

	select {
	case d.resumeChan <- struct{}{}:
	default:
	}
}

// IsPaused returns whether execution is currently paused.
func (d *DebugServer) IsPaused() bool {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.paused
}

// ---------------------------------------------------------------------------
// Call stack inspection
// ---------------------------------------------------------------------------

// GetCallStack returns the current call stack from the interpreter.
// This is a helper that constructs StackFrame objects from interpreter state.
func (d *DebugServer) GetCallStack(interp *Interpreter) []StackFrame {
	d.mu.Lock()
	defer d.mu.Unlock()

	if interp == nil || interp.fp < 0 {
		return nil
	}

	frames := make([]StackFrame, 0, interp.fp+1)

	for i := interp.fp; i >= 0; i-- {
		frame := interp.frames[i]
		if frame == nil {
			continue
		}

		sf := StackFrame{
			ID: i,
		}

		if frame.Block != nil {
			// Block frame
			sf.IsBlock = true
			sf.Method = "[block]"
			if frame.Block.Outer != nil {
				sf.Method = "[block in " + frame.Block.Outer.Name() + "]"
				if frame.Block.Outer.Class() != nil {
					sf.Class = frame.Block.Outer.Class().Name
				}
			}
			// Get source location from block's source map
			loc := frame.Block.SourceLocation(frame.IP)
			if loc != nil {
				sf.Line = loc.Line
				sf.Column = loc.Column
			}
		} else if frame.Method != nil {
			// Method frame
			sf.Method = frame.Method.Name()
			if frame.Method.Class() != nil {
				sf.Class = frame.Method.Class().Name
			}
			// Get source location from method's source map
			loc := frame.Method.SourceLocation(frame.IP)
			if loc != nil {
				sf.Line = loc.Line
				sf.Column = loc.Column
			}
		}

		frames = append(frames, sf)
	}

	return frames
}

// ---------------------------------------------------------------------------
// Variable inspection
// ---------------------------------------------------------------------------

// GetVariables returns the variables visible in the specified stack frame.
func (d *DebugServer) GetVariables(interp *Interpreter, frameID int) []Variable {
	d.mu.Lock()
	defer d.mu.Unlock()

	if interp == nil || frameID < 0 || frameID > interp.fp {
		return nil
	}

	frame := interp.frames[frameID]
	if frame == nil {
		return nil
	}

	var variables []Variable

	// Add "self" (receiver)
	variables = append(variables, Variable{
		Name:  "self",
		Value: d.formatValue(frame.Receiver),
		Type:  d.typeOf(frame.Receiver),
	})

	// Add temporaries (arguments and locals)
	var tempNames []string
	var numTemps int

	if frame.Block != nil {
		numTemps = frame.Block.NumTemps
		// Block temporaries are just numbered for now
		for i := 0; i < numTemps; i++ {
			if i < frame.Block.Arity {
				tempNames = append(tempNames, fmt.Sprintf("arg%d", i))
			} else {
				tempNames = append(tempNames, fmt.Sprintf("temp%d", i-frame.Block.Arity))
			}
		}
	} else if frame.Method != nil {
		numTemps = frame.Method.NumTemps
		// Method temporaries - try to use meaningful names
		// For now, just use generic names
		for i := 0; i < numTemps; i++ {
			if i < frame.Method.Arity {
				tempNames = append(tempNames, fmt.Sprintf("arg%d", i))
			} else {
				tempNames = append(tempNames, fmt.Sprintf("temp%d", i-frame.Method.Arity))
			}
		}
	}

	// Read temporary values from the stack
	for i := 0; i < numTemps && i < len(tempNames); i++ {
		stackIndex := frame.BP + i
		if stackIndex < len(interp.stack) && stackIndex >= 0 {
			val := interp.stack[stackIndex]
			variables = append(variables, Variable{
				Name:  tempNames[i],
				Value: d.formatValue(val),
				Type:  d.typeOf(val),
			})
		}
	}

	// Add captured variables for blocks
	if frame.Block != nil && len(frame.Captures) > 0 {
		for i, val := range frame.Captures {
			variables = append(variables, Variable{
				Name:  fmt.Sprintf("captured%d", i),
				Value: d.formatValue(val),
				Type:  d.typeOf(val),
			})
		}
	}

	// Add instance variables if receiver is an object
	if frame.Receiver.IsObject() {
		obj := ObjectFromValue(frame.Receiver)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			class := obj.VTablePtr().Class()
			ivarNames := class.AllInstVarNames()
			for i, name := range ivarNames {
				if i < obj.NumSlots() {
					val := obj.GetSlot(i)
					variables = append(variables, Variable{
						Name:  "@" + name,
						Value: d.formatValue(val),
						Type:  d.typeOf(val),
					})
				}
			}
		}
	}

	return variables
}

// GetVariableValue returns the value of a specific variable in a frame.
func (d *DebugServer) GetVariableValue(interp *Interpreter, frameID int, name string) (Value, bool) {
	d.mu.Lock()
	defer d.mu.Unlock()

	if interp == nil || frameID < 0 || frameID > interp.fp {
		return Nil, false
	}

	frame := interp.frames[frameID]
	if frame == nil {
		return Nil, false
	}

	// Check for "self"
	if name == "self" {
		return frame.Receiver, true
	}

	// Check instance variables (prefixed with @)
	if len(name) > 1 && name[0] == '@' {
		ivarName := name[1:]
		if frame.Receiver.IsObject() {
			obj := ObjectFromValue(frame.Receiver)
			if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
				class := obj.VTablePtr().Class()
				idx := class.InstVarIndex(ivarName)
				if idx >= 0 && idx < obj.NumSlots() {
					return obj.GetSlot(idx), true
				}
			}
		}
		return Nil, false
	}

	// Variable not found
	return Nil, false
}

// ---------------------------------------------------------------------------
// Helper methods
// ---------------------------------------------------------------------------

// formatValue returns a string representation of a value for display.
func (d *DebugServer) formatValue(v Value) string {
	switch {
	case v == Nil:
		return "nil"
	case v == True:
		return "true"
	case v == False:
		return "false"
	case v.IsSmallInt():
		return fmt.Sprintf("%d", v.SmallInt())
	case v.IsFloat():
		return fmt.Sprintf("%g", v.Float64())
	case v.IsSymbol():
		if d.vm != nil {
			name := d.vm.Symbols.Name(v.SymbolID())
			return "#" + name
		}
		return fmt.Sprintf("#<symbol:%d>", v.SymbolID())
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			return fmt.Sprintf("a %s", obj.VTablePtr().Class().Name)
		}
		return "<object>"
	default:
		return "<unknown>"
	}
}

// typeOf returns the type name for a value.
func (d *DebugServer) typeOf(v Value) string {
	switch {
	case v == Nil:
		return "UndefinedObject"
	case v == True:
		return "True"
	case v == False:
		return "False"
	case v.IsSmallInt():
		return "SmallInteger"
	case v.IsFloat():
		return "Float"
	case v.IsSymbol():
		return "Symbol"
	case v.IsObject():
		obj := ObjectFromValue(v)
		if obj != nil && obj.VTablePtr() != nil && obj.VTablePtr().Class() != nil {
			return obj.VTablePtr().Class().Name
		}
		return "Object"
	default:
		return "Unknown"
	}
}

// sendEvent sends a debug event to listeners.
func (d *DebugServer) sendEvent(event DebugEvent) {
	select {
	case d.eventChan <- event:
	default:
		// Channel full, drop event
	}
}

// ---------------------------------------------------------------------------
// Interpreter integration hooks (to be called by interpreter)
// ---------------------------------------------------------------------------

// ShouldBreak checks if execution should break at the given location.
// This is called by the interpreter at each instruction.
// Returns true if execution should pause, and the reason for pausing.
func (d *DebugServer) ShouldBreak(class, method string, line, framePtr int) (bool, string) {
	d.mu.Lock()
	defer d.mu.Unlock()

	if !d.active {
		return false, ""
	}

	// Check for pending pause request
	select {
	case req := <-d.pauseChan:
		d.paused = true
		d.pauseReason = req.reason
		return true, req.reason
	default:
	}

	// Check breakpoints
	key := breakpointKey{
		className:  class,
		methodName: method,
		line:       line,
	}
	if active, exists := d.breakpoints[key]; exists && active {
		d.paused = true
		d.pauseReason = "breakpoint"
		return true, "breakpoint"
	}

	// Check stepping modes
	switch d.stepMode {
	case StepInto:
		// Break on any line change
		if line != d.stepLine {
			d.paused = true
			d.pauseReason = "step"
			d.stepMode = StepNone
			return true, "step"
		}

	case StepOver:
		// Break if we're at the same or higher frame level and line changed
		if framePtr <= d.stepFrame && line != d.stepLine {
			d.paused = true
			d.pauseReason = "step"
			d.stepMode = StepNone
			return true, "step"
		}

	case StepOut:
		// Break when we return to a lower frame level
		if framePtr < d.stepFrame {
			d.paused = true
			d.pauseReason = "step"
			d.stepMode = StepNone
			return true, "step"
		}
	}

	return false, ""
}

// WaitForResume blocks until execution should continue.
// This is called by the interpreter when paused.
func (d *DebugServer) WaitForResume() {
	<-d.resumeChan
}

// NotifyBreakpointHit sends a breakpoint hit event to clients.
func (d *DebugServer) NotifyBreakpointHit(location *SourceLocation) {
	d.sendEvent(DebugEvent{
		Type:     "breakpointHit",
		Reason:   "breakpoint",
		Location: location,
	})
}

// NotifyStopped sends a stopped event to clients.
func (d *DebugServer) NotifyStopped(reason string, location *SourceLocation) {
	d.sendEvent(DebugEvent{
		Type:     "stopped",
		Reason:   reason,
		Location: location,
	})
}

// NotifyException sends an exception event to clients.
func (d *DebugServer) NotifyException(message string, location *SourceLocation) {
	d.sendEvent(DebugEvent{
		Type:     "exception",
		Reason:   message,
		Location: location,
	})
}
