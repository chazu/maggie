package runtime

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"strings"
)

// BlockKind indicates the execution mode of a block.
type BlockKind int

const (
	// BlockKindBash executes the block via the Bash runtime.
	// This is the fallback mode for blocks that can't be compiled to bytecode.
	BlockKindBash BlockKind = iota

	// BlockKindBytecode executes the block via the bytecode VM.
	// This provides native-speed execution for most block patterns.
	BlockKindBytecode

	// BlockKindNative represents a block inlined directly into Go code.
	// Used for iteration methods like do:, collect:, select: where the
	// block body is compiled as part of the enclosing method.
	BlockKindNative
)

// String returns a human-readable name for the block kind.
func (k BlockKind) String() string {
	switch k {
	case BlockKindBash:
		return "bash"
	case BlockKindBytecode:
		return "bytecode"
	case BlockKindNative:
		return "native"
	default:
		return fmt.Sprintf("BlockKind(%d)", k)
	}
}

// BlockInterface abstracts over different block implementations.
// This allows native code to work with blocks regardless of how they're stored
// or executed. Both Bash-based and bytecode-based blocks implement this interface.
type BlockInterface interface {
	// NumArgs returns the number of parameters the block expects.
	NumArgs() int

	// ParamNames returns the parameter names in order.
	// Returns nil for blocks with no parameters.
	ParamNames() []string

	// Kind returns how this block should be executed.
	Kind() BlockKind

	// Invoke executes the block with the given arguments.
	// The context provides access to captured variables and the runtime.
	// Returns the block's result and any error that occurred.
	Invoke(ctx *BlockContext, args ...string) (string, error)
}

// BlockContext provides the execution context for a block.
// This includes access to captured variables and the runtime environment.
type BlockContext struct {
	// Runtime provides access to instance persistence and message sending.
	Runtime *Runtime

	// InstanceID is the receiver that created this block.
	// Used for accessing instance variables from within the block.
	InstanceID string

	// Captured holds reference-captured variables.
	// Changes made inside the block are visible outside.
	Captured map[string]*CapturedVar

	// ParentFrame points to the enclosing block context for nested blocks.
	// This allows capture chains to be traversed for variable resolution.
	ParentFrame *BlockContext
}

// NewBlockContext creates a new block execution context.
func NewBlockContext(rt *Runtime, instanceID string) *BlockContext {
	return &BlockContext{
		Runtime:    rt,
		InstanceID: instanceID,
		Captured:   make(map[string]*CapturedVar),
	}
}

// GetCaptured retrieves a captured variable by name.
// Returns nil if the variable is not captured.
func (ctx *BlockContext) GetCaptured(name string) *CapturedVar {
	if v, ok := ctx.Captured[name]; ok {
		return v
	}
	// Check parent frames for nested blocks
	if ctx.ParentFrame != nil {
		return ctx.ParentFrame.GetCaptured(name)
	}
	return nil
}

// SetCaptured adds or updates a captured variable.
func (ctx *BlockContext) SetCaptured(name string, v *CapturedVar) {
	ctx.Captured[name] = v
}

// CapturedVar represents a mutable captured variable.
// Changes made inside a block are visible outside (reference semantics).
type CapturedVar struct {
	// Name is the variable name (for debugging and ivar writeback).
	Name string

	// Value is the current value of the variable.
	Value string

	// Source indicates where this variable comes from.
	Source VarSource
}

// VarSource indicates where a captured variable originates.
type VarSource int

const (
	// VarSourceLocal indicates a local variable in the enclosing scope.
	// Changes are kept in memory only.
	VarSourceLocal VarSource = iota

	// VarSourceIVar indicates an instance variable.
	// Changes are persisted to SQLite.
	VarSourceIVar

	// VarSourceGlobal indicates a global or class variable.
	VarSourceGlobal

	// VarSourceCapture indicates an already-captured variable from an outer scope.
	// Used for nested blocks that capture variables from enclosing blocks.
	VarSourceCapture
)

// String returns a human-readable name for the variable source.
func (s VarSource) String() string {
	switch s {
	case VarSourceLocal:
		return "local"
	case VarSourceIVar:
		return "ivar"
	case VarSourceGlobal:
		return "global"
	case VarSourceCapture:
		return "capture"
	default:
		return fmt.Sprintf("VarSource(%d)", s)
	}
}

// NewCapturedVar creates a new captured variable.
func NewCapturedVar(name, value string, source VarSource) *CapturedVar {
	return &CapturedVar{
		Name:   name,
		Value:  value,
		Source: source,
	}
}

// Get returns the current value of the captured variable.
func (v *CapturedVar) Get() string {
	return v.Value
}

// Set updates the value of the captured variable.
func (v *CapturedVar) Set(value string) {
	v.Value = value
}

// BashBlock represents a block that executes via the Bash runtime.
// This is used for blocks that can't be compiled to bytecode or as a fallback.
type BashBlock struct {
	// ID is the block identifier used by the Bash runtime.
	ID string

	// Params are the parameter names for this block.
	Params []string
}

// NumArgs implements BlockInterface.
func (b *BashBlock) NumArgs() int {
	return len(b.Params)
}

// ParamNames implements BlockInterface.
func (b *BashBlock) ParamNames() []string {
	return b.Params
}

// Kind implements BlockInterface.
func (b *BashBlock) Kind() BlockKind {
	return BlockKindBash
}

// Invoke implements BlockInterface.
// Executes the block via the Bash runtime.
func (b *BashBlock) Invoke(ctx *BlockContext, args ...string) (string, error) {
	if ctx == nil || ctx.Runtime == nil {
		return "", fmt.Errorf("no runtime context for bash block invocation")
	}
	return ctx.Runtime.InvokeBashBlock(b.ID, args...)
}

// InvokeBashBlock invokes a Bash-based block through the trash-send script.
// This is the fallback path for blocks that aren't compiled to bytecode.
func (r *Runtime) InvokeBashBlock(blockID string, args ...string) (string, error) {
	// Build command: trash-send <blockID> value/valueWith:/etc with args
	dispatchScript := filepath.Join(r.trashtalkRoot, "bin", "trash-send")

	// Determine selector based on arg count
	var selector string
	switch len(args) {
	case 0:
		selector = "value"
	case 1:
		selector = "valueWith:"
	case 2:
		selector = "valueWith:and:"
	default:
		selector = "valueWithArgs:"
	}

	cmdArgs := append([]string{blockID, selector}, args...)
	cmd := exec.Command(dispatchScript, cmdArgs...)
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("invoking bash block %s: %w", blockID, err)
	}

	return strings.TrimSpace(string(output)), nil
}
