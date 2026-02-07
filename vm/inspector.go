package vm

import (
	"fmt"
	"strings"
)

// Inspector provides debugging inspection of Maggie Values.
// It can recursively inspect objects and their instance variables,
// providing a structured view of any value in the VM.
type Inspector struct {
	vm *VM
}

// InspectionResult contains structured information about an inspected value.
type InspectionResult struct {
	Type      string              // Value type: SmallInt, Float, Symbol, Object, True, False, Nil, Channel, Process, Result
	Value     string              // String representation of the value
	ClassName string              // For objects: the class name
	InstVars  []InstVarInfo       // For objects: instance variable names and values
	Size      int                 // For collections: number of elements
	Elements  []*InspectionResult // For collections: preview of elements (limited)
}

// InstVarInfo contains information about a single instance variable.
type InstVarInfo struct {
	Name  string
	Value *InspectionResult
}

// MaxElementPreview is the maximum number of collection elements to preview.
const MaxElementPreview = 10

// DefaultMaxDepth is the default recursion depth for inspection.
const DefaultMaxDepth = 3

// NewInspector creates a new Inspector attached to the given VM.
func NewInspector(vm *VM) *Inspector {
	return &Inspector{vm: vm}
}

// Inspect inspects a value with the default maximum depth.
func (i *Inspector) Inspect(v Value) *InspectionResult {
	return i.InspectDepth(v, DefaultMaxDepth)
}

// InspectDepth inspects a value with a specified maximum recursion depth.
// When depth reaches 0, nested objects are shown as summaries only.
func (i *Inspector) InspectDepth(v Value, depth int) *InspectionResult {
	result := &InspectionResult{}

	switch {
	case v == Nil:
		result.Type = "Nil"
		result.Value = "nil"

	case v == True:
		result.Type = "True"
		result.Value = "true"

	case v == False:
		result.Type = "False"
		result.Value = "false"

	case v.IsSmallInt():
		result.Type = "SmallInt"
		result.Value = fmt.Sprintf("%d", v.SmallInt())

	case v.IsFloat():
		result.Type = "Float"
		result.Value = fmt.Sprintf("%g", v.Float64())

	case v.IsSymbol():
		// Check for special symbol-encoded values first
		if isClassValue(v) {
			return i.inspectClass(v, depth)
		}
		if isChannelValue(v) {
			return i.inspectChannel(v, depth)
		}
		if isProcessValue(v) {
			return i.inspectProcess(v, depth)
		}
		if isResultValue(v) {
			return i.inspectResult(v, depth)
		}
		// Regular symbol
		result.Type = "Symbol"
		if i.vm != nil {
			name := i.vm.Symbols.Name(v.SymbolID())
			result.Value = fmt.Sprintf("#%s", name)
		} else {
			result.Value = fmt.Sprintf("#<symbol:%d>", v.SymbolID())
		}

	case v.IsObject():
		return i.inspectObject(v, depth)

	default:
		result.Type = "Unknown"
		result.Value = fmt.Sprintf("<unknown:0x%016x>", uint64(v))
	}

	return result
}

// inspectObject handles inspection of heap-allocated objects.
func (i *Inspector) inspectObject(v Value, depth int) *InspectionResult {
	result := &InspectionResult{
		Type: "Object",
	}

	obj := ObjectFromValue(v)
	if obj == nil {
		result.Value = "<invalid object>"
		return result
	}

	// Get class name
	result.ClassName = obj.ClassName()
	if result.ClassName == "?" {
		result.ClassName = "Object"
	}

	// Create a summary representation
	result.Value = fmt.Sprintf("a %s", result.ClassName)

	// If we've reached max depth, just return the summary
	if depth <= 0 {
		return result
	}

	// Get instance variable information from the class
	vt := obj.VTablePtr()
	if vt != nil && vt.Class() != nil {
		class := vt.Class()
		instVars := i.collectInstVars(class)

		// Use class.NumSlots to determine how many slots are actually defined,
		// not obj.NumSlots() which includes all inline slots even if unused
		classNumSlots := class.NumSlots

		// Inspect named instance variables
		for idx := 0; idx < len(instVars); idx++ {
			slotVal := obj.GetSlot(idx)
			varInfo := InstVarInfo{
				Name:  instVars[idx],
				Value: i.InspectDepth(slotVal, depth-1),
			}
			result.InstVars = append(result.InstVars, varInfo)
		}

		// Handle anonymous slots (slots without names, but within class's NumSlots)
		for idx := len(instVars); idx < classNumSlots; idx++ {
			slotVal := obj.GetSlot(idx)
			varInfo := InstVarInfo{
				Name:  fmt.Sprintf("slot%d", idx),
				Value: i.InspectDepth(slotVal, depth-1),
			}
			result.InstVars = append(result.InstVars, varInfo)
		}

		// Check if this might be a collection-like object
		if result.ClassName == "Array" || result.ClassName == "OrderedCollection" {
			// Use obj.NumSlots() for variable-sized objects (arrays store
			// their logical size separately from class.NumSlots)
			actualSize := obj.NumSlots()
			result.Size = actualSize
			// Limit elements preview
			previewCount := actualSize
			if previewCount > MaxElementPreview {
				previewCount = MaxElementPreview
			}
			for idx := 0; idx < previewCount; idx++ {
				slotVal := obj.GetSlot(idx)
				result.Elements = append(result.Elements, i.InspectDepth(slotVal, depth-1))
			}
		}
	}

	return result
}

// inspectChannel handles inspection of Channel values.
func (i *Inspector) inspectChannel(v Value, depth int) *InspectionResult {
	result := &InspectionResult{
		Type:      "Channel",
		ClassName: "Channel",
	}

	ch := i.vm.getChannel(v)
	if ch == nil {
		result.Value = "<invalid channel>"
		return result
	}

	// Get channel state
	closed := ch.closed.Load()
	size := len(ch.ch)
	capacity := cap(ch.ch)

	if closed {
		result.Value = fmt.Sprintf("a Channel (closed, capacity: %d)", capacity)
	} else if capacity > 0 {
		result.Value = fmt.Sprintf("a Channel (buffered, size: %d, capacity: %d)", size, capacity)
	} else {
		result.Value = fmt.Sprintf("a Channel (unbuffered)")
	}

	result.Size = size

	return result
}

// inspectProcess handles inspection of Process values.
func (i *Inspector) inspectProcess(v Value, depth int) *InspectionResult {
	result := &InspectionResult{
		Type:      "Process",
		ClassName: "Process",
	}

	proc := i.vm.getProcess(v)
	if proc == nil {
		result.Value = "<invalid process>"
		return result
	}

	// Get process state
	state := ProcessState(proc.state.Load())
	stateStr := "unknown"
	switch state {
	case ProcessRunning:
		stateStr = "running"
	case ProcessSuspended:
		stateStr = "suspended"
	case ProcessTerminated:
		stateStr = "terminated"
	}

	result.Value = fmt.Sprintf("a Process (id: %d, state: %s)", proc.id, stateStr)

	// If terminated and we have depth, show the result
	if state == ProcessTerminated && depth > 0 {
		proc.mu.Lock()
		procResult := proc.result
		proc.mu.Unlock()

		result.InstVars = append(result.InstVars, InstVarInfo{
			Name:  "result",
			Value: i.InspectDepth(procResult, depth-1),
		})
	}

	return result
}

// inspectResult handles inspection of Result (Success/Failure) values.
func (i *Inspector) inspectResult(v Value, depth int) *InspectionResult {
	result := &InspectionResult{
		Type: "Result",
	}

	r := i.vm.registry.GetResultFromValue(v)
	if r == nil {
		result.Value = "<invalid result>"
		return result
	}

	switch r.resultType {
	case ResultSuccess:
		result.Type = "Success"
		result.ClassName = "Success"
		if depth > 0 {
			valueResult := i.InspectDepth(r.value, depth-1)
			result.Value = fmt.Sprintf("Success(%s)", valueResult.Value)
			result.InstVars = append(result.InstVars, InstVarInfo{
				Name:  "value",
				Value: valueResult,
			})
		} else {
			result.Value = "a Success"
		}

	case ResultFailure:
		result.Type = "Failure"
		result.ClassName = "Failure"
		if depth > 0 {
			errorResult := i.InspectDepth(r.value, depth-1)
			result.Value = fmt.Sprintf("Failure(%s)", errorResult.Value)
			result.InstVars = append(result.InstVars, InstVarInfo{
				Name:  "error",
				Value: errorResult,
			})
		} else {
			result.Value = "a Failure"
		}
	}

	return result
}

// inspectClass handles inspection of Class values.
func (i *Inspector) inspectClass(v Value, depth int) *InspectionResult {
	result := &InspectionResult{
		Type:      "Class",
		ClassName: "Class",
	}

	cls := i.vm.registry.GetClassFromValue(v)
	if cls == nil {
		result.Value = "<invalid class>"
		return result
	}

	result.Value = cls.Name

	if depth <= 0 {
		return result
	}

	// superclass
	if cls.Superclass != nil {
		superVal := i.vm.registry.RegisterClassValue(cls.Superclass)
		result.InstVars = append(result.InstVars, InstVarInfo{
			Name:  "superclass",
			Value: i.InspectDepth(superVal, depth-1),
		})
	} else {
		result.InstVars = append(result.InstVars, InstVarInfo{
			Name:  "superclass",
			Value: &InspectionResult{Type: "Nil", Value: "nil"},
		})
	}

	// instanceVariables (names as an array description)
	ivarNames := cls.AllInstVarNames()
	ivarStr := fmt.Sprintf("#(%s)", strings.Join(ivarNames, " "))
	result.InstVars = append(result.InstVars, InstVarInfo{
		Name:  "instanceVariables",
		Value: &InspectionResult{Type: "Array", Value: ivarStr, Size: len(ivarNames)},
	})

	// methodCount
	methodCount := 0
	if cls.VTable != nil {
		methodCount = len(cls.VTable.LocalMethods())
	}
	result.InstVars = append(result.InstVars, InstVarInfo{
		Name:  "methodCount",
		Value: &InspectionResult{Type: "SmallInt", Value: fmt.Sprintf("%d", methodCount)},
	})

	return result
}

// collectInstVars collects all instance variable names from a class hierarchy.
func (i *Inspector) collectInstVars(class *Class) []string {
	var vars []string

	// Collect from superclasses first (so inherited vars come before local vars)
	if class.Superclass != nil {
		vars = i.collectInstVars(class.Superclass)
	}

	// Add this class's instance variables
	vars = append(vars, class.InstVars...)

	return vars
}

// String returns a pretty-printed representation of the inspection result.
func (r *InspectionResult) String() string {
	return r.stringWithIndent(0)
}

// stringWithIndent creates a string representation with the given indentation level.
func (r *InspectionResult) stringWithIndent(indent int) string {
	var sb strings.Builder
	prefix := strings.Repeat("  ", indent)

	// Type and value header
	sb.WriteString(prefix)
	sb.WriteString(r.Type)
	sb.WriteString(": ")
	sb.WriteString(r.Value)
	sb.WriteString("\n")

	// Class name (if different from type)
	if r.ClassName != "" && r.ClassName != r.Type {
		sb.WriteString(prefix)
		sb.WriteString("  class: ")
		sb.WriteString(r.ClassName)
		sb.WriteString("\n")
	}

	// Instance variables
	if len(r.InstVars) > 0 {
		sb.WriteString(prefix)
		sb.WriteString("  instance variables:\n")
		for _, iv := range r.InstVars {
			sb.WriteString(prefix)
			sb.WriteString("    ")
			sb.WriteString(iv.Name)
			sb.WriteString(": ")
			if iv.Value != nil {
				sb.WriteString(iv.Value.Value)
			} else {
				sb.WriteString("<nil>")
			}
			sb.WriteString("\n")
		}
	}

	// Collection elements
	if len(r.Elements) > 0 {
		sb.WriteString(prefix)
		sb.WriteString(fmt.Sprintf("  elements (showing %d of %d):\n", len(r.Elements), r.Size))
		for idx, elem := range r.Elements {
			sb.WriteString(prefix)
			sb.WriteString(fmt.Sprintf("    [%d]: ", idx))
			if elem != nil {
				sb.WriteString(elem.Value)
			} else {
				sb.WriteString("<nil>")
			}
			sb.WriteString("\n")
		}
	}

	return sb.String()
}

// PrettyPrint returns a detailed multi-line representation with full nesting.
func (r *InspectionResult) PrettyPrint() string {
	return r.prettyPrintWithIndent(0)
}

// prettyPrintWithIndent creates a detailed representation with nesting.
func (r *InspectionResult) prettyPrintWithIndent(indent int) string {
	var sb strings.Builder
	prefix := strings.Repeat("  ", indent)

	// Type and value header
	sb.WriteString(prefix)
	sb.WriteString(r.Type)
	sb.WriteString(": ")
	sb.WriteString(r.Value)
	sb.WriteString("\n")

	// Class name (if present and different from type)
	if r.ClassName != "" && r.ClassName != r.Type {
		sb.WriteString(prefix)
		sb.WriteString("  class: ")
		sb.WriteString(r.ClassName)
		sb.WriteString("\n")
	}

	// Instance variables with full recursion
	if len(r.InstVars) > 0 {
		sb.WriteString(prefix)
		sb.WriteString("  instance variables:\n")
		for _, iv := range r.InstVars {
			sb.WriteString(prefix)
			sb.WriteString("    ")
			sb.WriteString(iv.Name)
			sb.WriteString(":\n")
			if iv.Value != nil {
				sb.WriteString(iv.Value.prettyPrintWithIndent(indent + 3))
			} else {
				sb.WriteString(prefix)
				sb.WriteString("      <nil>\n")
			}
		}
	}

	// Collection elements with full recursion
	if len(r.Elements) > 0 {
		sb.WriteString(prefix)
		sb.WriteString(fmt.Sprintf("  elements (showing %d of %d):\n", len(r.Elements), r.Size))
		for idx, elem := range r.Elements {
			sb.WriteString(prefix)
			sb.WriteString(fmt.Sprintf("    [%d]:\n", idx))
			if elem != nil {
				sb.WriteString(elem.prettyPrintWithIndent(indent + 3))
			} else {
				sb.WriteString(prefix)
				sb.WriteString("      <nil>\n")
			}
		}
	}

	return sb.String()
}
