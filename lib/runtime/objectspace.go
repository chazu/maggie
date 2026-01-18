package runtime

import (
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/google/uuid"
)

// MethodFunc is the signature for native method implementations
type MethodFunc func(self *Instance, args []Value) Value

// MethodFlags describes method properties
type MethodFlags uint32

const (
	MethodNative       MethodFlags = 1 << iota // Implemented in native code
	MethodBashFallback                         // Falls back to bash if not found
	MethodClassMethod                          // Is a class method (not instance)
)

// MethodEntry describes a single method
type MethodEntry struct {
	Selector string
	Impl     MethodFunc
	NumArgs  int
	Flags    MethodFlags
}

// MethodTable holds instance and class methods for a class
type MethodTable struct {
	InstanceMethods map[string]*MethodEntry
	ClassMethods    map[string]*MethodEntry
}

// Class represents a registered Trashtalk class
type Class struct {
	Name         string
	Superclass   string
	SuperclassP  *Class // Resolved superclass pointer
	InstanceVars []string
	Methods      *MethodTable
	Initialized  bool
}

// NewMethodTable creates an empty method table
func NewMethodTable() *MethodTable {
	return &MethodTable{
		InstanceMethods: make(map[string]*MethodEntry),
		ClassMethods:    make(map[string]*MethodEntry),
	}
}

// AddInstanceMethod adds an instance method
func (mt *MethodTable) AddInstanceMethod(selector string, impl MethodFunc, numArgs int, flags MethodFlags) {
	mt.InstanceMethods[selector] = &MethodEntry{
		Selector: selector,
		Impl:     impl,
		NumArgs:  numArgs,
		Flags:    flags | MethodNative,
	}
}

// AddClassMethod adds a class method
func (mt *MethodTable) AddClassMethod(selector string, impl MethodFunc, numArgs int, flags MethodFlags) {
	mt.ClassMethods[selector] = &MethodEntry{
		Selector: selector,
		Impl:     impl,
		NumArgs:  numArgs,
		Flags:    flags | MethodNative | MethodClassMethod,
	}
}

// LookupInstanceMethod finds an instance method
func (mt *MethodTable) LookupInstanceMethod(selector string) *MethodEntry {
	if mt == nil {
		return nil
	}
	return mt.InstanceMethods[selector]
}

// LookupClassMethod finds a class method
func (mt *MethodTable) LookupClassMethod(selector string) *MethodEntry {
	if mt == nil {
		return nil
	}
	return mt.ClassMethods[selector]
}

// Instance represents a Trashtalk object instance
type Instance struct {
	ID        string
	Class     *Class
	ClassName string // Fully qualified class name
	Vars      map[string]Value
	CreatedAt time.Time
	mu        sync.RWMutex
}

// NewInstance creates a new instance of a class
func (os *ObjectSpace) NewInstance(className string) (*Instance, error) {
	class := os.GetClass(className)
	if class == nil {
		return nil, fmt.Errorf("unknown class: %s", className)
	}

	// Generate unique instance ID
	id := os.GenerateID(className)

	inst := &Instance{
		ID:        id,
		Class:     class,
		ClassName: className,
		Vars:      make(map[string]Value),
		CreatedAt: time.Now(),
	}

	// Initialize instance variables to nil
	for _, varName := range class.InstanceVars {
		inst.Vars[varName] = NilValue()
	}

	// Register in object space
	os.RegisterInstance(inst)

	return inst, nil
}

// GetVar gets an instance variable value
func (inst *Instance) GetVar(name string) Value {
	inst.mu.RLock()
	defer inst.mu.RUnlock()
	if v, ok := inst.Vars[name]; ok {
		return v
	}
	return NilValue()
}

// SetVar sets an instance variable value
func (inst *Instance) SetVar(name string, v Value) {
	inst.mu.Lock()
	defer inst.mu.Unlock()
	inst.Vars[name] = v
}

// GetVarString gets an instance variable as a string
func (inst *Instance) GetVarString(name string) string {
	return inst.GetVar(name).AsString()
}

// SetVarString sets an instance variable from a string
func (inst *Instance) SetVarString(name string, s string) {
	inst.SetVar(name, StringValue(s))
}

// ToJSON serializes the instance to JSON
func (inst *Instance) ToJSON() string {
	inst.mu.RLock()
	defer inst.mu.RUnlock()

	// Build JSON manually for performance
	result := fmt.Sprintf(`{"class":"%s","created_at":"%s"`,
		inst.ClassName, inst.CreatedAt.Format(time.RFC3339))

	// Add instance variables
	varNames := make([]string, 0, len(inst.Vars))
	for name := range inst.Vars {
		varNames = append(varNames, name)
	}

	if len(varNames) > 0 {
		result += `,"_vars":["` + strings.Join(varNames, `","`) + `"]`
	}

	for name, val := range inst.Vars {
		result += fmt.Sprintf(`,"%s":%s`, name, val.ToJSON())
	}

	result += "}"
	return result
}

// ObjectSpace manages all instances and classes in the runtime
type ObjectSpace struct {
	classes   map[string]*Class
	instances map[string]*Instance
	classMu   sync.RWMutex
	instMu    sync.RWMutex
}

// NewObjectSpace creates a new empty object space
func NewObjectSpace() *ObjectSpace {
	return &ObjectSpace{
		classes:   make(map[string]*Class),
		instances: make(map[string]*Instance),
	}
}

// RegisterClass registers a class with the object space
func (os *ObjectSpace) RegisterClass(name, superclass string, instanceVars []string, methods *MethodTable) *Class {
	os.classMu.Lock()
	defer os.classMu.Unlock()

	class := &Class{
		Name:         name,
		Superclass:   superclass,
		InstanceVars: instanceVars,
		Methods:      methods,
		Initialized:  true,
	}

	// Resolve superclass if it exists
	if superclass != "" && superclass != "Object" {
		if super, ok := os.classes[superclass]; ok {
			class.SuperclassP = super
			// Inherit instance variables from superclass
			inherited := make([]string, 0, len(super.InstanceVars)+len(instanceVars))
			inherited = append(inherited, super.InstanceVars...)
			inherited = append(inherited, instanceVars...)
			class.InstanceVars = inherited
		}
	}

	os.classes[name] = class
	return class
}

// GetClass retrieves a registered class
func (os *ObjectSpace) GetClass(name string) *Class {
	os.classMu.RLock()
	defer os.classMu.RUnlock()
	return os.classes[name]
}

// RegisterInstance adds an instance to the object space
func (os *ObjectSpace) RegisterInstance(inst *Instance) {
	os.instMu.Lock()
	defer os.instMu.Unlock()
	os.instances[inst.ID] = inst
}

// GetInstance retrieves an instance by ID
func (os *ObjectSpace) GetInstance(id string) *Instance {
	os.instMu.RLock()
	defer os.instMu.RUnlock()
	return os.instances[id]
}

// RemoveInstance removes an instance from the object space
func (os *ObjectSpace) RemoveInstance(id string) {
	os.instMu.Lock()
	defer os.instMu.Unlock()
	delete(os.instances, id)
}

// LookupMethod finds a method, walking up the class hierarchy
func (os *ObjectSpace) LookupMethod(className, selector string, isClassMethod bool) *MethodEntry {
	os.classMu.RLock()
	defer os.classMu.RUnlock()

	class := os.classes[className]
	for class != nil {
		var method *MethodEntry
		if isClassMethod {
			method = class.Methods.LookupClassMethod(selector)
		} else {
			method = class.Methods.LookupInstanceMethod(selector)
		}
		if method != nil {
			return method
		}
		class = class.SuperclassP
	}
	return nil
}

// ClassNames returns all registered class names
func (os *ObjectSpace) ClassNames() []string {
	os.classMu.RLock()
	defer os.classMu.RUnlock()

	names := make([]string, 0, len(os.classes))
	for name := range os.classes {
		names = append(names, name)
	}
	return names
}

// InstanceCount returns the number of instances in the object space
func (os *ObjectSpace) InstanceCount() int {
	os.instMu.RLock()
	defer os.instMu.RUnlock()
	return len(os.instances)
}

// ClassCount returns the number of classes registered
func (os *ObjectSpace) ClassCount() int {
	os.classMu.RLock()
	defer os.classMu.RUnlock()
	return len(os.classes)
}

// GenerateID creates a new unique instance ID for the given class name
func (os *ObjectSpace) GenerateID(className string) string {
	idPrefix := strings.ToLower(strings.ReplaceAll(className, "::", "_"))
	return idPrefix + "_" + uuid.New().String()
}
