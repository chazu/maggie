package pipeline

import (
	"fmt"
	"strings"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	"github.com/chazu/maggie/vm"
)

// RehydrateFromStore compiles all uncompiled content in the VM's
// ContentStore into runnable classes and methods.
// Returns the number of methods compiled.
func RehydrateFromStore(vmInst *vm.VM) (int, error) {
	store := vmInst.ContentStore()

	// Phase 1: Collect class digests that need rehydration.
	// A class needs rehydration if it exists in the ContentStore
	// but NOT in the VM's ClassTable (or Globals).
	var toRehydrate []*vm.ClassDigest
	for _, h := range store.ClassHashes() {
		d := store.LookupClass(h)
		if d == nil {
			continue
		}
		fqn := classFQN(d.Name, d.Namespace)
		if vmInst.Classes.Lookup(fqn) != nil {
			continue
		}
		// Also check bare name for non-namespaced classes
		if d.Namespace == "" && vmInst.Classes.Lookup(d.Name) != nil {
			continue
		}
		toRehydrate = append(toRehydrate, d)
	}

	if len(toRehydrate) == 0 {
		return 0, nil
	}

	// Phase 2: Topological sort by superclass dependency.
	// Classes whose superclass is already in the VM sort first.
	// Classes whose superclass is also being rehydrated must come after it.
	sorted, err := topoSortClasses(toRehydrate, vmInst)
	if err != nil {
		return 0, fmt.Errorf("rehydrate: topological sort failed: %w", err)
	}

	// Phase 3: Create class skeletons (mirrors pass 1a/1b from CompileAll).
	classMap := make(map[string]*vm.Class) // FQN -> class
	for _, d := range sorted {
		fqn := classFQN(d.Name, d.Namespace)

		// Resolve superclass
		var superclass *vm.Class
		if d.SuperclassName == "" || d.SuperclassName == "Object" {
			superclass = vmInst.ObjectClass
		} else {
			// Check VM ClassTable first
			superclass = vmInst.Classes.Lookup(d.SuperclassName)
			if superclass == nil {
				// Check among classes we just created in this batch
				superclass = classMap[d.SuperclassName]
			}
			if superclass == nil && d.Namespace != "" {
				// Try with namespace prefix
				superclass = vmInst.Classes.Lookup(d.Namespace + "::" + d.SuperclassName)
				if superclass == nil {
					superclass = classMap[d.Namespace+"::"+d.SuperclassName]
				}
			}
			if superclass == nil {
				return 0, fmt.Errorf("rehydrate: class %s superclass %s not found", fqn, d.SuperclassName)
			}
		}

		// Create new Class with name, namespace, superclass, instVars
		class := vm.NewClassWithInstVars(d.Name, superclass, d.InstVars)
		if d.Namespace != "" {
			class.Namespace = d.Namespace
		}
		if d.DocString != "" {
			class.DocString = d.DocString
		}
		if len(d.ClassVars) > 0 {
			class.ClassVars = make([]string, len(d.ClassVars))
			copy(class.ClassVars, d.ClassVars)
		}

		// Register in VM ClassTable and Globals
		vmInst.Classes.Register(class)
		classVal := vmInst.ClassValue(class)
		if d.Namespace != "" {
			vmInst.Globals[fqn] = classVal
		} else {
			vmInst.Globals[d.Name] = classVal
		}

		// Mark as received from network (not locally loaded)
		vmInst.MarkRehydrated(fqn)

		classMap[fqn] = class
	}

	// Phase 4: Compile methods.
	compiled := 0
	for _, d := range sorted {
		fqn := classFQN(d.Name, d.Namespace)
		class := classMap[fqn]
		if class == nil {
			class = vmInst.Classes.Lookup(fqn)
		}
		if class == nil {
			return compiled, fmt.Errorf("rehydrate: class %s not found after skeleton creation", fqn)
		}

		allIvars := class.AllInstVarNames()

		for _, mh := range d.MethodHashes {
			stub := store.LookupMethod(mh)
			if stub == nil {
				return compiled, fmt.Errorf("rehydrate: method hash not found in store for class %s", fqn)
			}

			if stub.Source == "" {
				// No source text -- cannot recompile
				continue
			}

			// Determine if class-side from source prefix
			isClassSide := strings.HasPrefix(stub.Source, "classMethod:")

			// Parse the method source
			methodDef, err := compiler.ParseMethodDef(stub.Source)
			if err != nil {
				return compiled, fmt.Errorf("rehydrate: parse error for %s>>%s: %w", fqn, stub.Name(), err)
			}

			// Compile the method.
			// Note: imports are empty for now -- FQN resolution happened at original
			// compile time, but source has bare names. This is a known limitation:
			// rehydrated code relies on class names being globally unique or already
			// FQN-qualified in the source.
			var ivars []string
			if !isClassSide {
				ivars = allIvars
			}
			method, err := compiler.CompileMethodDefWithContext(
				methodDef,
				vmInst.Selectors,
				vmInst.Symbols,
				vmInst.Registry(),
				ivars,
				d.Namespace,
				nil, // no imports -- FQN already in source from original compilation
				vmInst.Classes,
			)
			if err != nil {
				return compiled, fmt.Errorf("rehydrate: compile error for %s>>%s: %w", fqn, methodDef.Selector, err)
			}

			// Set source text on compiled method
			method.Source = stub.Source
			if methodDef.DocString != "" {
				method.SetDocString(methodDef.DocString)
			}

			// Compute content hash and verify it matches the stored hash
			instVarMap := make(map[string]int, len(ivars))
			for idx, name := range ivars {
				instVarMap[name] = idx
			}
			computedHash := hash.HashMethod(methodDef, instVarMap, func(name string) string {
				return resolveGlobalForHash(name, d.Namespace, nil, vmInst.Classes)
			})
			method.SetContentHash(computedHash)

			if computedHash != mh {
				return compiled, fmt.Errorf("rehydrate: hash mismatch for %s>>%s: computed %x, expected %x",
					fqn, methodDef.Selector, computedHash[:8], mh[:8])
			}

			// Install in class VTable (or ClassVTable if class-side)
			method.SetClass(class)
			selectorID := vmInst.Selectors.Intern(method.Name())
			if isClassSide {
				method.IsClassMethod = true
				class.ClassVTable.AddMethod(selectorID, method)
			} else {
				class.VTable.AddMethod(selectorID, method)
			}

			// Phase 5: Re-index in ContentStore -- replace stub with real compiled method
			store.IndexMethod(method)

			compiled++
		}
	}

	return compiled, nil
}

// classFQN returns "namespace::name" if namespace is non-empty, otherwise just "name".
func classFQN(name, namespace string) string {
	if namespace != "" {
		return namespace + "::" + name
	}
	return name
}

// topoSortClasses performs a topological sort of class digests by superclass dependency.
// Classes whose superclass is already in the VM come first. Classes that depend on
// other classes in the batch come after their dependencies.
func topoSortClasses(digests []*vm.ClassDigest, vmInst *vm.VM) ([]*vm.ClassDigest, error) {
	// Build lookup maps
	byFQN := make(map[string]*vm.ClassDigest, len(digests))
	byName := make(map[string]*vm.ClassDigest, len(digests))
	for _, d := range digests {
		fqn := classFQN(d.Name, d.Namespace)
		byFQN[fqn] = d
		byName[d.Name] = d
	}

	// Kahn's algorithm
	// Compute in-degree: count how many batch-internal dependencies each class has
	inDegree := make(map[string]int, len(digests))
	for _, d := range digests {
		fqn := classFQN(d.Name, d.Namespace)
		if _, ok := inDegree[fqn]; !ok {
			inDegree[fqn] = 0
		}

		if d.SuperclassName == "" || d.SuperclassName == "Object" {
			continue
		}

		// Check if the superclass is in this batch
		superInBatch := false
		if _, ok := byFQN[d.SuperclassName]; ok {
			superInBatch = true
		} else if _, ok := byName[d.SuperclassName]; ok {
			superInBatch = true
		} else if d.Namespace != "" {
			if _, ok := byFQN[d.Namespace+"::"+d.SuperclassName]; ok {
				superInBatch = true
			}
		}

		if superInBatch {
			inDegree[fqn]++
		}
	}

	// Collect nodes with zero in-degree
	var queue []string
	for _, d := range digests {
		fqn := classFQN(d.Name, d.Namespace)
		if inDegree[fqn] == 0 {
			queue = append(queue, fqn)
		}
	}

	var sorted []*vm.ClassDigest
	for len(queue) > 0 {
		fqn := queue[0]
		queue = queue[1:]
		d := byFQN[fqn]
		sorted = append(sorted, d)

		// Find dependents: classes whose superclass is d.Name or fqn
		for _, dep := range digests {
			depFQN := classFQN(dep.Name, dep.Namespace)
			if depFQN == fqn {
				continue
			}

			superName := dep.SuperclassName
			if superName == "" {
				continue
			}

			// Check if dep depends on d
			isDep := false
			if superName == d.Name || superName == fqn {
				isDep = true
			} else if dep.Namespace != "" && dep.Namespace+"::"+superName == fqn {
				isDep = true
			}

			if isDep {
				inDegree[depFQN]--
				if inDegree[depFQN] == 0 {
					queue = append(queue, depFQN)
				}
			}
		}
	}

	if len(sorted) != len(digests) {
		return nil, fmt.Errorf("circular superclass dependency detected among %d classes", len(digests))
	}

	return sorted, nil
}

// verifyMethodHash computes a content hash from a method's AST and compares
// it to an expected hash. This is used to verify that recompiled methods
// match their stored content hashes.
func verifyMethodHash(methodDef *compiler.MethodDef, instVars map[string]int, namespace string, classes *vm.ClassTable, expected [32]byte) bool {
	computed := hash.HashMethod(methodDef, instVars, func(name string) string {
		return resolveGlobalForHash(name, namespace, nil, classes)
	})
	return computed == expected
}
