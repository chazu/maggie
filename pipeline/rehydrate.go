package pipeline

import (
	"bytes"
	"fmt"
	"sort"
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
	byFQN := make(map[string]*vm.ClassDigest)
	for _, h := range store.ClassHashes() {
		d := store.LookupClass(h)
		if d == nil {
			continue
		}
		fqn := classFQN(d.Name, d.Namespace)
		if vmInst.Classes.Lookup(fqn) != nil {
			continue
		}
		// Two different digests for one FQN are conflicting definitions of
		// the same class. Picking one would depend on map iteration order
		// (and used to compile the class twice), so refuse instead.
		if prev, ok := byFQN[fqn]; ok && prev.Hash != d.Hash {
			a, b := prev.Hash, d.Hash
			if bytes.Compare(a[:], b[:]) > 0 {
				a, b = b, a
			}
			return 0, fmt.Errorf("rehydrate: conflicting class digests for %s (%x and %x)", fqn, a[:8], b[:8])
		}
		byFQN[fqn] = d
	}

	if len(byFQN) == 0 {
		return 0, nil
	}

	// Deterministic order: FQN-sorted input to the topological sort.
	toRehydrate := make([]*vm.ClassDigest, 0, len(byFQN))
	for _, d := range byFQN {
		toRehydrate = append(toRehydrate, d)
	}
	sort.Slice(toRehydrate, func(i, j int) bool {
		return classFQN(toRehydrate[i].Name, toRehydrate[i].Namespace) < classFQN(toRehydrate[j].Name, toRehydrate[j].Namespace)
	})

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
			for _, cand := range superclassCandidates(d) {
				if superclass = classMap[cand]; superclass != nil {
					break
				}
				if superclass = vmInst.Classes.Lookup(cand); superclass != nil {
					break
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
			vmInst.SetGlobal(fqn, classVal)
		} else {
			vmInst.SetGlobal(d.Name, classVal)
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

	// Phase 6: Invalidate all inline caches. Rehydration may have installed
	// new or updated methods on class VTables, leaving stale cache entries at
	// call sites that previously dispatched to old (or nil) methods.
	// Caches will repopulate on next dispatch — this is safe.
	vm.InvalidateAllCaches(vmInst.Classes)

	return compiled, nil
}

// classFQN returns "namespace::name" if namespace is non-empty, otherwise just "name".
func classFQN(name, namespace string) string {
	if namespace != "" {
		return namespace + "::" + name
	}
	return name
}

// superclassCandidates returns the class-table keys a digest's superclass
// reference may denote, in resolution order. Digests record the superclass
// FQN; legacy digests may carry a short name. Either way the reference is
// tried in the class's own namespace first and then as written (FQN or root
// class) — the same order as ClassTable.LookupWithImports.
//
// The class's own FQN is never a candidate: App::Stream with superclass
// "Stream" names the root Stream, not itself.
func superclassCandidates(d *vm.ClassDigest) []string {
	if d.Namespace == "" {
		return []string{d.SuperclassName}
	}
	own := classFQN(d.Name, d.Namespace)
	var out []string
	for _, cand := range []string{d.Namespace + "::" + d.SuperclassName, d.SuperclassName} {
		if cand != own {
			out = append(out, cand)
		}
	}
	return out
}

// topoSortClasses performs a topological sort of class digests by superclass
// dependency. Classes whose superclass is already in the VM come first.
// Classes that depend on other classes in the batch come after their
// dependencies. The output is deterministic for a given input order.
func topoSortClasses(digests []*vm.ClassDigest, vmInst *vm.VM) ([]*vm.ClassDigest, error) {
	byFQN := make(map[string]*vm.ClassDigest, len(digests))
	for _, d := range digests {
		byFQN[classFQN(d.Name, d.Namespace)] = d
	}

	// batchSuper returns the FQN of d's superclass if it resolves to a class
	// in this batch, or "" if it is Object/absent or resolves to a class
	// already in the VM. Mirrors the resolution order of phase 3.
	batchSuper := func(d *vm.ClassDigest) string {
		if d.SuperclassName == "" || d.SuperclassName == "Object" {
			return ""
		}
		for _, cand := range superclassCandidates(d) {
			if _, ok := byFQN[cand]; ok {
				return cand
			}
			if vmInst.Classes.Lookup(cand) != nil {
				return ""
			}
		}
		return ""
	}

	// Kahn's algorithm
	inDegree := make(map[string]int, len(digests))
	dependents := make(map[string][]string, len(digests))
	for _, d := range digests {
		fqn := classFQN(d.Name, d.Namespace)
		if super := batchSuper(d); super != "" {
			inDegree[fqn]++
			dependents[super] = append(dependents[super], fqn)
		}
	}

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
		sorted = append(sorted, byFQN[fqn])

		for _, dep := range dependents[fqn] {
			inDegree[dep]--
			if inDegree[dep] == 0 {
				queue = append(queue, dep)
			}
		}
	}

	if len(sorted) != len(digests) {
		return nil, fmt.Errorf("circular superclass dependency detected among %d classes", len(digests))
	}

	return sorted, nil
}
