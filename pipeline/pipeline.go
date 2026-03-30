package pipeline

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/compiler/hash"
	"github.com/chazu/maggie/manifest"
	"github.com/chazu/maggie/vm"
)

// ParsedFile holds a parsed source file along with its resolved metadata.
type ParsedFile struct {
	SF        *compiler.SourceFile
	Namespace string   // effective namespace (override or declared)
	Imports   []string // import paths
	Path      string   // source file path
	BasePath  string   // for error messages
}

// Pipeline orchestrates compilation of Maggie source files into a VM.
type Pipeline struct {
	VM      *vm.VM
	Verbose io.Writer // nil = silent
	Exclude []string  // source exclude glob patterns from manifest
	DevMode bool      // when true, include dev-dependencies
}

// logf prints to Verbose if non-nil.
func (p *Pipeline) logf(format string, args ...interface{}) {
	if p.Verbose != nil {
		fmt.Fprintf(p.Verbose, format, args...)
	}
}

// CollectFiles resolves a path (file, directory, or .../...) into parsed files.
// It reads, parses, and derives namespaces but does NOT compile anything.
// Optional exclude patterns filter out files matching any glob pattern.
func CollectFiles(path string, exclude ...[]string) ([]ParsedFile, error) {
	var excludePatterns []string
	if len(exclude) > 0 {
		excludePatterns = exclude[0]
	}
	return collectFilesImpl(path, excludePatterns)
}

func collectFilesImpl(path string, exclude []string) ([]ParsedFile, error) {
	// Check for recursive pattern
	recursive := false
	if strings.HasSuffix(path, "/...") {
		recursive = true
		path = strings.TrimSuffix(path, "/...")
	}

	// Resolve path
	path, err := filepath.Abs(path)
	if err != nil {
		return nil, fmt.Errorf("invalid path %q: %w", path, err)
	}

	info, err := os.Stat(path)
	if err != nil {
		return nil, fmt.Errorf("cannot access %q: %w", path, err)
	}

	// basePath is the root from which namespaces are derived
	basePath := path

	var filePaths []string
	if info.IsDir() {
		if recursive {
			err = filepath.Walk(path, func(p string, info os.FileInfo, err error) error {
				if err != nil {
					return err
				}
				if !info.IsDir() && strings.HasSuffix(p, ".mag") {
					filePaths = append(filePaths, p)
				}
				return nil
			})
			if err != nil {
				return nil, fmt.Errorf("walking %q: %w", path, err)
			}
		} else {
			entries, err := os.ReadDir(path)
			if err != nil {
				return nil, fmt.Errorf("reading %q: %w", path, err)
			}
			for _, e := range entries {
				if !e.IsDir() && strings.HasSuffix(e.Name(), ".mag") {
					filePaths = append(filePaths, filepath.Join(path, e.Name()))
				}
			}
		}
	} else {
		basePath = ""
		if strings.HasSuffix(path, ".mag") {
			filePaths = append(filePaths, path)
		} else {
			return nil, fmt.Errorf("%q is not a .mag file", path)
		}
	}

	// Apply exclude patterns
	if len(exclude) > 0 {
		filePaths = filterExcluded(filePaths, basePath, exclude)
	}

	var result []ParsedFile
	for _, fp := range filePaths {
		content, err := os.ReadFile(fp)
		if err != nil {
			return nil, fmt.Errorf("reading %q: %w", fp, err)
		}

		sf, err := compiler.ParseSourceFileFromString(string(content))
		if err != nil {
			return nil, fmt.Errorf("parse error in %s: %v", fp, err)
		}

		// Derive namespace: file declaration > directory derivation
		namespace := ""
		if sf.Namespace != nil {
			namespace = sf.Namespace.Name
		} else if basePath != "" {
			namespace = DeriveNamespace(fp, basePath)
		}

		// Collect import paths
		var imports []string
		for _, imp := range sf.Imports {
			imports = append(imports, imp.Path)
		}

		result = append(result, ParsedFile{
			SF:        sf,
			Namespace: namespace,
			Imports:   imports,
			Path:      fp,
			BasePath:  basePath,
		})
	}

	return result, nil
}

// filterExcluded removes file paths matching any of the glob patterns.
// Patterns are matched against relative paths from basePath and against base names.
func filterExcluded(paths []string, basePath string, patterns []string) []string {
	if len(patterns) == 0 {
		return paths
	}
	var kept []string
	for _, p := range paths {
		rel := filepath.Base(p)
		if basePath != "" {
			if r, err := filepath.Rel(basePath, p); err == nil {
				rel = r
			}
		}
		excluded := false
		for _, pat := range patterns {
			if matched, _ := filepath.Match(pat, rel); matched {
				excluded = true
				break
			}
			if matched, _ := filepath.Match(pat, filepath.Base(p)); matched {
				excluded = true
				break
			}
		}
		if !excluded {
			kept = append(kept, p)
		}
	}
	return kept
}

// CompileAll implements two-pass compilation over a set of parsed files.
//
// Pass 1a: Register class and trait skeletons (names only, ObjectClass as temporary superclass).
// Pass 1b: Resolve actual superclass pointers using import resolution.
// Pass 2:  Compile trait methods, class methods (instance + class), and apply trait inclusions.
func (p *Pipeline) CompileAll(files []ParsedFile) (int, error) {
	vmInst := p.VM

	// Track classes created in this batch so we can fix up superclasses in pass 1b.
	type classEntry struct {
		class    *vm.Class
		classDef *compiler.ClassDef
		pf       *ParsedFile
	}
	var classEntries []classEntry

	// ---------------------------------------------------------------
	// Pass 1a — Register class and trait skeletons
	// ---------------------------------------------------------------
	for i := range files {
		pf := &files[i]

		// Register trait skeletons
		for _, traitDef := range pf.SF.Traits {
			trait := vm.NewTrait(traitDef.Name)
			if pf.Namespace != "" {
				trait.Namespace = pf.Namespace
			}
			if traitDef.DocString != "" {
				trait.DocString = traitDef.DocString
			}
			vmInst.Traits.Register(trait)

			p.logf("  Registered trait %s\n", qualifiedName(pf.Namespace, traitDef.Name))
		}

		// Register class skeletons
		for _, classDef := range pf.SF.Classes {
			// Check if the class already exists (e.g., extending a core class from the image)
			var class *vm.Class
			if pf.Namespace != "" {
				class = vmInst.Classes.LookupInNamespace(pf.Namespace, classDef.Name)
			}
			if class == nil {
				class = vmInst.Classes.Lookup(classDef.Name)
			}

			if class == nil {
				// Create with ObjectClass as temporary superclass — resolved in pass 1b
				class = vm.NewClassWithInstVars(classDef.Name, vmInst.ObjectClass, classDef.InstanceVariables)

				if pf.Namespace != "" {
					class.Namespace = pf.Namespace
				}
				if classDef.DocString != "" {
					class.DocString = classDef.DocString
				}

				vmInst.Classes.Register(class)

				// Register as first-class class value in Globals.
				// Namespaced classes are registered ONLY under the FQN.
				classVal := vmInst.ClassValue(class)
				if pf.Namespace != "" {
					fullName := pf.Namespace + "::" + classDef.Name
					vmInst.Globals[fullName] = classVal
				} else {
					vmInst.Globals[classDef.Name] = classVal
				}

				p.logf("  Created class %s (skeleton)\n", qualifiedName(pf.Namespace, classDef.Name))

				classEntries = append(classEntries, classEntry{class: class, classDef: classDef, pf: pf})
			} else {
				// Class already exists (extending core class) — still track for method compilation
				if classDef.DocString != "" && class.DocString == "" {
					class.DocString = classDef.DocString
				}
				classEntries = append(classEntries, classEntry{class: class, classDef: classDef, pf: pf})
			}
		}
	}

	// ---------------------------------------------------------------
	// Pass 1b — Resolve superclass pointers
	// ---------------------------------------------------------------
	for _, ce := range classEntries {
		if ce.classDef.Superclass == "" || ce.classDef.Superclass == "Object" {
			continue
		}

		if ce.class.Superclass != nil && ce.class.Superclass != vmInst.ObjectClass {
			continue
		}

		resolved := vmInst.Classes.LookupWithImports(ce.classDef.Superclass, ce.pf.Namespace, ce.pf.Imports)
		if resolved == nil {
			return 0, fmt.Errorf("class %s: superclass %s not found\n  declared in: %s\n  namespace: %s\n  imports searched: %v",
				ce.classDef.Name, ce.classDef.Superclass, ce.pf.Path, ce.pf.Namespace, ce.pf.Imports)
		}

		ce.class.Superclass = resolved
		ce.class.VTable.SetParent(resolved.VTable)
		ce.class.ClassVTable.SetParent(resolved.ClassVTable)
		ce.class.NumSlots = len(ce.class.AllInstVarNames())

		p.logf("  Resolved %s superclass -> %s\n", ce.classDef.Name, resolved.FullName())
	}

	// ---------------------------------------------------------------
	// Pass 2 — Compile trait methods and class methods
	// ---------------------------------------------------------------
	compiled := 0

	for i := range files {
		pf := &files[i]

		// Compile trait methods
		for _, traitDef := range pf.SF.Traits {
			trait := vmInst.Traits.Lookup(traitDef.Name)
			if trait == nil {
				return compiled, fmt.Errorf("internal error: trait %s not found after registration", traitDef.Name)
			}

			for _, methodDef := range traitDef.Methods {
				method, err := compiler.CompileMethodDefWithContext(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), nil, pf.Namespace, pf.Imports, vmInst.Classes)
				if err != nil {
					return compiled, fmt.Errorf("error compiling trait %s>>%s in %s: %v", traitDef.Name, methodDef.Selector, pf.Path, err)
				}

				if methodDef.SourceText != "" {
					method.Source = methodDef.SourceText
				}
				if methodDef.DocString != "" {
					method.SetDocString(methodDef.DocString)
				}

				hashAndSetMethod(methodDef, method, nil, pf, vmInst)

				selectorID := vmInst.Selectors.Intern(method.Name())
				trait.AddMethod(selectorID, method)
				compiled++
			}

			// Add required method selectors
			for _, reqSelector := range traitDef.Requires {
				selectorID := vmInst.Selectors.Intern(reqSelector)
				trait.AddRequires(selectorID)
			}

			p.logf("  trait %s: %d methods\n", traitDef.Name, len(traitDef.Methods))
		}
	}

	// Compile class methods (separate loop so all traits are fully compiled first)
	for _, ce := range classEntries {
		class := ce.class
		classDef := ce.classDef
		pf := ce.pf

		// Compile instance methods
		allIvars := class.AllInstVarNames()
		for _, methodDef := range classDef.Methods {
			if methodDef.IsPrimitiveStub {
				if methodDef.DocString != "" {
					selectorID := vmInst.Selectors.Lookup(methodDef.Selector)
					if selectorID >= 0 {
						existing := class.VTable.Lookup(selectorID)
						if existing != nil {
							if ds, ok := existing.(vm.DocStringable); ok {
								ds.SetDocString(methodDef.DocString)
							}
						}
					}
				}
				continue
			}

			method, err := compiler.CompileMethodDefWithContext(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), allIvars, pf.Namespace, pf.Imports, vmInst.Classes)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			if methodDef.SourceText != "" {
				method.Source = methodDef.SourceText
			}
			if methodDef.DocString != "" {
				method.SetDocString(methodDef.DocString)
			}

			instVarMap := make(map[string]int, len(allIvars))
			for idx, name := range allIvars {
				instVarMap[name] = idx
			}
			hashAndSetMethod(methodDef, method, instVarMap, pf, vmInst)

			method.SetClass(class)
			selectorID := vmInst.Selectors.Intern(method.Name())
			class.VTable.AddMethod(selectorID, method)
			compiled++
		}

		// Compile class methods
		for _, methodDef := range classDef.ClassMethods {
			if methodDef.IsPrimitiveStub {
				if methodDef.DocString != "" {
					selectorID := vmInst.Selectors.Lookup(methodDef.Selector)
					if selectorID >= 0 {
						existing := class.ClassVTable.Lookup(selectorID)
						if existing != nil {
							if ds, ok := existing.(vm.DocStringable); ok {
								ds.SetDocString(methodDef.DocString)
							}
						}
					}
				}
				continue
			}

			method, err := compiler.CompileMethodDefWithContext(methodDef, vmInst.Selectors, vmInst.Symbols, vmInst.Registry(), nil, pf.Namespace, pf.Imports, vmInst.Classes)
			if err != nil {
				return compiled, fmt.Errorf("error compiling %s class>>%s: %v", classDef.Name, methodDef.Selector, err)
			}

			if methodDef.SourceText != "" {
				method.Source = methodDef.SourceText
			}
			if methodDef.DocString != "" {
				method.SetDocString(methodDef.DocString)
			}

			hashAndSetMethod(methodDef, method, nil, pf, vmInst)

			method.SetClass(class)
			method.IsClassMethod = true
			selectorID := vmInst.Selectors.Intern(method.Name())
			class.ClassVTable.AddMethod(selectorID, method)
			compiled++
		}

		// Apply trait inclusions
		for _, traitName := range classDef.Traits {
			errMsg := class.IncludeTraitByName(traitName, vmInst.Traits, vmInst.Selectors)
			if errMsg != "" {
				return compiled, fmt.Errorf("error including trait %s in %s (%s): %s", traitName, classDef.Name, pf.Path, errMsg)
			}
			if p.Verbose != nil {
				trait := vmInst.Traits.Lookup(traitName)
				if trait != nil {
					p.logf("    included trait %s (%d methods)\n", traitName, trait.MethodCount())
				}
			}
		}

		if len(classDef.Methods)+len(classDef.ClassMethods) > 0 {
			p.logf("  %s: %d methods\n", classDef.Name, len(classDef.Methods)+len(classDef.ClassMethods))
		}
	}

	// ---------------------------------------------------------------
	// Populate ContentStore — index all compiled methods & class digests
	// ---------------------------------------------------------------
	store := vmInst.ContentStore()
	for _, ce := range classEntries {
		if ce.class.VTable != nil {
			for _, m := range ce.class.VTable.LocalMethods() {
				if cm, ok := m.(*vm.CompiledMethod); ok {
					store.IndexMethod(cm)
				}
			}
		}
		if ce.class.ClassVTable != nil {
			for _, m := range ce.class.ClassVTable.LocalMethods() {
				if cm, ok := m.(*vm.CompiledMethod); ok {
					store.IndexMethod(cm)
				}
			}
		}
		digest := vm.DigestClass(ce.class)
		store.IndexClass(digest)
	}

	return compiled, nil
}

// CompilePath compiles .mag files from a path into the VM.
// Supports ./... syntax for recursive loading.
func (p *Pipeline) CompilePath(path string) (int, error) {
	files, err := CollectFiles(path, p.Exclude)
	if err != nil {
		return 0, err
	}
	return p.CompileAll(files)
}

// CompileFile compiles a single .mag file into the VM.
func (p *Pipeline) CompileFile(path string) (int, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return 0, err
	}

	return p.CompileSourceFile(string(content), path, "")
}

// CompileSourceFile compiles source text into the VM.
// nsOverride is an optional namespace override; if empty, the file's own
// namespace: declaration is used.
func (p *Pipeline) CompileSourceFile(source, sourcePath, nsOverride string) (int, error) {
	sf, err := compiler.ParseSourceFileFromString(source)
	if err != nil {
		return 0, fmt.Errorf("parse error in %s: %v", sourcePath, err)
	}

	namespace := nsOverride
	if namespace == "" && sf.Namespace != nil {
		namespace = sf.Namespace.Name
	}

	var imports []string
	for _, imp := range sf.Imports {
		imports = append(imports, imp.Path)
	}

	pf := ParsedFile{
		SF:        sf,
		Namespace: namespace,
		Imports:   imports,
		Path:      sourcePath,
	}

	return p.CompileAll([]ParsedFile{pf})
}

// LoadProject resolves dependencies and loads source directories from a project manifest.
// Collects all files from all deps + project sources, then compiles in one two-pass batch.
func (p *Pipeline) LoadProject(m *manifest.Manifest) (int, error) {
	var allFiles []ParsedFile

	// Determine which dependencies to resolve
	var depsToResolve map[string]manifest.Dependency
	if p.DevMode {
		var err error
		depsToResolve, err = m.AllDependencies()
		if err != nil {
			return 0, err
		}
	} else {
		depsToResolve = m.Dependencies
	}

	// Collect files from dependencies
	if len(depsToResolve) > 0 {
		resolver := manifest.NewResolver(m, p.Verbose != nil, depsToResolve)
		deps, err := resolver.Resolve()
		if err != nil {
			return 0, fmt.Errorf("dependency resolution failed: %w", err)
		}

		if err := CheckNamespaceCollisions(deps); err != nil {
			return 0, err
		}

		for _, dep := range deps {
			var depFiles []ParsedFile
			if dep.Manifest != nil {
				for _, srcDir := range dep.Manifest.SourceDirPaths() {
					if _, err := os.Stat(srcDir); err != nil {
						continue
					}
					files, err := CollectFiles(srcDir + "/...")
					if err != nil {
						return 0, fmt.Errorf("collecting dependency %s: %w", dep.Name, err)
					}
					depFiles = append(depFiles, files...)
				}
			} else {
				depPath := dep.LocalPath + "/..."
				files, err := CollectFiles(depPath)
				if err != nil {
					return 0, fmt.Errorf("collecting dependency %s: %w", dep.Name, err)
				}
				depFiles = append(depFiles, files...)
			}

			PrefixDepNamespaces(depFiles, dep, p.Verbose)

			allFiles = append(allFiles, depFiles...)
		}
	}

	// Collect files from project source directories
	for _, srcDir := range m.SourceDirPaths() {
		if _, err := os.Stat(srcDir); err != nil {
			p.logf("  Skipping missing source dir: %s\n", srcDir)
			continue
		}
		files, err := CollectFiles(srcDir+"/...", m.Source.Exclude)
		if err != nil {
			return 0, fmt.Errorf("collecting source %s: %w", srcDir, err)
		}
		allFiles = append(allFiles, files...)
	}

	// Auto-import: within a project, all namespaces are implicitly available
	allNamespaces := make(map[string]bool)
	for _, pf := range allFiles {
		if pf.Namespace != "" {
			allNamespaces[pf.Namespace] = true
		}
	}
	if len(allNamespaces) > 0 {
		nsList := make([]string, 0, len(allNamespaces))
		for ns := range allNamespaces {
			nsList = append(nsList, ns)
		}
		for i := range allFiles {
			existing := make(map[string]bool, len(allFiles[i].Imports))
			for _, imp := range allFiles[i].Imports {
				existing[imp] = true
			}
			for _, ns := range nsList {
				if ns != allFiles[i].Namespace && !existing[ns] {
					allFiles[i].Imports = append(allFiles[i].Imports, ns)
				}
			}
		}
	}

	return p.CompileAll(allFiles)
}

// LoadTarget resolves dependencies and loads sources for a specific build target.
func (p *Pipeline) LoadTarget(m *manifest.Manifest, target *manifest.ResolvedTarget) (int, error) {
	var allFiles []ParsedFile

	// Determine which dependencies to resolve
	var depsToResolve map[string]manifest.Dependency
	if p.DevMode {
		var err error
		depsToResolve, err = m.AllDependencies()
		if err != nil {
			return 0, err
		}
	} else {
		depsToResolve = m.Dependencies
	}

	// Collect files from dependencies (same as LoadProject)
	if len(depsToResolve) > 0 {
		resolver := manifest.NewResolver(m, p.Verbose != nil, depsToResolve)
		deps, err := resolver.Resolve()
		if err != nil {
			return 0, fmt.Errorf("dependency resolution failed: %w", err)
		}

		if err := CheckNamespaceCollisions(deps); err != nil {
			return 0, err
		}

		for _, dep := range deps {
			var depFiles []ParsedFile
			if dep.Manifest != nil {
				for _, srcDir := range dep.Manifest.SourceDirPaths() {
					if _, err := os.Stat(srcDir); err != nil {
						continue
					}
					files, err := CollectFiles(srcDir + "/...")
					if err != nil {
						return 0, fmt.Errorf("collecting dependency %s: %w", dep.Name, err)
					}
					depFiles = append(depFiles, files...)
				}
			} else {
				depPath := dep.LocalPath + "/..."
				files, err := CollectFiles(depPath)
				if err != nil {
					return 0, fmt.Errorf("collecting dependency %s: %w", dep.Name, err)
				}
				depFiles = append(depFiles, files...)
			}

			PrefixDepNamespaces(depFiles, dep, p.Verbose)
			allFiles = append(allFiles, depFiles...)
		}
	}

	// Collect files from target source directories
	for _, dir := range target.Dirs {
		srcDir := filepath.Join(m.Dir, dir)
		if _, err := os.Stat(srcDir); err != nil {
			p.logf("  Skipping missing source dir: %s\n", srcDir)
			continue
		}
		files, err := CollectFiles(srcDir+"/...", target.Exclude)
		if err != nil {
			return 0, fmt.Errorf("collecting source %s: %w", srcDir, err)
		}
		allFiles = append(allFiles, files...)
	}

	// Auto-import: within a project, all namespaces are implicitly available
	allNamespaces := make(map[string]bool)
	for _, pf := range allFiles {
		if pf.Namespace != "" {
			allNamespaces[pf.Namespace] = true
		}
	}
	if len(allNamespaces) > 0 {
		nsList := make([]string, 0, len(allNamespaces))
		for ns := range allNamespaces {
			nsList = append(nsList, ns)
		}
		for i := range allFiles {
			existing := make(map[string]bool, len(allFiles[i].Imports))
			for _, imp := range allFiles[i].Imports {
				existing[imp] = true
			}
			for _, ns := range nsList {
				if ns != allFiles[i].Namespace && !existing[ns] {
					allFiles[i].Imports = append(allFiles[i].Imports, ns)
				}
			}
		}
	}

	return p.CompileAll(allFiles)
}

// resolveGlobalForHash resolves a bare name to its FQN for content hashing.
func resolveGlobalForHash(name, namespace string, imports []string, classes *vm.ClassTable) string {
	if classes == nil {
		return name
	}
	cls := classes.LookupWithImports(name, namespace, imports)
	if cls != nil && cls.Namespace != "" {
		return cls.Namespace + "::" + cls.Name
	}
	return name
}

// hashAndSetMethod computes and sets the content hash and typed hash on a compiled method.
func hashAndSetMethod(methodDef *compiler.MethodDef, method *vm.CompiledMethod, instVars map[string]int, pf *ParsedFile, vmInst *vm.VM) {
	resolveGlobal := func(name string) string {
		return resolveGlobalForHash(name, pf.Namespace, pf.Imports, vmInst.Classes)
	}
	h := hash.HashMethod(methodDef, instVars, resolveGlobal)
	method.SetContentHash(h)

	th := hash.HashTypedMethod(methodDef, instVars, resolveGlobal)
	method.SetTypedHash(th)
}

// qualifiedName returns "ns::name" if ns is non-empty, otherwise just "name".
func qualifiedName(ns, name string) string {
	if ns != "" {
		return ns + "::" + name
	}
	return name
}
