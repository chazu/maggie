package main

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// Conventions CI (docs/CONVENTIONS.md, "selector honesty"):
//
//  1. HARD GATE — every <primitive> stub declared in lib/ must resolve to a
//     Go-registered method on that class. A stub without a primitive is
//     phantom API: it parses, loads, and silently returns nil at runtime
//     (that is exactly how `forkAt:` shipped broken).
//  2. RATCHET — VM-registered selectors on lib classes that lib does not
//     declare are invisible API (implemented but undocumented, like
//     Channel>>onSend:do: was). The baseline may only shrink.
//  3. RATCHET — the number of lib methods without a ```example doctest may
//     only shrink. New code ships with doctests.

// libClassDefs parses every lib/*.mag (not guides) and returns class defs.
func libClassDefs(t *testing.T) map[string]*compiler.ClassDef {
	t.Helper()
	pattern := filepath.Join("..", "..", "lib", "*.mag")
	files, err := filepath.Glob(pattern)
	if err != nil || len(files) == 0 {
		t.Fatalf("no lib files found at %s: %v", pattern, err)
	}
	defs := make(map[string]*compiler.ClassDef)
	for _, f := range files {
		data, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		sf, err := compiler.ParseSourceFileFromString(string(data))
		if err != nil {
			t.Fatalf("parse %s: %v", f, err)
		}
		for _, cd := range sf.Classes {
			if existing, ok := defs[cd.Name]; ok {
				// Class extended across files: merge method lists.
				existing.Methods = append(existing.Methods, cd.Methods...)
				existing.ClassMethods = append(existing.ClassMethods, cd.ClassMethods...)
				continue
			}
			defs[cd.Name] = cd
		}
	}
	return defs
}

func vtableSelectorNames(vmInst *vm.VM, vt *vm.VTable) map[string]bool {
	names := make(map[string]bool)
	for _, id := range vt.OwnSelectorIDs() {
		if name := vmInst.Selectors.Name(id); name != "" {
			names[name] = true
		}
	}
	return names
}

func declaredSelectors(methods []*compiler.MethodDef) map[string]*compiler.MethodDef {
	out := make(map[string]*compiler.MethodDef, len(methods))
	for _, m := range methods {
		out[m.Selector] = m
	}
	return out
}

// TestConventions_PrimitiveStubsAreRegistered is the hard gate: a
// <primitive> stub in lib/ whose class has no Go-registered method with
// that selector is phantom API.
func TestConventions_PrimitiveStubsAreRegistered(t *testing.T) {
	vmInst := vm.NewVM() // Go-registered primitives only, no lib loaded
	defs := libClassDefs(t)

	var phantoms []string
	for className, cd := range defs {
		cls := vmInst.Classes.Lookup(className)
		if cls == nil {
			continue // pure-Maggie class: no primitive side to check
		}
		instRegistered := vtableSelectorNames(vmInst, cls.VTable)
		classRegistered := vtableSelectorNames(vmInst, cls.ClassVTable)

		for _, m := range cd.Methods {
			if m.IsPrimitiveStub && !instRegistered[m.Selector] {
				phantoms = append(phantoms, fmt.Sprintf("%s>>%s", className, m.Selector))
			}
		}
		for _, m := range cd.ClassMethods {
			if m.IsPrimitiveStub && !classRegistered[m.Selector] {
				phantoms = append(phantoms, fmt.Sprintf("%s class>>%s", className, m.Selector))
			}
		}
	}

	if len(phantoms) > 0 {
		sort.Strings(phantoms)
		t.Fatalf("phantom API: %d <primitive> stubs have no Go-registered primitive "+
			"(they will silently return nil at runtime):\n  %s",
			len(phantoms), strings.Join(phantoms, "\n  "))
	}
}

// isPrimConventionSelector matches the `prim<Uppercase>` naming convention for
// Go primitives that back a lib wrapper (primSign, primIsZero, primLock). It
// deliberately excludes ordinary selectors that merely start with "prim"
// (e.g. primary:) by requiring an uppercase letter after the prefix.
func isPrimConventionSelector(sel string) bool {
	return strings.HasPrefix(sel, "prim") && len(sel) > 4 && sel[4] >= 'A' && sel[4] <= 'Z'
}

// TestConventions_PrimSelectorSendsAreRegistered closes the hard gate's blind
// spot: a wrapper whose BODY sends a prim* selector that is registered nowhere
// (e.g. `method: isZero [ ^self primIsZero ]` when Go registered `isZero`) is
// just as phantom as an unbacked <primitive> stub — it DNUs at runtime — but
// the stub-only gate never sees it. Scan every lib method body for prim*
// sends and require each to resolve to a Go-registered method or a lib
// declaration.
func TestConventions_PrimSelectorSendsAreRegistered(t *testing.T) {
	vmInst := vm.NewVM() // Go-registered primitives only
	defs := libClassDefs(t)

	registered := make(map[string]bool)
	for _, c := range vmInst.Classes.All() {
		for name := range vtableSelectorNames(vmInst, c.VTable) {
			registered[name] = true
		}
		for name := range vtableSelectorNames(vmInst, c.ClassVTable) {
			registered[name] = true
		}
	}
	libDeclared := make(map[string]bool)
	for _, cd := range defs {
		for _, m := range cd.Methods {
			libDeclared[m.Selector] = true
		}
		for _, m := range cd.ClassMethods {
			libDeclared[m.Selector] = true
		}
	}

	var phantoms []string
	seen := make(map[string]bool)
	note := func(className string, m *compiler.MethodDef, sel string) {
		if !isPrimConventionSelector(sel) || registered[sel] || libDeclared[sel] {
			return
		}
		key := fmt.Sprintf("%s>>%s calls %s", className, m.Selector, sel)
		if !seen[key] {
			seen[key] = true
			phantoms = append(phantoms, key)
		}
	}
	scan := func(className string, m *compiler.MethodDef) {
		for _, stmt := range m.Statements {
			compiler.Inspect(stmt, func(n compiler.Node) bool {
				switch msg := n.(type) {
				case *compiler.UnaryMessage:
					note(className, m, msg.Selector)
				case *compiler.BinaryMessage:
					note(className, m, msg.Selector)
				case *compiler.KeywordMessage:
					note(className, m, msg.Selector)
				case *compiler.Cascade:
					for _, cm := range msg.Messages {
						note(className, m, cm.Selector)
					}
				}
				return true
			})
		}
	}
	for className, cd := range defs {
		for _, m := range cd.Methods {
			scan(className, m)
		}
		for _, m := range cd.ClassMethods {
			scan(className, m)
		}
	}

	if len(phantoms) > 0 {
		sort.Strings(phantoms)
		t.Fatalf("phantom prim* sends: %d lib method bodies call a prim-prefixed selector "+
			"that is neither Go-registered nor lib-declared (silent DNU at runtime):\n  %s",
			len(phantoms), strings.Join(phantoms, "\n  "))
	}
}

// TestConventions_UndeclaredSelectorRatchet: VM-registered selectors on lib
// classes that the lib source never declares are invisible API. The
// baseline file lists the current debt; it may only shrink.
func TestConventions_UndeclaredSelectorRatchet(t *testing.T) {
	vmInst := vm.NewVM()
	defs := libClassDefs(t)

	var undeclared []string
	for className, cd := range defs {
		cls := vmInst.Classes.Lookup(className)
		if cls == nil {
			continue
		}
		instDeclared := declaredSelectors(cd.Methods)
		classDeclared := declaredSelectors(cd.ClassMethods)

		for name := range vtableSelectorNames(vmInst, cls.VTable) {
			if _, ok := instDeclared[name]; !ok {
				undeclared = append(undeclared, fmt.Sprintf("%s>>%s", className, name))
			}
		}
		for name := range vtableSelectorNames(vmInst, cls.ClassVTable) {
			if _, ok := classDeclared[name]; !ok {
				undeclared = append(undeclared, fmt.Sprintf("%s class>>%s", className, name))
			}
		}
	}
	sort.Strings(undeclared)

	baselinePath := filepath.Join("testdata", "undeclared_selectors_baseline.txt")
	baselineData, err := os.ReadFile(baselinePath)
	if os.IsNotExist(err) {
		if mkErr := os.MkdirAll("testdata", 0o755); mkErr != nil {
			t.Fatal(mkErr)
		}
		if wErr := os.WriteFile(baselinePath, []byte(strings.Join(undeclared, "\n")+"\n"), 0o644); wErr != nil {
			t.Fatal(wErr)
		}
		t.Logf("created baseline with %d undeclared selectors: %s", len(undeclared), baselinePath)
		return
	}
	if err != nil {
		t.Fatal(err)
	}

	baseline := make(map[string]bool)
	for _, line := range strings.Split(strings.TrimSpace(string(baselineData)), "\n") {
		if line != "" {
			baseline[line] = true
		}
	}

	var fresh []string
	for _, u := range undeclared {
		if !baseline[u] {
			fresh = append(fresh, u)
		}
	}
	if len(fresh) > 0 {
		t.Fatalf("NEW invisible API: %d VM-registered selectors are not declared in lib/ "+
			"(declare them in the class's .mag file — as a <primitive> stub or wrapper — "+
			"with a docstring):\n  %s", len(fresh), strings.Join(fresh, "\n  "))
	}
	if len(undeclared) < len(baseline) {
		t.Logf("undeclared-selector debt shrank: %d → %d — regenerate %s to lock it in",
			len(baseline), len(undeclared), baselinePath)
	}
}

// TestConventions_DoctestRatchet: the count of lib methods without a
// ```example doctest may only shrink.
func TestConventions_DoctestRatchet(t *testing.T) {
	defs := libClassDefs(t)

	missing := 0
	total := 0
	for _, cd := range defs {
		for _, m := range append(append([]*compiler.MethodDef{}, cd.Methods...), cd.ClassMethods...) {
			total++
			if !strings.Contains(m.DocString, "```example") {
				missing++
			}
		}
	}

	baselinePath := filepath.Join("testdata", "doctest_ratchet.txt")
	baselineData, err := os.ReadFile(baselinePath)
	if os.IsNotExist(err) {
		if mkErr := os.MkdirAll("testdata", 0o755); mkErr != nil {
			t.Fatal(mkErr)
		}
		if wErr := os.WriteFile(baselinePath, []byte(strconv.Itoa(missing)+"\n"), 0o644); wErr != nil {
			t.Fatal(wErr)
		}
		t.Logf("created doctest ratchet baseline: %d/%d methods lack a doctest", missing, total)
		return
	}
	if err != nil {
		t.Fatal(err)
	}

	baselineCount, err := strconv.Atoi(strings.TrimSpace(string(baselineData)))
	if err != nil {
		t.Fatalf("bad baseline %s: %v", baselinePath, err)
	}

	if missing > baselineCount {
		t.Fatalf("doctest coverage regressed: %d methods lack a ```example doctest (baseline %d). "+
			"New lib methods must ship with doctests (docs/CONVENTIONS.md).", missing, baselineCount)
	}
	if missing < baselineCount {
		if wErr := os.WriteFile(baselinePath, []byte(strconv.Itoa(missing)+"\n"), 0o644); wErr != nil {
			t.Fatal(wErr)
		}
		t.Logf("doctest debt shrank %d → %d; ratchet tightened", baselineCount, missing)
	}
}
