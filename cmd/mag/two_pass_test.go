package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Forward reference: File A extends class from File B, A loaded before B
// ---------------------------------------------------------------------------

func TestTwoPass_ForwardReference(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// BaseWidget defined in B.mag, Button in A.mag extends BaseWidget.
	// Alphabetical ordering means A.mag is loaded first.
	writeMagFile(t, tmpDir, "A_Button.mag", `Button subclass: BaseWidget
  instanceVars: label

  method: label [
    ^label
  ]
`)

	writeMagFile(t, tmpDir, "B_BaseWidget.mag", `BaseWidget subclass: Object
  instanceVars: x y

  method: x [ ^x ]
  method: y [ ^y ]
`)

	methods, err := compilePath(tmpDir, vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

	// Verify Button's superclass is BaseWidget, not Object
	button := vmInst.LookupClass("Button")
	if button == nil {
		t.Fatal("Button class not found")
	}
	if button.Superclass == nil || button.Superclass.Name != "BaseWidget" {
		superName := "<nil>"
		if button.Superclass != nil {
			superName = button.Superclass.Name
		}
		t.Errorf("Button.Superclass = %s, want BaseWidget", superName)
	}

	// Verify Button inherits BaseWidget's instance variables
	allIvars := button.AllInstVarNames()
	if len(allIvars) != 3 {
		t.Errorf("Button allIvars = %v, want [x y label]", allIvars)
	}
}

// ---------------------------------------------------------------------------
// Circular imports: File A imports B, File B imports A
// ---------------------------------------------------------------------------

func TestTwoPass_CircularImports(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Create subdirectories for namespaces
	nsA := filepath.Join(tmpDir, "alpha")
	nsB := filepath.Join(tmpDir, "beta")
	os.Mkdir(nsA, 0755)
	os.Mkdir(nsB, 0755)

	writeMagFile(t, nsA, "Foo.mag", `namespace: 'Alpha'
import: 'Beta'

Foo subclass: Object
  method: greet [ ^'hello from Foo' ]
`)

	writeMagFile(t, nsB, "Bar.mag", `namespace: 'Beta'
import: 'Alpha'

Bar subclass: Object
  method: greet [ ^'hello from Bar' ]
`)

	methods, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}
	if methods < 2 {
		t.Errorf("expected at least 2 methods, got %d", methods)
	}

	// Both classes should exist with proper namespaces
	foo := vmInst.Classes.LookupInNamespace("Alpha", "Foo")
	if foo == nil {
		t.Error("Alpha::Foo not found")
	}
	bar := vmInst.Classes.LookupInNamespace("Beta", "Bar")
	if bar == nil {
		t.Error("Beta::Bar not found")
	}
}

// ---------------------------------------------------------------------------
// Unresolved superclass produces clear error
// ---------------------------------------------------------------------------

func TestTwoPass_UnresolvedSuperclassError(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Orphan.mag", `Orphan subclass: NonExistentParent
  method: test [ ^1 ]
`)

	_, err := compilePath(tmpDir, vmInst, false)
	if err == nil {
		t.Fatal("expected error for unresolved superclass, got nil")
	}

	errMsg := err.Error()
	if !strings.Contains(errMsg, "NonExistentParent") {
		t.Errorf("error should mention missing superclass name, got: %s", errMsg)
	}
	if !strings.Contains(errMsg, "not found") {
		t.Errorf("error should say 'not found', got: %s", errMsg)
	}
}

// ---------------------------------------------------------------------------
// Trait forward reference: trait in File B, used by class in File A
// ---------------------------------------------------------------------------

func TestTwoPass_TraitForwardReference(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// A_User.mag includes Greetable trait, defined in B_Greetable.mag
	writeMagFile(t, tmpDir, "A_User.mag", `User subclass: Object
  include: Greetable

  method: name [ ^'Alice' ]
`)

	writeMagFile(t, tmpDir, "B_Greetable.mag", `Greetable trait
  method: greet [ ^'Hello!' ]
`)

	methods, err := compilePath(tmpDir, vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

	// Verify User has the greet method from the trait
	user := vmInst.LookupClass("User")
	if user == nil {
		t.Fatal("User class not found")
	}

	greetID := vmInst.Selectors.Lookup("greet")
	if greetID < 0 {
		t.Fatal("greet selector not found")
	}
	if user.VTable.Lookup(greetID) == nil {
		t.Error("User should have greet method from Greetable trait")
	}
}

// ---------------------------------------------------------------------------
// Namespace isolation: state doesn't leak between files
// ---------------------------------------------------------------------------

func TestTwoPass_NamespaceIsolation(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsA := filepath.Join(tmpDir, "alpha")
	nsB := filepath.Join(tmpDir, "beta")
	os.Mkdir(nsA, 0755)
	os.Mkdir(nsB, 0755)

	// Both namespaces define a class named "Widget"
	writeMagFile(t, nsA, "Widget.mag", `Widget subclass: Object
  method: source [ ^'alpha' ]
`)

	writeMagFile(t, nsB, "Widget.mag", `Widget subclass: Object
  method: source [ ^'beta' ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Both namespaced Widgets should exist
	alphaWidget := vmInst.Classes.LookupInNamespace("Alpha", "Widget")
	if alphaWidget == nil {
		t.Error("Alpha::Widget not found")
	}
	betaWidget := vmInst.Classes.LookupInNamespace("Beta", "Widget")
	if betaWidget == nil {
		t.Error("Beta::Widget not found")
	}

	// They should be different classes
	if alphaWidget == betaWidget {
		t.Error("Alpha::Widget and Beta::Widget should be different class objects")
	}
}

// ---------------------------------------------------------------------------
// Cross-file superclass with namespaces and imports
// ---------------------------------------------------------------------------

func TestTwoPass_CrossNamespaceSuperclass(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsBase := filepath.Join(tmpDir, "base")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsBase, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsBase, "Component.mag", `Component subclass: Object
  instanceVars: id

  method: id [ ^id ]
`)

	writeMagFile(t, nsApp, "Button.mag", `namespace: 'App'
import: 'Base'

Button subclass: Component
  instanceVars: label

  method: label [ ^label ]
`)

	methods, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

	button := vmInst.Classes.LookupInNamespace("App", "Button")
	if button == nil {
		t.Fatal("App::Button not found")
	}

	// Button should inherit Component's ivars
	allIvars := button.AllInstVarNames()
	if len(allIvars) != 2 {
		t.Errorf("Button allIvars = %v, want [id label]", allIvars)
	}
}

// ---------------------------------------------------------------------------
// collectFiles returns parsed files without compilation
// ---------------------------------------------------------------------------

func TestCollectFiles_ReturnsWithoutCompiling(t *testing.T) {
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Foo.mag", `Foo subclass: Object
  method: test [ ^1 ]
`)
	writeMagFile(t, tmpDir, "Bar.mag", `Bar subclass: Object
  method: test [ ^2 ]
`)

	files, err := collectFiles(tmpDir)
	if err != nil {
		t.Fatalf("collectFiles failed: %v", err)
	}

	if len(files) != 2 {
		t.Fatalf("expected 2 files, got %d", len(files))
	}

	// Verify parsed data is present
	for _, pf := range files {
		if pf.sf == nil {
			t.Error("parsed file has nil SourceFile")
		}
		if pf.path == "" {
			t.Error("parsed file has empty path")
		}
	}
}

// ---------------------------------------------------------------------------
// Existing compilePath API still works for single files
// ---------------------------------------------------------------------------

func TestTwoPass_SingleFileCompilePath(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	p := writeMagFile(t, tmpDir, "Solo.mag", `Solo subclass: Object
  method: value [ ^42 ]
`)

	methods, err := compilePath(p, vmInst, false)
	if err != nil {
		t.Fatalf("compilePath single file failed: %v", err)
	}
	if methods != 1 {
		t.Errorf("expected 1 method, got %d", methods)
	}

	solo := vmInst.LookupClass("Solo")
	if solo == nil {
		t.Fatal("Solo class not found")
	}
}

// ---------------------------------------------------------------------------
// FQN resolution: cross-namespace class reference in method body
// ---------------------------------------------------------------------------

func TestFQN_CrossNamespaceClassReference(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsWidgets := filepath.Join(tmpDir, "widgets")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsWidgets, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsWidgets, "Button.mag", `Button subclass: Object
  method: label [ ^'OK' ]
`)

	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'
import: 'Widgets'

Factory subclass: Object
  classMethod: makeButton [
    ^Button new
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Execute App::Factory makeButton — should return a Widgets::Button instance
	factoryClass := vmInst.Classes.LookupInNamespace("App", "Factory")
	if factoryClass == nil {
		t.Fatal("App::Factory not found")
	}

	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Factory"), "makeButton", nil)
	// Send label to the result
	labelResult := vmInst.Send(result, "label", nil)
	if !vm.IsStringValue(labelResult) {
		t.Fatalf("expected string result from label, got %v", labelResult)
	}
	got := vmInst.Registry().GetStringContent(labelResult)
	if got != "OK" {
		t.Errorf("label = %q, want %q", got, "OK")
	}
}

// ---------------------------------------------------------------------------
// FQN resolution: same-namespace class reference (no import needed)
// ---------------------------------------------------------------------------

func TestFQN_SameNamespaceClassReference(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsModels := filepath.Join(tmpDir, "models")
	os.Mkdir(nsModels, 0755)

	writeMagFile(t, nsModels, "Base.mag", `Base subclass: Object
  method: kind [ ^'base' ]
`)

	writeMagFile(t, nsModels, "Derived.mag", `Derived subclass: Object
  classMethod: create [
    ^Base new
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Derived create should return a Models::Base instance
	result := vmInst.Send(vmInst.Symbols.SymbolValue("Models::Derived"), "create", nil)
	kindResult := vmInst.Send(result, "kind", nil)
	if !vm.IsStringValue(kindResult) {
		t.Fatalf("expected string result from kind, got %v", kindResult)
	}
	got := vmInst.Registry().GetStringContent(kindResult)
	if got != "base" {
		t.Errorf("kind = %q, want %q", got, "base")
	}
}

// ---------------------------------------------------------------------------
// FQN resolution: bare name still works for root namespace classes
// ---------------------------------------------------------------------------

func TestFQN_BareNameRootNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsApp, 0755)

	// Root-level class (no namespace)
	writeMagFile(t, tmpDir, "Helper.mag", `Helper subclass: Object
  method: help [ ^'helping' ]
`)

	// Namespaced class references the root-level Helper
	writeMagFile(t, nsApp, "Worker.mag", `namespace: 'App'

Worker subclass: Object
  classMethod: getHelper [
    ^Helper new
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Worker"), "getHelper", nil)
	helpResult := vmInst.Send(result, "help", nil)
	if !vm.IsStringValue(helpResult) {
		t.Fatalf("expected string result from help, got %v", helpResult)
	}
	got := vmInst.Registry().GetStringContent(helpResult)
	if got != "helping" {
		t.Errorf("help = %q, want %q", got, "helping")
	}
}

// ---------------------------------------------------------------------------
// FQN resolution: assignment to namespaced global
// ---------------------------------------------------------------------------

func TestFQN_AssignmentResolvesNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsWidgets := filepath.Join(tmpDir, "widgets")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsWidgets, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsWidgets, "Config.mag", `Config subclass: Object
  method: name [ ^'default' ]
`)

	writeMagFile(t, nsApp, "Setup.mag", `namespace: 'App'
import: 'Widgets'

Setup subclass: Object
  classMethod: createConfig [
    | cfg |
    cfg := Config new.
    ^cfg
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Setup"), "createConfig", nil)
	nameResult := vmInst.Send(result, "name", nil)
	if !vm.IsStringValue(nameResult) {
		t.Fatalf("expected string from name, got %v", nameResult)
	}
	got := vmInst.Registry().GetStringContent(nameResult)
	if got != "default" {
		t.Errorf("name = %q, want %q", got, "default")
	}
}

// ---------------------------------------------------------------------------
// Trait with namespace gets Namespace field set
// ---------------------------------------------------------------------------

func TestTwoPass_TraitNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsDir := filepath.Join(tmpDir, "mylib")
	os.Mkdir(nsDir, 0755)

	writeMagFile(t, nsDir, "Printable.mag", `Printable trait
  method: printString [ ^'<printable>' ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	trait := vmInst.Traits.Lookup("Printable")
	if trait == nil {
		t.Fatal("Printable trait not found")
	}
	if trait.Namespace != "Mylib" {
		t.Errorf("trait.Namespace = %q, want %q", trait.Namespace, "Mylib")
	}
}

// ---------------------------------------------------------------------------
// No short-name Globals: namespaced classes register ONLY under FQN
// ---------------------------------------------------------------------------

func TestNoShortNameGlobals_NamespacedClassOnlyFQN(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsA := filepath.Join(tmpDir, "alpha")
	nsB := filepath.Join(tmpDir, "beta")
	os.Mkdir(nsA, 0755)
	os.Mkdir(nsB, 0755)

	// Both namespaces define a class named "Widget"
	writeMagFile(t, nsA, "Widget.mag", `Widget subclass: Object
  method: source [ ^'alpha' ]
`)

	writeMagFile(t, nsB, "Widget.mag", `Widget subclass: Object
  method: source [ ^'beta' ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Both FQN entries should exist in Globals
	alphaFQN := "Alpha::Widget"
	betaFQN := "Beta::Widget"

	alphaVal, alphaOK := vmInst.Globals[alphaFQN]
	if !alphaOK {
		t.Errorf("Globals[%q] not found", alphaFQN)
	}
	betaVal, betaOK := vmInst.Globals[betaFQN]
	if !betaOK {
		t.Errorf("Globals[%q] not found", betaFQN)
	}

	// The two class values must be distinct
	if alphaOK && betaOK && alphaVal == betaVal {
		t.Error("Alpha::Widget and Beta::Widget should be different class values in Globals")
	}

	// Bare name "Widget" must NOT be in Globals (no short-name registration)
	if _, bareOK := vmInst.Globals["Widget"]; bareOK {
		t.Error("Globals[\"Widget\"] should not exist — namespaced classes must only register under FQN")
	}
}

// ---------------------------------------------------------------------------
// Root-namespace classes still register under bare name in Globals
// ---------------------------------------------------------------------------

func TestNoShortNameGlobals_RootClassBareNameInGlobals(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Root-level class (no namespace derived because it's directly in tmpDir)
	writeMagFile(t, tmpDir, "Helper.mag", `Helper subclass: Object
  method: help [ ^'helping' ]
`)

	_, err := compilePath(tmpDir, vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Bare name should be in Globals (root namespace class)
	if _, ok := vmInst.Globals["Helper"]; !ok {
		t.Error("Globals[\"Helper\"] should exist for root-namespace class")
	}
}

// ---------------------------------------------------------------------------
// Cross-namespace method body reference works via import + FQN Globals
// ---------------------------------------------------------------------------

func TestNoShortNameGlobals_CrossNamespaceMethodRef(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsLib := filepath.Join(tmpDir, "lib")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsLib, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsLib, "Greeter.mag", `Greeter subclass: Object
  method: greet [ ^'hello' ]
`)

	// App::Factory imports Lib so "Greeter" resolves to "Lib::Greeter" at compile time
	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'
import: 'Lib'

Factory subclass: Object
  classMethod: makeGreeter [
    ^Greeter new
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Execute App::Factory makeGreeter and verify it returns a Lib::Greeter
	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Factory"), "makeGreeter", nil)
	greetResult := vmInst.Send(result, "greet", nil)
	if !vm.IsStringValue(greetResult) {
		t.Fatalf("expected string result from greet, got %v", greetResult)
	}
	got := vmInst.Registry().GetStringContent(greetResult)
	if got != "hello" {
		t.Errorf("greet = %q, want %q", got, "hello")
	}

	// Bare "Greeter" should NOT be in Globals
	if _, ok := vmInst.Globals["Greeter"]; ok {
		t.Error("Globals[\"Greeter\"] should not exist — Greeter has namespace Lib")
	}
	// FQN should be in Globals
	if _, ok := vmInst.Globals["Lib::Greeter"]; !ok {
		t.Error("Globals[\"Lib::Greeter\"] should exist")
	}
}

// ---------------------------------------------------------------------------
// Explicit FQN syntax in method body: Widgets::Button new
// ---------------------------------------------------------------------------

func TestFQN_ExplicitFQNInMethodBody(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	nsWidgets := filepath.Join(tmpDir, "widgets")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsWidgets, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsWidgets, "Button.mag", `Button subclass: Object
  method: label [ ^'OK' ]
`)

	// App::Factory uses explicit FQN syntax Widgets::Button in method body
	// (no import needed when using explicit FQN)
	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'

Factory subclass: Object
  classMethod: makeButton [
    ^Widgets::Button new
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	// Execute App::Factory makeButton — should return a Widgets::Button instance
	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Factory"), "makeButton", nil)
	labelResult := vmInst.Send(result, "label", nil)
	if !vm.IsStringValue(labelResult) {
		t.Fatalf("expected string result from label, got %v", labelResult)
	}
	got := vmInst.Registry().GetStringContent(labelResult)
	if got != "OK" {
		t.Errorf("label = %q, want %q", got, "OK")
	}
}

// ---------------------------------------------------------------------------
// Explicit FQN syntax with multi-level namespace: A::B::C
// ---------------------------------------------------------------------------

func TestFQN_ExplicitMultiLevelFQN(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	// Create nested namespace: ui/widgets/
	nsUI := filepath.Join(tmpDir, "ui")
	nsWidgets := filepath.Join(nsUI, "widgets")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsUI, 0755)
	os.Mkdir(nsWidgets, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsWidgets, "Slider.mag", `Slider subclass: Object
  method: kind [ ^'slider' ]
`)

	// App::Factory uses explicit multi-level FQN: Ui::Widgets::Slider
	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'

Factory subclass: Object
  classMethod: makeSlider [
    ^Ui::Widgets::Slider new
  ]
`)

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Factory"), "makeSlider", nil)
	kindResult := vmInst.Send(result, "kind", nil)
	if !vm.IsStringValue(kindResult) {
		t.Fatalf("expected string result from kind, got %v", kindResult)
	}
	got := vmInst.Registry().GetStringContent(kindResult)
	if got != "slider" {
		t.Errorf("kind = %q, want %q", got, "slider")
	}
}
