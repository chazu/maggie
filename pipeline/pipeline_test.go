package pipeline

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func newTestVM(t *testing.T) *vm.VM {
	t.Helper()
	vmInst := vm.NewVM()
	imagePath := filepath.Join("..", "cmd", "mag", "maggie.image")
	data, err := os.ReadFile(imagePath)
	if err != nil {
		t.Fatalf("reading maggie.image: %v", err)
	}
	if err := vmInst.LoadImageFromBytes(data); err != nil {
		t.Fatalf("loading image: %v", err)
	}
	vmInst.ReRegisterNilPrimitives()
	vmInst.ReRegisterBooleanPrimitives()
	vmInst.UseGoCompiler(compiler.Compile)
	return vmInst
}

func newPipeline(vmInst *vm.VM) *Pipeline {
	return &Pipeline{VM: vmInst}
}

func writeMagFile(t *testing.T, dir, name, source string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte(source), 0644); err != nil {
		t.Fatalf("writing %s: %v", p, err)
	}
	return p
}

// ---------------------------------------------------------------------------
// Forward reference: File A extends class from File B, A loaded before B
// ---------------------------------------------------------------------------

func TestTwoPass_ForwardReference(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

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

	methods, err := pipe.CompilePath(tmpDir)
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

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

	allIvars := button.AllInstVarNames()
	if len(allIvars) != 3 {
		t.Errorf("Button allIvars = %v, want [x y label]", allIvars)
	}
}

func TestTwoPass_CircularImports(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

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

	methods, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}
	if methods < 2 {
		t.Errorf("expected at least 2 methods, got %d", methods)
	}

	foo := vmInst.Classes.LookupInNamespace("Alpha", "Foo")
	if foo == nil {
		t.Error("Alpha::Foo not found")
	}
	bar := vmInst.Classes.LookupInNamespace("Beta", "Bar")
	if bar == nil {
		t.Error("Beta::Bar not found")
	}
}

func TestTwoPass_UnresolvedSuperclassError(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Orphan.mag", `Orphan subclass: NonExistentParent
  method: test [ ^1 ]
`)

	_, err := pipe.CompilePath(tmpDir)
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

func TestTwoPass_TraitForwardReference(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "A_User.mag", `User subclass: Object
  include: Greetable

  method: name [ ^'Alice' ]
`)

	writeMagFile(t, tmpDir, "B_Greetable.mag", `Greetable trait
  method: greet [ ^'Hello!' ]
`)

	methods, err := pipe.CompilePath(tmpDir)
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

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

func TestTwoPass_NamespaceIsolation(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsA := filepath.Join(tmpDir, "alpha")
	nsB := filepath.Join(tmpDir, "beta")
	os.Mkdir(nsA, 0755)
	os.Mkdir(nsB, 0755)

	writeMagFile(t, nsA, "Widget.mag", `Widget subclass: Object
  method: source [ ^'alpha' ]
`)

	writeMagFile(t, nsB, "Widget.mag", `Widget subclass: Object
  method: source [ ^'beta' ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	alphaWidget := vmInst.Classes.LookupInNamespace("Alpha", "Widget")
	if alphaWidget == nil {
		t.Error("Alpha::Widget not found")
	}
	betaWidget := vmInst.Classes.LookupInNamespace("Beta", "Widget")
	if betaWidget == nil {
		t.Error("Beta::Widget not found")
	}

	if alphaWidget == betaWidget {
		t.Error("Alpha::Widget and Beta::Widget should be different class objects")
	}
}

func TestTwoPass_CrossNamespaceSuperclass(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
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

	methods, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

	button := vmInst.Classes.LookupInNamespace("App", "Button")
	if button == nil {
		t.Fatal("App::Button not found")
	}

	allIvars := button.AllInstVarNames()
	if len(allIvars) != 2 {
		t.Errorf("Button allIvars = %v, want [id label]", allIvars)
	}
}

func TestCollectFiles_ReturnsWithoutCompiling(t *testing.T) {
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Foo.mag", `Foo subclass: Object
  method: test [ ^1 ]
`)
	writeMagFile(t, tmpDir, "Bar.mag", `Bar subclass: Object
  method: test [ ^2 ]
`)

	files, err := CollectFiles(tmpDir)
	if err != nil {
		t.Fatalf("CollectFiles failed: %v", err)
	}

	if len(files) != 2 {
		t.Fatalf("expected 2 files, got %d", len(files))
	}

	for _, pf := range files {
		if pf.SF == nil {
			t.Error("parsed file has nil SourceFile")
		}
		if pf.Path == "" {
			t.Error("parsed file has empty path")
		}
	}
}

func TestTwoPass_SingleFileCompilePath(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	p := writeMagFile(t, tmpDir, "Solo.mag", `Solo subclass: Object
  method: value [ ^42 ]
`)

	methods, err := pipe.CompilePath(p)
	if err != nil {
		t.Fatalf("CompilePath single file failed: %v", err)
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
// FQN resolution tests
// ---------------------------------------------------------------------------

func TestFQN_CrossNamespaceClassReference(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
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

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

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

func TestFQN_SameNamespaceClassReference(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
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

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

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

func TestFQN_BareNameRootNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, tmpDir, "Helper.mag", `Helper subclass: Object
  method: help [ ^'helping' ]
`)

	writeMagFile(t, nsApp, "Worker.mag", `namespace: 'App'

Worker subclass: Object
  classMethod: getHelper [
    ^Helper new
  ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
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

func TestFQN_AssignmentResolvesNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
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

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
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

func TestTwoPass_TraitNamespace(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsDir := filepath.Join(tmpDir, "mylib")
	os.Mkdir(nsDir, 0755)

	writeMagFile(t, nsDir, "Printable.mag", `Printable trait
  method: printString [ ^'<printable>' ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
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
// No short-name Globals tests
// ---------------------------------------------------------------------------

func TestNoShortNameGlobals_NamespacedClassOnlyFQN(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsA := filepath.Join(tmpDir, "alpha")
	nsB := filepath.Join(tmpDir, "beta")
	os.Mkdir(nsA, 0755)
	os.Mkdir(nsB, 0755)

	writeMagFile(t, nsA, "Widget.mag", `Widget subclass: Object
  method: source [ ^'alpha' ]
`)

	writeMagFile(t, nsB, "Widget.mag", `Widget subclass: Object
  method: source [ ^'beta' ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

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

	if alphaOK && betaOK && alphaVal == betaVal {
		t.Error("Alpha::Widget and Beta::Widget should be different class values in Globals")
	}

	if _, bareOK := vmInst.Globals["Widget"]; bareOK {
		t.Error("Globals[\"Widget\"] should not exist — namespaced classes must only register under FQN")
	}
}

func TestNoShortNameGlobals_RootClassBareNameInGlobals(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Helper.mag", `Helper subclass: Object
  method: help [ ^'helping' ]
`)

	_, err := pipe.CompilePath(tmpDir)
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	if _, ok := vmInst.Globals["Helper"]; !ok {
		t.Error("Globals[\"Helper\"] should exist for root-namespace class")
	}
}

func TestNoShortNameGlobals_CrossNamespaceMethodRef(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsLib := filepath.Join(tmpDir, "lib")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsLib, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsLib, "Greeter.mag", `Greeter subclass: Object
  method: greet [ ^'hello' ]
`)

	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'
import: 'Lib'

Factory subclass: Object
  classMethod: makeGreeter [
    ^Greeter new
  ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	result := vmInst.Send(vmInst.Symbols.SymbolValue("App::Factory"), "makeGreeter", nil)
	greetResult := vmInst.Send(result, "greet", nil)
	if !vm.IsStringValue(greetResult) {
		t.Fatalf("expected string result from greet, got %v", greetResult)
	}
	got := vmInst.Registry().GetStringContent(greetResult)
	if got != "hello" {
		t.Errorf("greet = %q, want %q", got, "hello")
	}

	if _, ok := vmInst.Globals["Greeter"]; ok {
		t.Error("Globals[\"Greeter\"] should not exist — Greeter has namespace Lib")
	}
	if _, ok := vmInst.Globals["Lib::Greeter"]; !ok {
		t.Error("Globals[\"Lib::Greeter\"] should exist")
	}
}

func TestFQN_ExplicitFQNInMethodBody(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsWidgets := filepath.Join(tmpDir, "widgets")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsWidgets, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsWidgets, "Button.mag", `Button subclass: Object
  method: label [ ^'OK' ]
`)

	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'

Factory subclass: Object
  classMethod: makeButton [
    ^Widgets::Button new
  ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

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

func TestFQN_ExplicitMultiLevelFQN(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	nsUI := filepath.Join(tmpDir, "ui")
	nsWidgets := filepath.Join(nsUI, "widgets")
	nsApp := filepath.Join(tmpDir, "app")
	os.Mkdir(nsUI, 0755)
	os.Mkdir(nsWidgets, 0755)
	os.Mkdir(nsApp, 0755)

	writeMagFile(t, nsWidgets, "Slider.mag", `Slider subclass: Object
  method: kind [ ^'slider' ]
`)

	writeMagFile(t, nsApp, "Factory.mag", `namespace: 'App'

Factory subclass: Object
  classMethod: makeSlider [
    ^Ui::Widgets::Slider new
  ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
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

// ---------------------------------------------------------------------------
// ContentStore tests
// ---------------------------------------------------------------------------

func TestCompileAll_PopulatesContentStore(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Greeter.mag", `Greeter subclass: Object
  instanceVars: name

  method: name [ ^name ]
  method: greet [ ^'Hello, ', name ]

  classMethod: hello [ ^'Hello' ]
`)

	methods, err := pipe.CompilePath(tmpDir)
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}
	if methods == 0 {
		t.Fatal("expected compiled methods")
	}

	store := vmInst.ContentStore()

	if store.MethodCount() == 0 {
		t.Error("ContentStore has no methods after compilation")
	}
	if store.ClassCount() == 0 {
		t.Error("ContentStore has no class digests after compilation")
	}

	classHashes := store.ClassHashes()
	found := false
	for _, h := range classHashes {
		d := store.LookupClass(h)
		if d != nil && d.Name == "Greeter" {
			found = true
			if len(d.MethodHashes) == 0 {
				t.Error("Greeter class digest has no method hashes")
			}
			break
		}
	}
	if !found {
		t.Error("Greeter class digest not found in ContentStore")
	}

	allHashes := store.AllHashes()
	if len(allHashes) < methods+1 {
		t.Errorf("AllHashes: got %d, want at least %d (methods=%d + classes)", len(allHashes), methods+1, methods)
	}
}

func TestCompileAll_ContentStore_MultipleClasses(t *testing.T) {
	vmInst := newTestVM(t)
	pipe := newPipeline(vmInst)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Animal.mag", `Animal subclass: Object
  instanceVars: name

  method: name [ ^name ]
`)

	writeMagFile(t, tmpDir, "Dog.mag", `Dog subclass: Animal
  instanceVars: breed

  method: breed [ ^breed ]
  method: speak [ ^'Woof!' ]
`)

	_, err := pipe.CompilePath(tmpDir + "/...")
	if err != nil {
		t.Fatalf("CompilePath failed: %v", err)
	}

	store := vmInst.ContentStore()

	if store.ClassCount() < 2 {
		t.Errorf("ClassCount: got %d, want at least 2", store.ClassCount())
	}

	if store.MethodCount() < 3 {
		t.Errorf("MethodCount: got %d, want at least 3", store.MethodCount())
	}
}
