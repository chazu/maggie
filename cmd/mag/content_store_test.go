package main

import (
	"testing"
)

// TestCompileAll_PopulatesContentStore verifies that after compileAll runs,
// the VM's ContentStore contains indexed methods and class digests.
func TestCompileAll_PopulatesContentStore(t *testing.T) {
	vmInst := newTestVM(t)
	tmpDir := t.TempDir()

	writeMagFile(t, tmpDir, "Greeter.mag", `Greeter subclass: Object
  instanceVars: name

  method: name [ ^name ]
  method: greet [ ^'Hello, ', name ]

  classMethod: hello [ ^'Hello' ]
`)

	methods, err := compilePath(tmpDir, vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
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

	// Verify the Greeter class digest exists
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

	// Verify AllHashes returns both methods and classes
	allHashes := store.AllHashes()
	if len(allHashes) < methods+1 { // at least methods + 1 class
		t.Errorf("AllHashes: got %d, want at least %d (methods=%d + classes)", len(allHashes), methods+1, methods)
	}
}

// TestCompileAll_ContentStore_MultipleClasses verifies that multiple classes
// each get their own digest in the ContentStore.
func TestCompileAll_ContentStore_MultipleClasses(t *testing.T) {
	vmInst := newTestVM(t)
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

	_, err := compilePath(tmpDir+"/...", vmInst, false)
	if err != nil {
		t.Fatalf("compilePath failed: %v", err)
	}

	store := vmInst.ContentStore()

	// Should have at least 2 class digests (Animal, Dog)
	if store.ClassCount() < 2 {
		t.Errorf("ClassCount: got %d, want at least 2", store.ClassCount())
	}

	// Should have at least 3 methods (name, breed, speak)
	if store.MethodCount() < 3 {
		t.Errorf("MethodCount: got %d, want at least 3", store.MethodCount())
	}
}
