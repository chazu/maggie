package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/chazu/maggie/vm"
)

func TestBootstrapAndLoad(t *testing.T) {
	// Create temp directory for test
	tmpDir := t.TempDir()
	imagePath := filepath.Join(tmpDir, "test.image")

	// Create and bootstrap a VM
	vmInst := vm.NewVM()

	// Save the image
	if err := vmInst.SaveImage(imagePath); err != nil {
		t.Fatalf("Failed to save image: %v", err)
	}

	// Verify the image file exists
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Fatal("Image file was not created")
	}

	// Load the image into a fresh VM
	loadedVM := vm.NewVM()
	if err := loadedVM.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load image: %v", err)
	}

	// Verify some basic functionality works
	// Test SmallInteger arithmetic
	result := loadedVM.Send(vm.FromSmallInt(5), "+", []vm.Value{vm.FromSmallInt(3)})
	if !result.IsSmallInt() || result.SmallInt() != 8 {
		t.Errorf("5 + 3 = %v, want 8", result)
	}

	// Test Boolean
	result = loadedVM.Send(vm.True, "not", nil)
	if result != vm.False {
		t.Errorf("true not = %v, want false", result)
	}
}

func TestLoadMaggieImage(t *testing.T) {
	// Skip if maggie.image doesn't exist
	imagePath := "../../maggie.image"
	if _, err := os.Stat(imagePath); os.IsNotExist(err) {
		t.Skip("maggie.image not found - run bootstrap first")
	}

	// Load the image
	vmInst := vm.NewVM()
	if err := vmInst.LoadImage(imagePath); err != nil {
		t.Fatalf("Failed to load maggie.image: %v", err)
	}

	// Test that compiled methods work

	// Test SmallInteger factorial (compiled from .mag)
	result := vmInst.Send(vm.FromSmallInt(5), "factorial", nil)
	if !result.IsSmallInt() || result.SmallInt() != 120 {
		t.Errorf("5 factorial = %v, want 120", result)
	}

	// Test SmallInteger even (compiled from .mag)
	result = vmInst.Send(vm.FromSmallInt(4), "even", nil)
	if result != vm.True {
		t.Errorf("4 even = %v, want true", result)
	}

	result = vmInst.Send(vm.FromSmallInt(5), "even", nil)
	if result != vm.False {
		t.Errorf("5 even = %v, want false", result)
	}

	// Test SmallInteger abs (compiled from .mag)
	result = vmInst.Send(vm.FromSmallInt(-5), "abs", nil)
	if !result.IsSmallInt() || result.SmallInt() != 5 {
		t.Errorf("-5 abs = %v, want 5", result)
	}

	// Test SmallInteger max: (compiled from .mag)
	result = vmInst.Send(vm.FromSmallInt(3), "max:", []vm.Value{vm.FromSmallInt(7)})
	if !result.IsSmallInt() || result.SmallInt() != 7 {
		t.Errorf("3 max: 7 = %v, want 7", result)
	}
}
