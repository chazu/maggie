package main

import (
	"math"
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

// TestPrimitiveStubDoesNotOverwriteGoMethod verifies that a <primitive> stub
// in a .mag file does not overwrite the Go primitive registered on the class.
// This was the root cause of float math methods (sin, cos, ln, exp, pow, etc.)
// returning the receiver instead of the computed value.
func TestPrimitiveStubDoesNotOverwriteGoMethod(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	// Compile a .mag source that declares <primitive> stubs for Float
	source := `Float subclass: Object
  """Returns the sine."""
  method: sin [ <primitive> ]
  """Returns the cosine."""
  method: cos [ <primitive> ]
`
	files := []string{}
	// We need to call compileAllFiles, but it expects file paths.
	// Instead, directly test the logic by parsing and checking VTable.

	// Before loading any .mag: the Go primitive should be registered
	recv := vm.FromFloat64(1.0)
	result := vmInst.Send(recv, "sin", nil)
	if !result.IsFloat() {
		t.Fatalf("before .mag load: 1.0 sin should return float, got %v", result)
	}
	want := math.Sin(1.0)
	if math.Abs(result.Float64()-want) > 1e-10 {
		t.Fatalf("before .mag load: 1.0 sin = %g, want %g", result.Float64(), want)
	}

	// Parse the source and compile using the same logic as compileAllFiles
	sf, err := compiler.ParseSourceFileFromString(source)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Simulate what compileAllFiles does for the Float class
	class := vmInst.FloatClass
	for _, classDef := range sf.Classes {
		for _, methodDef := range classDef.Methods {
			if methodDef.IsPrimitiveStub {
				// This is what the fix does: skip compilation, just set docstring
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
			// Would compile here for non-stub methods
		}
	}

	// After processing: the Go primitive should still be in the VTable
	result = vmInst.Send(recv, "sin", nil)
	if !result.IsFloat() {
		t.Fatalf("after .mag load: 1.0 sin should return float, got %v", result)
	}
	if math.Abs(result.Float64()-want) > 1e-10 {
		t.Fatalf("after .mag load: 1.0 sin = %g, want %g", result.Float64(), want)
	}

	// Verify docstring was attached
	selectorID := vmInst.Selectors.Lookup("sin")
	method := class.VTable.Lookup(selectorID)
	if ds, ok := method.(vm.DocStringable); ok {
		if ds.DocString() != "Returns the sine." {
			t.Errorf("docstring = %q, want %q", ds.DocString(), "Returns the sine.")
		}
	} else {
		t.Error("sin method does not implement DocStringable")
	}

	_ = files // unused
}

// TestFloatMathThroughBootstrap verifies all float math methods work after
// compileAllFiles processes the Float.mag file with <primitive> stubs.
func TestFloatMathThroughBootstrap(t *testing.T) {
	vmInst := vm.NewVM()
	vmInst.UseGoCompiler(compiler.Compile)

	// Compile the actual lib files
	_, err := compileAllFiles([]string{"../../lib/Float.mag"}, vmInst, false)
	if err != nil {
		t.Fatalf("compileAllFiles: %v", err)
	}

	tests := []struct {
		expr string
		recv float64
		args []vm.Value
		want float64
	}{
		{"sin", 1.0, nil, math.Sin(1.0)},
		{"cos", 0.0, nil, 1.0},
		{"tan", 0.0, nil, 0.0},
		{"asin", 0.0, nil, 0.0},
		{"acos", 1.0, nil, 0.0},
		{"atan", 0.0, nil, 0.0},
		{"ln", 1.0, nil, 0.0},
		{"log10", 100.0, nil, 2.0},
		{"exp", 0.0, nil, 1.0},
		{"pow:", 2.0, []vm.Value{vm.FromFloat64(10.0)}, 1024.0},
	}

	for _, tt := range tests {
		result := vmInst.Send(vm.FromFloat64(tt.recv), tt.expr, tt.args)
		if !result.IsFloat() {
			t.Errorf("%g %s: expected float, got %v", tt.recv, tt.expr, result)
			continue
		}
		if math.Abs(result.Float64()-tt.want) > 1e-10 {
			t.Errorf("%g %s = %g, want %g", tt.recv, tt.expr, result.Float64(), tt.want)
		}
	}
}
