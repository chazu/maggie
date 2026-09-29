package pipeline

import (
	"testing"

	"github.com/chazu/maggie/compiler"
	"github.com/chazu/maggie/vm"
)

const classVarSource = `CvA subclass: Object
  classVars: Count
  classMethod: bump [ Count isNil ifTrue: [Count := 0]. Count := Count + 1. ^Count ]
  classMethod: viaBlock [ ^#(1 2) inject: 0 into: [:a :e | a + Count + e] ]
  method: instCount [ ^Count ]

CvSub subclass: CvA
  method: subCount [ ^[Count] value ]

CvB subclass: Object
  classVars: Count
  classMethod: bump [ Count isNil ifTrue: [Count := 0]. Count := Count + 1. ^Count ]
`

func wantInt(t *testing.T, what string, got vm.Value, want int64) {
	t.Helper()
	if !got.IsSmallInt() || got.SmallInt() != want {
		t.Errorf("%s = %v, want %d", what, got, want)
	}
}

// classVars: declares per-class storage, visible to the class, its
// subclasses, class-side methods and blocks — not a global shared by every
// class that declares the same name.
func TestClassVarsArePerClass(t *testing.T) {
	vmInst := newTestVM(t)
	dir := t.TempDir()
	writeMagFile(t, dir, "Cv.mag", classVarSource)
	if _, err := newPipeline(vmInst).CompilePath(dir); err != nil {
		t.Fatalf("CompilePath: %v", err)
	}

	cvA := vmInst.ClassValue(vmInst.LookupClass("CvA"))
	cvB := vmInst.ClassValue(vmInst.LookupClass("CvB"))
	for i := 0; i < 3; i++ {
		vmInst.Send(cvA, "bump", nil)
	}
	wantInt(t, "CvB bump", vmInst.Send(cvB, "bump", nil), 1)
	if g, ok := vmInst.LookupGlobal("Count"); ok && g != vm.Nil {
		t.Errorf("class variable leaked into globals: Count = %v", g)
	}
	wantInt(t, "CvA viaBlock", vmInst.Send(cvA, "viaBlock", nil), 9)
	wantInt(t, "CvA new instCount", vmInst.Send(vmInst.Send(cvA, "new", nil), "instCount", nil), 3)
	cvSub := vmInst.ClassValue(vmInst.LookupClass("CvSub"))
	wantInt(t, "CvSub new subCount", vmInst.Send(vmInst.Send(cvSub, "new", nil), "subCount", nil), 3)

	// The declaration and value survive an image round-trip, including a
	// declared variable that was never assigned.
	data, err := vmInst.SaveImageBytes()
	if err != nil {
		t.Fatalf("SaveImageBytes: %v", err)
	}
	reloaded := vm.NewVM()
	if err := reloaded.LoadImageFromBytes(data); err != nil {
		t.Fatalf("LoadImageFromBytes: %v", err)
	}
	reloaded.UseGoCompiler(compiler.Compile)
	wantInt(t, "reloaded CvA bump", reloaded.Send(reloaded.ClassValue(reloaded.LookupClass("CvA")), "bump", nil), 4)
	if cls := reloaded.LookupClass("CvB"); cls == nil || len(cls.ClassVars) != 1 {
		t.Errorf("CvB class var declaration lost on reload: %v", cls)
	}
}
