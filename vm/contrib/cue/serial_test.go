package cue

import (
	"testing"

	"cuelang.org/go/cue/cuecontext"

	vm "github.com/chazu/maggie/vm"
)

func TestSerial_CueValue(t *testing.T) {
	vmInst := vm.NewVM()
	defer vmInst.Shutdown()

	ctx := cuecontext.New()
	cueVal := ctx.CompileString(`{"name": "Alice", "age": 30}`)
	obj := &CueValueObject{val: cueVal}
	val := vmRegisterCueValue(vmInst, obj)

	data, err := vmInst.SerializeValue(val)
	if err != nil {
		t.Fatalf("serialize CueValue: %v", err)
	}
	got, err := vmInst.DeserializeValue(data)
	if err != nil {
		t.Fatalf("deserialize CueValue: %v", err)
	}
	if !isCueValueValue(got) {
		t.Fatal("deserialized value is not a CueValue")
	}
	gotCv := vmGetCueValue(vmInst, got)
	if gotCv == nil {
		t.Fatal("CueValue registry miss after deserialization")
	}
	if gotCv.val.Err() != nil {
		t.Fatalf("deserialized CueValue has error: %v", gotCv.val.Err())
	}
}
