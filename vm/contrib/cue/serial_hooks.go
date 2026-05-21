package cue

import (
	"encoding/json"
	"fmt"

	"cuelang.org/go/cue/cuecontext"
	"github.com/fxamacker/cbor/v2"

	vm "github.com/chazu/maggie/vm"
)

const cborTagCueValue = 27004

func init() {
	vm.RegisterSerializeHook(serializeCueValueHook)
	vm.RegisterDeserializeHook(cborTagCueValue, deserializeCueValueHook)
}

func serializeCueValueHook(vmInst *vm.VM, v vm.Value) ([]byte, bool, error) {
	if !isCueValueValue(v) {
		return nil, false, nil
	}
	cv := vmGetCueValue(vmInst, v)
	if cv == nil {
		return nil, false, fmt.Errorf("serial: CueValue registry miss")
	}
	data, err := json.Marshal(cueToInterface(cv.val))
	if err != nil {
		return nil, false, fmt.Errorf("serial: CueValue JSON: %w", err)
	}
	enc := vm.CborSerialEncMode()
	bytes, err := enc.Marshal(cbor.Tag{Number: cborTagCueValue, Content: string(data)})
	return bytes, true, err
}

func deserializeCueValueHook(vmInst *vm.VM, tag cbor.Tag) (vm.Value, error) {
	jsonText, ok := tag.Content.(string)
	if !ok {
		return vm.Nil, fmt.Errorf("serial: CueValue tag content not string")
	}
	ctx := cuecontext.New()
	cueVal := ctx.CompileString(jsonText)
	if cueVal.Err() != nil {
		return vm.Nil, fmt.Errorf("serial: CueValue compile: %w", cueVal.Err())
	}
	obj := &CueValueObject{val: cueVal}
	return vmRegisterCueValue(vmInst, obj), nil
}
