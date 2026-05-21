package cue

import vm "github.com/chazu/maggie/vm"

func init() {
	vm.RegisterContrib(&cuePlugin{})
}

type cuePlugin struct{}

func (p *cuePlugin) Name() string { return "cue" }

func (p *cuePlugin) Register(v *vm.VM) {
	RegisterCuePrimitives(v)
	registerConstraintStorePrimitives(v)
	registerTupleSpacePrimitives(v)

	// Register primAsCueValue on Object (requires CUE types)
	v.ObjectClass.AddMethod0(v.Selectors, "primAsCueValue", func(vmInst *vm.VM, recv vm.Value) vm.Value {
		cueObj := objectAsCueValue(vmInst, recv)
		return vmRegisterCueValue(vmInst, cueObj)
	})
}
