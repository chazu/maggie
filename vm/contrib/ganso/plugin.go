package ganso

import vm "github.com/chazu/maggie/vm"

func init() {
	vm.RegisterContrib(&gansoPlugin{})
}

type gansoPlugin struct{}

func (p *gansoPlugin) Name() string { return "ganso" }

func (p *gansoPlugin) Register(v *vm.VM) {
	RegisterGansoPrimitives(v)
}
