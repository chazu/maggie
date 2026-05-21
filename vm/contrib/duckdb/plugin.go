package duckdb

import vm "github.com/chazu/maggie/vm"

func init() {
	vm.RegisterContrib(&duckdbPlugin{})
}

type duckdbPlugin struct{}

func (p *duckdbPlugin) Name() string { return "duckdb" }

func (p *duckdbPlugin) Register(v *vm.VM) {
	RegisterDuckDBPrimitives(v)
}
