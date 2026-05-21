package sqlite

import vm "github.com/chazu/maggie/vm"

func init() {
	vm.RegisterContrib(&sqlitePlugin{})
}

type sqlitePlugin struct{}

func (p *sqlitePlugin) Name() string { return "sqlite" }

func (p *sqlitePlugin) Register(v *vm.VM) {
	RegisterSqlitePrimitives(v)
}
