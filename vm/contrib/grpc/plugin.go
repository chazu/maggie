package grpc

import vm "github.com/chazu/maggie/vm"

func init() {
	vm.RegisterContrib(&grpcPlugin{})
}

type grpcPlugin struct{}

func (p *grpcPlugin) Name() string { return "grpc" }

func (p *grpcPlugin) Register(v *vm.VM) {
	RegisterGrpcPrimitives(v)
}
