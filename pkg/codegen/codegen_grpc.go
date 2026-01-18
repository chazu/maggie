// Package codegen generates Go code from Trashtalk AST.
// This file contains gRPC-related code generation for GrpcClient class.
package codegen

import (
	"github.com/dave/jennifer/jen"
)

// generateGrpcClientMethod generates specialized gRPC implementations for GrpcClient methods.
// Returns true if the method was handled, false to fall through to default generation.
// Note: selectors use underscores (from AST), not colons (from source syntax).
func (g *generator) generateGrpcClientMethod(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	case "call_with_":
		// Unary call: call: method with: jsonPayload
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params(
			jen.Id("method").String(),
			jen.Id("jsonPayload").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("grpcCall").Call(jen.Id("method"), jen.Id("jsonPayload"))),
		)
		f.Line()
		return true

	case "call_":
		// Unary call with empty payload: call: method
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params(
			jen.Id("method").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("grpcCall").Call(jen.Id("method"), jen.Lit("{}"))),
		)
		f.Line()
		return true

	case "serverStream_with_handler_":
		// Server streaming: serverStream: method with: payload handler: block
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params(
			jen.Id("method").String(),
			jen.Id("payload").String(),
			jen.Id("handlerBlockID").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("serverStream").Call(
				jen.Id("method"),
				jen.Id("payload"),
				jen.Id("handlerBlockID"),
			)),
		)
		f.Line()
		return true

	case "clientStream_handler_":
		// Client streaming: clientStream: method handler: block
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params(
			jen.Id("method").String(),
			jen.Id("handlerBlockID").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("clientStream").Call(
				jen.Id("method"),
				jen.Id("handlerBlockID"),
			)),
		)
		f.Line()
		return true

	case "bidiStream_handler_":
		// Bidi streaming: bidiStream: method handler: block
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params(
			jen.Id("method").String(),
			jen.Id("handlerBlockID").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("c").Dot("bidiStream").Call(
				jen.Id("method"),
				jen.Id("handlerBlockID"),
			)),
		)
		f.Line()
		return true

	case "listServices":
		// List services via reflection
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("conn"), jen.Err()).Op(":=").Id("c").Dot("getConnection").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.If(jen.String().Call(jen.Id("c").Dot("PoolConnections")).Op("!=").Lit("yes")).Block(
				jen.Defer().Id("conn").Dot("Close").Call(),
			),
			jen.Line(),
			jen.Id("ctx").Op(":=").Qual("context", "Background").Call(),
			jen.Id("refClient").Op(":=").Qual("github.com/jhump/protoreflect/grpcreflect", "NewClientAuto").Call(
				jen.Id("ctx"),
				jen.Id("conn"),
			),
			jen.Defer().Id("refClient").Dot("Reset").Call(),
			jen.Line(),
			jen.List(jen.Id("services"), jen.Err()).Op(":=").Id("refClient").Dot("ListServices").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("services"), jen.Lit("\n")), jen.Nil()),
		)
		f.Line()
		return true

	case "listMethods_":
		// List methods for a service
		f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id(m.goName).Params(
			jen.Id("serviceName").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("conn"), jen.Err()).Op(":=").Id("c").Dot("getConnection").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.If(jen.String().Call(jen.Id("c").Dot("PoolConnections")).Op("!=").Lit("yes")).Block(
				jen.Defer().Id("conn").Dot("Close").Call(),
			),
			jen.Line(),
			jen.Id("ctx").Op(":=").Qual("context", "Background").Call(),
			jen.Id("refClient").Op(":=").Qual("github.com/jhump/protoreflect/grpcreflect", "NewClientAuto").Call(
				jen.Id("ctx"),
				jen.Id("conn"),
			),
			jen.Defer().Id("refClient").Dot("Reset").Call(),
			jen.Line(),
			jen.List(jen.Id("svcDesc"), jen.Err()).Op(":=").Id("refClient").Dot("ResolveService").Call(jen.Id("serviceName")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Line(),
			jen.Var().Id("methods").Index().String(),
			jen.For(jen.List(jen.Id("_"), jen.Id("m")).Op(":=").Range().Id("svcDesc").Dot("GetMethods").Call()).Block(
				jen.Id("methods").Op("=").Append(jen.Id("methods"), jen.Id("m").Dot("GetName").Call()),
			),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("methods"), jen.Lit("\n")), jen.Nil()),
		)
		f.Line()
		return true

	default:
		// Not a special GrpcClient method - fall through to default generation
		return false
	}
}

// generateGrpcHelpers generates helper functions for GrpcClient class
func (g *generator) generateGrpcHelpers(f *jen.File) {
	f.Line()
	f.Comment("// gRPC helper functions for GrpcClient")
	f.Line()

	// getConnection - lazy connection creation
	f.Comment("// getConnection returns an existing connection or creates a new one")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("getConnection").Params().Parens(jen.List(
		jen.Op("*").Qual("google.golang.org/grpc", "ClientConn"),
		jen.Error(),
	)).Block(
		jen.If(jen.Id("c").Dot("conn").Op("!=").Nil()).Block(
			jen.Return(jen.Id("c").Dot("conn"), jen.Nil()),
		),
		jen.Var().Id("opts").Index().Qual("google.golang.org/grpc", "DialOption"),
		jen.If(jen.String().Call(jen.Id("c").Dot("UsePlaintext")).Op("==").Lit("yes")).Block(
			jen.Id("opts").Op("=").Append(jen.Id("opts"), jen.Qual("google.golang.org/grpc", "WithTransportCredentials").Call(
				jen.Qual("google.golang.org/grpc/credentials/insecure", "NewCredentials").Call(),
			)),
		),
		jen.List(jen.Id("conn"), jen.Err()).Op(":=").Qual("google.golang.org/grpc", "NewClient").Call(
			jen.String().Call(jen.Id("c").Dot("Address")),
			jen.Id("opts").Op("..."),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Err()),
		),
		jen.If(jen.String().Call(jen.Id("c").Dot("PoolConnections")).Op("==").Lit("yes")).Block(
			jen.Id("c").Dot("conn").Op("=").Id("conn"),
		),
		jen.Return(jen.Id("conn"), jen.Nil()),
	)
	f.Line()

	// closeConnection - closes the pooled connection if any
	f.Comment("// closeConnection closes the pooled connection if any")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("closeConnection").Params().Block(
		jen.If(jen.Id("c").Dot("conn").Op("!=").Nil()).Block(
			jen.Id("c").Dot("conn").Dot("Close").Call(),
			jen.Id("c").Dot("conn").Op("=").Nil(),
		),
	)
	f.Line()

	// loadProtoFile - parses and caches proto file descriptors
	f.Comment("// loadProtoFile parses a proto file and caches the descriptors")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("loadProtoFile").Params().Error().Block(
		jen.If(jen.Len(jen.Id("c").Dot("fileDescs")).Op(">").Lit(0)).Block(
			jen.Return(jen.Nil()), // Already loaded
		),
		jen.If(jen.String().Call(jen.Id("c").Dot("ProtoFile")).Op("==").Lit("")).Block(
			jen.Return(jen.Qual("fmt", "Errorf").Call(jen.Lit("no proto file specified"))),
		),
		jen.Line(),
		jen.Id("parser").Op(":=").Qual("github.com/jhump/protoreflect/desc/protoparse", "Parser").Values(jen.Dict{
			jen.Id("ImportPaths"): jen.Index().String().Values(
				jen.Qual("path/filepath", "Dir").Call(jen.String().Call(jen.Id("c").Dot("ProtoFile"))),
				jen.Lit("."),
			),
		}),
		jen.Line(),
		jen.List(jen.Id("fds"), jen.Err()).Op(":=").Id("parser").Dot("ParseFiles").Call(
			jen.Qual("path/filepath", "Base").Call(jen.String().Call(jen.Id("c").Dot("ProtoFile"))),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to parse proto file %s: %w"), jen.String().Call(jen.Id("c").Dot("ProtoFile")), jen.Err())),
		),
		jen.Line(),
		jen.Id("c").Dot("fileDescs").Op("=").Id("fds"),
		jen.Return(jen.Nil()),
	)
	f.Line()

	// findMethodInProto - finds a method descriptor from cached proto descriptors
	f.Comment("// findMethodInProto finds a method descriptor from parsed proto files")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("findMethodInProto").Params(
		jen.Id("serviceName").String(),
		jen.Id("methodName").String(),
	).Parens(jen.List(
		jen.Op("*").Qual("github.com/jhump/protoreflect/desc", "MethodDescriptor"),
		jen.Error(),
	)).Block(
		jen.For(jen.List(jen.Id("_"), jen.Id("fd")).Op(":=").Range().Id("c").Dot("fileDescs")).Block(
			jen.For(jen.List(jen.Id("_"), jen.Id("svc")).Op(":=").Range().Id("fd").Dot("GetServices").Call()).Block(
				jen.If(jen.Id("svc").Dot("GetFullyQualifiedName").Call().Op("==").Id("serviceName")).Block(
					jen.Id("mtd").Op(":=").Id("svc").Dot("FindMethodByName").Call(jen.Id("methodName")),
					jen.If(jen.Id("mtd").Op("!=").Nil()).Block(
						jen.Return(jen.Id("mtd"), jen.Nil()),
					),
				),
			),
		),
		jen.Return(jen.Nil(), jen.Qual("fmt", "Errorf").Call(
			jen.Lit("method %s not found in service %s"), jen.Id("methodName"), jen.Id("serviceName"),
		)),
	)
	f.Line()

	// resolveMethod - common setup for all gRPC calls
	// Returns conn, ctx, methodDescriptor, stub, cleanup function, error
	f.Comment("// resolveMethod resolves a gRPC method using server reflection or proto file")
	f.Comment("// Returns connection, context, method descriptor, stub, cleanup func, and error")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("resolveMethod").Params(
		jen.Id("method").String(),
	).Parens(jen.List(
		jen.Op("*").Qual("google.golang.org/grpc", "ClientConn"),
		jen.Qual("context", "Context"),
		jen.Op("*").Qual("github.com/jhump/protoreflect/desc", "MethodDescriptor"),
		jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub"),
		jen.Func().Params(),
		jen.Error(),
	)).Block(
		// Get connection
		jen.List(jen.Id("conn"), jen.Err()).Op(":=").Id("c").Dot("getConnection").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Err()),
		),
		jen.Line(),
		// Parse method name: "service.Name/Method" -> service, method
		jen.Id("parts").Op(":=").Qual("strings", "SplitN").Call(jen.Id("method"), jen.Lit("/"), jen.Lit(2)),
		jen.If(jen.Len(jen.Id("parts")).Op("!=").Lit(2)).Block(
			jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(),
				jen.Qual("fmt", "Errorf").Call(jen.Lit("invalid method format: %s (expected service/method)"), jen.Id("method"))),
		),
		jen.Id("serviceName").Op(":=").Id("parts").Index(jen.Lit(0)),
		jen.Id("methodName").Op(":=").Id("parts").Index(jen.Lit(1)),
		jen.Line(),
		jen.Id("ctx").Op(":=").Qual("context", "Background").Call(),
		jen.Var().Id("mtdDesc").Op("*").Qual("github.com/jhump/protoreflect/desc", "MethodDescriptor"),
		jen.Var().Id("refClient").Op("*").Qual("github.com/jhump/protoreflect/grpcreflect", "Client"),
		jen.Line(),
		// Branch based on proto file vs reflection mode
		jen.If(jen.String().Call(jen.Id("c").Dot("ProtoFile")).Op("!=").Lit("")).Block(
			// Proto file mode - parse file and find method
			jen.If(jen.Err().Op(":=").Id("c").Dot("loadProtoFile").Call().Op(";").Err().Op("!=").Nil()).Block(
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Err()),
			),
			jen.List(jen.Id("mtdDesc"), jen.Err()).Op("=").Id("c").Dot("findMethodInProto").Call(jen.Id("serviceName"), jen.Id("methodName")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(), jen.Err()),
			),
		).Else().Block(
			// Reflection mode - query server for descriptors
			jen.Id("refClient").Op("=").Qual("github.com/jhump/protoreflect/grpcreflect", "NewClientAuto").Call(
				jen.Id("ctx"),
				jen.Id("conn"),
			),
			jen.List(jen.Id("svcDesc"), jen.Err()).Op(":=").Id("refClient").Dot("ResolveService").Call(jen.Id("serviceName")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Id("refClient").Dot("Reset").Call(),
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(),
					jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to resolve service %s: %w"), jen.Id("serviceName"), jen.Err())),
			),
			jen.Id("mtdDesc").Op("=").Id("svcDesc").Dot("FindMethodByName").Call(jen.Id("methodName")),
			jen.If(jen.Id("mtdDesc").Op("==").Nil()).Block(
				jen.Id("refClient").Dot("Reset").Call(),
				jen.Return(jen.Nil(), jen.Nil(), jen.Nil(), jen.Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "Stub").Values(), jen.Nil(),
					jen.Qual("fmt", "Errorf").Call(jen.Lit("method %s not found in service %s"), jen.Id("methodName"), jen.Id("serviceName"))),
			),
		),
		jen.Line(),
		// Create stub
		jen.Id("stub").Op(":=").Qual("github.com/jhump/protoreflect/dynamic/grpcdynamic", "NewStub").Call(jen.Id("conn")),
		jen.Line(),
		// Build cleanup function
		jen.Id("cleanup").Op(":=").Func().Params().Block(
			jen.If(jen.Id("refClient").Op("!=").Nil()).Block(
				jen.Id("refClient").Dot("Reset").Call(),
			),
			jen.If(jen.String().Call(jen.Id("c").Dot("PoolConnections")).Op("!=").Lit("yes")).Block(
				jen.Id("conn").Dot("Close").Call(),
			),
		),
		jen.Line(),
		jen.Return(jen.Id("conn"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Nil()),
	)
	f.Line()

	// grpcCall - makes a unary gRPC call using reflection
	f.Comment("// grpcCall makes a unary gRPC call using reflection")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("grpcCall").Params(
		jen.Id("method").String(),
		jen.Id("jsonPayload").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.Line(),
		// Create dynamic message for request
		jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(
			jen.Id("mtdDesc").Dot("GetInputType").Call(),
		),
		jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("jsonPayload"))).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to parse request JSON: %w"), jen.Err())),
		),
		jen.Line(),
		// Invoke RPC
		jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpc").Call(
			jen.Id("ctx"),
			jen.Id("mtdDesc"),
			jen.Id("reqMsg"),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("RPC failed: %w"), jen.Err())),
		),
		jen.Line(),
		// Marshal response to JSON - type assert to *dynamic.Message since InvokeRpc returns proto.Message
		jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to marshal response: %w"), jen.Err())),
		),
		jen.Return(jen.String().Parens(jen.Id("respJSON")), jen.Nil()),
	)
	f.Line()

	// serverStream - makes a server streaming gRPC call
	f.Comment("// serverStream makes a server streaming gRPC call with callback")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("serverStream").Params(
		jen.Id("method").String(),
		jen.Id("jsonPayload").String(),
		jen.Id("handlerBlockID").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.Line(),
		// Validate streaming type
		jen.If(jen.Op("!").Id("mtdDesc").Dot("IsServerStreaming").Call()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("method is not server streaming"))),
		),
		jen.Line(),
		// Create request message
		jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(
			jen.Id("mtdDesc").Dot("GetInputType").Call(),
		),
		jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("jsonPayload"))).Op(";").Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to parse request: %w"), jen.Err())),
		),
		jen.Line(),
		// Invoke server streaming RPC
		jen.List(jen.Id("stream"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpcServerStream").Call(
			jen.Id("ctx"),
			jen.Id("mtdDesc"),
			jen.Id("reqMsg"),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to start stream: %w"), jen.Err())),
		),
		jen.Line(),
		// Read responses and invoke block for each
		jen.Id("count").Op(":=").Lit(0),
		jen.For().Block(
			jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stream").Dot("RecvMsg").Call(),
			jen.If(jen.Err().Op("==").Qual("io", "EOF")).Block(
				jen.Break(),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("stream error: %w"), jen.Err())),
			),
			jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Continue(),
			),
			jen.Id("invokeBlock").Call(jen.Id("handlerBlockID"), jen.String().Parens(jen.Id("respJSON"))),
			jen.Id("count").Op("++"),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d"), jen.Id("count")), jen.Nil()),
	)
	f.Line()

	// clientStream - makes a client streaming gRPC call
	f.Comment("// clientStream makes a client streaming gRPC call")
	f.Comment("// Block is called repeatedly to get messages; return empty string to end stream")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("clientStream").Params(
		jen.Id("method").String(),
		jen.Id("handlerBlockID").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.Line(),
		// Validate streaming type
		jen.If(jen.Op("!").Id("mtdDesc").Dot("IsClientStreaming").Call()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("method is not client streaming"))),
		),
		jen.Line(),
		// Start client stream
		jen.List(jen.Id("stream"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpcClientStream").Call(
			jen.Id("ctx"),
			jen.Id("mtdDesc"),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to start stream: %w"), jen.Err())),
		),
		jen.Line(),
		// Send messages by invoking block until it returns empty
		jen.For().Block(
			jen.Id("msgJSON").Op(":=").Id("invokeBlock").Call(jen.Id("handlerBlockID")),
			jen.If(jen.Id("msgJSON").Op("==").Lit("")).Block(
				jen.Break(),
			),
			jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(
				jen.Id("mtdDesc").Dot("GetInputType").Call(),
			),
			jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("msgJSON"))).Op(";").Err().Op("!=").Nil()).Block(
				jen.Continue(),
			),
			jen.If(jen.Err().Op(":=").Id("stream").Dot("SendMsg").Call(jen.Id("reqMsg")).Op(";").Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("send error: %w"), jen.Err())),
			),
		),
		jen.Line(),
		// Close and receive response
		jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stream").Dot("CloseAndReceive").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("close error: %w"), jen.Err())),
		),
		jen.List(jen.Id("respJSON"), jen.Err()).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Return(jen.String().Parens(jen.Id("respJSON")), jen.Nil()),
	)
	f.Line()

	// bidiStream - makes a bidirectional streaming gRPC call
	f.Comment("// bidiStream makes a bidirectional streaming gRPC call")
	f.Comment("// Block receives responses and returns messages to send; return empty to stop sending")
	f.Func().Parens(jen.Id("c").Op("*").Id("GrpcClient")).Id("bidiStream").Params(
		jen.Id("method").String(),
		jen.Id("handlerBlockID").String(),
	).Parens(jen.List(jen.String(), jen.Error())).Block(
		jen.List(jen.Id("_"), jen.Id("ctx"), jen.Id("mtdDesc"), jen.Id("stub"), jen.Id("cleanup"), jen.Err()).Op(":=").Id("c").Dot("resolveMethod").Call(jen.Id("method")),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Err()),
		),
		jen.Defer().Id("cleanup").Call(),
		jen.Line(),
		// Validate streaming type
		jen.If(jen.Op("!").Id("mtdDesc").Dot("IsClientStreaming").Call().Op("||").Op("!").Id("mtdDesc").Dot("IsServerStreaming").Call()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("method is not bidirectional streaming"))),
		),
		jen.Line(),
		// Start bidi stream
		jen.List(jen.Id("stream"), jen.Err()).Op(":=").Id("stub").Dot("InvokeRpcBidiStream").Call(
			jen.Id("ctx"),
			jen.Id("mtdDesc"),
		),
		jen.If(jen.Err().Op("!=").Nil()).Block(
			jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("failed to start stream: %w"), jen.Err())),
		),
		jen.Line(),
		// Run send/receive loop
		jen.Id("count").Op(":=").Lit(0),
		jen.Id("doneSending").Op(":=").False(),
		jen.For().Block(
			jen.List(jen.Id("respMsg"), jen.Err()).Op(":=").Id("stream").Dot("RecvMsg").Call(),
			jen.If(jen.Err().Op("==").Qual("io", "EOF")).Block(
				jen.Break(),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Qual("fmt", "Errorf").Call(jen.Lit("recv error: %w"), jen.Err())),
			),
			jen.List(jen.Id("respJSON"), jen.Id("_")).Op(":=").Id("respMsg").Assert(jen.Op("*").Qual("github.com/jhump/protoreflect/dynamic", "Message")).Dot("MarshalJSON").Call(),
			jen.Id("reply").Op(":=").Id("invokeBlock").Call(jen.Id("handlerBlockID"), jen.String().Parens(jen.Id("respJSON"))),
			jen.Id("count").Op("++"),
			jen.If(jen.Op("!").Id("doneSending").Op("&&").Id("reply").Op("!=").Lit("")).Block(
				jen.Id("reqMsg").Op(":=").Qual("github.com/jhump/protoreflect/dynamic", "NewMessage").Call(
					jen.Id("mtdDesc").Dot("GetInputType").Call(),
				),
				jen.If(jen.Err().Op(":=").Id("reqMsg").Dot("UnmarshalJSON").Call(jen.Index().Byte().Parens(jen.Id("reply"))).Op(";").Err().Op("==").Nil()).Block(
					jen.Id("stream").Dot("SendMsg").Call(jen.Id("reqMsg")),
				),
			).Else().If(jen.Op("!").Id("doneSending").Op("&&").Id("reply").Op("==").Lit("")).Block(
				jen.Id("stream").Dot("CloseSend").Call(),
				jen.Id("doneSending").Op("=").True(),
			),
		),
		jen.Return(jen.Qual("fmt", "Sprintf").Call(jen.Lit("%d"), jen.Id("count")), jen.Nil()),
	)
	f.Line()
}
