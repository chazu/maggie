// Package codegen generates Go code from Trashtalk AST.
// This file contains primitive method implementations for the Shell class.
package codegen

import (
	"github.com/dave/jennifer/jen"
)

// generatePrimitiveMethodShell generates native Shell class methods.
// Shell provides command execution primitives - all methods are class methods.
func (g *generator) generatePrimitiveMethodShell(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	// Simple execution
	case "exec_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.List(jen.Id("output"), jen.Err()).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Comment("Return output even on error (non-zero exit)"),
				jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
			),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	case "run_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	case "silent_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("_").Op("=").Id("cmd").Dot("Run").Call(),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "exitCode_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Err().Op(":=").Id("cmd").Dot("Run").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.If(
					jen.List(jen.Id("exitErr"), jen.Id("ok")).Op(":=").Err().Dot("").Parens(jen.Op("*").Qual("os/exec", "ExitError")),
					jen.Id("ok"),
				).Block(
					jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("exitErr").Dot("ExitCode").Call()), jen.Nil()),
				),
				jen.Return(jen.Lit("1"), jen.Nil()),
			),
			jen.Return(jen.Lit("0"), jen.Nil()),
		)
		f.Line()
		return true

	case "succeeds_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Err().Op(":=").Id("cmd").Dot("Run").Call(),
			jen.If(jen.Err().Op("==").Nil()).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "fails_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Err().Op(":=").Id("cmd").Dot("Run").Call(),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	// Output capture
	case "execAll_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("CombinedOutput").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	case "execErr_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Var().Id("stderr").Qual("bytes", "Buffer"),
			jen.Id("cmd").Dot("Stderr").Op("=").Op("&").Id("stderr"),
			jen.Id("_").Op("=").Id("cmd").Dot("Run").Call(),
			jen.Return(jen.Id("stderr").Dot("String").Call(), jen.Nil()),
		)
		f.Line()
		return true

	case "execFull_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Var().Id("stdout").Qual("bytes", "Buffer"),
			jen.Var().Id("stderr").Qual("bytes", "Buffer"),
			jen.Id("cmd").Dot("Stdout").Op("=").Op("&").Id("stdout"),
			jen.Id("cmd").Dot("Stderr").Op("=").Op("&").Id("stderr"),
			jen.Err().Op(":=").Id("cmd").Dot("Run").Call(),
			jen.Id("exitCode").Op(":=").Lit(0),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.If(
					jen.List(jen.Id("exitErr"), jen.Id("ok")).Op(":=").Err().Dot("").Parens(jen.Op("*").Qual("os/exec", "ExitError")),
					jen.Id("ok"),
				).Block(
					jen.Id("exitCode").Op("=").Id("exitErr").Dot("ExitCode").Call(),
				).Else().Block(
					jen.Id("exitCode").Op("=").Lit(1),
				),
			),
			jen.Comment("Return JSON object with stdout, stderr, exitCode"),
			jen.Id("result").Op(":=").Qual("fmt", "Sprintf").Call(
				jen.Lit(`{"stdout":%q,"stderr":%q,"exitCode":%d}`),
				jen.Id("stdout").Dot("String").Call(),
				jen.Id("stderr").Dot("String").Call(),
				jen.Id("exitCode"),
			),
			jen.Return(jen.Id("result"), jen.Nil()),
		)
		f.Line()
		return true

	// Background execution
	case "spawn_":
		f.Func().Id(m.goName).Params(jen.Id("command").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.If(jen.Err().Op(":=").Id("cmd").Dot("Start").Call(), jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("cmd").Dot("Process").Dot("Pid")), jen.Nil()),
		)
		f.Line()
		return true

	case "spawn_outputTo_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("filepath").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("outFile"), jen.Err()).Op(":=").Qual("os", "Create").Call(jen.Id("filepath")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("cmd").Dot("Stdout").Op("=").Id("outFile"),
			jen.Id("cmd").Dot("Stderr").Op("=").Id("outFile"),
			jen.If(jen.Err().Op(":=").Id("cmd").Dot("Start").Call(), jen.Err().Op("!=").Nil()).Block(
				jen.Id("outFile").Dot("Close").Call(),
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Comment("Close file when process exits (in goroutine)"),
			jen.Go().Func().Params().Block(
				jen.Id("cmd").Dot("Wait").Call(),
				jen.Id("outFile").Dot("Close").Call(),
			).Call(),
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("cmd").Dot("Process").Dot("Pid")), jen.Nil()),
		)
		f.Line()
		return true

	case "spawn_stdoutTo_stderrTo_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("stdoutPath").String(),
			jen.Id("stderrPath").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("stdoutFile"), jen.Err()).Op(":=").Qual("os", "Create").Call(jen.Id("stdoutPath")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.List(jen.Id("stderrFile"), jen.Err()).Op(":=").Qual("os", "Create").Call(jen.Id("stderrPath")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Id("stdoutFile").Dot("Close").Call(),
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("cmd").Dot("Stdout").Op("=").Id("stdoutFile"),
			jen.Id("cmd").Dot("Stderr").Op("=").Id("stderrFile"),
			jen.If(jen.Err().Op(":=").Id("cmd").Dot("Start").Call(), jen.Err().Op("!=").Nil()).Block(
				jen.Id("stdoutFile").Dot("Close").Call(),
				jen.Id("stderrFile").Dot("Close").Call(),
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Comment("Close files when process exits (in goroutine)"),
			jen.Go().Func().Params().Block(
				jen.Id("cmd").Dot("Wait").Call(),
				jen.Id("stdoutFile").Dot("Close").Call(),
				jen.Id("stderrFile").Dot("Close").Call(),
			).Call(),
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("cmd").Dot("Process").Dot("Pid")), jen.Nil()),
		)
		f.Line()
		return true

	case "wait_":
		f.Func().Id(m.goName).Params(jen.Id("pidStr").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("1"), jen.Err()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("1"), jen.Nil()),
			),
			jen.List(jen.Id("state"), jen.Id("_")).Op(":=").Id("proc").Dot("Wait").Call(),
			jen.If(jen.Id("state").Op("!=").Nil()).Block(
				jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Id("state").Dot("ExitCode").Call()), jen.Nil()),
			),
			jen.Return(jen.Lit("0"), jen.Nil()),
		)
		f.Line()
		return true

	// Process control
	case "isAlive_":
		f.Func().Id(m.goName).Params(jen.Id("pidStr").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit("false"), jen.Nil()),
			),
			jen.Comment("On Unix, FindProcess always succeeds - use Signal(0) to check"),
			jen.Err().Op("=").Id("proc").Dot("Signal").Call(jen.Qual("syscall", "Signal").Call(jen.Lit(0))),
			jen.If(jen.Err().Op("==").Nil()).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "signal_to_":
		f.Func().Id(m.goName).Params(
			jen.Id("signalName").String(),
			jen.Id("pidStr").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Comment("Map signal name to signal"),
			jen.Var().Id("sig").Qual("os", "Signal"),
			jen.Switch(jen.Id("signalName")).Block(
				jen.Case(jen.Lit("TERM"), jen.Lit("SIGTERM")).Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGTERM"),
				),
				jen.Case(jen.Lit("KILL"), jen.Lit("SIGKILL")).Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGKILL"),
				),
				jen.Case(jen.Lit("STOP"), jen.Lit("SIGSTOP")).Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGSTOP"),
				),
				jen.Case(jen.Lit("CONT"), jen.Lit("SIGCONT")).Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGCONT"),
				),
				jen.Case(jen.Lit("INT"), jen.Lit("SIGINT")).Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGINT"),
				),
				jen.Case(jen.Lit("HUP"), jen.Lit("SIGHUP")).Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGHUP"),
				),
				jen.Default().Block(
					jen.Id("sig").Op("=").Qual("syscall", "SIGTERM"),
				),
			),
			jen.Id("proc").Dot("Signal").Call(jen.Id("sig")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "terminate_":
		f.Func().Id(m.goName).Params(jen.Id("pidStr").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Id("proc").Dot("Signal").Call(jen.Qual("syscall", "SIGTERM")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "kill_":
		f.Func().Id(m.goName).Params(jen.Id("pidStr").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Id("proc").Dot("Kill").Call(),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "pause_":
		f.Func().Id(m.goName).Params(jen.Id("pidStr").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Id("proc").Dot("Signal").Call(jen.Qual("syscall", "SIGSTOP")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "resume_":
		f.Func().Id(m.goName).Params(jen.Id("pidStr").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("pid"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("pidStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.List(jen.Id("proc"), jen.Err()).Op(":=").Qual("os", "FindProcess").Call(jen.Id("pid")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Id("proc").Dot("Signal").Call(jen.Qual("syscall", "SIGCONT")),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	// Piping and chaining
	case "exec_pipeTo_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("pipeCommand").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Execute first command and pipe to second"),
			jen.Id("fullCmd").Op(":=").Id("command").Op("+").Lit(" | ").Op("+").Id("pipeCommand"),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("fullCmd")),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	case "exec_pipeTo_pipeTo_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("pipe1").String(),
			jen.Id("pipe2").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("fullCmd").Op(":=").Id("command").Op("+").Lit(" | ").Op("+").Id("pipe1").Op("+").Lit(" | ").Op("+").Id("pipe2"),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("fullCmd")),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	// Input/Output
	case "exec_withInput_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("input").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("cmd").Dot("Stdin").Op("=").Qual("strings", "NewReader").Call(jen.Id("input")),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	case "exec_withInputFrom_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("filepath").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("inputFile"), jen.Err()).Op(":=").Qual("os", "Open").Call(jen.Id("filepath")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("inputFile").Dot("Close").Call(),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("cmd").Dot("Stdin").Op("=").Id("inputFile"),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	case "exec_outputTo_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("filepath").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("outFile"), jen.Err()).Op(":=").Qual("os", "Create").Call(jen.Id("filepath")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("outFile").Dot("Close").Call(),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("cmd").Dot("Stdout").Op("=").Id("outFile"),
			jen.Id("_").Op("=").Id("cmd").Dot("Run").Call(),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "exec_appendTo_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("filepath").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("outFile"), jen.Err()).Op(":=").Qual("os", "OpenFile").Call(
				jen.Id("filepath"),
				jen.Qual("os", "O_APPEND").Op("|").Qual("os", "O_CREATE").Op("|").Qual("os", "O_WRONLY"),
				jen.Lit(0644),
			),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Defer().Id("outFile").Dot("Close").Call(),
			jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.Id("cmd").Dot("Stdout").Op("=").Id("outFile"),
			jen.Id("_").Op("=").Id("cmd").Dot("Run").Call(),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	// Conditional execution
	case "if_then_":
		f.Func().Id(m.goName).Params(
			jen.Id("condition").String(),
			jen.Id("command").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("condCmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("condition")),
			jen.If(jen.Err().Op(":=").Id("condCmd").Dot("Run").Call(), jen.Err().Op("==").Nil()).Block(
				jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
				jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
				jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
			),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "unless_then_":
		f.Func().Id(m.goName).Params(
			jen.Id("condition").String(),
			jen.Id("command").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("condCmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("condition")),
			jen.If(jen.Err().Op(":=").Id("condCmd").Dot("Run").Call(), jen.Err().Op("!=").Nil()).Block(
				jen.Id("cmd").Op(":=").Qual("os/exec", "Command").Call(jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
				jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
				jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
			),
			jen.Return(jen.Lit(""), jen.Nil()),
		)
		f.Line()
		return true

	case "exec_timeout_":
		f.Func().Id(m.goName).Params(
			jen.Id("command").String(),
			jen.Id("secondsStr").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("seconds"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("secondsStr")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.List(jen.Id("ctx"), jen.Id("cancel")).Op(":=").Qual("context", "WithTimeout").Call(
				jen.Qual("context", "Background").Call(),
				jen.Qual("time", "Duration").Call(jen.Id("seconds")).Op("*").Qual("time", "Second"),
			),
			jen.Defer().Id("cancel").Call(),
			jen.Id("cmd").Op(":=").Qual("os/exec", "CommandContext").Call(jen.Id("ctx"), jen.Lit("bash"), jen.Lit("-c"), jen.Id("command")),
			jen.List(jen.Id("output"), jen.Id("_")).Op(":=").Id("cmd").Dot("Output").Call(),
			jen.Return(jen.String().Call(jen.Id("output")), jen.Nil()),
		)
		f.Line()
		return true

	// Current shell state
	case "pid":
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Qual("os", "Getpid").Call()), jen.Nil()),
		)
		f.Line()
		return true

	case "ppid":
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Qual("os", "Getppid").Call()), jen.Nil()),
		)
		f.Line()
		return true

	case "lastExitCode":
		f.Func().Id(m.goName).Params().Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("In native Go, we don't have a global last exit code - return 0"),
			jen.Return(jen.Lit("0"), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}
