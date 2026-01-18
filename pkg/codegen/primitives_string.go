// Package codegen generates Go code from Trashtalk AST.
// This file contains primitive method implementations for the String class.
package codegen

import (
	"github.com/dave/jennifer/jen"
)

// generatePrimitiveMethodString generates native String class methods.
// All String methods are class methods for string manipulation.
func (g *generator) generatePrimitiveMethodString(f *jen.File, m *compiledMethod) bool {
	switch m.selector {
	// String tests
	case "isEmpty_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Len(jen.Id("str")).Op("==").Lit(0)).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "notEmpty_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Len(jen.Id("str")).Op(">").Lit(0)).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "contains_substring_":
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("sub").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Qual("strings", "Contains").Call(jen.Id("str"), jen.Id("sub"))).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "startsWith_prefix_":
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("prefix").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Qual("strings", "HasPrefix").Call(jen.Id("str"), jen.Id("prefix"))).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "endsWith_suffix_":
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("suffix").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Qual("strings", "HasSuffix").Call(jen.Id("str"), jen.Id("suffix"))).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	case "equals_to_":
		f.Func().Id(m.goName).Params(
			jen.Id("a").String(),
			jen.Id("b").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.If(jen.Id("a").Op("==").Id("b")).Block(
				jen.Return(jen.Lit("true"), jen.Nil()),
			),
			jen.Return(jen.Lit("false"), jen.Nil()),
		)
		f.Line()
		return true

	// String manipulation
	case "trimPrefix_from_":
		f.Func().Id(m.goName).Params(
			jen.Id("prefix").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimPrefix").Call(jen.Id("str"), jen.Id("prefix")), jen.Nil()),
		)
		f.Line()
		return true

	case "trimSuffix_from_":
		f.Func().Id(m.goName).Params(
			jen.Id("suffix").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimSuffix").Call(jen.Id("str"), jen.Id("suffix")), jen.Nil()),
		)
		f.Line()
		return true

	case "trimShortPrefix_from_":
		// Go doesn't have exact equivalent of bash ${str#pattern} - use TrimPrefix
		f.Func().Id(m.goName).Params(
			jen.Id("prefix").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimPrefix").Call(jen.Id("str"), jen.Id("prefix")), jen.Nil()),
		)
		f.Line()
		return true

	case "trimShortSuffix_from_":
		// Go doesn't have exact equivalent of bash ${str%pattern} - use TrimSuffix
		f.Func().Id(m.goName).Params(
			jen.Id("suffix").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimSuffix").Call(jen.Id("str"), jen.Id("suffix")), jen.Nil()),
		)
		f.Line()
		return true

	case "replace_with_in_":
		f.Func().Id(m.goName).Params(
			jen.Id("old").String(),
			jen.Id("new").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "Replace").Call(jen.Id("str"), jen.Id("old"), jen.Id("new"), jen.Lit(1)), jen.Nil()),
		)
		f.Line()
		return true

	case "replaceAll_with_in_":
		f.Func().Id(m.goName).Params(
			jen.Id("old").String(),
			jen.Id("new").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "ReplaceAll").Call(jen.Id("str"), jen.Id("old"), jen.Id("new")), jen.Nil()),
		)
		f.Line()
		return true

	case "substring_from_length_":
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("start").String(),
			jen.Id("length").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("s"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("start")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.List(jen.Id("l"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("length")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.If(jen.Id("s").Op("<").Lit(0).Op("||").Id("s").Op(">=").Len(jen.Id("str"))).Block(
				jen.Return(jen.Lit(""), jen.Nil()),
			),
			jen.Id("end").Op(":=").Id("s").Op("+").Id("l"),
			jen.If(jen.Id("end").Op(">").Len(jen.Id("str"))).Block(
				jen.Id("end").Op("=").Len(jen.Id("str")),
			),
			jen.Return(jen.Id("str").Index(jen.Id("s"), jen.Id("end")), jen.Nil()),
		)
		f.Line()
		return true

	case "length_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strconv", "Itoa").Call(jen.Len(jen.Id("str"))), jen.Nil()),
		)
		f.Line()
		return true

	case "uppercase_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "ToUpper").Call(jen.Id("str")), jen.Nil()),
		)
		f.Line()
		return true

	case "lowercase_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "ToLower").Call(jen.Id("str")), jen.Nil()),
		)
		f.Line()
		return true

	// String splitting
	case "split_on_":
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("delim").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("parts").Op(":=").Qual("strings", "Split").Call(jen.Id("str"), jen.Id("delim")),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("parts"), jen.Lit("\n")), jen.Nil()),
		)
		f.Line()
		return true

	case "splitToArray_on_":
		// Split string on delimiter and return as JSON array
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("delim").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("parts").Op(":=").Qual("strings", "Split").Call(jen.Id("str"), jen.Id("delim")),
			jen.List(jen.Id("jsonBytes"), jen.Err()).Op(":=").Qual("encoding/json", "Marshal").Call(jen.Id("parts")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.String().Parens(jen.Id("jsonBytes")), jen.Nil()),
		)
		f.Line()
		return true

	case "before_in_":
		f.Func().Id(m.goName).Params(
			jen.Id("delim").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("idx").Op(":=").Qual("strings", "Index").Call(jen.Id("str"), jen.Id("delim")),
			jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
				jen.Return(jen.Id("str"), jen.Nil()),
			),
			jen.Return(jen.Id("str").Index(jen.Empty(), jen.Id("idx")), jen.Nil()),
		)
		f.Line()
		return true

	case "after_in_":
		f.Func().Id(m.goName).Params(
			jen.Id("delim").String(),
			jen.Id("str").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Id("idx").Op(":=").Qual("strings", "Index").Call(jen.Id("str"), jen.Id("delim")),
			jen.If(jen.Id("idx").Op("<").Lit(0)).Block(
				jen.Return(jen.Id("str"), jen.Nil()),
			),
			jen.Return(jen.Id("str").Index(jen.Id("idx").Op("+").Len(jen.Id("delim")), jen.Empty()), jen.Nil()),
		)
		f.Line()
		return true

	// String building
	case "concat_with_":
		f.Func().Id(m.goName).Params(
			jen.Id("a").String(),
			jen.Id("b").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("a").Op("+").Id("b"), jen.Nil()),
		)
		f.Line()
		return true

	case "concat_with_with_":
		f.Func().Id(m.goName).Params(
			jen.Id("a").String(),
			jen.Id("b").String(),
			jen.Id("c").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Id("a").Op("+").Id("b").Op("+").Id("c"), jen.Nil()),
		)
		f.Line()
		return true

	case "join_values_":
		f.Func().Id(m.goName).Params(
			jen.Id("delim").String(),
			jen.Id("values").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Comment("Split values on whitespace, rejoin with delimiter"),
			jen.Id("parts").Op(":=").Qual("strings", "Fields").Call(jen.Id("values")),
			jen.Return(jen.Qual("strings", "Join").Call(jen.Id("parts"), jen.Id("delim")), jen.Nil()),
		)
		f.Line()
		return true

	case "repeat_times_":
		f.Func().Id(m.goName).Params(
			jen.Id("str").String(),
			jen.Id("times").String(),
		).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.List(jen.Id("n"), jen.Err()).Op(":=").Qual("strconv", "Atoi").Call(jen.Id("times")),
			jen.If(jen.Err().Op("!=").Nil()).Block(
				jen.Return(jen.Lit(""), jen.Err()),
			),
			jen.Return(jen.Qual("strings", "Repeat").Call(jen.Id("str"), jen.Id("n")), jen.Nil()),
		)
		f.Line()
		return true

	// Whitespace handling
	case "trim_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimSpace").Call(jen.Id("str")), jen.Nil()),
		)
		f.Line()
		return true

	case "trimLeft_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimLeft").Call(jen.Id("str"), jen.Lit(" \t\n\r")), jen.Nil()),
		)
		f.Line()
		return true

	case "trimRight_":
		f.Func().Id(m.goName).Params(jen.Id("str").String()).Parens(jen.List(jen.String(), jen.Error())).Block(
			jen.Return(jen.Qual("strings", "TrimRight").Call(jen.Id("str"), jen.Lit(" \t\n\r")), jen.Nil()),
		)
		f.Line()
		return true

	default:
		return false
	}
}
