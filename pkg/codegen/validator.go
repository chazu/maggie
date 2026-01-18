// Package codegen generates Go code from Trashtalk AST.
// This file contains in-memory Go code validation using go/parser and go/types.
package codegen

import (
	"go/ast"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"strings"
)

// ValidationError represents a Go validation error with position info
type ValidationError struct {
	Line     int
	Column   int
	Function string // Method/function name containing the error
	Receiver string // Receiver type for methods (empty for functions)
	Message  string
}

// CodeValidator validates generated Go source code in-memory
type CodeValidator struct {
	fset     *token.FileSet
	filename string
}

// NewCodeValidator creates a validator for the given filename (used in error messages)
func NewCodeValidator(filename string) *CodeValidator {
	return &CodeValidator{
		filename: filename,
	}
}

// Validate parses and type-checks Go source code, returning any errors
func (cv *CodeValidator) Validate(source string) []ValidationError {
	cv.fset = token.NewFileSet()

	// Step 1: Parse the source
	file, err := parser.ParseFile(cv.fset, cv.filename, source, parser.AllErrors)
	if err != nil {
		return cv.parseErrorsToValidationErrors(err, source)
	}

	// Build function location map for error attribution
	funcMap := cv.buildFunctionMap(file)

	// Step 2: Type-check
	var typeCheckErrors []ValidationError

	conf := types.Config{
		Importer:    importer.Default(),
		FakeImportC: true, // Handle import "C" for cgo plugins
		Error: func(err error) {
			// types.Error has a Pos field (not a Pos() method)
			if typeErr, ok := err.(types.Error); ok {
				pos := cv.fset.Position(typeErr.Pos)

				fn := funcMap[pos.Line]
				if fn == nil {
					fn = &functionInfo{Name: "<package>"}
				}

				typeCheckErrors = append(typeCheckErrors, ValidationError{
					Line:     pos.Line,
					Column:   pos.Column,
					Function: fn.Name,
					Receiver: fn.Receiver,
					Message:  err.Error(),
				})
			}
		},
	}

	info := &types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}

	_, _ = conf.Check(file.Name.Name, cv.fset, []*ast.File{file}, info)

	return typeCheckErrors
}

// GetMethodsWithErrors returns the set of method names that have errors
func (cv *CodeValidator) GetMethodsWithErrors(errors []ValidationError) map[string]bool {
	methods := make(map[string]bool)
	for _, err := range errors {
		if err.Function != "" && err.Function != "<package>" {
			// Use receiver.method format for methods, just method for functions
			if err.Receiver != "" {
				methods[err.Receiver+"."+err.Function] = true
			} else {
				methods[err.Function] = true
			}
		}
	}
	return methods
}

// GetMethodSelectorsWithErrors maps Go method names back to Trashtalk selectors
// and returns the selectors that have errors
func (cv *CodeValidator) GetMethodSelectorsWithErrors(errors []ValidationError, goNameToSelector map[string]string) map[string]bool {
	selectors := make(map[string]bool)
	for _, err := range errors {
		if err.Function != "" && err.Function != "<package>" {
			// Look up the original selector from the Go name
			if selector, ok := goNameToSelector[err.Function]; ok {
				selectors[selector] = true
			} else {
				// Fallback: use the function name as-is
				selectors[err.Function] = true
			}
		}
	}
	return selectors
}

type functionInfo struct {
	Name      string
	Receiver  string
	StartLine int
	EndLine   int
}

func (cv *CodeValidator) parseErrorsToValidationErrors(err error, source string) []ValidationError {
	var errors []ValidationError

	// Parse errors can be scanner.ErrorList or a single error
	errStr := err.Error()

	// Try to extract line number from error message
	// Format is typically "filename:line:col: message"
	errors = append(errors, ValidationError{
		Line:    1,
		Column:  1,
		Message: errStr,
	})

	return errors
}

func (cv *CodeValidator) buildFunctionMap(file *ast.File) map[int]*functionInfo {
	funcMap := make(map[int]*functionInfo)

	for _, decl := range file.Decls {
		if fn, ok := decl.(*ast.FuncDecl); ok {
			startPos := cv.fset.Position(fn.Pos())
			endPos := cv.fset.Position(fn.End())

			info := &functionInfo{
				Name:      fn.Name.Name,
				StartLine: startPos.Line,
				EndLine:   endPos.Line,
			}

			// Check if it's a method (has a receiver)
			if fn.Recv != nil && len(fn.Recv.List) > 0 {
				recvType := fn.Recv.List[0].Type
				info.Receiver = cv.extractReceiverType(recvType)
			}

			// Map each line in the function to this function info
			for line := startPos.Line; line <= endPos.Line; line++ {
				funcMap[line] = info
			}
		}
	}

	return funcMap
}

func (cv *CodeValidator) extractReceiverType(expr ast.Expr) string {
	switch t := expr.(type) {
	case *ast.Ident:
		return t.Name
	case *ast.StarExpr:
		if ident, ok := t.X.(*ast.Ident); ok {
			return "*" + ident.Name
		}
	}
	return ""
}

// FormatErrors returns a human-readable error report
func FormatValidationErrors(errors []ValidationError, filename string) string {
	if len(errors) == 0 {
		return ""
	}

	var sb strings.Builder
	for _, err := range errors {
		sb.WriteString("  ")
		if err.Function != "" && err.Function != "<package>" {
			if err.Receiver != "" {
				sb.WriteString("(" + err.Receiver + ")." + err.Function)
			} else {
				sb.WriteString(err.Function)
			}
			sb.WriteString(": ")
		}
		sb.WriteString(err.Message)
		sb.WriteString("\n")
	}

	return sb.String()
}
