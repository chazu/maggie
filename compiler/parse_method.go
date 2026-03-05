package compiler

import "fmt"

// ParseMethodDef parses a single method definition from source text.
// The source should be a complete method definition like:
//
//	"method: hello [ ^42 ]"
//	"classMethod: new [ ^super new initialize ]"
//
// This is a direct parser entry point that creates a parser instance,
// detects the method:/classMethod: keyword, and delegates to the
// internal parseMethodInBrackets method. The returned MethodDef is
// identical to what the parser produces when parsing the same method
// inside a class definition.
func ParseMethodDef(source string) (*MethodDef, error) {
	if source == "" {
		return nil, fmt.Errorf("empty source")
	}

	p := NewParser(source)

	// Expect "method:" or "classMethod:" keyword
	if !p.curTokenIs(TokenKeyword) {
		return nil, fmt.Errorf("expected 'method:' or 'classMethod:', got %s (%q)", p.curToken.Type, p.curToken.Literal)
	}

	var isClassMethod bool
	switch p.curToken.Literal {
	case "method:":
		isClassMethod = false
	case "classMethod:":
		isClassMethod = true
	default:
		return nil, fmt.Errorf("expected 'method:' or 'classMethod:', got %q", p.curToken.Literal)
	}

	method := p.parseMethodInBrackets(isClassMethod)
	if method == nil {
		if len(p.Errors()) > 0 {
			return nil, fmt.Errorf("parse errors: %v", p.Errors())
		}
		return nil, fmt.Errorf("failed to parse method definition")
	}

	if len(p.Errors()) > 0 {
		return nil, fmt.Errorf("parse errors: %v", p.Errors())
	}

	return method, nil
}
