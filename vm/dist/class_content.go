package dist

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/chazu/maggie/vm"
)

// EncodeClassContent serializes the structural metadata of a ClassDigest into
// a deterministic, line-oriented text format suitable for the Content field of
// a class Chunk. The format uses tag-value lines:
//
//	CLASS Greeter
//	NAMESPACE MyApp
//	SUPER Object
//	IVARS name greeting
//	CVARS defaultGreeting
//	DOC A simple greeter class.
//
// Empty/zero fields are omitted. CLASS is always present.
// IVARS and CVARS are space-separated. A docstring spanning several lines
// (the common case for class comments) is written as DOCQ followed by the
// Go-quoted string, since a raw newline would end the DOC line; single-line
// docstrings keep the plain DOC form.
// MethodHashes and Hash are NOT encoded — they live on the Chunk itself.
func EncodeClassContent(d *vm.ClassDigest) string {
	var b strings.Builder

	b.WriteString("CLASS ")
	b.WriteString(d.Name)

	if d.Namespace != "" {
		b.WriteString("\nNAMESPACE ")
		b.WriteString(d.Namespace)
	}
	if d.SuperclassName != "" {
		b.WriteString("\nSUPER ")
		b.WriteString(d.SuperclassName)
	}
	if len(d.InstVars) > 0 {
		b.WriteString("\nIVARS ")
		b.WriteString(strings.Join(d.InstVars, " "))
	}
	if len(d.ClassVars) > 0 {
		b.WriteString("\nCVARS ")
		b.WriteString(strings.Join(d.ClassVars, " "))
	}
	if d.DocString != "" {
		if strings.ContainsAny(d.DocString, "\r\n") {
			b.WriteString("\nDOCQ ")
			b.WriteString(strconv.Quote(d.DocString))
		} else {
			b.WriteString("\nDOC ")
			b.WriteString(d.DocString)
		}
	}

	return b.String()
}

// DecodeClassContent parses the line-oriented text format produced by
// EncodeClassContent and returns a ClassDigest with Name, Namespace,
// SuperclassName, InstVars, ClassVars, and DocString populated.
// MethodHashes and Hash are NOT populated — those come from the Chunk.
func DecodeClassContent(content string) (*vm.ClassDigest, error) {
	d := &vm.ClassDigest{}
	foundClass := false

	lines := strings.Split(content, "\n")
	for _, line := range lines {
		if line == "" {
			continue
		}

		idx := strings.IndexByte(line, ' ')
		if idx < 0 {
			// Tag with no value — only CLASS could theoretically be bare,
			// but CLASS requires a name.
			return nil, fmt.Errorf("dist: invalid class content line: %q", line)
		}

		tag := line[:idx]
		value := line[idx+1:]

		switch tag {
		case "CLASS":
			d.Name = value
			foundClass = true
		case "NAMESPACE":
			d.Namespace = value
		case "SUPER":
			d.SuperclassName = value
		case "IVARS":
			d.InstVars = strings.Fields(value)
		case "CVARS":
			d.ClassVars = strings.Fields(value)
		case "DOC":
			d.DocString = value
		case "DOCQ":
			doc, err := strconv.Unquote(value)
			if err != nil {
				return nil, fmt.Errorf("dist: invalid quoted docstring: %w", err)
			}
			d.DocString = doc
		default:
			return nil, fmt.Errorf("dist: unknown class content tag: %q", tag)
		}
	}

	if !foundClass {
		return nil, fmt.Errorf("dist: class content missing CLASS tag")
	}

	return d, nil
}
