package manifest

import (
	"strings"
	"unicode"
)

// ToPascalCase converts a directory or dependency name to a namespace
// segment: the first letter of each word is upper-cased and the rest is kept
// as written, so acronyms survive.
//
//	"my-app" -> "MyApp", "models" -> "Models", "myApp" -> "MyApp",
//	"UI" -> "UI", "HTTPServer" -> "HTTPServer", "émile" -> "Émile"
//
// Any character that cannot appear in an identifier ('-', '_', '.', spaces,
// …) separates words, so the result is always identifier characters.
func ToPascalCase(s string) string {
	var b strings.Builder
	startWord := true
	for _, r := range s {
		if !unicode.IsLetter(r) && !unicode.IsDigit(r) {
			startWord = true
			continue
		}
		if startWord {
			r = unicode.ToUpper(r)
			startWord = false
		}
		b.WriteRune(r)
	}
	return b.String()
}

// reservedNamespaces lists core VM class names that cannot be used as
// the root segment of a dependency namespace.
var reservedNamespaces = map[string]bool{
	"Object":              true,
	"Class":               true,
	"Boolean":             true,
	"True":                true,
	"False":               true,
	"UndefinedObject":     true,
	"SmallInteger":        true,
	"Float":               true,
	"String":              true,
	"Symbol":              true,
	"Array":               true,
	"Block":               true,
	"Channel":             true,
	"Process":             true,
	"Mutex":               true,
	"WaitGroup":           true,
	"Semaphore":           true,
	"CancellationContext": true,
	"Result":              true,
	"Success":             true,
	"Failure":             true,
	"Dictionary":          true,
	"GrpcClient":          true,
	"GrpcStream":          true,
	"Context":             true,
	"WeakReference":       true,
	"Character":           true,
	"Compiler":            true,
	"File":                true,
	"HttpServer":          true,
	"HttpResponse":        true,
	"Debugger":            true,
	"Message":             true,
	"Exception":           true,
	"Error":               true,
	"MessageNotUnderstood": true,
	"ZeroDivide":          true,
	"SubscriptOutOfBounds": true,
	"StackOverflow":       true,
	"Warning":             true,
	"Halt":                true,
	"Notification":        true,
}

// IsReservedNamespace reports whether name is a core VM class name
// that must not be used as the root segment of a dependency namespace.
// Only the root segment is checked: "ThirdParty::Array" is fine
// because the root is "ThirdParty".
func IsReservedNamespace(name string) bool {
	root := name
	if idx := strings.Index(name, "::"); idx >= 0 {
		root = name[:idx]
	}
	return reservedNamespaces[root]
}
