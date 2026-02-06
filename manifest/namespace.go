package manifest

import "strings"

// ToPascalCase converts a string to PascalCase.
// "my-app" -> "MyApp", "models" -> "Models", "myApp" -> "MyApp"
func ToPascalCase(s string) string {
	var words []string
	current := ""
	for i, r := range s {
		if r == '-' || r == '_' {
			if current != "" {
				words = append(words, current)
				current = ""
			}
			continue
		}
		if i > 0 && r >= 'A' && r <= 'Z' {
			prev := rune(s[i-1])
			if prev >= 'a' && prev <= 'z' {
				words = append(words, current)
				current = ""
			}
		}
		current += string(r)
	}
	if current != "" {
		words = append(words, current)
	}

	var result string
	for _, w := range words {
		if w == "" {
			continue
		}
		result += strings.ToUpper(w[:1]) + strings.ToLower(w[1:])
	}
	return result
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
