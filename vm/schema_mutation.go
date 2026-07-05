package vm

import "strings"

// schemaMutatingSelectorHeads are the LEADING keyword parts of the reflective
// selectors through which *evaluated source* can structurally mutate shared VM
// state at runtime: define or redefine classes and methods, write globals, file
// in code, or serialize the whole image. Ordinary class/method definition
// happens only through file-in declaration syntax (`X subclass: Y`, method
// chunks), never a runtime message, so a `doIt` expression can reach VM
// structure only via this family — chiefly `Compiler evaluate:` (which compiles
// and files in arbitrary source) and the `compileAndInstall*` reflection on
// classes.
//
// We match the leading keyword part, not the full selector, because in
// Smalltalk source a multi-keyword message interleaves selector parts with
// arguments (`setGlobal: #X to: 42`), so the joined selector `setGlobal:to:`
// never appears contiguously — but its first part `setGlobal:` always does, and
// is distinctive enough that no read-only selector shares it. Keep in sync with
// the selectors registered in compiler_primitives.go and
// class_reflection_primitives.go.
var schemaMutatingSelectorHeads = []string{
	"evaluate:",                    // Compiler evaluate: / evaluate:in: / evaluate:withLocals:
	"setGlobal:",                   // Compiler setGlobal:to: — writes a global
	"compileMethod:",               // Compiler compileMethod:
	"compileAndInstall:",           // <class> compileAndInstall: — installs an instance method
	"compileAndInstallClassMethod:", // <class> compileAndInstallClassMethod:
	"fileIn:",                      // Compiler fileIn: — loads code (defines classes/methods)
	"fileInAll:",                   // Compiler fileInAll:
	"saveImage:",                   // reads the whole VM — must not overlap a mutator
	"saveImageAtomic:",             // Compiler saveImageAtomic:
	"updateMethodInFile:",          // Compiler updateMethodInFile:selector:source:
}

// SourceMayMutateSchema reports whether evaluated source *might* structurally
// mutate the VM — define/redefine classes or methods, write globals, file in
// code, or save the image — and so should run under the server's EXCLUSIVE gate
// (VMWorker.Do) rather than the shared reader gate (VMWorker.DoConcurrent).
//
// It is a deliberately conservative, best-effort textual check. Memory safety
// does NOT depend on it: the shared Selectors/Symbols/ClassTable/registry each
// carry their own locks and exclusive writers are mutually excluded from
// readers, so a misclassified eval can never corrupt VM state. What the check
// buys is *consistency* — a class-defining eval will not run concurrently with a
// browse (which could otherwise observe a half-built class) or with an image
// save. False positives (the token happens to appear in a string literal, or as
// the selector a user is sending) only cost a little parallelism by
// over-serializing; the recognised selectors are the sole runtime path to schema
// mutation, so genuine definition is caught even when it hides inside a block or
// a `Compiler evaluate:` string argument.
//
// Known gap: this only inspects the *source being evaluated*. Mutation reachable
// only through an already-installed method that the source invokes — e.g. an
// object whose `printString` (run by the Inspect handlers) itself calls
// `Compiler evaluate:` — is not visible here and would run under the shared read
// gate. This does not affect memory safety (as above); it is a residual
// consistency gap accepted because inspecting/messaging is a hot IDE path and
// such reflective side effects in a display method are pathological.
func SourceMayMutateSchema(source string) bool {
	for _, head := range schemaMutatingSelectorHeads {
		if containsSelectorToken(source, head) {
			return true
		}
	}
	return false
}

// containsSelectorToken reports whether sel occurs in source as a token rather
// than as the tail of a longer identifier — the character immediately before an
// occurrence must not be an identifier character, so a user selector like
// `reevaluate:` does not match `evaluate:`. (A trailing boundary is unnecessary:
// these tokens end in ':', which cannot continue an identifier.)
func containsSelectorToken(source, sel string) bool {
	for from := 0; ; {
		rel := strings.Index(source[from:], sel)
		if rel < 0 {
			return false
		}
		i := from + rel
		if i == 0 || !isIdentChar(source[i-1]) {
			return true
		}
		from = i + 1
	}
}

func isIdentChar(b byte) bool {
	return b == '_' ||
		(b >= 'a' && b <= 'z') ||
		(b >= 'A' && b <= 'Z') ||
		(b >= '0' && b <= '9')
}
