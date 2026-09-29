package vm

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// sampleMagSource is a small class exercising unary, keyword, class-side, and
// nested-bracket / string-bracket method bodies — the cases most likely to
// break the range finder. UpdateMethodInFile replaces the whole matched
// `method: … [ … ]` range, so newSource is a COMPLETE method definition.
const sampleMagSource = `"""
Widget: a test class.
"""
class: Widget [
  method: greet [
    ^'hello'
  ]

  method: add: a to: b [
    ^a + b
  ]

  method: filter: aBlock [
    ^items select: [ :x | aBlock value: x ]
  ]

  method: bracketStr [
    ^']not a close]'
  ]

  classMethod: create [
    ^self new
  ]
]
`

func writeTempMag(t *testing.T, content string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "Widget.mag")
	if err := os.WriteFile(path, []byte(content), 0644); err != nil {
		t.Fatalf("write temp: %v", err)
	}
	return path
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	return string(data)
}

// assertSurvivors checks that every method other than the one just edited is
// still present intact.
func assertSurvivors(t *testing.T, got string, fragments ...string) {
	t.Helper()
	for _, want := range fragments {
		if !strings.Contains(got, want) {
			t.Errorf("expected surviving fragment %q to still be present", want)
		}
	}
}

func TestUpdateMethodInFile_RoundTrip(t *testing.T) {
	path := writeTempMag(t, sampleMagSource)

	if err := UpdateMethodInFile(path, "greet", "method: greet [\n^'hi there'\n]", false); err != nil {
		t.Fatalf("UpdateMethodInFile: %v", err)
	}
	got := readFile(t, path)

	if !strings.Contains(got, "method: greet [") || !strings.Contains(got, "^'hi there'") {
		t.Error("updated method definition not present/well-formed")
	}
	if strings.Contains(got, "^'hello'") {
		t.Error("old body still present")
	}
	assertSurvivors(t, got,
		"method: add: a to: b", "^a + b",
		"method: filter: aBlock", "select: [ :x | aBlock value: x ]",
		"method: bracketStr", "^']not a close]'",
		"classMethod: create", "^self new",
	)
}

func TestUpdateMethodInFile_KeywordSelector(t *testing.T) {
	path := writeTempMag(t, sampleMagSource)

	if err := UpdateMethodInFile(path, "add:to:", "method: add: a to: b [\n^(a + b) * 2\n]", false); err != nil {
		t.Fatalf("UpdateMethodInFile: %v", err)
	}
	got := readFile(t, path)

	if !strings.Contains(got, "^(a + b) * 2") {
		t.Error("keyword method body not updated")
	}
	// The following method (with nested brackets) must be intact — a range that
	// overran into it would drop or corrupt this.
	assertSurvivors(t, got, "method: filter: aBlock", "select: [ :x | aBlock value: x ]")
}

func TestUpdateMethodInFile_ClassMethod(t *testing.T) {
	path := writeTempMag(t, sampleMagSource)

	if err := UpdateMethodInFile(path, "create", "classMethod: create [\n^self basicNew\n]", true); err != nil {
		t.Fatalf("UpdateMethodInFile: %v", err)
	}
	got := readFile(t, path)
	if !strings.Contains(got, "classMethod: create [") || !strings.Contains(got, "^self basicNew") {
		t.Error("class method not updated / prefix lost")
	}
	// Instance-side methods untouched.
	assertSurvivors(t, got, "method: greet", "method: bracketStr")
}

func TestUpdateMethodInFile_NestedAndStringBrackets(t *testing.T) {
	path := writeTempMag(t, sampleMagSource)

	// The ']' inside the string body must not end the range early, and the
	// class method after it must survive.
	if err := UpdateMethodInFile(path, "bracketStr", "method: bracketStr [\n^'safe'\n]", false); err != nil {
		t.Fatalf("UpdateMethodInFile: %v", err)
	}
	got := readFile(t, path)
	if !strings.Contains(got, "^'safe'") {
		t.Error("bracketStr not updated")
	}
	if strings.Contains(got, "not a close") {
		t.Error("old string-bracket body still present")
	}
	assertSurvivors(t, got, "classMethod: create", "^self new")
}

func TestUpdateMethodInFile_NotFound_FileUntouched(t *testing.T) {
	path := writeTempMag(t, sampleMagSource)
	before := readFile(t, path)

	err := UpdateMethodInFile(path, "doesNotExist", "method: doesNotExist [\n^42\n]", false)
	if err == nil {
		t.Fatal("expected an error for a missing method")
	}
	if after := readFile(t, path); before != after {
		t.Error("file was modified despite the method not being found")
	}
}

func TestUpdateMethodInFile_MissingFile(t *testing.T) {
	err := UpdateMethodInFile(filepath.Join(t.TempDir(), "nope.mag"), "x", "method: x [\n^1\n]", false)
	if err == nil {
		t.Error("expected an error for a missing file")
	}
}

// A keyword selector must match exactly: updating at: must not clobber at:put:
// (which also contains "at:"), and put: must not match xput:.
func TestUpdateMethodInFile_KeywordSelectorExactMatch(t *testing.T) {
	src := "Box subclass: Object\n" +
		"  method: at: i put: v [\n    ^'atput'\n  ]\n\n" +
		"  method: xput: v [\n    ^'xput'\n  ]\n\n" +
		"  method: at: i [\n    ^'at'\n  ]\n\n" +
		"  method: put: v [\n    ^'put'\n  ]\n"
	path := writeTempMag(t, src)

	if err := UpdateMethodInFile(path, "at:", "method: at: i [\n^'AT'\n]", false); err != nil {
		t.Fatalf("UpdateMethodInFile at:: %v", err)
	}
	if err := UpdateMethodInFile(path, "put:", "method: put: v [\n^'PUT'\n]", false); err != nil {
		t.Fatalf("UpdateMethodInFile put:: %v", err)
	}
	got := readFile(t, path)
	assertSurvivors(t, got, "method: at: i put: v", "^'atput'", "method: xput: v", "^'xput'", "^'AT'", "^'PUT'")
	if strings.Contains(got, "^'at'\n") || strings.Contains(got, "^'put'\n") {
		t.Errorf("old at:/put: bodies still present:\n%s", got)
	}
}

// Type-annotated headers still match their selector.
func TestUpdateMethodInFile_TypedHeader(t *testing.T) {
	src := "Box subclass: Object\n" +
		"  method: post: url <String> body: b <String> ^<Result> [\n    ^1\n  ]\n\n" +
		"  method: size ^<Integer> [\n    ^0\n  ]\n"
	path := writeTempMag(t, src)
	if err := UpdateMethodInFile(path, "post:body:", "method: post: url body: b [\n^2\n]", false); err != nil {
		t.Fatalf("post:body:: %v", err)
	}
	if err := UpdateMethodInFile(path, "size", "method: size [\n^3\n]", false); err != nil {
		t.Fatalf("size: %v", err)
	}
	got := readFile(t, path)
	assertSurvivors(t, got, "^2", "^3")
}

// Character literals ($] and $') must not affect bracket/string tracking.
func TestUpdateMethodInFile_CharacterLiterals(t *testing.T) {
	src := "Box subclass: Object\n" +
		"  method: close [\n    ^$]\n  ]\n\n" +
		"  method: quote [\n    ^$'\n  ]\n\n" +
		"  method: after [\n    ^'after'\n  ]\n"
	path := writeTempMag(t, src)
	if err := UpdateMethodInFile(path, "close", "method: close [\n^1\n]", false); err != nil {
		t.Fatalf("close: %v", err)
	}
	if err := UpdateMethodInFile(path, "quote", "method: quote [\n^2\n]", false); err != nil {
		t.Fatalf("quote: %v", err)
	}
	got := readFile(t, path)
	assertSurvivors(t, got, "method: close [", "^1", "method: quote [", "^2", "method: after [", "^'after'")
	if strings.Contains(got, "$") {
		t.Errorf("old character-literal bodies still present:\n%s", got)
	}
	if n := strings.Count(got, "]"); n != 3 {
		t.Errorf("expected 3 closing brackets, got %d:\n%s", n, got)
	}
}

// A multi-line docstring with bare """ delimiter lines is replaced with the method.
func TestUpdateMethodInFile_MultiLineDocstring(t *testing.T) {
	src := "Box subclass: Object\n" +
		"  method: first [\n    ^0\n  ]\n\n" +
		"  \"\"\"\n  Old doc line one.\n  Old doc line two.\n  \"\"\"\n" +
		"  method: greet [\n    ^'hello'\n  ]\n"
	path := writeTempMag(t, src)
	if err := UpdateMethodInFile(path, "greet", "\"\"\"\nNew doc.\n\"\"\"\nmethod: greet [\n^'hi'\n]", false); err != nil {
		t.Fatalf("greet: %v", err)
	}
	got := readFile(t, path)
	if strings.Contains(got, "Old doc") {
		t.Errorf("old docstring not replaced:\n%s", got)
	}
	if n := strings.Count(got, `"""`); n != 2 {
		t.Errorf("expected exactly one docstring (2 delimiters), got %d:\n%s", n, got)
	}
	assertSurvivors(t, got, "method: first [", "^0", "New doc.", "^'hi'")
}
