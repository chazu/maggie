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
