package compiler

import "testing"

// Text after the method must be an error, not silently dropped.
func TestParseMethodDef_RejectsTrailingInput(t *testing.T) {
	for _, src := range []string{
		"method: foo [ ^1 ] method: bar [ ^2 ]",
		"method: foo [ ^1 ] garbage 3 4 ]]]",
	} {
		if m, err := ParseMethodDef(src); err == nil {
			t.Errorf("ParseMethodDef(%q) = %s, want error", src, m.Selector)
		}
	}
	for _, src := range []string{
		"method: foo [ ^1 ]",
		"method: foo [ ^1 ]\n\n",
		"method: foo [ ^1 ] \"trailing comment\"",
	} {
		if _, err := ParseMethodDef(src); err != nil {
			t.Errorf("ParseMethodDef(%q): unexpected error %v", src, err)
		}
	}
}
