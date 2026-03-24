package manifest

import "testing"

func TestRunScriptSuccess(t *testing.T) {
	if err := RunScript("test", "true", ".", false); err != nil {
		t.Errorf("expected success, got: %v", err)
	}
}

func TestRunScriptFailure(t *testing.T) {
	if err := RunScript("test", "false", ".", false); err == nil {
		t.Error("expected error for failing script")
	}
}

func TestRunScriptEmpty(t *testing.T) {
	if err := RunScript("test", "", ".", false); err != nil {
		t.Errorf("expected no-op for empty script, got: %v", err)
	}
}
