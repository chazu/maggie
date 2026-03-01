package vm

import (
	"os"
	"testing"
)

func getSystemClass(v *VM) Value {
	return v.Globals["System"]
}

func TestSystemPid(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sys := getSystemClass(v)
	result := v.Send(sys, "pid", nil)
	if !result.IsSmallInt() {
		t.Fatalf("System pid should return SmallInt, got %v", result)
	}
	pid := result.SmallInt()
	if pid != int64(os.Getpid()) {
		t.Errorf("System pid = %d, want %d", pid, os.Getpid())
	}
}

func TestSystemEnv(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	os.Setenv("MAGGIE_TEST_VAR", "hello_maggie")
	defer os.Unsetenv("MAGGIE_TEST_VAR")

	sys := getSystemClass(v)
	key := v.registry.NewStringValue("MAGGIE_TEST_VAR")
	result := v.Send(sys, "env:", []Value{key})
	s := v.valueToString(result)
	if s != "hello_maggie" {
		t.Errorf("System env: 'MAGGIE_TEST_VAR' = %q, want %q", s, "hello_maggie")
	}

	// Unset var returns empty string
	key2 := v.registry.NewStringValue("MAGGIE_NONEXISTENT_VAR_XYZ")
	result = v.Send(sys, "env:", []Value{key2})
	s = v.valueToString(result)
	if s != "" {
		t.Errorf("System env: for unset var = %q, want empty string", s)
	}
}

func TestSystemArgs(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sys := getSystemClass(v)
	result := v.Send(sys, "args", nil)
	arr := v.getArrayValue(result)
	if arr == nil {
		t.Fatal("System args should return an Array")
	}
	expectedLen := len(os.Args) - 1
	if expectedLen < 0 {
		expectedLen = 0
	}
	if len(arr) != expectedLen {
		t.Errorf("System args length = %d, want %d", len(arr), expectedLen)
	}
}

func TestSystemPidString(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sys := getSystemClass(v)
	result := v.Send(sys, "pidString", nil)
	s := v.valueToString(result)
	if s == "" {
		t.Error("System pidString should return non-empty string")
	}
}

func TestSystemHostname(t *testing.T) {
	v := NewVM()
	defer v.Shutdown()

	sys := getSystemClass(v)
	result := v.Send(sys, "hostname", nil)
	s := v.valueToString(result)
	expected, _ := os.Hostname()
	if s != expected {
		t.Errorf("System hostname = %q, want %q", s, expected)
	}
}
