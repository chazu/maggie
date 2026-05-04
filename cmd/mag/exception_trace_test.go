package main

import (
	"strings"
	"testing"

	"github.com/chazu/maggie/vm"
)

// installMethod compiles a single method source on the given class.
func installMethod(t *testing.T, vmInst *vm.VM, cls *vm.Class, source string) {
	t.Helper()
	cm, err := vmInst.Compile(source, cls)
	if err != nil {
		t.Fatalf("compile %q: %v", source, err)
	}
	if cm == nil {
		t.Fatalf("compile returned nil for: %q", source)
	}
	cm.SetClass(cls)
	selectorID := vmInst.Selectors.Intern(cm.Name())
	cls.VTable.AddMethod(selectorID, cm)
}

// TestExceptionStackTraceFromDeepCallChain is the end-to-end integration test
// for review item 4e: a Maggie script signals from inside a multi-frame call
// chain, catches the exception at the top, calls `stackTrace`, and we assert
// the captured frames mention the signaling chain — proving the trace was
// captured at signal time, not at handler time.
func TestExceptionStackTraceFromDeepCallChain(t *testing.T) {
	vmInst := newTestVM(t)
	objClass := vmInst.LookupClass("Object")
	if objClass == nil {
		t.Fatal("Object class not found")
	}

	installMethod(t, vmInst, objClass, "level3\n    ^Error signal: 'deep boom'")
	installMethod(t, vmInst, objClass, "level2\n    ^self level3")
	installMethod(t, vmInst, objClass, "level1\n    ^self level2")
	installMethod(t, vmInst, objClass, `traceTest
    | trace |
    [self level1] on: Error do: [:ex | trace := ex stackTrace].
    ^trace`)

	cm := lookupInstanceMethod(vmInst, "Object", "traceTest")
	if cm == nil {
		t.Fatal("traceTest method not installed")
	}

	result, err := vmInst.ExecuteSafe(cm, vm.Nil, nil)
	if err != nil {
		t.Fatalf("execute: %v", err)
	}
	if !vm.IsStringValue(result) {
		t.Fatalf("expected stackTrace to return a string, got %v", result)
	}
	trace := vmInst.Registry().GetStringContent(result)
	if trace == "" {
		t.Fatal("stack trace was empty")
	}
	t.Logf("captured stack trace:\n%s", trace)

	// We don't pin the exact format (line numbers depend on bytecode emission)
	// but every level should appear by selector name in the captured trace.
	for _, want := range []string{"level1", "level2", "level3"} {
		if !strings.Contains(trace, want) {
			t.Errorf("expected trace to mention %q, got:\n%s", want, trace)
		}
	}
}

// TestExceptionStackTraceUnhandledIncludesTrace verifies that an unhandled
// exception surfaced via ExecuteSafe includes the captured trace in the
// returned error message.
func TestExceptionStackTraceUnhandledIncludesTrace(t *testing.T) {
	vmInst := newTestVM(t)
	objClass := vmInst.LookupClass("Object")
	if objClass == nil {
		t.Fatal("Object class not found")
	}
	installMethod(t, vmInst, objClass, "innerSignal\n    ^Error signal: 'unhandled boom'")
	installMethod(t, vmInst, objClass, "outerCall\n    ^self innerSignal")

	cm := lookupInstanceMethod(vmInst, "Object", "outerCall")
	if cm == nil {
		t.Fatal("outerCall method not installed")
	}

	_, err := vmInst.ExecuteSafe(cm, vm.Nil, nil)
	if err == nil {
		t.Fatal("expected unhandled exception error, got nil")
	}
	msg := err.Error()
	if !strings.Contains(msg, "unhandled boom") {
		t.Errorf("expected message text in error, got: %s", msg)
	}
	for _, want := range []string{"innerSignal", "outerCall"} {
		if !strings.Contains(msg, want) {
			t.Errorf("expected unhandled-exception output to include %q, got:\n%s", want, msg)
		}
	}
}

