package server

import (
	"strings"
	"testing"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Inspect — happy paths
// ---------------------------------------------------------------------------

func TestInspect_SmallInteger(t *testing.T) {
	svc := newTestInspectService()

	// Create a handle for a SmallInteger
	handleID := testHandles.Create(vm.FromSmallInt(42), "SmallInteger", "42", "")

	resp, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("Inspect returned error: %v", err)
	}
	if resp.Msg.ClassName != "SmallInteger" {
		t.Errorf("ClassName = %q, want %q", resp.Msg.ClassName, "SmallInteger")
	}
	if resp.Msg.Handle == nil {
		t.Error("Inspect should return a handle")
	}
	if resp.Msg.Handle.Id != handleID {
		t.Errorf("Handle.Id = %q, want %q", resp.Msg.Handle.Id, handleID)
	}
}

func TestInspect_Nil(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.Nil, "UndefinedObject", "nil", "")

	resp, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("Inspect returned error: %v", err)
	}
	if resp.Msg.ClassName != "UndefinedObject" {
		t.Errorf("ClassName = %q, want %q", resp.Msg.ClassName, "UndefinedObject")
	}
}

func TestInspect_Boolean(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.True, "True", "true", "")

	resp, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("Inspect returned error: %v", err)
	}
	if resp.Msg.ClassName != "True" {
		t.Errorf("ClassName = %q, want %q", resp.Msg.ClassName, "True")
	}
}

// ---------------------------------------------------------------------------
// Inspect — error paths
// ---------------------------------------------------------------------------

func TestInspect_EmptyHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: "",
	}))
	if err == nil {
		t.Fatal("Inspect with empty handle_id should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestInspect_InvalidHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: "h-nonexistent",
	}))
	if err == nil {
		t.Fatal("Inspect with invalid handle_id should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// Inspect — array (indexable)
// ---------------------------------------------------------------------------

func TestInspect_Array(t *testing.T) {
	svc := newTestInspectService()

	// Create an array via the VM directly (avoids printString requirement)
	var handleID string
	_, err := testWorker.Do(func(v *vm.VM) interface{} {
		arr := v.NewArray(3)
		handleID = testHandles.Create(arr, "Array", "an Array", "")
		return nil
	})
	if err != nil {
		t.Fatalf("creating array returned error: %v", err)
	}

	resp, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("Inspect returned error: %v", err)
	}
	if resp.Msg.ClassName != "Array" {
		t.Errorf("ClassName = %q, want %q", resp.Msg.ClassName, "Array")
	}
	if !resp.Msg.IsIndexable {
		t.Error("Array should be indexable")
	}
	if resp.Msg.IndexableSize != 3 {
		t.Errorf("IndexableSize = %d, want 3", resp.Msg.IndexableSize)
	}
}

// ---------------------------------------------------------------------------
// InspectSlot
// ---------------------------------------------------------------------------

func TestInspectSlot_EmptyHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.InspectSlot(bg(), connectReq(&maggiev1.InspectSlotRequest{
		HandleId: "",
		SlotName: "foo",
	}))
	if err == nil {
		t.Fatal("InspectSlot with empty handle_id should return error")
	}
}

func TestInspectSlot_EmptySlotName(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(42), "SmallInteger", "42", "")

	_, err := svc.InspectSlot(bg(), connectReq(&maggiev1.InspectSlotRequest{
		HandleId: handleID,
		SlotName: "",
	}))
	if err == nil {
		t.Fatal("InspectSlot with empty slot_name should return error")
	}
}

func TestInspectSlot_InvalidHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.InspectSlot(bg(), connectReq(&maggiev1.InspectSlotRequest{
		HandleId: "h-nonexistent",
		SlotName: "foo",
	}))
	if err == nil {
		t.Fatal("InspectSlot with invalid handle_id should return error")
	}
}

func TestInspectSlot_IndexedSlot(t *testing.T) {
	svc := newTestInspectService()

	// Create an array with a known value via the VM directly
	var handleID string
	_, err := testWorker.Do(func(v *vm.VM) interface{} {
		arr := v.NewArrayWithElements([]vm.Value{vm.FromSmallInt(99)})
		handleID = testHandles.Create(arr, "Array", "an Array", "")
		return nil
	})
	if err != nil {
		t.Fatalf("creating array returned error: %v", err)
	}

	// Drill into [0]
	resp, err := svc.InspectSlot(bg(), connectReq(&maggiev1.InspectSlotRequest{
		HandleId: handleID,
		SlotName: "[0]",
	}))
	if err != nil {
		t.Fatalf("InspectSlot returned error: %v", err)
	}
	if resp.Msg.ClassName != "SmallInteger" {
		t.Errorf("ClassName = %q, want %q", resp.Msg.ClassName, "SmallInteger")
	}
	if resp.Msg.DisplayString != "99" {
		t.Errorf("DisplayString = %q, want %q", resp.Msg.DisplayString, "99")
	}
}

// ---------------------------------------------------------------------------
// InspectIndex
// ---------------------------------------------------------------------------

func TestInspectIndex_EmptyHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.InspectIndex(bg(), connectReq(&maggiev1.InspectIndexRequest{
		HandleId: "",
		Index:    0,
	}))
	if err == nil {
		t.Fatal("InspectIndex with empty handle_id should return error")
	}
}

func TestInspectIndex_InvalidHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.InspectIndex(bg(), connectReq(&maggiev1.InspectIndexRequest{
		HandleId: "h-nonexistent",
		Index:    0,
	}))
	if err == nil {
		t.Fatal("InspectIndex with invalid handle_id should return error")
	}
}

func TestInspectIndex_ValidArray(t *testing.T) {
	svc := newTestInspectService()

	// Create an array with a known value via the VM directly
	var handleID string
	_, err := testWorker.Do(func(v *vm.VM) interface{} {
		arr := v.NewArrayWithElements([]vm.Value{vm.FromSmallInt(7)})
		handleID = testHandles.Create(arr, "Array", "an Array", "")
		return nil
	})
	if err != nil {
		t.Fatalf("creating array returned error: %v", err)
	}

	resp, err := svc.InspectIndex(bg(), connectReq(&maggiev1.InspectIndexRequest{
		HandleId: handleID,
		Index:    0,
	}))
	if err != nil {
		t.Fatalf("InspectIndex returned error: %v", err)
	}
	if resp.Msg.ClassName != "SmallInteger" {
		t.Errorf("ClassName = %q, want %q", resp.Msg.ClassName, "SmallInteger")
	}
}

// ---------------------------------------------------------------------------
// SendMessage
// ---------------------------------------------------------------------------

func TestSendMessage_SimpleUnary(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(42), "SmallInteger", "42", "")

	resp, err := svc.SendMessage(bg(), connectReq(&maggiev1.SendMessageRequest{
		HandleId: handleID,
		Selector: "isNil",
	}))
	if err != nil {
		t.Fatalf("SendMessage returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("SendMessage was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "false" {
		t.Errorf("Result = %q, want %q", resp.Msg.Result, "false")
	}
}

func TestSendMessage_EmptyHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.SendMessage(bg(), connectReq(&maggiev1.SendMessageRequest{
		HandleId: "",
		Selector: "isNil",
	}))
	if err == nil {
		t.Fatal("SendMessage with empty handle_id should return error")
	}
}

func TestSendMessage_EmptySelector(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(42), "SmallInteger", "42", "")

	_, err := svc.SendMessage(bg(), connectReq(&maggiev1.SendMessageRequest{
		HandleId: handleID,
		Selector: "",
	}))
	if err == nil {
		t.Fatal("SendMessage with empty selector should return error")
	}
}

func TestSendMessage_InvalidHandle(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.SendMessage(bg(), connectReq(&maggiev1.SendMessageRequest{
		HandleId: "h-nonexistent",
		Selector: "isNil",
	}))
	if err == nil {
		t.Fatal("SendMessage with invalid handle_id should return error")
	}
}

// ---------------------------------------------------------------------------
// ReleaseHandle
// ---------------------------------------------------------------------------

func TestReleaseHandle_Valid(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(99), "SmallInteger", "99", "")

	resp, err := svc.ReleaseHandle(bg(), connectReq(&maggiev1.ReleaseHandleRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("ReleaseHandle returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Error("ReleaseHandle should succeed")
	}

	// After release, the handle should not be found
	_, found := testHandles.Lookup(handleID)
	if found {
		t.Error("handle should be gone after ReleaseHandle")
	}
}

func TestReleaseHandle_EmptyHandleId(t *testing.T) {
	svc := newTestInspectService()

	_, err := svc.ReleaseHandle(bg(), connectReq(&maggiev1.ReleaseHandleRequest{
		HandleId: "",
	}))
	if err == nil {
		t.Fatal("ReleaseHandle with empty handle_id should return error")
	}
}

func TestReleaseHandle_Idempotent(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(1), "SmallInteger", "1", "")

	// Release once
	_, err := svc.ReleaseHandle(bg(), connectReq(&maggiev1.ReleaseHandleRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("first ReleaseHandle returned error: %v", err)
	}

	// Release again — should succeed (no-op)
	resp, err := svc.ReleaseHandle(bg(), connectReq(&maggiev1.ReleaseHandleRequest{
		HandleId: handleID,
	}))
	if err != nil {
		t.Fatalf("second ReleaseHandle returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Error("second ReleaseHandle should still succeed")
	}
}

// ---------------------------------------------------------------------------
// Inspect — class values
// ---------------------------------------------------------------------------

func TestInspect_ClassValue(t *testing.T) {
	svc := newTestInspectService()
	evalSvc := newTestEvalService()

	// Evaluate to get a class value
	evalResp, err := evalSvc.Evaluate(bg(), connectReq(&maggiev1.EvaluateRequest{
		Source: "SmallInteger",
	}))
	if err != nil {
		t.Fatalf("Evaluate returned error: %v", err)
	}
	if !evalResp.Msg.Success {
		t.Fatalf("Evaluate was not successful: %s", evalResp.Msg.ErrorMessage)
	}

	resp, err := svc.Inspect(bg(), connectReq(&maggiev1.InspectRequest{
		HandleId: evalResp.Msg.Handle.Id,
	}))
	if err != nil {
		t.Fatalf("Inspect returned error: %v", err)
	}
	// Class value inspection should include special slots
	if len(resp.Msg.Slots) == 0 {
		t.Error("inspecting a class should produce slots (superclass, instanceVariables, methodCount)")
	}

	// Check that we get at least a superclass slot
	foundSuperclass := false
	for _, slot := range resp.Msg.Slots {
		if slot.Name == "superclass" {
			foundSuperclass = true
			if slot.ValueDisplay == "" {
				t.Error("superclass slot should have a display value")
			}
		}
	}
	if !foundSuperclass {
		t.Error("class inspection should include a 'superclass' slot")
	}

	// Check methodCount slot
	foundMethodCount := false
	for _, slot := range resp.Msg.Slots {
		if slot.Name == "methodCount" {
			foundMethodCount = true
			break
		}
	}
	if !foundMethodCount {
		t.Error("class inspection should include a 'methodCount' slot")
	}
}

// ---------------------------------------------------------------------------
// SendMessage with arguments
// ---------------------------------------------------------------------------

func TestSendMessage_WithArguments(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(10), "SmallInteger", "10", "")

	resp, err := svc.SendMessage(bg(), connectReq(&maggiev1.SendMessageRequest{
		HandleId:  handleID,
		Selector:  "+",
		Arguments: []string{"5"},
	}))
	if err != nil {
		t.Fatalf("SendMessage returned error: %v", err)
	}
	if !resp.Msg.Success {
		t.Fatalf("SendMessage was not successful: %s", resp.Msg.ErrorMessage)
	}
	if resp.Msg.Result != "15" {
		t.Errorf("Result = %q, want %q", resp.Msg.Result, "15")
	}
	if resp.Msg.ResultHandle == nil {
		t.Error("SendMessage should return a result handle")
	}
}

func TestSendMessage_BadArgumentSyntax(t *testing.T) {
	svc := newTestInspectService()

	handleID := testHandles.Create(vm.FromSmallInt(10), "SmallInteger", "10", "")

	resp, err := svc.SendMessage(bg(), connectReq(&maggiev1.SendMessageRequest{
		HandleId:  handleID,
		Selector:  "+",
		Arguments: []string{"[[["},
	}))
	if err != nil {
		t.Fatalf("SendMessage returned transport error: %v", err)
	}
	if resp.Msg.Success {
		t.Error("SendMessage should fail when argument has bad syntax")
	}
	if resp.Msg.ErrorMessage == "" {
		t.Error("ErrorMessage should describe the compile error")
	}
	if !strings.Contains(resp.Msg.ErrorMessage, "compile") && !strings.Contains(resp.Msg.ErrorMessage, "Compile") {
		// Just make sure it mentions compilation, but don't be too strict
		t.Logf("note: ErrorMessage = %q", resp.Msg.ErrorMessage)
	}
}
