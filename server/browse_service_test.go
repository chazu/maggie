package server

import (
	"testing"

	"connectrpc.com/connect"

	maggiev1 "github.com/chazu/maggie/gen/maggie/v1"
)

// ---------------------------------------------------------------------------
// ListClasses
// ---------------------------------------------------------------------------

func TestListClasses_ReturnsClasses(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.ListClasses(bg(), connectReq(&maggiev1.ListClassesRequest{}))
	if err != nil {
		t.Fatalf("ListClasses returned error: %v", err)
	}
	if len(resp.Msg.Classes) == 0 {
		t.Fatal("ListClasses returned no classes; expected at least the core classes")
	}

	// Verify well-known core classes are present
	found := make(map[string]bool)
	for _, cls := range resp.Msg.Classes {
		found[cls.Name] = true
	}
	for _, name := range []string{"Object", "SmallInteger", "String", "Array", "Boolean", "Block"} {
		if !found[name] {
			t.Errorf("expected core class %q in ListClasses result", name)
		}
	}
}

func TestListClasses_FilterByCategory(t *testing.T) {
	svc := newTestBrowseService()

	// Filter by a category that almost certainly has no matches
	resp, err := svc.ListClasses(bg(), connectReq(&maggiev1.ListClassesRequest{
		Category: "NonExistentNamespace9999",
	}))
	if err != nil {
		t.Fatalf("ListClasses with category returned error: %v", err)
	}
	if len(resp.Msg.Classes) != 0 {
		t.Errorf("expected 0 classes for bogus category, got %d", len(resp.Msg.Classes))
	}
}

func TestListClasses_ResultsAreSorted(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.ListClasses(bg(), connectReq(&maggiev1.ListClassesRequest{}))
	if err != nil {
		t.Fatalf("ListClasses returned error: %v", err)
	}
	for i := 1; i < len(resp.Msg.Classes); i++ {
		if resp.Msg.Classes[i].Name < resp.Msg.Classes[i-1].Name {
			t.Fatalf("ListClasses results are not sorted: %q comes after %q",
				resp.Msg.Classes[i].Name, resp.Msg.Classes[i-1].Name)
		}
	}
}

// ---------------------------------------------------------------------------
// GetClass
// ---------------------------------------------------------------------------

func TestGetClass_ValidClass(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.GetClass(bg(), connectReq(&maggiev1.GetClassRequest{Name: "SmallInteger"}))
	if err != nil {
		t.Fatalf("GetClass returned error: %v", err)
	}
	if resp.Msg.Class == nil {
		t.Fatal("GetClass returned nil Class")
	}
	if resp.Msg.Class.Name != "SmallInteger" {
		t.Errorf("GetClass name = %q, want %q", resp.Msg.Class.Name, "SmallInteger")
	}
	// SmallInteger should have a superclass
	if resp.Msg.Class.SuperclassName == "" {
		t.Error("SmallInteger should have a superclass name")
	}
}

func TestGetClass_NotFound(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetClass(bg(), connectReq(&maggiev1.GetClassRequest{Name: "NoSuchClass12345"}))
	if err == nil {
		t.Fatal("GetClass should return error for unknown class")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

func TestGetClass_EmptyName(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetClass(bg(), connectReq(&maggiev1.GetClassRequest{Name: ""}))
	if err == nil {
		t.Fatal("GetClass with empty name should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// GetHierarchy
// ---------------------------------------------------------------------------

func TestGetHierarchy_ValidClass(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.GetHierarchy(bg(), connectReq(&maggiev1.GetHierarchyRequest{ClassName: "True"}))
	if err != nil {
		t.Fatalf("GetHierarchy returned error: %v", err)
	}

	// True inherits Boolean -> Object, so superclasses should include at least Object and Boolean
	if len(resp.Msg.Superclasses) < 1 {
		t.Error("expected at least 1 superclass for True")
	}
	if resp.Msg.Self == nil {
		t.Fatal("Self entry should not be nil")
	}
	if resp.Msg.Self.Name != "True" {
		t.Errorf("Self.Name = %q, want %q", resp.Msg.Self.Name, "True")
	}
}

func TestGetHierarchy_NotFound(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetHierarchy(bg(), connectReq(&maggiev1.GetHierarchyRequest{ClassName: "NoSuchClass"}))
	if err == nil {
		t.Fatal("GetHierarchy should error for unknown class")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

func TestGetHierarchy_EmptyClassName(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetHierarchy(bg(), connectReq(&maggiev1.GetHierarchyRequest{ClassName: ""}))
	if err == nil {
		t.Fatal("GetHierarchy with empty class_name should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestGetHierarchy_ObjectHasNoSuperclasses(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.GetHierarchy(bg(), connectReq(&maggiev1.GetHierarchyRequest{ClassName: "Object"}))
	if err != nil {
		t.Fatalf("GetHierarchy returned error: %v", err)
	}
	if len(resp.Msg.Superclasses) != 0 {
		t.Errorf("Object should have 0 superclasses, got %d", len(resp.Msg.Superclasses))
	}
	if resp.Msg.Self.Name != "Object" {
		t.Errorf("Self.Name = %q, want %q", resp.Msg.Self.Name, "Object")
	}
	// Object should have subclasses (basically everything inherits from it)
	if len(resp.Msg.Subclasses) == 0 {
		t.Error("Object should have at least one subclass")
	}
}

// ---------------------------------------------------------------------------
// ListMethods
// ---------------------------------------------------------------------------

func TestListMethods_InstanceSide(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.ListMethods(bg(), connectReq(&maggiev1.ListMethodsRequest{
		ClassName: "SmallInteger",
		Side:      maggiev1.ListMethodsRequest_INSTANCE,
	}))
	if err != nil {
		t.Fatalf("ListMethods returned error: %v", err)
	}
	if len(resp.Msg.Methods) == 0 {
		t.Error("SmallInteger should have instance methods")
	}

	// Verify methods are sorted by selector
	for i := 1; i < len(resp.Msg.Methods); i++ {
		if resp.Msg.Methods[i].Selector < resp.Msg.Methods[i-1].Selector {
			t.Fatalf("ListMethods results are not sorted: %q comes after %q",
				resp.Msg.Methods[i].Selector, resp.Msg.Methods[i-1].Selector)
		}
	}
}

func TestListMethods_BothSides(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.ListMethods(bg(), connectReq(&maggiev1.ListMethodsRequest{
		ClassName: "Object",
		Side:      maggiev1.ListMethodsRequest_BOTH,
	}))
	if err != nil {
		t.Fatalf("ListMethods returned error: %v", err)
	}
	if len(resp.Msg.Methods) == 0 {
		t.Error("Object should have methods on at least one side")
	}
}

func TestListMethods_NotFound(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.ListMethods(bg(), connectReq(&maggiev1.ListMethodsRequest{
		ClassName: "NoSuchClass",
		Side:      maggiev1.ListMethodsRequest_INSTANCE,
	}))
	if err == nil {
		t.Fatal("ListMethods should error for unknown class")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

func TestListMethods_EmptyClassName(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.ListMethods(bg(), connectReq(&maggiev1.ListMethodsRequest{
		ClassName: "",
		Side:      maggiev1.ListMethodsRequest_INSTANCE,
	}))
	if err == nil {
		t.Fatal("ListMethods with empty class_name should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// GetMethod
// ---------------------------------------------------------------------------

func TestGetMethod_ValidMethod(t *testing.T) {
	svc := newTestBrowseService()

	// "isNil" is a known method on Object
	resp, err := svc.GetMethod(bg(), connectReq(&maggiev1.GetMethodRequest{
		ClassName: "Object",
		Selector:  "isNil",
	}))
	if err != nil {
		t.Fatalf("GetMethod returned error: %v", err)
	}
	if resp.Msg.Method == nil {
		t.Fatal("GetMethod returned nil Method")
	}
	if resp.Msg.Method.Selector != "isNil" {
		t.Errorf("Method.Selector = %q, want %q", resp.Msg.Method.Selector, "isNil")
	}
}

func TestGetMethod_ClassNotFound(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetMethod(bg(), connectReq(&maggiev1.GetMethodRequest{
		ClassName: "NoSuchClass",
		Selector:  "foo",
	}))
	if err == nil {
		t.Fatal("GetMethod should error for unknown class")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

func TestGetMethod_SelectorNotFound(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetMethod(bg(), connectReq(&maggiev1.GetMethodRequest{
		ClassName: "Object",
		Selector:  "noSuchMethod12345",
	}))
	if err == nil {
		t.Fatal("GetMethod should error for unknown selector")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeNotFound {
			t.Errorf("expected CodeNotFound, got %v", connectErr.Code())
		}
	}
}

func TestGetMethod_EmptyArguments(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.GetMethod(bg(), connectReq(&maggiev1.GetMethodRequest{
		ClassName: "",
		Selector:  "",
	}))
	if err == nil {
		t.Fatal("GetMethod with empty class_name and selector should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// FindImplementors
// ---------------------------------------------------------------------------

func TestFindImplementors_KnownSelector(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.FindImplementors(bg(), connectReq(&maggiev1.FindImplementorsRequest{
		Selector: "isNil",
	}))
	if err != nil {
		t.Fatalf("FindImplementors returned error: %v", err)
	}
	if len(resp.Msg.Implementors) == 0 {
		t.Error("isNil should have at least one implementor")
	}
}

func TestFindImplementors_UnknownSelector(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.FindImplementors(bg(), connectReq(&maggiev1.FindImplementorsRequest{
		Selector: "xyzNonExistent99",
	}))
	if err != nil {
		t.Fatalf("FindImplementors returned error: %v", err)
	}
	if len(resp.Msg.Implementors) != 0 {
		t.Errorf("expected 0 implementors for bogus selector, got %d", len(resp.Msg.Implementors))
	}
}

func TestFindImplementors_EmptySelector(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.FindImplementors(bg(), connectReq(&maggiev1.FindImplementorsRequest{
		Selector: "",
	}))
	if err == nil {
		t.Fatal("FindImplementors with empty selector should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

// ---------------------------------------------------------------------------
// SearchSelectors
// ---------------------------------------------------------------------------

func TestSearchSelectors_FindsResults(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.SearchSelectors(bg(), connectReq(&maggiev1.SearchSelectorsRequest{
		Query: "isNil",
	}))
	if err != nil {
		t.Fatalf("SearchSelectors returned error: %v", err)
	}
	if len(resp.Msg.Selectors) == 0 {
		t.Error("SearchSelectors for 'isNil' should return results")
	}
	found := false
	for _, sel := range resp.Msg.Selectors {
		if sel == "isNil" {
			found = true
			break
		}
	}
	if !found {
		t.Error("SearchSelectors for 'isNil' should include 'isNil' in results")
	}
}

func TestSearchSelectors_CaseInsensitive(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.SearchSelectors(bg(), connectReq(&maggiev1.SearchSelectorsRequest{
		Query: "ISNIL",
	}))
	if err != nil {
		t.Fatalf("SearchSelectors returned error: %v", err)
	}
	// Should find isNil even with uppercase query
	if len(resp.Msg.Selectors) == 0 {
		t.Error("SearchSelectors should be case-insensitive")
	}
}

func TestSearchSelectors_EmptyQuery(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.SearchSelectors(bg(), connectReq(&maggiev1.SearchSelectorsRequest{
		Query: "",
	}))
	if err == nil {
		t.Fatal("SearchSelectors with empty query should return error")
	}
}

// ---------------------------------------------------------------------------
// SearchClasses
// ---------------------------------------------------------------------------

func TestSearchClasses_FindsResults(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.SearchClasses(bg(), connectReq(&maggiev1.SearchClassesRequest{
		Query: "Integer",
	}))
	if err != nil {
		t.Fatalf("SearchClasses returned error: %v", err)
	}
	if len(resp.Msg.Classes) == 0 {
		t.Error("SearchClasses for 'Integer' should return SmallInteger at minimum")
	}
}

func TestSearchClasses_EmptyQuery(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.SearchClasses(bg(), connectReq(&maggiev1.SearchClassesRequest{
		Query: "",
	}))
	if err == nil {
		t.Fatal("SearchClasses with empty query should return error")
	}
}

// ---------------------------------------------------------------------------
// FindSenders
// ---------------------------------------------------------------------------

func TestFindSenders_EmptySelector(t *testing.T) {
	svc := newTestBrowseService()

	_, err := svc.FindSenders(bg(), connectReq(&maggiev1.FindSendersRequest{
		Selector: "",
	}))
	if err == nil {
		t.Fatal("FindSenders with empty selector should return error")
	}
	var connectErr *connect.Error
	if ok := asConnectError(err, &connectErr); ok {
		if connectErr.Code() != connect.CodeInvalidArgument {
			t.Errorf("expected CodeInvalidArgument, got %v", connectErr.Code())
		}
	}
}

func TestFindSenders_UnknownSelector(t *testing.T) {
	svc := newTestBrowseService()

	resp, err := svc.FindSenders(bg(), connectReq(&maggiev1.FindSendersRequest{
		Selector: "xyzNonExistent99",
	}))
	if err != nil {
		t.Fatalf("FindSenders returned error: %v", err)
	}
	// No one sends a non-existent selector
	if len(resp.Msg.Senders) != 0 {
		t.Errorf("expected 0 senders for bogus selector, got %d", len(resp.Msg.Senders))
	}
}

// ---------------------------------------------------------------------------
// helper: unwrap connect error
// ---------------------------------------------------------------------------

func asConnectError(err error, target **connect.Error) bool {
	if ce, ok := err.(*connect.Error); ok {
		*target = ce
		return true
	}
	return false
}
