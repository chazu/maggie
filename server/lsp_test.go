package server

import (
	"testing"

	protocol "github.com/tliron/glsp/protocol_3_16"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// LSP text extraction helpers
// ---------------------------------------------------------------------------

func TestExtractPrefix_SimpleWord(t *testing.T) {
	text := "Object new"
	pos := protocol.Position{Line: 0, Character: 10}
	prefix := extractPrefix(text, pos)
	if prefix != "new" {
		t.Errorf("extractPrefix = %q, want %q", prefix, "new")
	}
}

func TestExtractPrefix_AtStart(t *testing.T) {
	text := "Obj"
	pos := protocol.Position{Line: 0, Character: 3}
	prefix := extractPrefix(text, pos)
	if prefix != "Obj" {
		t.Errorf("extractPrefix = %q, want %q", prefix, "Obj")
	}
}

func TestExtractPrefix_EmptyLine(t *testing.T) {
	text := ""
	pos := protocol.Position{Line: 0, Character: 0}
	prefix := extractPrefix(text, pos)
	if prefix != "" {
		t.Errorf("extractPrefix = %q, want empty string", prefix)
	}
}

func TestExtractPrefix_MultiLine(t *testing.T) {
	text := "first line\nsecond line\nObj"
	pos := protocol.Position{Line: 2, Character: 3}
	prefix := extractPrefix(text, pos)
	if prefix != "Obj" {
		t.Errorf("extractPrefix = %q, want %q", prefix, "Obj")
	}
}

func TestExtractPrefix_AfterSpace(t *testing.T) {
	text := "x := SmallInt"
	pos := protocol.Position{Line: 0, Character: 13}
	prefix := extractPrefix(text, pos)
	if prefix != "SmallInt" {
		t.Errorf("extractPrefix = %q, want %q", prefix, "SmallInt")
	}
}

func TestExtractPrefix_WithKeywordColon(t *testing.T) {
	text := "obj at:put:"
	pos := protocol.Position{Line: 0, Character: 11}
	prefix := extractPrefix(text, pos)
	if prefix != "at:put:" {
		t.Errorf("extractPrefix = %q, want %q", prefix, "at:put:")
	}
}

func TestExtractPrefix_CursorAtBeginning(t *testing.T) {
	text := "hello"
	pos := protocol.Position{Line: 0, Character: 0}
	prefix := extractPrefix(text, pos)
	if prefix != "" {
		t.Errorf("extractPrefix at position 0 = %q, want empty string", prefix)
	}
}

func TestExtractPrefix_LineBeyondDocument(t *testing.T) {
	text := "single line"
	pos := protocol.Position{Line: 5, Character: 0}
	prefix := extractPrefix(text, pos)
	if prefix != "" {
		t.Errorf("extractPrefix beyond doc = %q, want empty string", prefix)
	}
}

// ---------------------------------------------------------------------------
// extractWord
// ---------------------------------------------------------------------------

func TestExtractWord_SimpleWord(t *testing.T) {
	text := "hello world"
	pos := protocol.Position{Line: 0, Character: 3}
	word := extractWord(text, pos)
	if word != "hello" {
		t.Errorf("extractWord = %q, want %q", word, "hello")
	}
}

func TestExtractWord_AtEnd(t *testing.T) {
	text := "hello world"
	pos := protocol.Position{Line: 0, Character: 5}
	word := extractWord(text, pos)
	if word != "hello" {
		t.Errorf("extractWord = %q, want %q", word, "hello")
	}
}

func TestExtractWord_AtSpace(t *testing.T) {
	text := "hello world"
	// Position at the space between words
	pos := protocol.Position{Line: 0, Character: 5}
	word := extractWord(text, pos)
	// Cursor at end of "hello" (char 5 is the space), so it should find "hello"
	// because start walks back from col=5, and line[4]='o' is a letter
	if word != "hello" {
		t.Errorf("extractWord at space = %q, want %q", word, "hello")
	}
}

func TestExtractWord_SecondWord(t *testing.T) {
	text := "hello world"
	pos := protocol.Position{Line: 0, Character: 8}
	word := extractWord(text, pos)
	if word != "world" {
		t.Errorf("extractWord = %q, want %q", word, "world")
	}
}

func TestExtractWord_EmptyLine(t *testing.T) {
	text := ""
	pos := protocol.Position{Line: 0, Character: 0}
	word := extractWord(text, pos)
	if word != "" {
		t.Errorf("extractWord = %q, want empty string", word)
	}
}

func TestExtractWord_MultiLine(t *testing.T) {
	text := "first\nObject"
	pos := protocol.Position{Line: 1, Character: 3}
	word := extractWord(text, pos)
	if word != "Object" {
		t.Errorf("extractWord = %q, want %q", word, "Object")
	}
}

func TestExtractWord_WithUnderscore(t *testing.T) {
	text := "my_var"
	pos := protocol.Position{Line: 0, Character: 3}
	word := extractWord(text, pos)
	if word != "my_var" {
		t.Errorf("extractWord = %q, want %q", word, "my_var")
	}
}

func TestExtractWord_LineBeyondDocument(t *testing.T) {
	text := "single line"
	pos := protocol.Position{Line: 5, Character: 0}
	word := extractWord(text, pos)
	if word != "" {
		t.Errorf("extractWord beyond doc = %q, want empty string", word)
	}
}

// ---------------------------------------------------------------------------
// extractWord with :: namespace separators
// ---------------------------------------------------------------------------

func TestExtractWord_FQN_CursorOnBare(t *testing.T) {
	text := "Widgets::Button new"
	// Cursor on "Button" (col 12)
	pos := protocol.Position{Line: 0, Character: 12}
	word := extractWord(text, pos)
	if word != "Widgets::Button" {
		t.Errorf("extractWord = %q, want %q", word, "Widgets::Button")
	}
}

func TestExtractWord_FQN_CursorOnNamespace(t *testing.T) {
	text := "Widgets::Button new"
	// Cursor on "Widgets" (col 3)
	pos := protocol.Position{Line: 0, Character: 3}
	word := extractWord(text, pos)
	if word != "Widgets::Button" {
		t.Errorf("extractWord = %q, want %q", word, "Widgets::Button")
	}
}

func TestExtractWord_FQN_DeepNamespace(t *testing.T) {
	text := "Yutani::Widgets::Button new"
	pos := protocol.Position{Line: 0, Character: 20}
	word := extractWord(text, pos)
	if word != "Yutani::Widgets::Button" {
		t.Errorf("extractWord = %q, want %q", word, "Yutani::Widgets::Button")
	}
}

func TestExtractWord_SingleColonNotFQN(t *testing.T) {
	text := "obj at: value"
	// Cursor on "at" (col 5)
	pos := protocol.Position{Line: 0, Character: 5}
	word := extractWord(text, pos)
	// Single colon should NOT be treated as namespace separator
	if word != "at" {
		t.Errorf("extractWord = %q, want %q", word, "at")
	}
}

// ---------------------------------------------------------------------------
// Document symbols
// ---------------------------------------------------------------------------

func TestLSP_DocumentSymbol(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	source := `namespace: 'MyApp'

MyWidget subclass: Object
  method: render [ ^self ]
  classMethod: new [ ^super new ]
`
	lsp.mu.Lock()
	lsp.docs["file:///test.mag"] = source
	lsp.mu.Unlock()

	result, err := lsp.textDocumentDocumentSymbol(nil, &protocol.DocumentSymbolParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: "file:///test.mag"},
	})
	if err != nil {
		t.Fatalf("documentSymbol returned error: %v", err)
	}
	symbols, ok := result.([]protocol.DocumentSymbol)
	if !ok {
		t.Fatalf("documentSymbol result type = %T, want []protocol.DocumentSymbol", result)
	}

	// Should have namespace + class = 2 top-level symbols
	if len(symbols) < 2 {
		t.Fatalf("documentSymbol returned %d symbols, want at least 2", len(symbols))
	}

	// First should be namespace
	if symbols[0].Kind != protocol.SymbolKindNamespace {
		t.Errorf("first symbol kind = %v, want Namespace", symbols[0].Kind)
	}

	// Second should be class with children (methods)
	classSym := symbols[1]
	if classSym.Kind != protocol.SymbolKindClass {
		t.Errorf("class symbol kind = %v, want Class", classSym.Kind)
	}
	if classSym.Name != "MyWidget" {
		t.Errorf("class symbol name = %q, want %q", classSym.Name, "MyWidget")
	}
	if len(classSym.Children) != 2 {
		t.Errorf("class children count = %d, want 2 (instance + class method)", len(classSym.Children))
	}
}

// ---------------------------------------------------------------------------
// boolPtr
// ---------------------------------------------------------------------------

func TestBoolPtr(t *testing.T) {
	p := boolPtr(true)
	if p == nil {
		t.Fatal("boolPtr should not return nil")
	}
	if *p != true {
		t.Errorf("boolPtr(true) = %v, want true", *p)
	}

	p = boolPtr(false)
	if *p != false {
		t.Errorf("boolPtr(false) = %v, want false", *p)
	}
}

// ---------------------------------------------------------------------------
// LSP VM-backed logic (complete, hover, definition, references)
// These test the internal methods directly using the shared worker.
// ---------------------------------------------------------------------------

func TestLSP_Complete(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.complete(v, "Obj")
	})
	if err != nil {
		t.Fatalf("complete returned error: %v", err)
	}
	items := result.([]protocol.CompletionItem)
	if len(items) == 0 {
		t.Error("complete for 'Obj' should return at least Object")
	}
	found := false
	for _, item := range items {
		if item.Label == "Object" {
			found = true
			if item.Kind == nil || *item.Kind != protocol.CompletionItemKindClass {
				t.Error("Object completion should have Kind=Class")
			}
			break
		}
	}
	if !found {
		t.Error("complete for 'Obj' should include 'Object'")
	}
}

func TestLSP_Hover_ClassName(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.hover(v, "SmallInteger")
	})
	if err != nil {
		t.Fatalf("hover returned error: %v", err)
	}
	if result == nil {
		t.Fatal("hover for 'SmallInteger' should return a result")
	}
	hover := result.(*protocol.Hover)
	mc, ok := hover.Contents.(protocol.MarkupContent)
	if !ok {
		t.Fatal("hover contents should be MarkupContent")
	}
	if mc.Kind != protocol.MarkupKindMarkdown {
		t.Errorf("hover markup kind = %q, want %q", mc.Kind, protocol.MarkupKindMarkdown)
	}
	if mc.Value == "" {
		t.Error("hover content should not be empty")
	}
}

func TestLSP_Hover_UnknownWord(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.hover(v, "XYZNOSUCHCLASSTHING99")
	})
	if err != nil {
		t.Fatalf("hover returned error: %v", err)
	}
	// hover returns *protocol.Hover which may be typed-nil wrapped in interface{}
	if hover, ok := result.(*protocol.Hover); ok && hover != nil {
		t.Error("hover for unknown uppercase word should return nil Hover")
	}
}

func TestLSP_Hover_SelectorLookup(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.hover(v, "isNil")
	})
	if err != nil {
		t.Fatalf("hover returned error: %v", err)
	}
	if result == nil {
		t.Fatal("hover for 'isNil' should return a result")
	}
	hover := result.(*protocol.Hover)
	mc, ok := hover.Contents.(protocol.MarkupContent)
	if !ok {
		t.Fatal("hover contents should be MarkupContent")
	}
	if mc.Value == "" {
		t.Error("hover content for selector should not be empty")
	}
}

func TestLSP_Definition_ClassName(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.definition(v, "Object")
	})
	if err != nil {
		t.Fatalf("definition returned error: %v", err)
	}
	if result == nil {
		t.Fatal("definition for 'Object' should return a result")
	}
	locations := result.([]protocol.Location)
	if len(locations) == 0 {
		t.Error("definition for 'Object' should return at least one location")
	}
	if string(locations[0].URI) != "maggie://class/Object" {
		t.Errorf("definition URI = %q, want %q", locations[0].URI, "maggie://class/Object")
	}
}

func TestLSP_Definition_UnknownWord(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.definition(v, "NoSuchClass9999")
	})
	if err != nil {
		t.Fatalf("definition returned error: %v", err)
	}
	// definition returns []protocol.Location; a nil slice wraps to non-nil interface{}
	if locations, ok := result.([]protocol.Location); ok && len(locations) > 0 {
		t.Errorf("definition for unknown class should return empty locations, got %d", len(locations))
	}
}

func TestLSP_References_KnownSelector(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.references(v, "isNil")
	})
	if err != nil {
		t.Fatalf("references returned error: %v", err)
	}
	// isNil might or might not have senders in compiled methods; just check no error
	_ = result
}

func TestLSP_References_UnknownSelector(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	result, err := testWorker.Do(func(v *vm.VM) interface{} {
		return lsp.references(v, "xyzNonExistent99")
	})
	if err != nil {
		t.Fatalf("references returned error: %v", err)
	}
	// references returns []protocol.Location; a nil slice wraps to non-nil interface{}
	if locations, ok := result.([]protocol.Location); ok && len(locations) > 0 {
		t.Errorf("references for unknown selector should return empty, got %d", len(locations))
	}
}

// ---------------------------------------------------------------------------
// LSP document synchronization state
// ---------------------------------------------------------------------------

func TestLSP_DocumentStore(t *testing.T) {
	lsp := &LspServer{
		worker: testWorker,
		docs:   make(map[string]string),
	}

	// Simulate didOpen
	lsp.mu.Lock()
	lsp.docs["file:///test.mag"] = "Object new"
	lsp.mu.Unlock()

	// Verify the doc was stored
	lsp.mu.Lock()
	text, ok := lsp.docs["file:///test.mag"]
	lsp.mu.Unlock()
	if !ok {
		t.Error("document should be stored after open")
	}
	if text != "Object new" {
		t.Errorf("document text = %q, want %q", text, "Object new")
	}

	// Simulate didClose
	lsp.mu.Lock()
	delete(lsp.docs, "file:///test.mag")
	lsp.mu.Unlock()

	lsp.mu.Lock()
	_, ok = lsp.docs["file:///test.mag"]
	lsp.mu.Unlock()
	if ok {
		t.Error("document should be removed after close")
	}
}
