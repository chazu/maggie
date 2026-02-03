package server

import (
	"fmt"
	"sort"
	"strings"
	"sync"
	"unicode"

	"github.com/tliron/commonlog"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
	glspserver "github.com/tliron/glsp/server"

	"github.com/chazu/maggie/vm"

	_ "github.com/tliron/commonlog/simple"
)

const lspName = "maggie-lsp"

// LspServer bridges LSP editor features to the Maggie VM via VMWorker.
type LspServer struct {
	worker *VMWorker

	mu   sync.Mutex
	docs map[string]string // URI → full document content

	handler protocol.Handler
	server  *glspserver.Server
	version string
}

// NewLSP creates a new LSP server wrapping the given VM.
func NewLSP(v *vm.VM) *LspServer {
	worker := NewVMWorker(v)
	s := &LspServer{
		worker:  worker,
		docs:    make(map[string]string),
		version: "0.1.0",
	}

	s.handler = protocol.Handler{
		Initialize:  s.initialize,
		Initialized: s.initialized,
		Shutdown:    s.shutdown,
		SetTrace:    s.setTrace,

		TextDocumentDidOpen:   s.textDocumentDidOpen,
		TextDocumentDidChange: s.textDocumentDidChange,
		TextDocumentDidClose:  s.textDocumentDidClose,

		TextDocumentCompletion: s.textDocumentCompletion,
		TextDocumentHover:      s.textDocumentHover,
		TextDocumentDefinition: s.textDocumentDefinition,
		TextDocumentReferences: s.textDocumentReferences,
	}

	s.server = glspserver.NewServer(&s.handler, lspName, false)

	return s
}

// Run starts the LSP server on stdio. Blocks until the client disconnects.
func (s *LspServer) Run() error {
	return s.server.RunStdio()
}

// --- LSP lifecycle handlers ---

func (s *LspServer) initialize(ctx *glsp.Context, params *protocol.InitializeParams) (any, error) {
	commonlog.NewInfoMessage(0, "Maggie LSP initializing")

	capabilities := s.handler.CreateServerCapabilities()

	syncKind := protocol.TextDocumentSyncKindFull
	capabilities.TextDocumentSync = &protocol.TextDocumentSyncOptions{
		OpenClose: boolPtr(true),
		Change:    &syncKind,
	}

	capabilities.CompletionProvider = &protocol.CompletionOptions{
		TriggerCharacters: []string{".", ":"},
	}

	capabilities.HoverProvider = true
	capabilities.DefinitionProvider = true
	capabilities.ReferencesProvider = true

	return protocol.InitializeResult{
		Capabilities: capabilities,
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    lspName,
			Version: &s.version,
		},
	}, nil
}

func (s *LspServer) initialized(ctx *glsp.Context, params *protocol.InitializedParams) error {
	return nil
}

func (s *LspServer) shutdown(ctx *glsp.Context) error {
	s.worker.Stop()
	return nil
}

func (s *LspServer) setTrace(ctx *glsp.Context, params *protocol.SetTraceParams) error {
	return nil
}

// --- Document synchronization ---

func (s *LspServer) textDocumentDidOpen(ctx *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	uri := params.TextDocument.URI
	text := params.TextDocument.Text

	s.mu.Lock()
	s.docs[string(uri)] = text
	s.mu.Unlock()

	s.publishDiagnostics(ctx, uri, text)
	return nil
}

func (s *LspServer) textDocumentDidChange(ctx *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	uri := params.TextDocument.URI

	// With Full sync, the last change event contains the full text
	if len(params.ContentChanges) > 0 {
		last := params.ContentChanges[len(params.ContentChanges)-1]
		if whole, ok := last.(protocol.TextDocumentContentChangeEventWhole); ok {
			s.mu.Lock()
			s.docs[string(uri)] = whole.Text
			text := whole.Text
			s.mu.Unlock()

			s.publishDiagnostics(ctx, uri, text)
		}
	}
	return nil
}

func (s *LspServer) textDocumentDidClose(ctx *glsp.Context, params *protocol.DidCloseTextDocumentParams) error {
	uri := params.TextDocument.URI

	s.mu.Lock()
	delete(s.docs, string(uri))
	s.mu.Unlock()

	// Clear diagnostics for the closed document
	go ctx.Notify(protocol.ServerTextDocumentPublishDiagnostics, protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: []protocol.Diagnostic{},
	})
	return nil
}

// --- Language features ---

func (s *LspServer) textDocumentCompletion(ctx *glsp.Context, params *protocol.CompletionParams) (any, error) {
	uri := params.TextDocument.URI
	pos := params.Position

	s.mu.Lock()
	text, ok := s.docs[string(uri)]
	s.mu.Unlock()

	if !ok {
		return nil, nil
	}

	prefix := extractPrefix(text, pos)
	if prefix == "" {
		return nil, nil
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.complete(v, prefix)
	})
	if err != nil {
		return nil, err
	}

	return result, nil
}

func (s *LspServer) textDocumentHover(ctx *glsp.Context, params *protocol.HoverParams) (*protocol.Hover, error) {
	uri := params.TextDocument.URI
	pos := params.Position

	s.mu.Lock()
	text, ok := s.docs[string(uri)]
	s.mu.Unlock()

	if !ok {
		return nil, nil
	}

	word := extractWord(text, pos)
	if word == "" {
		return nil, nil
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.hover(v, word)
	})
	if err != nil {
		return nil, nil
	}
	if result == nil {
		return nil, nil
	}

	return result.(*protocol.Hover), nil
}

func (s *LspServer) textDocumentDefinition(ctx *glsp.Context, params *protocol.DefinitionParams) (any, error) {
	uri := params.TextDocument.URI
	pos := params.Position

	s.mu.Lock()
	text, ok := s.docs[string(uri)]
	s.mu.Unlock()

	if !ok {
		return nil, nil
	}

	word := extractWord(text, pos)
	if word == "" {
		return nil, nil
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.definition(v, word)
	})
	if err != nil || result == nil {
		return nil, nil
	}

	return result, nil
}

func (s *LspServer) textDocumentReferences(ctx *glsp.Context, params *protocol.ReferenceParams) ([]protocol.Location, error) {
	uri := params.TextDocument.URI
	pos := params.Position

	s.mu.Lock()
	text, ok := s.docs[string(uri)]
	s.mu.Unlock()

	if !ok {
		return nil, nil
	}

	word := extractWord(text, pos)
	if word == "" {
		return nil, nil
	}

	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		return s.references(v, word)
	})
	if err != nil || result == nil {
		return nil, nil
	}

	return result.([]protocol.Location), nil
}

// --- VM-backed logic (called on worker goroutine) ---

func (s *LspServer) complete(v *vm.VM, prefix string) []protocol.CompletionItem {
	var items []protocol.CompletionItem
	lowerPrefix := strings.ToLower(prefix)

	// Class names
	for _, cls := range v.Classes.All() {
		if strings.HasPrefix(strings.ToLower(cls.Name), lowerPrefix) {
			kind := protocol.CompletionItemKindClass
			detail := "class"
			if cls.Superclass != nil {
				detail = fmt.Sprintf("class (< %s)", cls.Superclass.Name)
			}
			items = append(items, protocol.CompletionItem{
				Label:      cls.Name,
				Kind:       &kind,
				Detail:     &detail,
				InsertText: &cls.Name,
			})
		}
	}

	// Global names
	for name := range v.Globals {
		if strings.HasPrefix(strings.ToLower(name), lowerPrefix) {
			if v.Classes.Lookup(name) != nil {
				continue // already added as class
			}
			kind := protocol.CompletionItemKindVariable
			detail := "global"
			nameCopy := name
			items = append(items, protocol.CompletionItem{
				Label:      name,
				Kind:       &kind,
				Detail:     &detail,
				InsertText: &nameCopy,
			})
		}
	}

	// Selectors
	for _, name := range v.Selectors.All() {
		if name != "" && strings.HasPrefix(strings.ToLower(name), lowerPrefix) {
			kind := protocol.CompletionItemKindFunction
			detail := "selector"
			nameCopy := name
			items = append(items, protocol.CompletionItem{
				Label:      name,
				Kind:       &kind,
				Detail:     &detail,
				InsertText: &nameCopy,
			})
		}
	}

	// Limit results
	const maxItems = 100
	if len(items) > maxItems {
		items = items[:maxItems]
	}

	return items
}

func (s *LspServer) hover(v *vm.VM, word string) *protocol.Hover {
	// Uppercase word → class lookup
	if len(word) > 0 && unicode.IsUpper(rune(word[0])) {
		cls := v.Classes.Lookup(word)
		if cls == nil {
			return nil
		}

		var b strings.Builder
		fmt.Fprintf(&b, "**%s**", cls.Name)
		if cls.Superclass != nil {
			fmt.Fprintf(&b, " < %s", cls.Superclass.Name)
		}
		b.WriteString("\n\n")

		if cls.DocString != "" {
			b.WriteString("---\n\n")
			b.WriteString(cls.DocString)
			b.WriteString("\n\n")
		}

		if len(cls.InstVars) > 0 {
			fmt.Fprintf(&b, "Instance variables: `%s`\n\n", strings.Join(cls.InstVars, " "))
		}

		instCount := countLocalMethods(cls.VTable)
		classCount := countLocalMethods(cls.ClassVTable)
		fmt.Fprintf(&b, "%d instance methods, %d class methods", instCount, classCount)

		// Show hierarchy
		supers := cls.Superclasses()
		if len(supers) > 0 {
			b.WriteString("\n\n**Hierarchy:** ")
			names := make([]string, len(supers))
			for i, sup := range supers {
				names[i] = sup.Name
			}
			b.WriteString(strings.Join(names, " → "))
			fmt.Fprintf(&b, " → **%s**", cls.Name)
		}

		return &protocol.Hover{
			Contents: protocol.MarkupContent{
				Kind:  protocol.MarkupKindMarkdown,
				Value: b.String(),
			},
		}
	}

	// Lowercase or keyword → selector lookup (find implementors)
	selector := word
	// Check if it's a known selector, also try with colon for keyword messages
	selID := v.Selectors.Lookup(selector)
	if selID < 0 {
		selector = word + ":"
		selID = v.Selectors.Lookup(selector)
	}
	if selID < 0 {
		return nil
	}

	var implementors []string
	for _, cls := range v.Classes.All() {
		if cls.VTable.LookupLocal(selID) != nil {
			implementors = append(implementors, cls.Name)
		}
		if cls.ClassVTable != nil && cls.ClassVTable.LookupLocal(selID) != nil {
			implementors = append(implementors, cls.Name+" class")
		}
	}
	sort.Strings(implementors)

	if len(implementors) == 0 {
		return nil
	}

	var b strings.Builder
	fmt.Fprintf(&b, "**#%s**\n\n", selector)
	fmt.Fprintf(&b, "Implemented by %d classes:\n", len(implementors))
	for _, name := range implementors {
		fmt.Fprintf(&b, "- %s\n", name)
	}

	// Show docstring from first implementor that has one
	for _, cls := range v.Classes.All() {
		method := cls.VTable.LookupLocal(selID)
		if method == nil && cls.ClassVTable != nil {
			method = cls.ClassVTable.LookupLocal(selID)
		}
		if method == nil {
			continue
		}
		doc := vm.MethodDocString(method)
		if doc != "" {
			fmt.Fprintf(&b, "\n---\n\n%s\n", doc)
			break
		}
	}

	return &protocol.Hover{
		Contents: protocol.MarkupContent{
			Kind:  protocol.MarkupKindMarkdown,
			Value: b.String(),
		},
	}
}

func (s *LspServer) definition(v *vm.VM, word string) []protocol.Location {
	// For class names, return a virtual URI
	if len(word) > 0 && unicode.IsUpper(rune(word[0])) {
		cls := v.Classes.Lookup(word)
		if cls == nil {
			return nil
		}
		return []protocol.Location{{
			URI: protocol.DocumentUri(fmt.Sprintf("maggie://class/%s", cls.Name)),
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: 0, Character: 0},
			},
		}}
	}

	// For selectors, find implementors
	selector := word
	selID := v.Selectors.Lookup(selector)
	if selID < 0 {
		selector = word + ":"
		selID = v.Selectors.Lookup(selector)
	}
	if selID < 0 {
		return nil
	}

	var locations []protocol.Location
	for _, cls := range v.Classes.All() {
		if cls.VTable.LookupLocal(selID) != nil {
			locations = append(locations, protocol.Location{
				URI: protocol.DocumentUri(fmt.Sprintf("maggie://class/%s/%s", cls.Name, selector)),
				Range: protocol.Range{
					Start: protocol.Position{Line: 0, Character: 0},
					End:   protocol.Position{Line: 0, Character: 0},
				},
			})
		}
		if cls.ClassVTable != nil && cls.ClassVTable.LookupLocal(selID) != nil {
			locations = append(locations, protocol.Location{
				URI: protocol.DocumentUri(fmt.Sprintf("maggie://class/%s class/%s", cls.Name, selector)),
				Range: protocol.Range{
					Start: protocol.Position{Line: 0, Character: 0},
					End:   protocol.Position{Line: 0, Character: 0},
				},
			})
		}
	}

	return locations
}

func (s *LspServer) references(v *vm.VM, word string) []protocol.Location {
	selector := word
	selID := v.Selectors.Lookup(selector)
	if selID < 0 {
		selector = word + ":"
		selID = v.Selectors.Lookup(selector)
	}
	if selID < 0 {
		return nil
	}

	// Map specialized opcodes
	specializedSends := map[string]vm.Opcode{
		"+": vm.OpSendPlus, "-": vm.OpSendMinus, "*": vm.OpSendTimes,
		"/": vm.OpSendDiv, "\\\\": vm.OpSendMod,
		"<": vm.OpSendLT, ">": vm.OpSendGT, "<=": vm.OpSendLE,
		">=": vm.OpSendGE, "=": vm.OpSendEQ, "~=": vm.OpSendNE,
		"at:": vm.OpSendAt, "at:put:": vm.OpSendAtPut,
		"size": vm.OpSendSize, "value": vm.OpSendValue,
		"value:": vm.OpSendValue1, "value:value:": vm.OpSendValue2,
		"new": vm.OpSendNew, "class": vm.OpSendClass,
	}

	targetSpecialized, isSpecialized := specializedSends[selector]

	var locations []protocol.Location
	for _, cls := range v.Classes.All() {
		for methodSelID, method := range cls.VTable.LocalMethods() {
			cm, ok := method.(*vm.CompiledMethod)
			if !ok {
				continue
			}
			if bytecodeSendsSelector(cm.Bytecode, selID, isSpecialized, targetSpecialized) {
				methodName := v.Selectors.Name(methodSelID)
				locations = append(locations, protocol.Location{
					URI: protocol.DocumentUri(fmt.Sprintf("maggie://class/%s/%s", cls.Name, methodName)),
					Range: protocol.Range{
						Start: protocol.Position{Line: 0, Character: 0},
						End:   protocol.Position{Line: 0, Character: 0},
					},
				})
			}
		}
		if cls.ClassVTable != nil {
			for methodSelID, method := range cls.ClassVTable.LocalMethods() {
				cm, ok := method.(*vm.CompiledMethod)
				if !ok {
					continue
				}
				if bytecodeSendsSelector(cm.Bytecode, selID, isSpecialized, targetSpecialized) {
					methodName := v.Selectors.Name(methodSelID)
					locations = append(locations, protocol.Location{
						URI: protocol.DocumentUri(fmt.Sprintf("maggie://class/%s class/%s", cls.Name, methodName)),
						Range: protocol.Range{
							Start: protocol.Position{Line: 0, Character: 0},
							End:   protocol.Position{Line: 0, Character: 0},
						},
					})
				}
			}
		}
	}

	return locations
}

// --- Diagnostics ---

func (s *LspServer) publishDiagnostics(ctx *glsp.Context, uri protocol.DocumentUri, text string) {
	result, err := s.worker.Do(func(v *vm.VM) interface{} {
		_, compileErr := v.CompileExpression(text)
		if compileErr != nil {
			return compileErr.Error()
		}
		return nil
	})
	if err != nil {
		return
	}

	var diagnostics []protocol.Diagnostic
	if result != nil {
		errMsg := result.(string)
		severity := protocol.DiagnosticSeverityError
		source := lspName
		diagnostics = append(diagnostics, protocol.Diagnostic{
			Range: protocol.Range{
				Start: protocol.Position{Line: 0, Character: 0},
				End:   protocol.Position{Line: 0, Character: 0},
			},
			Severity: &severity,
			Source:   &source,
			Message:  errMsg,
		})
	}

	go ctx.Notify(protocol.ServerTextDocumentPublishDiagnostics, protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnostics,
	})
}

// --- Text extraction helpers ---

// extractPrefix returns the word fragment before the cursor for completion.
func extractPrefix(text string, pos protocol.Position) string {
	lines := strings.Split(text, "\n")
	if int(pos.Line) >= len(lines) {
		return ""
	}
	line := lines[pos.Line]
	col := int(pos.Character)
	if col > len(line) {
		col = len(line)
	}

	// Walk backwards from cursor to find the start of the identifier
	start := col
	for start > 0 {
		ch := rune(line[start-1])
		if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' || ch == ':' {
			start--
		} else {
			break
		}
	}

	if start == col {
		return ""
	}

	return line[start:col]
}

// extractWord returns the full identifier under the cursor.
func extractWord(text string, pos protocol.Position) string {
	lines := strings.Split(text, "\n")
	if int(pos.Line) >= len(lines) {
		return ""
	}
	line := lines[pos.Line]
	col := int(pos.Character)
	if col > len(line) {
		col = len(line)
	}

	// Find start
	start := col
	for start > 0 {
		ch := rune(line[start-1])
		if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
			start--
		} else {
			break
		}
	}

	// Find end
	end := col
	for end < len(line) {
		ch := rune(line[end])
		if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
			end++
		} else {
			break
		}
	}

	if start == end {
		return ""
	}

	return line[start:end]
}

func boolPtr(b bool) *bool {
	return &b
}
