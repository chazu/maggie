package server

import (
	"fmt"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"
	"unicode"

	"github.com/tliron/commonlog"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
	glspserver "github.com/tliron/glsp/server"

	"github.com/chazu/maggie/compiler"
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

		TextDocumentCompletion:     s.textDocumentCompletion,
		TextDocumentHover:          s.textDocumentHover,
		TextDocumentDefinition:     s.textDocumentDefinition,
		TextDocumentReferences:     s.textDocumentReferences,
		TextDocumentDocumentSymbol: s.textDocumentDocumentSymbol,
		TextDocumentFormatting:     s.textDocumentFormatting,
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
	capabilities.DocumentSymbolProvider = true
	capabilities.DocumentFormattingProvider = true

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

	// Class names (match against both bare name and FQN)
	for _, cls := range v.Classes.All() {
		fullName := cls.FullName()
		lowerFull := strings.ToLower(fullName)
		lowerBare := strings.ToLower(cls.Name)
		if strings.HasPrefix(lowerFull, lowerPrefix) || strings.HasPrefix(lowerBare, lowerPrefix) {
			kind := protocol.CompletionItemKindClass
			detail := "class"
			if cls.Superclass != nil {
				detail = fmt.Sprintf("class (< %s)", cls.Superclass.FullName())
			}
			label := fullName
			items = append(items, protocol.CompletionItem{
				Label:      label,
				Kind:       &kind,
				Detail:     &detail,
				InsertText: &label,
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
	// Uppercase word or FQN (contains ::) → class lookup
	if len(word) > 0 && (unicode.IsUpper(rune(word[0])) || strings.Contains(word, "::")) {
		cls := lookupClassByName(v, word)
		if cls == nil {
			return nil
		}

		var b strings.Builder
		fmt.Fprintf(&b, "**%s**", cls.FullName())
		if cls.Superclass != nil {
			fmt.Fprintf(&b, " < %s", cls.Superclass.FullName())
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
				names[i] = sup.FullName()
			}
			b.WriteString(strings.Join(names, " → "))
			fmt.Fprintf(&b, " → **%s**", cls.FullName())
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
	// For class names (uppercase or FQN), return a virtual URI
	if len(word) > 0 && (unicode.IsUpper(rune(word[0])) || strings.Contains(word, "::")) {
		cls := lookupClassByName(v, word)
		if cls == nil {
			return nil
		}
		return []protocol.Location{{
			URI: protocol.DocumentUri(fmt.Sprintf("maggie://class/%s", cls.FullName())),
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

// lineErrorRe extracts "line N: message" from parser error strings.
var lineErrorRe = regexp.MustCompile(`line (\d+): (.+)`)

func (s *LspServer) publishDiagnostics(ctx *glsp.Context, uri protocol.DocumentUri, text string) {
	// Use the full source file parser for .mag files (handles namespace:, import:,
	// class/trait definitions). Fall back to CompileExpression for non-.mag URIs.
	var diagnostics []protocol.Diagnostic
	severity := protocol.DiagnosticSeverityError
	source := lspName

	if strings.HasSuffix(string(uri), ".mag") {
		_, parseErr := compiler.ParseSourceFileFromString(text)
		if parseErr != nil {
			// Parser errors are formatted as "parse errors: [line N: msg; line M: msg]"
			// or individual "line N: msg" entries joined by semicolons.
			errStr := parseErr.Error()
			matches := lineErrorRe.FindAllStringSubmatch(errStr, -1)
			if len(matches) > 0 {
				for _, m := range matches {
					line, _ := strconv.Atoi(m[1])
					if line > 0 {
						line-- // parser lines are 1-based, LSP is 0-based
					}
					diagnostics = append(diagnostics, protocol.Diagnostic{
						Range: protocol.Range{
							Start: protocol.Position{Line: protocol.UInteger(line), Character: 0},
							End:   protocol.Position{Line: protocol.UInteger(line), Character: 0},
						},
						Severity: &severity,
						Source:   &source,
						Message:  m[2],
					})
				}
			} else {
				// Fallback: report the raw error at line 0
				diagnostics = append(diagnostics, protocol.Diagnostic{
					Range: protocol.Range{
						Start: protocol.Position{Line: 0, Character: 0},
						End:   protocol.Position{Line: 0, Character: 0},
					},
					Severity: &severity,
					Source:   &source,
					Message:  errStr,
				})
			}
		}
	} else {
		// Non-.mag files: try as expression (REPL-style)
		result, err := s.worker.Do(func(v *vm.VM) interface{} {
			_, compileErr := v.CompileExpression(text)
			if compileErr != nil {
				return compileErr.Error()
			}
			return nil
		})
		if err == nil && result != nil {
			diagnostics = append(diagnostics, protocol.Diagnostic{
				Range: protocol.Range{
					Start: protocol.Position{Line: 0, Character: 0},
					End:   protocol.Position{Line: 0, Character: 0},
				},
				Severity: &severity,
				Source:   &source,
				Message:  result.(string),
			})
		}
	}

	go ctx.Notify(protocol.ServerTextDocumentPublishDiagnostics, protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnostics,
	})
}

// --- Text extraction helpers ---

// extractPrefix returns the word fragment before the cursor for completion.
// Includes :: namespace separators so that typing "Widgets::B" completes FQN class names.
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

	// The existing logic already includes ':' which covers '::' for FQN prefixes.
	return line[start:col]
}

// extractWord returns the full identifier under the cursor, including :: namespace separators.
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

	// Find start of the immediate identifier segment
	start := col
	for start > 0 {
		ch := rune(line[start-1])
		if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
			start--
		} else {
			break
		}
	}

	// Extend backwards across :: separators (e.g., Widgets::Button)
	for start >= 2 && line[start-1] == ':' && line[start-2] == ':' {
		start -= 2
		for start > 0 {
			ch := rune(line[start-1])
			if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
				start--
			} else {
				break
			}
		}
	}

	// Find end of the immediate identifier segment
	end := col
	for end < len(line) {
		ch := rune(line[end])
		if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
			end++
		} else {
			break
		}
	}

	// Extend forwards across :: separators
	for end+1 < len(line) && line[end] == ':' && line[end+1] == ':' {
		end += 2
		for end < len(line) {
			ch := rune(line[end])
			if unicode.IsLetter(ch) || unicode.IsDigit(ch) || ch == '_' {
				end++
			} else {
				break
			}
		}
	}

	if start == end {
		return ""
	}

	return line[start:end]
}

// --- Document symbols ---

func (s *LspServer) textDocumentDocumentSymbol(ctx *glsp.Context, params *protocol.DocumentSymbolParams) (any, error) {
	uri := params.TextDocument.URI

	s.mu.Lock()
	text, ok := s.docs[string(uri)]
	s.mu.Unlock()

	if !ok {
		return nil, nil
	}

	sf, err := compiler.ParseSourceFileFromString(text)
	if err != nil {
		return nil, nil // don't report symbols for unparseable files
	}

	var symbols []protocol.DocumentSymbol

	// Namespace declaration
	if sf.Namespace != nil {
		nsKind := protocol.SymbolKindNamespace
		nsRange := spanToRange(sf.Namespace.SpanVal)
		symbols = append(symbols, protocol.DocumentSymbol{
			Name:           "namespace: " + sf.Namespace.Name,
			Kind:           nsKind,
			Range:          nsRange,
			SelectionRange: nsRange,
		})
	}

	// Classes
	for _, cls := range sf.Classes {
		clsKind := protocol.SymbolKindClass
		clsRange := spanToRange(cls.SpanVal)
		clsSym := protocol.DocumentSymbol{
			Name:           cls.Name,
			Kind:           clsKind,
			Range:          clsRange,
			SelectionRange: clsRange,
		}
		if cls.Superclass != "" {
			detail := "< " + cls.Superclass
			clsSym.Detail = &detail
		}

		// Instance methods
		for _, m := range cls.Methods {
			methodKind := protocol.SymbolKindMethod
			mRange := spanToRange(m.SpanVal)
			clsSym.Children = append(clsSym.Children, protocol.DocumentSymbol{
				Name:           m.Selector,
				Kind:           methodKind,
				Range:          mRange,
				SelectionRange: mRange,
			})
		}

		// Class methods
		for _, m := range cls.ClassMethods {
			methodKind := protocol.SymbolKindMethod
			mRange := spanToRange(m.SpanVal)
			detail := "class-side"
			clsSym.Children = append(clsSym.Children, protocol.DocumentSymbol{
				Name:           m.Selector,
				Kind:           methodKind,
				Detail:         &detail,
				Range:          mRange,
				SelectionRange: mRange,
			})
		}

		symbols = append(symbols, clsSym)
	}

	// Traits
	for _, trait := range sf.Traits {
		traitKind := protocol.SymbolKindInterface
		traitRange := spanToRange(trait.SpanVal)
		traitSym := protocol.DocumentSymbol{
			Name:           trait.Name,
			Kind:           traitKind,
			Range:          traitRange,
			SelectionRange: traitRange,
		}
		for _, m := range trait.Methods {
			methodKind := protocol.SymbolKindMethod
			mRange := spanToRange(m.SpanVal)
			traitSym.Children = append(traitSym.Children, protocol.DocumentSymbol{
				Name:           m.Selector,
				Kind:           methodKind,
				Range:          mRange,
				SelectionRange: mRange,
			})
		}
		symbols = append(symbols, traitSym)
	}

	// Extension methods (methods outside any class)
	for _, m := range sf.Methods {
		methodKind := protocol.SymbolKindFunction
		mRange := spanToRange(m.SpanVal)
		symbols = append(symbols, protocol.DocumentSymbol{
			Name:           m.Selector,
			Kind:           methodKind,
			Range:          mRange,
			SelectionRange: mRange,
		})
	}

	return symbols, nil
}

// spanToRange converts a compiler.Span (1-based) to an LSP Range (0-based).
func spanToRange(s compiler.Span) protocol.Range {
	startLine := s.Start.Line
	if startLine > 0 {
		startLine--
	}
	startCol := s.Start.Column
	if startCol > 0 {
		startCol--
	}
	endLine := s.End.Line
	if endLine > 0 {
		endLine--
	}
	endCol := s.End.Column
	if endCol > 0 {
		endCol--
	}
	return protocol.Range{
		Start: protocol.Position{Line: protocol.UInteger(startLine), Character: protocol.UInteger(startCol)},
		End:   protocol.Position{Line: protocol.UInteger(endLine), Character: protocol.UInteger(endCol)},
	}
}

// --- Formatting ---

func (s *LspServer) textDocumentFormatting(ctx *glsp.Context, params *protocol.DocumentFormattingParams) ([]protocol.TextEdit, error) {
	uri := params.TextDocument.URI

	s.mu.Lock()
	text, ok := s.docs[string(uri)]
	s.mu.Unlock()

	if !ok {
		return nil, nil
	}

	formatted, err := formatMaggieSource(text)
	if err != nil || formatted == text {
		return nil, nil // don't format broken files; no-op if unchanged
	}

	// Count lines in original to build a range covering the entire document
	lines := strings.Count(text, "\n")
	lastLineLen := len(text) - strings.LastIndex(text, "\n") - 1
	if lastLineLen < 0 {
		lastLineLen = len(text)
	}

	return []protocol.TextEdit{{
		Range: protocol.Range{
			Start: protocol.Position{Line: 0, Character: 0},
			End:   protocol.Position{Line: protocol.UInteger(lines), Character: protocol.UInteger(lastLineLen)},
		},
		NewText: formatted,
	}}, nil
}

// formatMaggieSource formats Maggie source code. This duplicates the core logic
// from cmd/mag/format.go since that's in package main and can't be imported.
// It parses the source and returns canonical output.
func formatMaggieSource(source string) (string, error) {
	// We call ParseSourceFileFromString to validate, but the actual formatting
	// requires the formatter from cmd/mag. For now, we just validate and return
	// the source unchanged if valid — the full formatter will be extracted to a
	// shared package in a follow-up.
	_, err := compiler.ParseSourceFileFromString(source)
	if err != nil {
		return "", err
	}
	// TODO: extract Format() from cmd/mag/format.go to a shared package
	// and call it here. For now, formatting is advertised but is a no-op.
	return source, nil
}

// --- Helpers ---

// lookupClassByName tries to find a class by exact name, FQN, or bare name scan.
func lookupClassByName(v *vm.VM, name string) *vm.Class {
	// Direct lookup (works for both bare names and FQNs)
	cls := v.Classes.Lookup(name)
	if cls != nil {
		return cls
	}

	// If it doesn't contain ::, scan all classes for a bare name match
	if !strings.Contains(name, "::") {
		var match *vm.Class
		for _, c := range v.Classes.All() {
			if c.Name == name {
				if match != nil {
					return nil // ambiguous — multiple namespaces define this name
				}
				match = c
			}
		}
		return match
	}
	return nil
}

func boolPtr(b bool) *bool {
	return &b
}
