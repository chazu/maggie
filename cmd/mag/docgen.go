package main

import (
	"fmt"
	"html/template"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/chazu/maggie/vm"
)

// ---------------------------------------------------------------------------
// Docstring Parser (shared types used by doctest.go too)
// ---------------------------------------------------------------------------

// DocSectionType represents the type of a parsed docstring section.
type DocSectionType int

const (
	DocProse   DocSectionType = iota
	DocTest                    // ```test blocks with >>> assertions
	DocExample                 // ```example blocks for interactive playground
)

// DocSection represents a parsed section of a docstring.
type DocSection struct {
	Type    DocSectionType
	Content string
}

// ParseDocString parses a docstring into structured sections.
// It splits on ```test / ```example / ``` fences.
func ParseDocString(doc string) []DocSection {
	var sections []DocSection

	lines := strings.Split(doc, "\n")
	var currentContent strings.Builder
	currentType := DocProse
	inFence := false

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		if !inFence {
			if trimmed == "```test" {
				// Flush any accumulated prose
				prose := strings.TrimSpace(currentContent.String())
				if prose != "" {
					sections = append(sections, DocSection{Type: DocProse, Content: prose})
				}
				currentContent.Reset()
				currentType = DocTest
				inFence = true
				continue
			}
			if trimmed == "```example" {
				// Flush any accumulated prose
				prose := strings.TrimSpace(currentContent.String())
				if prose != "" {
					sections = append(sections, DocSection{Type: DocProse, Content: prose})
				}
				currentContent.Reset()
				currentType = DocExample
				inFence = true
				continue
			}
			// Regular prose line
			if currentContent.Len() > 0 {
				currentContent.WriteString("\n")
			}
			currentContent.WriteString(line)
		} else {
			// Inside a fence
			if trimmed == "```" {
				// Close the fence
				fenced := strings.TrimSpace(currentContent.String())
				if fenced != "" {
					sections = append(sections, DocSection{Type: currentType, Content: fenced})
				}
				currentContent.Reset()
				currentType = DocProse
				inFence = false
				continue
			}
			if currentContent.Len() > 0 {
				currentContent.WriteString("\n")
			}
			currentContent.WriteString(line)
		}
	}

	// Flush remaining content
	remaining := strings.TrimSpace(currentContent.String())
	if remaining != "" {
		sections = append(sections, DocSection{Type: currentType, Content: remaining})
	}

	return sections
}

// ---------------------------------------------------------------------------
// Data structures for template rendering
// ---------------------------------------------------------------------------

// classDoc holds all documentation data for a single class.
type classDoc struct {
	Name            string
	FullName        string
	Namespace       string
	SuperclassChain []string
	InstVars        []string
	DocString       string
	DocSections     []DocSection
	Categories      []categoryDoc
	ClassCategories []categoryDoc
	HasMethods      bool
	HasClassMethods bool
	RelPath         string // relative path for linking (e.g., "classes/Array.html")
	BriefDoc        string // first sentence of docstring for index page
}

// categoryDoc holds methods grouped by category.
type categoryDoc struct {
	Name    string
	Methods []methodDoc
}

// methodDoc holds documentation data for a single method.
type methodDoc struct {
	Selector    string
	IsClass     bool
	DocString   string
	DocSections []DocSection
	Source      string
	Category    string
}

// namespaceGroup holds classes grouped by namespace for the index page.
type namespaceGroup struct {
	Namespace string
	Classes   []classDoc
}

// ---------------------------------------------------------------------------
// handleDocCommand: Entry point for the doc subcommand
// ---------------------------------------------------------------------------

// handleDocCommand generates HTML API documentation from all classes in the VM.
func handleDocCommand(vmInst *vm.VM, args []string) {
	outputDir := "docs/api"
	title := "Maggie API Reference"

	// Parse flags from args (skip flags handled by docserve: --serve, --port)
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--output":
			if i+1 < len(args) {
				outputDir = args[i+1]
				i++
			} else {
				fmt.Fprintf(os.Stderr, "Error: --output requires a directory argument\n")
				os.Exit(1)
			}
		case "--title":
			if i+1 < len(args) {
				title = args[i+1]
				i++
			} else {
				fmt.Fprintf(os.Stderr, "Error: --title requires a string argument\n")
				os.Exit(1)
			}
		case "--serve":
			// handled by docserve.go
		case "--port":
			i++ // skip the port value
		default:
			if strings.HasPrefix(args[i], "--output=") {
				outputDir = strings.TrimPrefix(args[i], "--output=")
			} else if strings.HasPrefix(args[i], "--title=") {
				title = strings.TrimPrefix(args[i], "--title=")
			} else if strings.HasPrefix(args[i], "--port=") {
				// handled by docserve.go
			} else if strings.HasPrefix(args[i], "--serve=") {
				// handled by docserve.go
			} else {
				fmt.Fprintf(os.Stderr, "Warning: unknown doc flag %q\n", args[i])
			}
		}
	}

	// Collect all classes
	allClasses := vmInst.Classes.All()
	fmt.Printf("Generating docs for %d classes...\n", len(allClasses))

	// Build class documentation structures
	var classDocs []classDoc
	for _, cls := range allClasses {
		doc := buildClassDoc(cls, vmInst)
		classDocs = append(classDocs, doc)
	}

	// Sort classes alphabetically by full name
	sort.Slice(classDocs, func(i, j int) bool {
		return classDocs[i].FullName < classDocs[j].FullName
	})

	// Group by namespace
	nsGroups := groupByNamespace(classDocs)

	// Create output directories
	if err := os.MkdirAll(filepath.Join(outputDir, "classes"), 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating output directory: %v\n", err)
		os.Exit(1)
	}
	if err := os.MkdirAll(filepath.Join(outputDir, "css"), 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating css directory: %v\n", err)
		os.Exit(1)
	}
	if err := os.MkdirAll(filepath.Join(outputDir, "js"), 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating js directory: %v\n", err)
		os.Exit(1)
	}

	// Write CSS
	if err := writeCSS(outputDir); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing CSS: %v\n", err)
		os.Exit(1)
	}

	// Write playground JS
	if err := writePlaygroundJS(outputDir); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing playground JS: %v\n", err)
		os.Exit(1)
	}

	// Write index page
	if err := writeIndexPage(outputDir, title, nsGroups); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing index page: %v\n", err)
		os.Exit(1)
	}

	// Write individual class pages
	for _, doc := range classDocs {
		if err := writeClassPage(outputDir, title, doc); err != nil {
			fmt.Fprintf(os.Stderr, "Error writing class page for %s: %v\n", doc.FullName, err)
			os.Exit(1)
		}
	}

	fmt.Printf("Documentation written to %s\n", outputDir)
}

// ---------------------------------------------------------------------------
// Data collection from VM
// ---------------------------------------------------------------------------

// buildClassDoc extracts documentation data from a vm.Class.
func buildClassDoc(cls *vm.Class, vmInst *vm.VM) classDoc {
	doc := classDoc{
		Name:      cls.Name,
		FullName:  cls.FullName(),
		Namespace: cls.Namespace,
		InstVars:  cls.InstVars,
		DocString: cls.DocString,
	}

	// Parse docstring sections
	if cls.DocString != "" {
		doc.DocSections = ParseDocString(cls.DocString)
		doc.BriefDoc = extractBrief(cls.DocString)
	}

	// Build superclass chain
	for sc := cls.Superclass; sc != nil; sc = sc.Superclass {
		doc.SuperclassChain = append(doc.SuperclassChain, sc.FullName())
	}

	// Compute relative path for linking
	doc.RelPath = classRelPath(cls)

	// Collect instance methods
	doc.Categories = collectMethods(cls.VTable, vmInst.Selectors, false)
	doc.HasMethods = len(doc.Categories) > 0

	// Collect class methods
	doc.ClassCategories = collectMethods(cls.ClassVTable, vmInst.Selectors, true)
	doc.HasClassMethods = len(doc.ClassCategories) > 0

	return doc
}

// collectMethods collects methods from a VTable, grouped by category.
func collectMethods(vt *vm.VTable, selectors *vm.SelectorTable, isClassSide bool) []categoryDoc {
	localMethods := vt.LocalMethods()
	if len(localMethods) == 0 {
		return nil
	}

	// Collect all methods
	byCategory := make(map[string][]methodDoc)
	for selectorID, method := range localMethods {
		cm, ok := method.(*vm.CompiledMethod)
		if !ok {
			// Non-compiled (primitive) methods: include with minimal info
			selectorName := selectors.Name(selectorID)
			if selectorName == "" {
				continue
			}
			md := methodDoc{
				Selector: selectorName,
				IsClass:  isClassSide,
				Category: "primitives",
			}
			doc := vm.MethodDocString(method)
			if doc != "" {
				md.DocString = doc
				md.DocSections = ParseDocString(doc)
			}
			byCategory["primitives"] = append(byCategory["primitives"], md)
			continue
		}

		selectorName := selectors.Name(selectorID)
		if selectorName == "" {
			selectorName = cm.Name()
		}

		cat := cm.Category()
		if cat == "" {
			cat = "uncategorized"
		}

		md := methodDoc{
			Selector:  selectorName,
			IsClass:   isClassSide,
			DocString: cm.DocString(),
			Source:    cm.Source,
			Category:  cat,
		}

		if cm.DocString() != "" {
			md.DocSections = ParseDocString(cm.DocString())
		}

		byCategory[cat] = append(byCategory[cat], md)
	}

	// Sort categories alphabetically
	var catNames []string
	for name := range byCategory {
		catNames = append(catNames, name)
	}
	sort.Strings(catNames)

	// Build sorted category docs
	var categories []categoryDoc
	for _, name := range catNames {
		methods := byCategory[name]
		// Sort methods within category
		sort.Slice(methods, func(i, j int) bool {
			return methods[i].Selector < methods[j].Selector
		})
		categories = append(categories, categoryDoc{
			Name:    name,
			Methods: methods,
		})
	}

	return categories
}

// groupByNamespace groups classes by their namespace.
func groupByNamespace(classes []classDoc) []namespaceGroup {
	grouped := make(map[string][]classDoc)
	for _, cls := range classes {
		ns := cls.Namespace
		if ns == "" {
			ns = "(root)"
		}
		grouped[ns] = append(grouped[ns], cls)
	}

	// Sort namespace names
	var nsNames []string
	for name := range grouped {
		nsNames = append(nsNames, name)
	}
	sort.Strings(nsNames)

	var groups []namespaceGroup
	for _, name := range nsNames {
		classes := grouped[name]
		sort.Slice(classes, func(i, j int) bool {
			return classes[i].Name < classes[j].Name
		})
		groups = append(groups, namespaceGroup{
			Namespace: name,
			Classes:   classes,
		})
	}

	return groups
}

// classRelPath computes the relative HTML path for a class.
func classRelPath(cls *vm.Class) string {
	if cls.Namespace == "" {
		return "classes/" + cls.Name + ".html"
	}
	// Convert namespace separators to directory separators
	nsPath := strings.ReplaceAll(cls.Namespace, "::", string(filepath.Separator))
	return filepath.Join("classes", nsPath, cls.Name+".html")
}

// extractBrief returns the first sentence of a docstring for the index page.
func extractBrief(doc string) string {
	// Take first line or first sentence
	lines := strings.SplitN(doc, "\n", 2)
	first := strings.TrimSpace(lines[0])

	// Truncate at first period followed by space, or at 120 chars
	if idx := strings.Index(first, ". "); idx != -1 && idx < 120 {
		return first[:idx+1]
	}
	if len(first) > 120 {
		return first[:117] + "..."
	}
	return first
}

// ---------------------------------------------------------------------------
// File writing
// ---------------------------------------------------------------------------

// writeCSS writes the stylesheet to the output directory.
func writeCSS(outputDir string) error {
	cssPath := filepath.Join(outputDir, "css", "style.css")
	return os.WriteFile(cssPath, []byte(cssContent), 0644)
}

// writePlaygroundJS writes the playground JavaScript to the output directory.
func writePlaygroundJS(outputDir string) error {
	jsPath := filepath.Join(outputDir, "js", "playground.js")
	return os.WriteFile(jsPath, []byte(playgroundJSContent), 0644)
}

// writeIndexPage generates the index.html class listing.
func writeIndexPage(outputDir, title string, groups []namespaceGroup) error {
	tmpl, err := template.New("index").Parse(indexTemplate)
	if err != nil {
		return fmt.Errorf("parsing index template: %w", err)
	}

	outPath := filepath.Join(outputDir, "index.html")
	f, err := os.Create(outPath)
	if err != nil {
		return fmt.Errorf("creating %s: %w", outPath, err)
	}
	defer f.Close()

	data := struct {
		Title  string
		Groups []namespaceGroup
	}{
		Title:  title,
		Groups: groups,
	}

	return tmpl.Execute(f, data)
}

// writeClassPage generates an individual class HTML page.
func writeClassPage(outputDir, siteTitle string, doc classDoc) error {
	funcMap := template.FuncMap{
		"renderDocSections": renderDocSectionsHTML,
		"hasDocString":      func(s string) bool { return s != "" },
		"classMethodPrefix": func(isClass bool) string {
			if isClass {
				return "class "
			}
			return ""
		},
		"join": strings.Join,
		"depthCSS": func(relPath string) string {
			// Compute relative path back to root from the class page
			depth := strings.Count(relPath, string(filepath.Separator))
			parts := make([]string, depth+1)
			for i := range parts {
				parts[i] = ".."
			}
			return strings.Join(parts, "/")
		},
	}

	tmpl, err := template.New("class").Funcs(funcMap).Parse(classTemplate)
	if err != nil {
		return fmt.Errorf("parsing class template: %w", err)
	}

	// Ensure the output directory for this class exists
	outPath := filepath.Join(outputDir, doc.RelPath)
	outDir := filepath.Dir(outPath)
	if err := os.MkdirAll(outDir, 0755); err != nil {
		return fmt.Errorf("creating directory %s: %w", outDir, err)
	}

	f, err := os.Create(outPath)
	if err != nil {
		return fmt.Errorf("creating %s: %w", outPath, err)
	}
	defer f.Close()

	data := struct {
		SiteTitle string
		Class     classDoc
	}{
		SiteTitle: siteTitle,
		Class:     doc,
	}

	return tmpl.Execute(f, data)
}

// renderDocSectionsHTML renders parsed docstring sections as HTML.
func renderDocSectionsHTML(sections []DocSection) template.HTML {
	var buf strings.Builder

	for _, sec := range sections {
		switch sec.Type {
		case DocProse:
			// Wrap prose in paragraphs, splitting on double newlines
			paragraphs := strings.Split(sec.Content, "\n\n")
			for _, p := range paragraphs {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				buf.WriteString("<p>")
				buf.WriteString(template.HTMLEscapeString(p))
				buf.WriteString("</p>\n")
			}

		case DocTest:
			buf.WriteString("<div class=\"doc-block doc-test\">\n")
			buf.WriteString("<span class=\"doc-block-label\">Test</span>\n")
			buf.WriteString("<pre><code>")
			buf.WriteString(template.HTMLEscapeString(sec.Content))
			buf.WriteString("</code></pre>\n")
			buf.WriteString("</div>\n")

		case DocExample:
			buf.WriteString("<div class=\"doc-block doc-example\">\n")
			buf.WriteString("<span class=\"doc-block-label\">Example</span>\n")
			buf.WriteString("<pre><code>")
			buf.WriteString(template.HTMLEscapeString(sec.Content))
			buf.WriteString("</code></pre>\n")
			buf.WriteString("</div>\n")
		}
	}

	return template.HTML(buf.String())
}

// ---------------------------------------------------------------------------
// CSS
// ---------------------------------------------------------------------------

const cssContent = `/* Maggie API Documentation */
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
    line-height: 1.6;
    color: #24292e;
    background: #ffffff;
    max-width: 960px;
    margin: 0 auto;
    padding: 2rem 1.5rem;
}

a {
    color: #0366d6;
    text-decoration: none;
}
a:hover {
    text-decoration: underline;
}

h1 {
    font-size: 2rem;
    border-bottom: 1px solid #e1e4e8;
    padding-bottom: 0.5rem;
    margin-bottom: 1.5rem;
}

h2 {
    font-size: 1.5rem;
    margin-top: 2rem;
    margin-bottom: 0.75rem;
    border-bottom: 1px solid #eaecef;
    padding-bottom: 0.3rem;
}

h3 {
    font-size: 1.25rem;
    margin-top: 1.5rem;
    margin-bottom: 0.5rem;
}

h4 {
    font-size: 1rem;
    margin-top: 1rem;
    margin-bottom: 0.25rem;
    color: #586069;
}

p {
    margin-bottom: 0.75rem;
}

code, pre {
    font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
    font-size: 0.875rem;
}

code {
    background: #f6f8fa;
    padding: 0.15rem 0.3rem;
    border-radius: 3px;
}

pre {
    background: #f6f8fa;
    padding: 1rem;
    border-radius: 6px;
    overflow-x: auto;
    margin-bottom: 1rem;
}

pre code {
    background: none;
    padding: 0;
}

/* Namespace grouping on index page */
.namespace-group {
    margin-bottom: 2rem;
}

.namespace-group h2 {
    font-size: 1.25rem;
    color: #6a737d;
    font-weight: 600;
}

.class-list {
    list-style: none;
    padding-left: 0;
}

.class-list li {
    padding: 0.4rem 0;
    border-bottom: 1px solid #f0f0f0;
}

.class-list li:last-child {
    border-bottom: none;
}

.class-list .class-name {
    font-weight: 600;
}

.class-list .class-brief {
    color: #586069;
    font-size: 0.875rem;
    margin-left: 0.5rem;
}

/* Breadcrumb navigation */
.breadcrumb {
    font-size: 0.875rem;
    color: #586069;
    margin-bottom: 1rem;
}

.breadcrumb a {
    color: #586069;
}

/* Class page sections */
.class-header {
    margin-bottom: 1.5rem;
}

.superclass-chain {
    font-size: 0.875rem;
    color: #586069;
    margin-top: 0.25rem;
}

.inst-vars {
    font-size: 0.875rem;
    color: #586069;
    margin-top: 0.5rem;
}

.inst-vars code {
    font-size: 0.8125rem;
}

.class-doc {
    margin-bottom: 2rem;
    padding: 1rem;
    background: #f8f9fa;
    border-radius: 6px;
    border-left: 3px solid #0366d6;
}

.class-doc p {
    margin-bottom: 0.5rem;
}

.class-doc p:last-child {
    margin-bottom: 0;
}

/* Method sections */
.method-category {
    margin-top: 1.5rem;
    margin-bottom: 1rem;
}

.method-category h3 {
    font-size: 1.1rem;
    color: #24292e;
    text-transform: capitalize;
    border-bottom: 1px solid #eaecef;
    padding-bottom: 0.25rem;
}

.method {
    margin-bottom: 1.5rem;
    padding: 0.75rem 1rem;
    border: 1px solid #e1e4e8;
    border-radius: 6px;
}

.method-selector {
    font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
    font-size: 1rem;
    font-weight: 600;
    color: #22863a;
}

.method-selector .class-side-label {
    color: #6a737d;
    font-weight: normal;
    font-size: 0.8125rem;
}

.method-doc {
    margin-top: 0.5rem;
}

.method-doc p {
    margin-bottom: 0.5rem;
}

.method-source {
    margin-top: 0.75rem;
}

.method-source summary {
    cursor: pointer;
    font-size: 0.8125rem;
    color: #586069;
    user-select: none;
}

.method-source summary:hover {
    color: #0366d6;
}

.method-source pre {
    margin-top: 0.5rem;
}

/* Docstring test/example blocks */
.doc-block {
    margin: 0.75rem 0;
    border-radius: 6px;
    overflow: hidden;
}

.doc-block-label {
    display: block;
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    padding: 0.25rem 1rem;
}

.doc-test {
    border: 1px solid #d1d5da;
}

.doc-test .doc-block-label {
    background: #e8f5e9;
    color: #2e7d32;
}

.doc-test pre {
    margin: 0;
    border-radius: 0;
}

.doc-example {
    border: 1px solid #d1d5da;
}

.doc-example .doc-block-label {
    background: #e3f2fd;
    color: #1565c0;
}

.doc-example pre {
    margin: 0;
    border-radius: 0;
}

/* Footer */
.footer {
    margin-top: 3rem;
    padding-top: 1rem;
    border-top: 1px solid #e1e4e8;
    font-size: 0.8125rem;
    color: #6a737d;
}

/* Playground */
.playground-run {
    display: inline-block;
    padding: 4px 12px;
    margin-bottom: 4px;
    background: #1565c0;
    color: #fff;
    border: none;
    border-radius: 3px;
    cursor: pointer;
    font-size: 13px;
}
.playground-run:hover {
    background: #0d47a1;
}
.playground-run:disabled {
    background: #90a4ae;
    cursor: wait;
}
.playground-output {
    padding: 8px 12px;
    margin-top: 4px;
    border-radius: 0 0 4px 4px;
    font-family: 'SF Mono', 'Monaco', 'Menlo', monospace;
    font-size: 14px;
    white-space: pre-wrap;
}
.playground-loading {
    color: #78909c;
    background: #f5f5f5;
}
.playground-success {
    color: #1b5e20;
    background: #e8f5e9;
    border: 1px solid #a5d6a7;
}
.playground-error {
    color: #b71c1c;
    background: #ffebee;
    border: 1px solid #ef9a9a;
}
`

// ---------------------------------------------------------------------------
// Playground JavaScript
// ---------------------------------------------------------------------------

const playgroundJSContent = `// Maggie Documentation Playground
// Sends example code to /api/eval and displays results inline
(function() {
    'use strict';

    document.addEventListener('DOMContentLoaded', function() {
        // Find all example blocks and add Run buttons
        var examples = document.querySelectorAll('.doc-example');
        examples.forEach(function(block) {
            var pre = block.querySelector('pre');
            var code = block.querySelector('code');
            if (!code) return;

            // Create Run button
            var btn = document.createElement('button');
            btn.className = 'playground-run';
            btn.textContent = 'Run';
            btn.addEventListener('click', function() {
                runExample(btn, code.textContent);
            });

            // Create output area
            var output = document.createElement('div');
            output.className = 'playground-output';
            output.style.display = 'none';

            // Insert button before pre, output after pre
            block.insertBefore(btn, pre);
            block.appendChild(output);
        });
    });

    function runExample(btn, code) {
        var output = btn.parentElement.querySelector('.playground-output');
        btn.disabled = true;
        btn.textContent = 'Running...';
        output.style.display = 'block';
        output.className = 'playground-output playground-loading';
        output.textContent = 'Evaluating...';

        fetch('/api/eval', {
            method: 'POST',
            headers: { 'Content-Type': 'text/plain' },
            body: code
        })
        .then(function(resp) {
            return resp.text().then(function(text) {
                return { ok: resp.ok, status: resp.status, text: text };
            });
        })
        .then(function(result) {
            btn.disabled = false;
            btn.textContent = 'Run';
            output.className = 'playground-output ' + (result.ok ? 'playground-success' : 'playground-error');
            output.textContent = result.text;
        })
        .catch(function(err) {
            btn.disabled = false;
            btn.textContent = 'Run';
            output.className = 'playground-output playground-error';
            output.textContent = 'Error: ' + err.message;
        });
    }
})();
`

// ---------------------------------------------------------------------------
// HTML Templates
// ---------------------------------------------------------------------------

const indexTemplate = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>{{.Title}}</title>
    <link rel="stylesheet" href="css/style.css">
</head>
<body>
    <h1>{{.Title}}</h1>

{{range .Groups}}
    <div class="namespace-group">
        <h2>{{.Namespace}}</h2>
        <ul class="class-list">
{{range .Classes}}
            <li>
                <a href="{{.RelPath}}" class="class-name">{{.Name}}</a>
{{if .BriefDoc}}
                <span class="class-brief">&mdash; {{.BriefDoc}}</span>
{{end}}
            </li>
{{end}}
        </ul>
    </div>
{{end}}

    <div class="footer">
        Generated by Maggie doc generator.
    </div>
</body>
</html>
`

const classTemplate = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>{{.Class.FullName}} &mdash; {{.SiteTitle}}</title>
    <link rel="stylesheet" href="{{depthCSS .Class.RelPath}}/css/style.css">
</head>
<body>
    <div class="breadcrumb">
        <a href="{{depthCSS .Class.RelPath}}/index.html">{{.SiteTitle}}</a>
{{if .Class.Namespace}}
        &rsaquo; {{.Class.Namespace}}
{{end}}
        &rsaquo; {{.Class.Name}}
    </div>

    <div class="class-header">
        <h1>{{.Class.Name}}</h1>
{{if .Class.SuperclassChain}}
        <div class="superclass-chain">
            Inherits from: {{join .Class.SuperclassChain " &larr; "}}
        </div>
{{end}}
{{if .Class.InstVars}}
        <div class="inst-vars">
            Instance variables: {{range $i, $v := .Class.InstVars}}{{if $i}}, {{end}}<code>{{$v}}</code>{{end}}
        </div>
{{end}}
    </div>

{{if .Class.DocSections}}
    <div class="class-doc">
        {{renderDocSections .Class.DocSections}}
    </div>
{{end}}

{{if .Class.HasClassMethods}}
    <h2>Class Methods</h2>
{{range .Class.ClassCategories}}
    <div class="method-category">
        <h3>{{.Name}}</h3>
{{range .Methods}}
        <div class="method">
            <div class="method-selector">
                <span class="class-side-label">class </span>{{.Selector}}
            </div>
{{if .DocSections}}
            <div class="method-doc">
                {{renderDocSections .DocSections}}
            </div>
{{end}}
{{if hasDocString .Source}}
            <details class="method-source">
                <summary>Source</summary>
                <pre><code>{{.Source}}</code></pre>
            </details>
{{end}}
        </div>
{{end}}
    </div>
{{end}}
{{end}}

{{if .Class.HasMethods}}
    <h2>Instance Methods</h2>
{{range .Class.Categories}}
    <div class="method-category">
        <h3>{{.Name}}</h3>
{{range .Methods}}
        <div class="method">
            <div class="method-selector">{{.Selector}}</div>
{{if .DocSections}}
            <div class="method-doc">
                {{renderDocSections .DocSections}}
            </div>
{{end}}
{{if hasDocString .Source}}
            <details class="method-source">
                <summary>Source</summary>
                <pre><code>{{.Source}}</code></pre>
            </details>
{{end}}
        </div>
{{end}}
    </div>
{{end}}
{{end}}

    <div class="footer">
        Generated by Maggie doc generator.
    </div>

    <script src="{{depthCSS .Class.RelPath}}/js/playground.js"></script>
</body>
</html>
`
