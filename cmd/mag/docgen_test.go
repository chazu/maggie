package main

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// ParseDocString
// ---------------------------------------------------------------------------

func TestDocgen_ParseDocString(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		wantLen  int
		wantType []DocSectionType
		check    func(t *testing.T, sections []DocSection)
	}{
		{
			name:    "empty string",
			input:   "",
			wantLen: 0,
		},
		{
			name:     "pure prose",
			input:    "This is a simple description.\nSecond line.",
			wantLen:  1,
			wantType: []DocSectionType{DocProse},
			check: func(t *testing.T, sections []DocSection) {
				if !strings.Contains(sections[0].Content, "simple description") {
					t.Errorf("expected prose content, got %q", sections[0].Content)
				}
			},
		},
		{
			name:     "test fenced block",
			input:    "```test\n>>> 3 + 4\n7\n```",
			wantLen:  1,
			wantType: []DocSectionType{DocTest},
			check: func(t *testing.T, sections []DocSection) {
				if !strings.Contains(sections[0].Content, ">>> 3 + 4") {
					t.Errorf("expected test content, got %q", sections[0].Content)
				}
			},
		},
		{
			name:     "example fenced block",
			input:    "```example\nArray new: 3\n```",
			wantLen:  1,
			wantType: []DocSectionType{DocExample},
			check: func(t *testing.T, sections []DocSection) {
				if !strings.Contains(sections[0].Content, "Array new: 3") {
					t.Errorf("expected example content, got %q", sections[0].Content)
				}
			},
		},
		{
			name:     "mixed prose test example",
			input:    "Some intro.\n```test\n>>> 1 + 1\n2\n```\nMore prose.\n```example\nx := 42\n```",
			wantLen:  4,
			wantType: []DocSectionType{DocProse, DocTest, DocProse, DocExample},
		},
		{
			name:     "nested backticks in prose",
			input:    "Use `foo` for things.\nAlso `bar`.",
			wantLen:  1,
			wantType: []DocSectionType{DocProse},
			check: func(t *testing.T, sections []DocSection) {
				if !strings.Contains(sections[0].Content, "`foo`") {
					t.Errorf("expected backticks preserved in prose, got %q", sections[0].Content)
				}
			},
		},
		{
			name:    "empty fence produces no section",
			input:   "```test\n```",
			wantLen: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ParseDocString(tt.input)
			if len(got) != tt.wantLen {
				t.Fatalf("ParseDocString(%q): got %d sections, want %d; sections=%+v", tt.input, len(got), tt.wantLen, got)
			}
			for i, wt := range tt.wantType {
				if got[i].Type != wt {
					t.Errorf("section[%d].Type = %d, want %d", i, got[i].Type, wt)
				}
			}
			if tt.check != nil {
				tt.check(t, got)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// isGuideClass
// ---------------------------------------------------------------------------

func TestDocgen_isGuideClass(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{"Guide01Intro", true},
		{"Guide99Advanced", true},
		{"Guide10", true},
		{"Guide1Intro", false},    // only one digit
		{"NotAGuide", false},
		{"MyGuide01", false},       // doesn't start with Guide
		{"Guide", false},
		{"", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := isGuideClass(tt.name); got != tt.want {
				t.Errorf("isGuideClass(%q) = %v, want %v", tt.name, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// extractGuideNumber
// ---------------------------------------------------------------------------

func TestDocgen_extractGuideNumber(t *testing.T) {
	tests := []struct {
		name string
		want int
	}{
		{"Guide01Intro", 1},
		{"Guide10Advanced", 10},
		{"Guide99Foo", 99},
		{"NotAGuide", 0},
		{"Guide", 0},
		{"", 0},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := extractGuideNumber(tt.name); got != tt.want {
				t.Errorf("extractGuideNumber(%q) = %d, want %d", tt.name, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// extractGuideTitle
// ---------------------------------------------------------------------------

func TestDocgen_extractGuideTitle(t *testing.T) {
	tests := []struct {
		name      string
		className string
		docString string
		want      string
	}{
		{
			name:      "title from heading",
			className: "Guide01Intro",
			docString: "# Getting Started\nSome body text.",
			want:      "Getting Started",
		},
		{
			name:      "fallback from class name",
			className: "Guide01GettingStarted",
			docString: "No heading here.",
			want:      "Getting Started",
		},
		{
			name:      "empty docstring fallback",
			className: "Guide03AdvancedTopics",
			docString: "",
			want:      "Advanced Topics",
		},
		{
			name:      "heading with extra whitespace",
			className: "Guide02Foo",
			docString: "  # My Title  \nBody.",
			want:      "My Title",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := extractGuideTitle(tt.className, tt.docString); got != tt.want {
				t.Errorf("extractGuideTitle(%q, %q) = %q, want %q", tt.className, tt.docString, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// guideSlug
// ---------------------------------------------------------------------------

func TestDocgen_guideSlug(t *testing.T) {
	tests := []struct {
		number int
		title  string
		want   string
	}{
		{1, "Getting Started", "01-getting-started"},
		{10, "Advanced Topics", "10-advanced-topics"},
		{3, "Hello, World!", "03-hello-world"},
		{5, "Foo & Bar's Baz", "05-foo-bars-baz"}, // special chars dropped, double dashes collapsed
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			got := guideSlug(tt.number, tt.title)
			if got != tt.want {
				t.Errorf("guideSlug(%d, %q) = %q, want %q", tt.number, tt.title, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// depthPrefix
// ---------------------------------------------------------------------------

func TestDocgen_depthPrefix(t *testing.T) {
	tests := []struct {
		relPath string
		want    string
	}{
		{"index.html", "."},
		{"classes/Array.html", ".."},
		{"classes/Ns/Foo.html", "../.."},
	}

	for _, tt := range tests {
		t.Run(tt.relPath, func(t *testing.T) {
			if got := depthPrefix(tt.relPath); got != tt.want {
				t.Errorf("depthPrefix(%q) = %q, want %q", tt.relPath, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// dedentBlock
// ---------------------------------------------------------------------------

func TestDocgen_dedentBlock(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "no indentation",
			input: "hello\nworld",
			want:  "hello\nworld",
		},
		{
			name:  "uniform indentation",
			input: "    hello\n    world",
			want:  "hello\nworld",
		},
		{
			name:  "mixed indentation",
			input: "    hello\n        world",
			want:  "hello\n    world",
		},
		{
			name:  "empty lines ignored for min calc",
			input: "    hello\n\n    world",
			want:  "hello\n\nworld",
		},
		{
			name:  "single line",
			input: "  hello",
			want:  "hello",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := dedentBlock(tt.input)
			if got != tt.want {
				t.Errorf("dedentBlock(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// renderProseBlockHTML
// ---------------------------------------------------------------------------

func TestDocgen_renderProseBlockHTML(t *testing.T) {
	tests := []struct {
		name  string
		input string
		check func(t *testing.T, got string)
	}{
		{
			name:  "empty string",
			input: "",
			check: func(t *testing.T, got string) {
				if got != "" {
					t.Errorf("expected empty, got %q", got)
				}
			},
		},
		{
			name:  "h2 heading",
			input: "## My Heading",
			check: func(t *testing.T, got string) {
				if !strings.Contains(got, "<h2>") || !strings.Contains(got, "My Heading") {
					t.Errorf("expected <h2>, got %q", got)
				}
			},
		},
		{
			name:  "h3 heading",
			input: "### Sub Heading",
			check: func(t *testing.T, got string) {
				if !strings.Contains(got, "<h3>") || !strings.Contains(got, "Sub Heading") {
					t.Errorf("expected <h3>, got %q", got)
				}
			},
		},
		{
			name:  "h1 rendered as h2",
			input: "# Title",
			check: func(t *testing.T, got string) {
				if !strings.Contains(got, "<h2>") {
					t.Errorf("expected # to render as <h2>, got %q", got)
				}
			},
		},
		{
			name:  "indented code block",
			input: "    x := 42\n    y := x + 1",
			check: func(t *testing.T, got string) {
				if !strings.Contains(got, "<pre><code>") {
					t.Errorf("expected code block, got %q", got)
				}
				if !strings.Contains(got, "x := 42") {
					t.Errorf("expected code content, got %q", got)
				}
			},
		},
		{
			name:  "unordered list",
			input: "- item one\n- item two",
			check: func(t *testing.T, got string) {
				if !strings.Contains(got, "<ul>") || !strings.Contains(got, "<li>") {
					t.Errorf("expected list, got %q", got)
				}
				if !strings.Contains(got, "item one") {
					t.Errorf("expected list content, got %q", got)
				}
			},
		},
		{
			name:  "regular paragraph",
			input: "Just some text.",
			check: func(t *testing.T, got string) {
				if !strings.Contains(got, "<p>") || !strings.Contains(got, "Just some text.") {
					t.Errorf("expected paragraph, got %q", got)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := renderProseBlockHTML(tt.input)
			tt.check(t, got)
		})
	}
}

// ---------------------------------------------------------------------------
// renderInlineMarkdown
// ---------------------------------------------------------------------------

func TestDocgen_renderInlineMarkdown(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "backtick code",
			input: "use `foo` here",
			want:  "use <code>foo</code> here",
		},
		{
			name:  "bold",
			input: "this is **bold** text",
			want:  "this is <strong>bold</strong> text",
		},
		{
			name:  "no markers",
			input: "plain text",
			want:  "plain text",
		},
		{
			name:  "unpaired backtick",
			input: "just `one",
			want:  "just `one",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := renderInlineMarkdown(tt.input); got != tt.want {
				t.Errorf("renderInlineMarkdown(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// extractBrief
// ---------------------------------------------------------------------------

func TestDocgen_extractBrief(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "first sentence",
			input: "This is brief. And this is more.",
			want:  "This is brief.",
		},
		{
			name:  "no period",
			input: "Short description",
			want:  "Short description",
		},
		{
			name:  "first line only",
			input: "First line\nSecond line",
			want:  "First line",
		},
		{
			name:  "truncation at 120 chars",
			input: strings.Repeat("x", 200),
			want:  strings.Repeat("x", 117) + "...",
		},
		{
			name:  "empty string",
			input: "",
			want:  "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := extractBrief(tt.input); got != tt.want {
				t.Errorf("extractBrief(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}
