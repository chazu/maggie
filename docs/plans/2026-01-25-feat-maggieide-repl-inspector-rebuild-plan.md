---
title: "feat: Rebuild MaggieIDE with REPL and Inspector"
type: feat
date: 2026-01-25
brainstorm: docs/brainstorms/2026-01-25-maggieide-rebuild-brainstorm.md
---

# Rebuild MaggieIDE with REPL and Inspector

## Overview

Combine the existing MaggieREPL and Inspector into a single two-panel IDE. The current MaggieIDE doesn't work because each tool calls `stopEventLoop`, preventing multi-panel operation. The fix: remove those calls, put both panels in one layout, add keyboard shortcuts.

**This is a wiring job, not a rewrite.** The existing MaggieREPL (160 lines) and Inspector (179 lines) contain working code. We port them into a shared layout.

## Problem Statement

The existing MaggieIDE (`lib/yutani/ide/MaggieIDE.mag`) is non-functional:
- Each tool calls `stopEventLoop` when closing, which kills the session
- Cannot have REPL and Inspector visible simultaneously

## Proposed Solution

Create `MaggieIDE.mag` (replacing the broken launcher) with:
- Two-panel layout: REPL left, Inspector right
- Single event loop (no `stopEventLoop` except quit)
- Keyboard shortcuts for panel switching and inspection

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      MaggieIDE                              │
│  ┌─────────────────────────┬───────────────────────────┐   │
│  │      REPL Panel         │     Inspector Panel       │   │
│  │  ┌───────────────────┐  │  ┌─────────────────────┐  │   │
│  │  │    Transcript     │  │  │    Slot List        │  │   │
│  │  │   (TextView)      │  │  │    (List)           │  │   │
│  │  │                   │  │  ├─────────────────────┤  │   │
│  │  ├───────────────────┤  │  │    Value View       │  │   │
│  │  │   Input           │  │  │    (TextView)       │  │   │
│  │  │   (TextArea)      │  │  │                     │  │   │
│  │  └───────────────────┘  │  └─────────────────────┘  │   │
│  └─────────────────────────┴───────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Technical Approach

### Keybindings

| Action | Shortcut | Context |
|--------|----------|---------|
| Evaluate | Ctrl+Enter | REPL input |
| Inspect last result | Ctrl+I | Global |
| Switch panels | Ctrl+Tab | Global |
| History up/down | Up/Down | REPL input |
| Drill into slot | Enter | Inspector slot list |
| Go back | Escape | Inspector |
| Quit | Ctrl+Q | Global |

### Implementation Phases

#### Phase 1: Two Panels, One Loop

**Goal:** Prove both panels can coexist in single event loop with correct focus routing.

**Tasks:**
1. Create `MaggieIDE.mag` with `YutaniFlex` row layout (50/50 split)
2. Left panel: REPL with transcript (`YutaniTextView`) + input (`YutaniTextArea`)
3. Right panel: Inspector with slot list (`YutaniList`) + value view (`YutaniTextView`)
4. Remove all `stopEventLoop` calls except quit
5. Implement Ctrl+Tab to switch focus between panels
6. Prove: typing in REPL only affects REPL, not inspector

**Key Question to Answer:** How does Yutani route key events to focused widget? Options:
- Widget-specific `onKey:` handlers
- Focus-aware global handler that delegates
- Yutani built-in routing

**Source Code to Port:**
- From `MaggieREPL.mag`: `buildUI`, `setupKeyBindings`, transcript/input creation
- From `Inspector.mag`: `buildUI`, slot list creation, value view

**Success Criteria:**
- [x] Two panels render side-by-side
- [x] Ctrl+Tab switches focus between panels
- [x] Keys typed in REPL stay in REPL

---

#### Phase 2: REPL Evaluation + Inspector Drill-Down

**Goal:** Complete the core workflow: evaluate code, inspect result, explore object.

**Tasks:**
1. Implement Ctrl+Enter evaluation (port from `MaggieREPL.evaluateInput`)
2. Store `lastResult` for inspection
3. Implement Ctrl+I to inspect `lastResult` in right panel
4. Port inspector drill-down (Enter on slot) from `Inspector.drillDown`
5. Port inspector go-back (Escape) from `Inspector.goBack`
6. Add evaluation bindings so variables persist: `x := 42` then `x + 1` works
7. Bind `it` to last result automatically

**Source Code to Port:**
- From `MaggieREPL.mag:93-112`: `evaluateInput` method
- From `Inspector.mag:95-122`: `inspectObject:`, `drillDown`, `goBack`

**Success Criteria:**
- [x] Can type code, Ctrl+Enter evaluates, result shown in transcript
- [x] Ctrl+I shows last result in inspector
- [x] Enter on slot drills into nested object
- [x] Escape returns to previous object
- [x] Variables persist across evaluations (`x := 42` then `x + 1` → 43)
- [x] `it` bound to last result

---

#### Phase 3: History + Polish

**Goal:** Add command history and handle errors gracefully.

**Tasks:**
1. Port history navigation from `MaggieREPL.mag:124-141`
2. Handle compilation errors (show in transcript with `!! ` prefix)
3. Handle runtime errors (show in transcript with `!! ` prefix)
4. Handle edge cases:
   - Inspecting `nil` → show "nil (UndefinedObject)", no slots
   - Inspecting primitives → show value, no instance variable slots
   - Empty input → no-op (don't evaluate)

**Success Criteria:**
- [x] Up/Down navigates command history
- [x] Errors displayed in transcript
- [x] Edge cases handled gracefully

---

## What's NOT in MVP (Deferred)

Based on review feedback, these are explicitly deferred:

- **Inspector tabs** - Use object-stack navigation instead (existing pattern)
- **Status bar** - Users can learn shortcuts from docs
- **History limit** - Add only if performance issues arise
- **Transcript truncation** - Add only if performance issues arise
- **Minimum terminal size check** - Let UI break visibly; users will resize
- **Session persistence** - Future consideration
- **Syntax highlighting** - Future consideration

## Acceptance Criteria

### Functional Requirements

- [x] Application launches with two-panel layout
- [x] REPL accepts multi-line code input
- [x] Ctrl+Enter evaluates code and shows result
- [x] Errors displayed in transcript
- [x] Up/Down navigates command history
- [x] Ctrl+I inspects last result
- [x] Inspector shows object class and instance variables
- [x] Enter drills into nested object
- [x] Escape returns to previous object
- [x] Ctrl+Tab switches focus between panels
- [x] Ctrl+Q quits application

### Non-Functional Requirements

- [x] Works over SSH (no Cmd key dependency - using Ctrl)

## Source Code Reference

| Existing File | Lines | What to Port |
|---------------|-------|--------------|
| `lib/yutani/ide/MaggieREPL.mag` | 93-112 | `evaluateInput` method |
| `lib/yutani/ide/MaggieREPL.mag` | 124-141 | History navigation |
| `lib/yutani/ide/MaggieREPL.mag` | 29-51 | UI creation pattern |
| `lib/yutani/ide/Inspector.mag` | 95-122 | Object inspection, drill-down |
| `lib/yutani/ide/Inspector.mag` | 29-58 | UI creation pattern |

## Key Decisions from Review

| Decision | Rationale |
|----------|-----------|
| Replace `MaggieIDE.mag` (not "v2") | Don't version filenames. The old code is broken. |
| Use `TextView` for transcript | Read-only display; `TextArea` is for editable content |
| Object-stack navigation, not tabs | Simpler; existing pattern works; tabs can be added later |
| Evaluation bindings persist | Standard REPL behavior; needed for iterative exploration |
| Port code, don't rewrite | 90% of the logic already exists and works |
