# Brainstorm: Smalltalk-Style IDE Refactor

**Date:** 2026-01-29
**Status:** Ready for planning

## What We're Building

A traditional Smalltalk-style IDE for Maggie, replacing the current REPL+Inspector two-panel layout with a windowed desktop environment. The IDE consists of:

1. **Scratchpad windows** — mini text editors where you select text (Emacs-style: Ctrl+Space to set mark, readline navigation to extend selection) and evaluate it with Smalltalk keybinds (Ctrl+D do it, Ctrl+P print it, Ctrl+I inspect it). Results print on the line below the evaluated expression. Each scratchpad has its own lexical scope layered over global Compiler scope.

2. **Inspector windows** — floating windows that pop up on Ctrl+I, showing slot names/values with a mini evaluator at the bottom where `self` refers to the inspected object. In-place navigation (drill into a slot, back out) within a single inspector window.

3. **Menu bar** — a proper Yutani MenuBar widget at the top of the screen, with menus for creating new scratchpads, quitting, and future IDE actions.

4. **Desktop shell** — a WindowManager-based environment. Launches to an empty desktop with a menu bar. User creates scratchpads from the menu.

## Why This Approach

The current MaggieIDE is a fixed two-panel REPL that evaluates whole lines. A Smalltalk workspace model is fundamentally more powerful:

- **Select-and-evaluate** lets you keep scratch notes, examples, and working code in the same buffer
- **Multiple scratchpads** let you work on different things simultaneously
- **Floating inspectors** let you compare objects side-by-side
- **Lexical scoping per scratchpad** prevents namespace collisions while still accessing globals
- **Windowed desktop** is extensible — class browsers, debuggers, etc. can be added later

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Text selection | New Yutani TextArea feature | Selection is fundamental enough to be a framework primitive, not an app-level hack |
| Selection mechanics | Emacs-style (Ctrl+Space mark, readline nav) | Consistent with terminal conventions, muscle memory |
| Evaluation keybinds | Smalltalk classic (Ctrl+D/P/I) | Standard Smalltalk vocabulary, well-understood |
| Scratchpad scope | Per-scratchpad local scope over global | Isolation without sacrificing utility — locals shadow globals, globals readable everywhere |
| Inspector placement | Separate floating windows | Multiple inspectors open simultaneously, compare objects |
| Inspector navigation | In-place drill-down/back | Lighter weight than spawning windows per slot; Ctrl+I on a slot can still open new inspector if needed later |
| Menu bar | New Yutani MenuBar/MenuItem widget | Proper widget, not a fake built from Buttons+List |
| Startup state | Empty desktop + menu bar | User creates scratchpads from menu — clean, intentional |
| Sequencing | Bottom-up (Yutani primitives first) | Build selection API and menu bar in Yutani, then build IDE on solid foundations |

## Architecture Sketch

```
+-----------------------------------------------------------+
| File  Edit  Tools                          [Menu Bar]      |
+-----------------------------------------------------------+
|                                                            |
|  +-- Scratchpad 1 --------+   +-- Inspector: anArray --+  |
|  | x := Array new: 5.     |   | Slots:                 |  |
|  | x at: 1 put: 'hello'.  |   |   class: Array         |  |
|  | x size                  |   |   size: 5              |  |
|  | => 5                    |   |   1: 'hello'           |  |
|  |                         |   |   2: nil               |  |
|  |                         |   |   ...                  |  |
|  |                         |   +-------------------------+  |
|  +-------------------------+   | self> self reversed     |  |
|                                +-------------------------+  |
|                                                            |
+-----------------------------------------------------------+
```

## Phased Delivery

### Phase 1: Yutani TextArea Selection API
- Cursor position tracking (line, column)
- Mark/selection range (start, end)
- `SetMark`, `ClearMark`, `GetSelectedText` RPCs
- Visual highlight of selected region (using existing highlight infrastructure)
- Modifier-aware key events already supported

### Phase 2: Yutani MenuBar Widget
- `WIDGET_MENU_BAR` type — horizontal bar of menu titles
- `WIDGET_MENU` type — dropdown list of items (supports separators, submenus later)
- `WIDGET_MENU_ITEM` type — clickable item with optional keyboard shortcut display
- Events: `MENU_ITEM_SELECTED` with menu path info
- Auto-close on selection or click-away

### Phase 3: Maggie IDE — Scratchpad
- `ScratchpadComponent` class wrapping a Yutani TextArea
- Emacs keybind layer: Ctrl+Space (set mark), Ctrl+F/B/N/P/A/E (navigation), Ctrl+W (cut), Alt+W (copy)
- Eval keybinds: Ctrl+D (do it), Ctrl+P (print it — inserts result below selection), Ctrl+I (inspect it — opens inspector)
- Per-scratchpad `ScratchpadEnvironment` — local variable dictionary, falls through to `Compiler` globals for reads
- Evaluation: `Compiler evaluate: selectedText in: environment`

### Phase 4: Maggie IDE — Inspector Window
- `InspectorWindow` class — floating Window containing:
  - Top: slot list (List widget showing name: value pairs)
  - Bottom: mini TextArea evaluator where `self` = inspected object
- In-place navigation: Enter on slot drills in (pushes to object stack), Escape backs out
- `Compiler evaluate: expr in: inspectedObject` for the mini evaluator

### Phase 5: Maggie IDE — Desktop Shell
- `MaggieDesktop` class — WindowManager + MenuBar
- Menu structure: File > New Scratchpad, Quit | Edit > (future) | Tools > (future)
- Keyboard shortcuts: Ctrl+N (new scratchpad), Ctrl+Q (quit)
- Window management: focus tracking, z-order via WindowManager

## Open Questions

1. **Clipboard integration** — Should Ctrl+W/Alt+W (cut/copy) interact with system clipboard, or only an internal paste buffer? Terminal clipboard access is inconsistent.
2. **Undo in scratchpad** — Should the TextArea support undo/redo? Not currently a Yutani feature.
3. **Saving scratchpad content** — Should scratchpads be persistable to files? Or are they ephemeral?
4. **Multiple inspectors from same scratchpad** — When Ctrl+I is pressed multiple times, always new window? Or reuse existing inspector window?
5. **TextArea line manipulation** — Does Yutani TextArea need an "insert text at position" RPC for printing eval results on the next line?

## Dependencies

- Yutani Window/WindowManager: **Already implemented**
- Yutani modifier key support: **Already implemented**
- Yutani TextArea selection: **Needs building** (Phase 1)
- Yutani MenuBar widget: **Needs building** (Phase 2)
- Maggie Compiler evaluate:in: with custom scope: **May need extension** for per-scratchpad environments
