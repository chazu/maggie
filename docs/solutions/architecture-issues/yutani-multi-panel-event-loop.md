---
title: "Yutani multi-panel apps fail when tools call stopEventLoop"
category: architecture-issues
tags:
  - yutani
  - tui
  - event-loop
  - session-management
  - multi-panel
  - ide
symptoms:
  - "Cannot have multiple tools visible simultaneously"
  - "Switching tools kills the session"
  - "Menu launcher works but tool launches terminate the app"
  - "stopEventLoop called unexpectedly"
module: lib/yutani/ide
date: 2026-01-25
---

# Yutani Multi-Panel Apps Fail When Tools Call stopEventLoop

## Problem

When building multi-panel Yutani TUI applications (like an IDE with REPL and Inspector side-by-side), the app fails because individual tools call `session stopEventLoop` when they finish. This terminates the entire event loop, killing the session.

### Symptoms

- Cannot switch between IDE tools without app termination
- Menu launcher renders but selecting a tool kills everything
- Multi-panel layouts are impossible
- Session disconnects unexpectedly

### Root Cause

The broken pattern treats each tool as a standalone application that owns the event loop:

```smalltalk
"BROKEN: Tool launcher that kills the session"
method: launchREPL [
    statusBar text: 'Launching REPL...'.
    session stopEventLoop.    "← KILLS THE SESSION!"
    MaggieREPL openIn: session
]
```

When `stopEventLoop` is called, the Yutani event loop terminates. Any subsequent `session run` starts a new loop, but the original UI context is lost.

## Solution

### The Three Rules

1. **One Session → One Event Loop** - Don't stop and restart
2. **stopEventLoop Only in close** - The quit handler, nowhere else
3. **Combine Tools by Layout, Not by Launching** - Embed as widgets, don't spawn as apps

### Architecture Change

Replace sequential tool launching with concurrent panel embedding:

```smalltalk
"WORKING: Two-panel layout with single event loop"
MaggieIDE subclass: Object
  instanceVars: session mainFlex replPanel inspectorPanel focusedPanel

  method: buildUI [
      mainFlex := session createFlex.
      mainFlex direction: #row.           "← Horizontal split"

      self buildReplPanel.                "← Build left panel"
      self buildInspectorPanel.           "← Build right panel"

      mainFlex addItem: replPanel proportion: 1.
      mainFlex addItem: inspectorPanel proportion: 1.

      self setupKeyBindings
  ]

  method: open [
      session setRoot: mainFlex.
      inputArea focus.
      session run                         "← ONE event loop for entire app"
  ]

  method: close [
      session stopEventLoop.              "← ONLY place stopEventLoop is called"
      session disconnect
  ]
```

### Focus-Aware Key Routing

Track which panel has focus and route events accordingly:

```smalltalk
method: setupKeyBindings [
    session onKey: [:event |
        "Global shortcuts (work anywhere)"
        event isCtrl ifTrue: [
            event rune = 'q' ifTrue: [^self close].
            event rune = 'i' ifTrue: [^self inspectLastResult].
            event isTab ifTrue: [^self switchPanel]
        ].

        "Panel-specific shortcuts"
        focusedPanel = #repl ifTrue: [
            event isUp ifTrue: [^self historyPrevious].
            event isDown ifTrue: [^self historyNext]
        ].

        focusedPanel = #inspector ifTrue: [
            event isEnter ifTrue: [^self drillDown].
            event isEscape ifTrue: [^self goBack]
        ]
    ]
]

method: switchPanel [
    focusedPanel = #repl
        ifTrue: [
            focusedPanel := #inspector.
            slotList focus.
            inspectorPanel title: 'Inspector *'.
            replPanel title: 'REPL'
        ]
        ifFalse: [
            focusedPanel := #repl.
            inputArea focus.
            replPanel title: 'REPL *'.
            inspectorPanel title: 'Inspector'
        ]
]
```

## Prevention

### Red Flags to Watch For

1. **`stopEventLoop` in any method except `close`** - Almost always wrong
2. **Multiple `session run` calls** - Should only be one per app lifecycle
3. **Tools that take over the session** - Embed as panels instead
4. **Creating new sessions for sub-tools** - Reuse the parent session

### Checklist for Multi-Panel Yutani Apps

- [ ] Single `session run` call in `open` method
- [ ] Single `stopEventLoop` call in `close` method only
- [ ] All panels are widgets added to a parent `YutaniFlex`
- [ ] Focus state tracked with instance variable
- [ ] Global shortcuts handled before panel-specific ones
- [ ] Panel switching updates focus and visual indicator

### Layout Pattern

```
YutaniFlex (root, direction: #row)
├── YutaniFlex (leftPanel, direction: #column)
│   ├── Widget1 (proportion: N)
│   └── Widget2 (proportion: M)
└── YutaniFlex (rightPanel, direction: #column)
    ├── Widget3 (proportion: X)
    └── Widget4 (proportion: Y)
```

## Related

- Working implementation: `lib/yutani/ide/MaggieIDE.mag`
- Standalone tool pattern (for reference): `lib/yutani/ide/MaggieREPL.mag`
- Yutani debugging: See CLAUDE.md section "Debugging Yutani TUI Applications"

## Debugging Commands

```bash
# See widget hierarchy and focus
yutani debug bounds -s <session-id>

# Inspect specific widget state
yutani debug widget -s <session-id> -w <widget-id>

# ASCII screen dump with widget overlay
yutani debug screen -s <session-id> --bounds --legend
```

Session ID is printed when app starts:
```
YutaniSession: session created with ID: f1e6eb39-...
```
