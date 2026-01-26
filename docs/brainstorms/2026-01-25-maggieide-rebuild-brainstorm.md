# MaggieIDE Rebuild Brainstorm

**Date:** 2026-01-25
**Status:** Ready for planning

## What We're Building

A functional MaggieIDE with two core components:

1. **REPL/Workspace** (left panel) - Evaluate Maggie expressions, see results
2. **Object Inspector** (right panel) - Inspect objects with tabs/collapsible sections for multiple inspected objects

### Core Workflow

1. User types Maggie code in REPL
2. Evaluates with keyboard shortcut (e.g., Cmd+Enter or similar)
3. Result displays in REPL transcript
4. User selects result, hits inspect shortcut (e.g., Cmd+I)
5. Inspector panel shows object details in a new tab/section

## Why This Approach

**Problem:** Current MaggieIDE doesn't work at all—the menu launcher doesn't navigate anywhere, widgets don't render or respond properly. It needs a significant rework, not incremental fixes.

**Approach:** Test-Driven Rebuild

- Write integration tests for expected behaviors first
- Use Yutani's DebugService to verify widget state (`yutani debug screen`, `yutani debug widget`)
- Let failing tests guide the implementation
- Port working pieces from existing code incrementally

**Why TDD here:**
- DebugService already exists and can inspect widget state
- Catches regressions as we rebuild
- Documents expected behavior explicitly
- Provides confidence during ongoing development

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| MVP scope | REPL + Inspector only | Smallest useful unit; class browser can come later |
| Layout | Fixed side-by-side split | REPL left, Inspector right; simpler than floating windows |
| Inspector organization | Tabs or collapsible sections | Multiple objects can be inspected simultaneously |
| Inspect trigger | Keyboard shortcut on selection | Classic Smalltalk workflow |
| Development approach | Test-driven with DebugService | Validates each layer works; catches regressions |
| Code reuse | Iterative port | Start minimal, bring over working pieces incrementally |

## Technical Notes

### Yutani Layout Widgets Available

- `YutaniFlex` - Flexbox (use `direction: #row` for horizontal split)
- `YutaniPages` - Tabbed content (for inspector tabs)
- `YutaniBox` - Container with optional border/title

### Current Pain Points to Address

1. Event loop management—tools currently call `stopEventLoop`, preventing multi-panel
2. Widget rendering issues—need to validate basic rendering works
3. Keyboard event handling—need reliable key capture for shortcuts

### Testing Strategy

Use `yutani debug` commands to verify:
- Widget hierarchy is correct
- Focus is on expected widget
- Text content matches expected output
- Events are being received

Example test flow:
```bash
# Start MaggieIDE, get session ID
# Verify REPL panel exists and has focus
yutani debug bounds -s <session-id>

# Type in REPL, verify text appears
yutani debug widget -s <session-id> -w <repl-widget-id>

# Evaluate, verify result in transcript
# Select result, trigger inspect
# Verify inspector panel has new tab with object details
```

## Open Questions

1. **What keybindings?** Need to decide exact shortcuts (Cmd+Enter to eval, Cmd+I to inspect, etc.)
2. **Inspector depth?** How deep should object inspection go? (slots only, or nested?)
3. **REPL history?** Up/down arrow for command history?

## Next Steps

Run `/workflows:plan` to create implementation plan with specific test cases and milestones.
