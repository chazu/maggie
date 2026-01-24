## Concurrency Primitives

Maggie provides Go-style concurrency primitives. All are fully implemented.

### Channel (Go-style communication)

```smalltalk
ch := Channel new.          "Unbuffered channel"
ch := Channel new: 5.       "Buffered channel with capacity 5"

ch send: value.             "Blocking send"
ch receive.                 "Blocking receive"
ch trySend: value.          "Non-blocking send (returns true/false)"
ch tryReceive.              "Non-blocking receive (returns value or nil)"
ch close.                   "Close channel"
ch isClosed.                "Check if closed"
ch isEmpty.                 "Check if empty"
ch size.                    "Number of buffered items"
ch capacity.                "Buffer capacity"
```

### Channel Select (multiplexed channel operations)

```smalltalk
"Wait on multiple channels"
result := Channel select: {
    ch1 onReceive: [:v | 'Got from ch1: ', v].
    ch2 onReceive: [:v | 'Got from ch2: ', v]
}.

"Non-blocking with default"
result := Channel select: {
    ch onReceive: [:v | 'Received: ', v]
} ifNone: [
    'No channel ready'
].
```

### Process (lightweight goroutines)

```smalltalk
proc := [expression] fork.         "Fork block, returns Process"
proc := [expr] forkWith: arg.      "Fork with argument"
proc := [:ctx | ...] forkWithContext: ctx.  "Fork with cancellation context"

proc wait.                         "Block until complete, returns result"
proc isDone.                       "Check if finished"
proc result.                       "Get result (nil if not done)"

Process current.                   "Current process"
Process yield.                     "Yield to other goroutines"
Process sleep: milliseconds.       "Sleep"
```

**fork vs forkWithResult**: `fork` treats non-local returns (^) as local returns within the forked process—they don't escape. This prevents crashes from NLRs crossing goroutine boundaries.

### Mutex (mutual exclusion)

```smalltalk
mutex := Mutex new.
mutex lock.                        "Acquire lock (blocks)"
mutex unlock.                      "Release lock"
mutex tryLock.                     "Non-blocking (returns true/false)"
mutex isLocked.                    "Check if locked"
mutex critical: [protected code]. "Execute block while holding lock"
```

### WaitGroup (synchronization barrier)

```smalltalk
wg := WaitGroup new.
wg add: count.                     "Add to counter"
wg done.                           "Decrement counter"
wg wait.                           "Block until counter reaches zero"
wg count.                          "Current counter value"
wg wrap: [block].                  "Convenience: add 1, fork, auto-done"
```

### Semaphore (counting permits)

```smalltalk
sem := Semaphore new.              "Binary semaphore (1 permit)"
sem := Semaphore new: 3.           "Semaphore with 3 permits"

sem acquire.                       "Acquire permit (blocks)"
sem release.                       "Release permit"
sem tryAcquire.                    "Non-blocking (returns true/false)"
sem available.                     "Number of available permits"
sem capacity.                      "Total capacity"
sem critical: [block].             "Execute while holding permit"
```

### CancellationContext (timeouts and cancellation)

```smalltalk
ctx := CancellationContext background.      "Never-cancelled base"
ctx := CancellationContext withCancel.      "Cancellable context"
ctx := CancellationContext withTimeout: ms. "Timeout in milliseconds"

ctx cancel.                        "Cancel this context"
ctx isCancelled.                   "Check if cancelled"
ctx isDone.                        "Alias for isCancelled"
ctx hasDeadline.                   "Check if has timeout"
ctx deadline.                      "Deadline in milliseconds"
ctx remainingTime.                 "Milliseconds until deadline"
ctx wait.                          "Block until cancelled"
ctx doneChannel.                   "Channel that closes on cancel"

"Child contexts inherit parent cancellation"
child := parent withCancel.
child := parent withTimeout: 500.

"Fork with context"
[:context |
    [context isCancelled not] whileTrue: [work]
] forkWithContext: ctx.
```

---

## Issue Tracking

We use bd (beads) for issue tracking instead of Markdown TODOs or external tools.

### Quick Reference

```bash
# Find ready work (no blockers)
bd ready --json

# Find ready work including future deferred issues
bd ready --include-deferred --json

# Create new issue
bd create "Issue title" -t bug|feature|task -p 0-4 -d "Description" --json

# Create issue with due date and defer (GH#820)
bd create "Task" --due=+6h              # Due in 6 hours
bd create "Task" --defer=tomorrow       # Hidden from bd ready until tomorrow
bd create "Task" --due="next monday" --defer=+1h  # Both

# Update issue status
bd update <id> --status in_progress --json

# Update issue with due/defer dates
bd update <id> --due=+2d                # Set due date
bd update <id> --defer=""               # Clear defer (show immediately)

# Link discovered work
bd dep add <discovered-id> <parent-id> --type discovered-from

# Complete work
bd close <id> --reason "Done" --json

# Show dependency tree
bd dep tree <id>

# Get issue details
bd show <id> --json

# Query issues by time-based scheduling (GH#820)
bd list --deferred              # Show issues with defer_until set
bd list --defer-before=tomorrow # Deferred before tomorrow
bd list --defer-after=+1w       # Deferred after one week from now
bd list --due-before=+2d        # Due within 2 days
bd list --due-after="next monday" # Due after next Monday
bd list --overdue               # Due date in past (not closed)
```

### Workflow

1. **Check for ready work**: Run `bd ready` to see what's unblocked
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work**: If you find bugs or TODOs, create issues:
   - `bd create "Found bug in auth" -t bug -p 1 --json`
   - Link it: `bd dep add <new-id> <current-id> --type discovered-from`
5. **Complete**: `bd close <id> --reason "Implemented"`
6. **Export**: Run `bd export -o .beads/issues.jsonl` before committing

### Issue Types

- `bug` - Something broken that needs fixing
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature composed of multiple issues
- `chore` - Maintenance work (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (nice-to-have features, minor bugs)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Dependency Types

- `blocks` - Hard dependency (issue X blocks issue Y)
- `related` - Soft relationship (issues are connected)
- `parent-child` - Epic/subtask relationship
- `discovered-from` - Track issues discovered during work

Only `blocks` dependencies affect the ready work queue.
