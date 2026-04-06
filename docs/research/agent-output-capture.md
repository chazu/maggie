# Research: Robust Agent Output Capture for PP Harness

**Date:** 2026-04-06
**Scout:** agent-output-capture

## Summary

The PP harness spawns Claude Code CLI agents via Maggie's `ExternalProcess` (Go `os/exec`). Currently, agent output is captured into in-memory `bytes.Buffer` and only accessible after the process completes. The harness already computes a per-task output file path (`~/.pp/sessions/<taskId>.jsonl`) but **never actually uses it** — the variable `outputFile` is assigned but not wired into the process invocation. This document investigates how to close that gap.

---

## 1. How ExternalProcess Captures stdout/stderr Today

**File:** `vm/exec_primitives.go`

The `ExternalProcessObject` struct uses Go's `os/exec.Cmd` with `bytes.Buffer`:

```go
var stdout, stderr bytes.Buffer
c.Stdout = &stdout
c.Stderr = &stderr
```

**Key characteristics:**
- Output is buffered entirely in memory until the process exits
- For synchronous `run`, output is available via `p.stdout` / `p.stderr` after `c.Run()` returns
- For async `start`, a background goroutine calls `c.Wait()` and captures output after completion
- **No streaming** — there's no way to see output in real time while the agent is running
- **No tee** — output goes only to the buffer, not to the console

**Implication:** A 30-minute agent session with verbose output (including `stream-json` events) could accumulate significant memory. More importantly, if the agent is killed (timeout, OOM), partial output is lost because `bytes.Buffer` contents are only captured on clean Wait().

### Can We Tee to Console + File?

Yes, using Go's `io.MultiWriter`. The `Cmd.Stdout` and `Cmd.Stderr` fields accept any `io.Writer`:

```go
logFile, _ := os.Create(outputPath)
c.Stdout = io.MultiWriter(os.Stdout, logFile)
c.Stderr = io.MultiWriter(os.Stderr, logFile)
```

This would require changes to `exec_primitives.go` — see recommendations.

---

## 2. Go Libraries for Robust Process Output Capture

### Standard Library (sufficient for our needs)

| Approach | Use Case | Streams in Real Time? |
|---|---|---|
| `Cmd.Stdout = &bytes.Buffer{}` | Current approach. Simple. | No |
| `Cmd.StdoutPipe()` | Returns `io.ReadCloser`. Manual read loop. | Yes |
| `io.MultiWriter(w1, w2)` | Fan-out to multiple writers simultaneously | Yes |
| `io.TeeReader(r, w)` | Read from r, copy to w as a side effect | Yes |

### Recommended Pattern: MultiWriter

```go
// Open log file
logFile, err := os.Create(filepath.Join(sessionsDir, taskId+".jsonl"))
if err != nil { ... }
defer logFile.Close()

// Tee stdout to both console and file
c.Stdout = io.MultiWriter(os.Stdout, logFile)
c.Stderr = io.MultiWriter(os.Stderr, logFile)
```

This is the simplest approach — no goroutines, no pipes, no buffering issues. Output flows to both destinations in real time. If the process is killed, the file contains everything written up to that point (OS-level buffering aside; use `bufio.Writer` with periodic flush if needed).

### Alternative: StdoutPipe + Scanner (for line-by-line processing)

```go
pipe, _ := c.StdoutPipe()
scanner := bufio.NewScanner(pipe)
c.Start()
for scanner.Scan() {
    line := scanner.Text()
    fmt.Println(line)          // echo to console
    logFile.WriteString(line)  // write to file
    // Could also parse JSON events here
}
c.Wait()
```

More complex but allows per-line processing (e.g., parsing `stream-json` events, extracting tool calls).

---

## 3. Claude Code `--output-format stream-json`

The Claude CLI supports structured JSON streaming output:

```
claude -p --output-format stream-json "your prompt"
```

**Key flags:**
- `--output-format stream-json` — emits one JSON object per line for every event
- `--include-partial-messages` — include partial message chunks as they arrive (only with `stream-json`)
- `-p` / `--print` — non-interactive mode (required for `--output-format`)

**What stream-json emits** (based on inspection of Claude's session JSONL files):

Each line is a JSON object with fields like:
- `type` — `"user"`, `"assistant"`, `"progress"`, `"file-history-snapshot"`, etc.
- `message.content` — array of content blocks (text, tool_use, tool_result)
- `timestamp`, `sessionId`, `version`, `gitBranch`, `cwd`
- For tool calls: `tool_use_id`, tool name, input parameters
- For tool results: `content`, `is_error`

**This is ideal for our use case.** By adding `--output-format stream-json` to the harness args, we get:
1. Every tool call and response as structured JSON
2. All text output from the agent
3. Timestamps for each event
4. Session IDs for cross-referencing

**Current harness args** (from `ClaudeHarness.mag`):
```
--system-prompt <prompt> --allowedTools Bash,Read,Write,Edit,Glob,Grep --max-turns 200 -p <taskDescription>
```

**Proposed addition:**
```
--output-format stream-json
```

**Trade-off:** With `stream-json`, stdout becomes JSON rather than human-readable text. If we want both console-readable output AND a structured log file, we have two options:
1. Use `stream-json` and pipe to file only (console gets nothing or a progress summary)
2. Use `text` output for console, and rely on Claude's own session persistence for structured data

---

## 4. Existing Output Capture in the PP Harness

**File:** `src/harness/ClaudeHarness.mag` (latest version from story-1775498619-1606)

The harness already has **scaffolding** for output capture that was never completed:

```smalltalk
"Ensure sessions directory exists for transcript capture"
home := System env: 'HOME'.
(home isNil or: [home isEmpty]) ifTrue: [home := '/tmp'].
sessionsDir := home, '/.pp/sessions'.
Shell run: 'mkdir -p "', sessionsDir, '"'.
outputFile := sessionsDir, '/', taskId, '.jsonl'.
```

**The `outputFile` variable is computed but never used.** It's not passed to the process args, not used as a redirect target, and not referenced anywhere else in the method. This was clearly intended to be wired up but left incomplete.

The current process is spawned via:
```smalltalk
process := ExternalProcess command: 'timeout'.
process args: timeoutArgs.
process env: env.
process dir: workDir.
process run.
```

After `run`, only `process isSuccess`, `process exitCode`, and `process stderr` are checked. The `process stdout` (which would contain the full agent output) is never saved to disk.

---

## 5. Claude CLI `--resume` and Session Persistence

### Built-in Session Persistence

Claude Code automatically persists sessions to `~/.claude/projects/<project-hash>/<session-id>.jsonl`. These files contain the **complete conversation transcript** including all tool calls, responses, and text — exactly the structured data we want.

**Session files observed:**
- `~/.claude/sessions/<pid>.json` — session metadata (pid, sessionId, cwd, startedAt)
- `~/.claude/projects/<hash>/<uuid>.jsonl` — full conversation transcript

### The `--resume` Flag

```
-r, --resume [value]    Resume a conversation by session ID
--session-id <uuid>     Use a specific session ID (must be valid UUID)
--fork-session          Create new session ID when resuming
```

### The `--no-session-persistence` Flag

```
--no-session-persistence    Disable session persistence (only with --print)
```

By default, even `-p` mode persists sessions. This means **Claude is already saving structured transcripts** for every agent invocation. The question is whether we want to:
- (a) Rely on Claude's built-in persistence and just record the session ID
- (b) Capture our own copy via `stream-json` output

### Using `--session-id` for Deterministic Session Tracking

We can pass `--session-id <uuid>` to control the session ID. If we use the PP task ID (which is already a string), we could generate a deterministic UUID from it:

```smalltalk
args := args copyWith: '--session-id'.
args := args copyWith: taskId.  "Must be valid UUID format"
```

Then the transcript is always at `~/.claude/projects/<hash>/<taskId>.jsonl`.

**Caveat:** `--session-id` requires a valid UUID. PP task IDs like `story-1775498619-1606` are not UUIDs. We'd need to either generate a UUID per task or hash the task ID into UUID format.

---

## 6. Recommendations

### Approach A: Minimal — Use Claude's Built-in Persistence (Recommended)

**Effort: Low. No Go changes needed.**

Claude already saves full transcripts. We just need to:

1. **Generate a UUID per task** and pass `--session-id <uuid>` to the Claude CLI
2. **Record the session-id → task-id mapping** in the BBS or a local index file
3. **After the agent completes**, copy or symlink the Claude transcript to `~/.pp/sessions/<taskId>.jsonl`

Changes to `ClaudeHarness.mag` only:
```smalltalk
"Generate a session UUID (could use a primitive or shell uuidgen)"
sessionUuid := (ExternalProcess run: 'uuidgen' args: #()) trim.

args := args copyWith: '--session-id'.
args := args copyWith: sessionUuid.

"After process completes, copy the transcript"
| claudeProjectDir transcriptSrc |
claudeProjectDir := home, '/.claude/projects/...'.  "Need project hash"
transcriptSrc := claudeProjectDir, '/', sessionUuid, '.jsonl'.
Shell run: 'cp "', transcriptSrc, '" "', outputFile, '"'.
```

**Pros:** No Go code changes, full structured data, tool calls included.
**Cons:** Depends on Claude's internal storage format (could change), need to find the project hash directory, transcript only complete after session ends.

### Approach B: Stream-JSON to File (Recommended for Real-Time Capture)

**Effort: Medium. Requires Go changes to ExternalProcess.**

1. **Add `--output-format stream-json` to the Claude args** in `ClaudeHarness.mag`
2. **Add a new ExternalProcess method** (Go primitive) that supports output teeing:

Recommended new primitive on ExternalProcess:
```smalltalk
process logTo: '/path/to/file.jsonl'.  "Tee stdout to this file"
```

Go implementation sketch:
```go
epClass.AddMethod1(vm.Selectors, "logTo:", func(vmPtr interface{}, recv, pathVal Value) Value {
    v := vmPtr.(*VM)
    p := v.vmGetExtProcess(recv)
    if p == nil { return Nil }
    p.logPath = v.valueToString(pathVal)
    return recv
})
```

Then in the `run` method, when `logPath` is set:
```go
if p.logPath != "" {
    logFile, err := os.Create(p.logPath)
    if err == nil {
        defer logFile.Close()
        c.Stdout = io.MultiWriter(&stdout, logFile)
        c.Stderr = io.MultiWriter(&stderr, logFile)
    }
}
```

Harness usage:
```smalltalk
args := args copyWith: '--output-format'.
args := args copyWith: 'stream-json'.

process := ExternalProcess command: 'timeout'.
process args: timeoutArgs.
process env: env.
process dir: workDir.
process logTo: outputFile.   "NEW: tee to file"
process run.
```

**Pros:** Real-time capture, survives crashes/kills, structured JSON with tool calls.
**Cons:** Console output becomes JSON (need separate progress reporting), requires Go primitive change.

### Approach C: Shell-Level Tee (Quick Hack, No Go Changes)

**Effort: Very Low. Maggie-only change.**

Instead of `timeout 1800 claude ...`, use:
```
timeout 1800 sh -c 'claude ... 2>&1 | tee /path/to/file.jsonl'
```

Or even simpler, just redirect:
```
timeout 1800 sh -c 'claude ... > /path/to/file.jsonl 2>&1'
```

Harness change (Maggie only):
```smalltalk
| shellCmd |
shellCmd := 'claude --output-format stream-json ... > "', outputFile, '" 2>&1'.
timeoutArgs := Array new: 0.
timeoutArgs := timeoutArgs copyWith: '1800'.
timeoutArgs := timeoutArgs copyWith: 'sh'.
timeoutArgs := timeoutArgs copyWith: '-c'.
timeoutArgs := timeoutArgs copyWith: shellCmd.
```

**Pros:** Zero Go changes, works today.
**Cons:** Loses separate stdout/stderr, shell escaping complexity, `ExternalProcess stdout` will be empty (all went to file).

### Recommended Path

**Start with Approach C** (shell-level redirect) as an immediate fix — it requires zero Go changes and wires up the already-computed `outputFile`. Then evolve to **Approach B** (native `logTo:` primitive) for production quality.

The key insight is that `--output-format stream-json` gives us structured data including tool calls, and `io.MultiWriter` is the clean Go pattern for splitting output to multiple destinations.

### File Output Format

Regardless of approach, the output file at `~/.pp/sessions/<taskId>.jsonl` should contain:
- One JSON object per line (JSONL format)
- Each line includes: event type, timestamp, content
- Tool calls with their inputs and outputs
- Agent text responses
- Error messages and exit status

This enables:
- Post-mortem debugging of failed agents
- Cost tracking (count API calls)
- Audit trail of what each agent did
- Replay/analysis tooling

---

## Observations

1. **Dead code in ClaudeHarness:** The `outputFile` variable (line 56) is computed but never used. This is the clearest quick-win — just wire it up.

2. **Claude's native session persistence** already captures everything we need. The transcripts at `~/.claude/projects/<hash>/<session-id>.jsonl` contain full tool call/response data. We could just index these by task ID.

3. **`--output-format stream-json`** is the cleanest way to get structured output. Combined with `io.MultiWriter` in Go, this gives us real-time, crash-safe, structured logging.

4. **ExternalProcess lacks streaming output.** The current `bytes.Buffer` approach means no real-time visibility and potential memory issues for long-running agents. Adding a `logTo:` or `tee:` method would be generally useful beyond just the PP harness.

5. **The `--session-id` flag** could let us use deterministic session IDs tied to task IDs, making it trivial to find Claude's built-in transcript for any task. But task IDs aren't UUIDs, so a mapping is needed.
