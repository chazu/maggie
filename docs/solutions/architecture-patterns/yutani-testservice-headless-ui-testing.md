---
title: "Yutani TestService: Headless UI Testing Infrastructure"
category: architecture-patterns
tags:
  - yutani
  - grpc
  - testing
  - headless-testing
  - ui-testing
  - tui
  - inject-key
  - wait-for-idle
  - simulation-screen
  - tcell
module: yutani/testservice
severity: medium
date: 2026-01-26
symptoms:
  - "No programmatic way to inject keyboard input for UI testing"
  - "Cannot wait for UI to settle before assertions in headless tests"
  - "Missing gRPC endpoints for test automation"
  - "Need headless testing capability for TUI applications"
  - "Integration tests require simulated user input"
---

# Yutani TestService: Headless UI Testing Infrastructure

## Problem

Yutani's TUI applications needed programmatic testing capabilities. Specifically:
- Inject keyboard input without a real terminal
- Inject text as sequences of keystrokes
- Wait for UI to process all events before assertions
- Determine if a server supports injection (test mode detection)

## Solution Overview

Added a new `TestService` gRPC service with four endpoints:
- `InjectKey` - Inject single keystroke via SimulationScreen
- `InjectText` - Inject string as KEY_RUNE sequence
- `WaitForIdle` - Block until tview processes all events
- `IsTestMode` - Check if server supports injection

## Implementation

### Step 1: Define Proto Service

File: `api/proto/industries/loosh/yutani/v1/test.proto`

```protobuf
syntax = "proto3";
package industries.loosh.yutani.v1;
import "industries/loosh/yutani/v1/types.proto";
option go_package = "github.com/chazu/yutani/pkg/proto/yutani";

service TestService {
  rpc InjectKey(InjectKeyRequest) returns (InjectKeyResponse);
  rpc InjectText(InjectTextRequest) returns (InjectTextResponse);
  rpc WaitForIdle(WaitForIdleRequest) returns (WaitForIdleResponse);
  rpc IsTestMode(IsTestModeRequest) returns (IsTestModeResponse);
}
```

Key: Reuse existing types from `types.proto` (SessionId, Key, Modifier).

### Step 2: Add Server Methods

File: `pkg/server/server.go`

```go
func (s *Server) IsTestMode() bool {
    return s.testMode
}

func (s *Server) GetSimulationScreen() tcell.SimulationScreen {
    if !s.testMode {
        return nil
    }
    if sim, ok := s.screen.(tcell.SimulationScreen); ok {
        return sim
    }
    return nil
}
```

### Step 3: Implement Inverse Key Conversion

File: `pkg/services/test.go`

The existing `event_convert.go` converts tcell → proto. TestService needs the inverse:

```go
func convertProtoKeyToTcell(key pb.Key) tcell.Key {
    switch key {
    case pb.Key_KEY_ENTER:
        return tcell.KeyEnter
    case pb.Key_KEY_TAB:
        return tcell.KeyTab
    // ... complete mapping
    default:
        return tcell.KeyRune
    }
}

func convertProtoModifiersToTcell(mods []pb.Modifier) tcell.ModMask {
    var result tcell.ModMask
    for _, mod := range mods {
        switch mod {
        case pb.Modifier_MOD_SHIFT:
            result |= tcell.ModShift
        case pb.Modifier_MOD_CTRL:
            result |= tcell.ModCtrl
        // ...
        }
    }
    return result
}
```

### Step 4: Implement WaitForIdle Pattern

```go
func (s *TestService) WaitForIdle(ctx context.Context, req *pb.WaitForIdleRequest) (*pb.WaitForIdleResponse, error) {
    // ... validation ...

    done := make(chan struct{})

    // Queue an update - when callback executes, all prior events are processed
    s.server.App().QueueUpdate(func() {
        close(done)
    })

    select {
    case <-done:
        return &pb.WaitForIdleResponse{Success: true, TimedOut: false}, nil
    case <-time.After(time.Duration(timeout) * time.Millisecond):
        return &pb.WaitForIdleResponse{Success: false, TimedOut: true}, nil
    case <-ctx.Done():
        return nil, status.Error(codes.Canceled, "request canceled")
    }
}
```

**Why this works**: `QueueUpdate` adds a function to tview's event queue. When the function executes, all prior events have been processed.

### Step 5: Security Pattern - Test Mode Guard

Every test endpoint starts with:

```go
func (s *TestService) InjectKey(ctx context.Context, req *pb.InjectKeyRequest) (*pb.InjectKeyResponse, error) {
    if !s.server.IsTestMode() {
        return nil, status.Error(codes.FailedPrecondition, "server not in test mode")
    }
    // ... rest of implementation
}
```

This ensures production servers reject all injection attempts.

### Step 6: Register Service in BOTH Servers

**Critical**: Register the TestService in the unified `yutani` server binary:

```go
testService := services.NewTestService(yutaniServer)
pb.RegisterTestServiceServer(grpcServer, testService)
```

### Step 7: CLI Integration

File: `pkg/cli/inject.go`

```go
var injectCmd = &cobra.Command{
    Use:   "inject",
    Short: "Inject keys and text into a test server",
}

// Subcommands: key, text, wait-idle, test-mode
```

Register in `pkg/cli/root.go`:
```go
rootCmd.AddCommand(injectCmd)
```

## Files Created/Modified

| File | Change |
|------|--------|
| `api/proto/.../test.proto` | New - gRPC service definition |
| `pkg/proto/yutani/test.pb.go` | Generated |
| `pkg/proto/yutani/test_grpc.pb.go` | Generated |
| `pkg/services/test.go` | New - Service implementation |
| `pkg/cli/inject.go` | New - CLI commands |
| `pkg/server/server.go` | Added IsTestMode, GetSimulationScreen |
| `cmd/yutani/main.go` | Registered TestService |
| `pkg/cli/root.go` | Added injectCmd |

## Usage

```bash
# Check if server supports injection
yutani inject test-mode -a localhost:7755

# Inject Enter key
yutani inject key -s <session-id> --key enter

# Inject Ctrl+C
yutani inject key -s <session-id> --key c --mod ctrl

# Inject text
yutani inject text -s <session-id> "x := 42"

# Wait for UI to settle
yutani inject wait-idle -s <session-id> --timeout 5000
```

## Gotchas

### 1. SimulationScreen is an Interface

```go
// WRONG - can't use *tcell.SimulationScreen
func (s *Server) GetSimulationScreen() *tcell.SimulationScreen

// CORRECT - it's an interface type
func (s *Server) GetSimulationScreen() tcell.SimulationScreen
```

### 2. Single Binary, Two Modes

The TestService is registered in the unified `yutani` binary and is available in both TUI and headless (`--headless`) modes.

### 3. WaitForIdle Needs Timeout

Without timeout handling, WaitForIdle could block forever if the event loop is stuck.

## Best Practices Demonstrated

1. **Separation of concerns**: TestService is separate from DebugService (read vs write operations)
2. **Security by default**: Test mode check on every endpoint
3. **Consistent CLI patterns**: Same flags (`-a`, `-s`, `-f`) as existing commands
4. **JSON output support**: `--format json` for programmatic use
5. **Inverse conversion pattern**: Keep bidirectional type conversion in dedicated functions

## Pattern: Adding a New gRPC Service to Yutani

1. Create `api/proto/.../service.proto` importing `types.proto`
2. Run `make proto` to generate Go code
3. Create `pkg/services/service.go` with implementation
4. Add any needed server methods to `pkg/server/server.go`
5. Register in the unified `yutani` server (`cmd/yutani/main.go`)
6. Create CLI commands in `pkg/cli/`
7. Register CLI in `pkg/cli/root.go`
8. Build and test: `make build && go test ./...`

## Related

- Yutani DebugService (read-only inspection)
- tcell SimulationScreen documentation
- tview QueueUpdate pattern
