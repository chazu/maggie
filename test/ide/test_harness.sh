#!/usr/bin/env bash
# test_harness.sh -- Reusable functions for headless Yutani IDE tests.
# Source this file from individual test scripts.

set -euo pipefail

PROJ_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
YUTANI_BIN="${YUTANI_BIN:-yutani}"
MAG_BIN="${MAG_BIN:-${PROJ_ROOT}/mag}"
YUTANI_PID=""
MAG_PID=""
TEST_PORT="${TEST_PORT:-17755}"
TEST_ADDR="localhost:${TEST_PORT}"
PASS_COUNT=0
FAIL_COUNT=0
TEST_NAME="${TEST_NAME:-unknown}"

# --- Cleanup ---

_cleanup() {
    stop_maggie 2>/dev/null || true
    stop_yutani 2>/dev/null || true
}
trap _cleanup EXIT

# --- Server management ---

start_yutani_headless() {
    local port="${1:-$TEST_PORT}"
    TEST_PORT="$port"
    TEST_ADDR="localhost:${port}"
    "$YUTANI_BIN" server --headless --address ":${port}" &>/dev/null &
    YUTANI_PID=$!
    # Wait for server to be ready
    local retries=30
    while ! "$YUTANI_BIN" ping --address "$TEST_ADDR" &>/dev/null; do
        retries=$((retries - 1))
        if [ "$retries" -le 0 ]; then
            echo "FAIL: Yutani server did not start on port $port"
            return 1
        fi
        sleep 0.1
    done
}

stop_yutani() {
    if [ -n "$YUTANI_PID" ] && kill -0 "$YUTANI_PID" 2>/dev/null; then
        kill "$YUTANI_PID" 2>/dev/null || true
        wait "$YUTANI_PID" 2>/dev/null || true
    fi
    YUTANI_PID=""
}

# --- Maggie management ---

start_maggie_tool() {
    local tool="$1"
    local addr="${2:-$TEST_ADDR}"
    "$MAG_BIN" --yutani --ide-tool "$tool" --yutani-addr "$addr" &>/dev/null &
    MAG_PID=$!
    # Wait for a session to appear (tool needs to connect and build UI)
    local retries=20
    local sid=""
    while [ -z "$sid" ] && [ "$retries" -gt 0 ]; do
        sleep 0.3
        sid=$(get_session "$addr" 2>/dev/null) || true
        retries=$((retries - 1))
    done
    # Wait for idle to ensure UI is fully rendered
    if [ -n "$sid" ]; then
        wait_idle "$sid" "$addr" 2>/dev/null || true
    fi
}

stop_maggie() {
    if [ -n "$MAG_PID" ] && kill -0 "$MAG_PID" 2>/dev/null; then
        kill "$MAG_PID" 2>/dev/null || true
        wait "$MAG_PID" 2>/dev/null || true
    fi
    MAG_PID=""
}

# --- Session helpers ---

get_session() {
    local addr="${1:-$TEST_ADDR}"
    local json
    json=$("$YUTANI_BIN" session list --address "$addr" --format json 2>/dev/null)
    # Field is "session_id" in Yutani's JSON output
    echo "$json" | python3 -c "
import sys, json
sessions = json.load(sys.stdin)
if sessions:
    s = sessions[0]
    print(s.get('session_id', s.get('id', '')))
else:
    print('')
" 2>/dev/null
}

wait_idle() {
    local sid="$1"
    local addr="${2:-$TEST_ADDR}"
    "$YUTANI_BIN" inject wait-idle -s "$sid" --address "$addr" --timeout 5000 &>/dev/null
}

# --- Injection helpers ---

inject_text() {
    local sid="$1"
    local text="$2"
    local addr="${3:-$TEST_ADDR}"
    "$YUTANI_BIN" inject text -s "$sid" --address "$addr" "$text" &>/dev/null
    wait_idle "$sid" "$addr"
}

inject_key() {
    local sid="$1"
    local key="$2"
    local addr="${3:-$TEST_ADDR}"
    local mod="${4:-}"
    if [ -n "$mod" ]; then
        "$YUTANI_BIN" inject key -s "$sid" --address "$addr" --key "$key" --mod "$mod" &>/dev/null
    else
        "$YUTANI_BIN" inject key -s "$sid" --address "$addr" --key "$key" &>/dev/null
    fi
    wait_idle "$sid" "$addr"
}

# --- Debug helpers ---

get_screen() {
    local sid="$1"
    local addr="${2:-$TEST_ADDR}"
    "$YUTANI_BIN" debug screen -s "$sid" --address "$addr" 2>/dev/null
}

get_tree() {
    local sid="$1"
    local addr="${2:-$TEST_ADDR}"
    "$YUTANI_BIN" debug tree -s "$sid" --address "$addr" 2>/dev/null
}

get_bounds() {
    local sid="$1"
    local addr="${2:-$TEST_ADDR}"
    "$YUTANI_BIN" debug bounds -s "$sid" --address "$addr" 2>/dev/null
}

get_widget() {
    local sid="$1"
    local wid="$2"
    local addr="${3:-$TEST_ADDR}"
    "$YUTANI_BIN" debug widget -s "$sid" -w "$wid" --address "$addr" 2>/dev/null
}

get_widget_json() {
    local sid="$1"
    local wid="$2"
    local addr="${3:-$TEST_ADDR}"
    "$YUTANI_BIN" debug widget -s "$sid" -w "$wid" --address "$addr" --format json 2>/dev/null
}

get_tree_json() {
    local sid="$1"
    local addr="${2:-$TEST_ADDR}"
    "$YUTANI_BIN" debug tree -s "$sid" --address "$addr" --format json 2>/dev/null
}

# --- Assertion helpers ---

assert_screen_contains() {
    local sid="$1"
    local expected="$2"
    local label="${3:-screen contains '$expected'}"
    local addr="${4:-$TEST_ADDR}"
    local screen
    screen=$(get_screen "$sid" "$addr")
    if echo "$screen" | grep -qF "$expected"; then
        pass "$label"
    else
        fail "$label" "Expected screen to contain: '$expected'"
        echo "--- Screen dump ---"
        echo "$screen" | head -40
        echo "---"
    fi
}

assert_screen_not_contains() {
    local sid="$1"
    local unexpected="$2"
    local label="${3:-screen does not contain '$unexpected'}"
    local addr="${4:-$TEST_ADDR}"
    local screen
    screen=$(get_screen "$sid" "$addr")
    if echo "$screen" | grep -qF "$unexpected"; then
        fail "$label" "Screen unexpectedly contains: '$unexpected'"
    else
        pass "$label"
    fi
}

assert_tree_contains() {
    local sid="$1"
    local expected="$2"
    local label="${3:-tree contains '$expected'}"
    local addr="${4:-$TEST_ADDR}"
    local tree
    tree=$(get_tree "$sid" "$addr")
    if echo "$tree" | grep -qF "$expected"; then
        pass "$label"
    else
        fail "$label" "Expected tree to contain: '$expected'"
        echo "--- Tree dump ---"
        echo "$tree"
        echo "---"
    fi
}

# --- Reporting ---

pass() {
    local label="$1"
    PASS_COUNT=$((PASS_COUNT + 1))
    echo "  PASS: $label"
}

fail() {
    local label="$1"
    local detail="${2:-}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    echo "  FAIL: $label"
    [ -n "$detail" ] && echo "        $detail"
}

report() {
    echo ""
    echo "=== $TEST_NAME: $PASS_COUNT passed, $FAIL_COUNT failed ==="
    [ "$FAIL_COUNT" -gt 0 ] && return 1
    return 0
}
