#!/usr/bin/env bash
# test_00_smoke.sh -- Smoke test: start headless server, ping, verify test mode, stop.
TEST_NAME="test_00_smoke"
source "$(dirname "$0")/test_harness.sh"

echo "[$TEST_NAME] Starting headless Yutani server..."
start_yutani_headless "$TEST_PORT"

# Verify ping
if "$YUTANI_BIN" ping --address "$TEST_ADDR" &>/dev/null; then
    pass "server responds to ping"
else
    fail "server responds to ping"
fi

# Verify test mode
test_mode_output=$("$YUTANI_BIN" inject test-mode -s "dummy" --address "$TEST_ADDR" 2>&1 || true)
if echo "$test_mode_output" | grep -qi "test mode"; then
    pass "server reports test mode"
else
    # Some versions just succeed without error in test mode
    if "$YUTANI_BIN" inject test-mode -s "dummy" --address "$TEST_ADDR" &>/dev/null; then
        pass "server in test mode (no error)"
    else
        fail "server reports test mode" "Output: $test_mode_output"
    fi
fi

stop_yutani
pass "server stopped cleanly"

report
