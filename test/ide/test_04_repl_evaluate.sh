#!/usr/bin/env bash
# test_04_repl_evaluate.sh -- Test REPL evaluation via text injection.
# This is the critical proving ground for headless text injection fidelity.
TEST_NAME="test_04_repl_evaluate"
source "$(dirname "$0")/test_harness.sh"

echo "[$TEST_NAME] Starting headless server + REPL..."
start_yutani_headless "$TEST_PORT"
start_maggie_tool repl "$TEST_ADDR"

SID=$(get_session "$TEST_ADDR")
if [ -z "$SID" ]; then
    fail "get session ID"
    report; exit $?
fi
pass "got session: $SID"

# Test 1: Simple arithmetic
inject_text "$SID" "3 + 4"
inject_key "$SID" "enter"
sleep 0.3
wait_idle "$SID"
assert_screen_contains "$SID" "=> 7" "3 + 4 evaluates to 7"

# Test 2: String operation
inject_text "$SID" "'hello' size"
inject_key "$SID" "enter"
sleep 0.3
wait_idle "$SID"
assert_screen_contains "$SID" "=> 5" "'hello' size evaluates to 5"

# Test 3: Variable assignment and use
inject_text "$SID" "x := 42"
inject_key "$SID" "enter"
sleep 0.3
wait_idle "$SID"
assert_screen_contains "$SID" "=> 42" "x := 42 evaluates to 42"

stop_maggie
stop_yutani
report
