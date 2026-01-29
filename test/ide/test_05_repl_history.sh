#!/usr/bin/env bash
# test_05_repl_history.sh -- Test REPL history navigation via Up/Down arrows.
TEST_NAME="test_05_repl_history"
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

# Evaluate two expressions to populate history
inject_text "$SID" "100"
inject_key "$SID" "enter"
sleep 0.3
wait_idle "$SID"
assert_screen_contains "$SID" "=> 100" "first expression evaluated"

inject_text "$SID" "200"
inject_key "$SID" "enter"
sleep 0.3
wait_idle "$SID"
assert_screen_contains "$SID" "=> 200" "second expression evaluated"

# Press Up arrow -- should recall "200" (most recent)
inject_key "$SID" "up"
sleep 0.2
# The input field should now show "200"
# Check the last line of the screen (the input field)
SCREEN=$(get_screen "$SID")
if echo "$SCREEN" | tail -1 | grep -qF "200"; then
    pass "Up recalls '200' (most recent)"
else
    fail "Up recalls '200'" "Last line: $(echo "$SCREEN" | tail -1)"
fi

# Press Up again -- should recall "100" (previous)
inject_key "$SID" "up"
sleep 0.2
SCREEN=$(get_screen "$SID")
if echo "$SCREEN" | tail -1 | grep -qF "100"; then
    pass "Up again recalls '100' (previous)"
else
    fail "Up again recalls '100'" "Last line: $(echo "$SCREEN" | tail -1)"
fi

# Press Down -- should go back to "200"
inject_key "$SID" "down"
sleep 0.2
SCREEN=$(get_screen "$SID")
if echo "$SCREEN" | tail -1 | grep -qF "200"; then
    pass "Down returns to '200'"
else
    fail "Down returns to '200'" "Last line: $(echo "$SCREEN" | tail -1)"
fi

stop_maggie
stop_yutani
report
