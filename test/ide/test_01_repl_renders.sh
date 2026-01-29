#!/usr/bin/env bash
# test_01_repl_renders.sh -- Verify REPL renders welcome text and widget tree.
TEST_NAME="test_01_repl_renders"
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

# Check widget tree contains expected types (Yutani uses uppercase type names)
assert_tree_contains "$SID" "FLEX" "tree has FLEX root"
assert_tree_contains "$SID" "TEXT_VIEW" "tree has TEXT_VIEW (historyView)"
assert_tree_contains "$SID" "INPUT_FIELD" "tree has INPUT_FIELD"

# Check screen shows welcome text
assert_screen_contains "$SID" "Welcome to Maggie REPL" "welcome text visible"

stop_maggie
stop_yutani
report
