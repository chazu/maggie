#!/usr/bin/env bash
# test_01_ide_renders.sh -- Verify MaggieIDE renders two-panel layout.
TEST_NAME="test_01_ide_renders"
source "$(dirname "$0")/test_harness.sh"

echo "[$TEST_NAME] Starting headless server + MaggieIDE..."
start_yutani_headless "$TEST_PORT"
start_maggie_tool launcher "$TEST_ADDR"

SID=$(get_session "$TEST_ADDR")
if [ -z "$SID" ]; then
    fail "get session ID"
    report; exit $?
fi
pass "got session: $SID"

# Check widget tree (Yutani uses uppercase type names)
assert_tree_contains "$SID" "FLEX" "tree has FLEX layout"

# Check screen shows content from both panels
# Note: border titles may not render in small headless screen (80x25)
assert_screen_contains "$SID" "Welcome to Maggie IDE" "IDE welcome text visible"
assert_screen_contains "$SID" "Select a slot" "Inspector default text visible"
assert_screen_contains "$SID" ">" "input prompt visible"

stop_maggie
stop_yutani
report
