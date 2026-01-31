#!/usr/bin/env bash
# test_01_ide_renders.sh -- Verify MaggieDesktop renders windowed desktop layout.
TEST_NAME="test_01_ide_renders"
source "$(dirname "$0")/test_harness.sh"

echo "[$TEST_NAME] Starting headless server + MaggieDesktop..."
start_yutani_headless "$TEST_PORT"
start_maggie_tool launcher "$TEST_ADDR"

SID=$(get_session "$TEST_ADDR")
if [ -z "$SID" ]; then
    fail "get session ID"
    report; exit $?
fi
pass "got session: $SID"

# Check widget tree for desktop components
assert_tree_contains "$SID" "FLEX" "tree has FLEX layout"
assert_tree_contains "$SID" "MENU_BAR" "tree has MENU_BAR"
assert_tree_contains "$SID" "WINDOW_MANAGER" "tree has WINDOW_MANAGER"

# Check screen shows menu bar content
assert_screen_contains "$SID" "File" "menu bar shows File menu"

stop_maggie
stop_yutani
report
