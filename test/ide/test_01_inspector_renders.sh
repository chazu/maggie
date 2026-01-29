#!/usr/bin/env bash
# test_01_inspector_renders.sh -- Verify Inspector renders slot list and value view.
TEST_NAME="test_01_inspector_renders"
source "$(dirname "$0")/test_harness.sh"

echo "[$TEST_NAME] Starting headless server + Inspector..."
start_yutani_headless "$TEST_PORT"
start_maggie_tool inspector "$TEST_ADDR"

SID=$(get_session "$TEST_ADDR")
if [ -z "$SID" ]; then
    fail "get session ID"
    report; exit $?
fi
pass "got session: $SID"

# Check widget tree (Yutani uses uppercase type names)
assert_tree_contains "$SID" "FLEX" "tree has FLEX layout"
assert_tree_contains "$SID" "LIST" "tree has LIST (slotList)"
assert_tree_contains "$SID" "TEXT_VIEW" "tree has TEXT_VIEW (valueView)"

# Inspector inspects nil by default, so slot list shows "self" with class "UndefinedObject"
assert_screen_contains "$SID" "self" "slot list shows 'self'"
assert_screen_contains "$SID" "UndefinedObject" "shows class name for nil"

stop_maggie
stop_yutani
report
