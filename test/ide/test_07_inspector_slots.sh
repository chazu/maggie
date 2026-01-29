#!/usr/bin/env bash
# test_07_inspector_slots.sh -- Test Inspector slot list population and layout.
TEST_NAME="test_07_inspector_slots"
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

# Inspector opens inspecting nil -> shows "self" / "UndefinedObject"
assert_screen_contains "$SID" "self" "slot list shows 'self'"
assert_screen_contains "$SID" "UndefinedObject" "shows class name for nil"

# Verify the two-panel layout (LIST + TEXT_VIEW)
TREE=$(get_tree "$SID")
if echo "$TREE" | grep -qF "FLEX"; then
    pass "tree has FLEX layout"
else
    fail "tree has FLEX layout"
fi

if echo "$TREE" | grep -qF "LIST"; then
    pass "tree has LIST (slotList)"
else
    fail "tree has LIST (slotList)"
fi

if echo "$TREE" | grep -qF "TEXT_VIEW"; then
    pass "tree has TEXT_VIEW (valueView)"
else
    fail "tree has TEXT_VIEW (valueView)"
fi

# Verify the list has the "self" item by checking bounds show list is focused
BOUNDS=$(get_bounds "$SID")
if echo "$BOUNDS" | grep -q "LIST.*\*"; then
    pass "slot list is focused"
else
    fail "slot list is focused"
fi

stop_maggie
stop_yutani
report
