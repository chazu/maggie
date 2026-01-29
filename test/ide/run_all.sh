#!/usr/bin/env bash
# run_all.sh -- Run all IDE headless tests.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TOTAL_PASS=0
TOTAL_FAIL=0
FAILED_TESTS=()

echo "=== Maggie IDE Headless Tests ==="
echo ""

for test_script in "$SCRIPT_DIR"/test_*.sh; do
    [ "$(basename "$test_script")" = "test_harness.sh" ] && continue
    name="$(basename "$test_script" .sh)"
    echo "--- Running $name ---"
    if bash "$test_script"; then
        TOTAL_PASS=$((TOTAL_PASS + 1))
    else
        TOTAL_FAIL=$((TOTAL_FAIL + 1))
        FAILED_TESTS+=("$name")
    fi
    echo ""
done

echo "=============================="
echo "Total: $((TOTAL_PASS + TOTAL_FAIL)) test scripts, $TOTAL_PASS passed, $TOTAL_FAIL failed"
if [ "$TOTAL_FAIL" -gt 0 ]; then
    echo "Failed: ${FAILED_TESTS[*]}"
    exit 1
fi
echo "All tests passed."
