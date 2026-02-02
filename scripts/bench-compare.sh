#!/usr/bin/env bash
# bench-compare.sh — Run VM benchmarks and compare against baseline.
# Exit non-zero if benchstat reports a statistically significant regression.
#
# Usage:
#   ./scripts/bench-compare.sh              # compare against benchmarks/baseline.txt
#   ./scripts/bench-compare.sh path/to.txt  # compare against a custom baseline
#
# Requires: benchstat (go install golang.org/x/perf/cmd/benchstat@latest)

set -euo pipefail

BASELINE="${1:-benchmarks/baseline.txt}"
CURRENT="$(mktemp)"
trap 'rm -f "$CURRENT"' EXIT

if ! command -v benchstat &>/dev/null; then
  echo "ERROR: benchstat not found. Install it with:"
  echo "  go install golang.org/x/perf/cmd/benchstat@latest"
  exit 1
fi

if [ ! -f "$BASELINE" ]; then
  echo "ERROR: baseline file not found: $BASELINE"
  echo "Generate one with: go test -bench=BenchmarkHotPath -run='^$' -count=10 -benchmem ./vm/ > benchmarks/baseline.txt"
  exit 1
fi

echo "Running benchmarks (count=10, benchtime=100ms)..."
go test -bench=BenchmarkHotPath -run='^$' -count=10 -benchmem -benchtime=100ms -timeout=30m ./vm/ > "$CURRENT" 2>&1

echo ""
echo "=== Benchmark Comparison ==="
echo ""

# benchstat: old=baseline, new=current
OUTPUT=$(benchstat "$BASELINE" "$CURRENT" 2>&1)
echo "$OUTPUT"

# Check for regressions: lines containing a "+" delta and "p=" (statistically significant)
if echo "$OUTPUT" | grep -qE '\+[0-9]+.*%.*p='; then
  echo ""
  echo "WARNING: Statistically significant regression(s) detected above."
  exit 1
fi

echo ""
echo "No significant regressions detected."
