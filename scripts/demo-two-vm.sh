#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COMPOSE_FILE="$PROJECT_ROOT/demo/docker-compose.yml"
TIMEOUT=30

# Parse flags
KEEP=false
VERBOSE=false
for arg in "$@"; do
  case $arg in
    --keep) KEEP=true ;;
    --verbose|-v) VERBOSE=true ;;
    --help|-h)
      echo "Usage: $0 [--keep] [--verbose]"
      echo ""
      echo "Run the two-VM Docker demo: VM-A pushes a Greeter class to VM-B,"
      echo "VM-B rehydrates it, runs it sandboxed, and sends the result back."
      echo ""
      echo "Options:"
      echo "  --keep      Leave containers running after the demo"
      echo "  --verbose   Show detailed output"
      echo "  --help      Show this help message"
      exit 0
      ;;
  esac
done

compose() {
  docker compose -f "$COMPOSE_FILE" "$@"
}

cleanup() {
  if [ "$KEEP" = false ]; then
    echo ""
    echo "=== Tearing down ==="
    compose down --remove-orphans 2>/dev/null || true
  fi
}
trap cleanup EXIT

# Step 1: Build Docker images
echo "=== Building Docker images ==="
compose build

# Step 2: Start both containers
echo "=== Starting VM-A and VM-B ==="
compose up -d

# Step 3: Wait for sync servers to become healthy
# VM-A listens on :8081, VM-B listens on :8082 (from maggie.toml sync.listen)
echo "=== Waiting for sync servers (timeout: ${TIMEOUT}s) ==="

wait_for_sync() {
  local service="$1"
  local port="$2"
  for i in $(seq 1 "$TIMEOUT"); do
    # Use curl to hit the Connect Ping RPC endpoint inside the container
    if compose exec -T "$service" mag sync status >/dev/null 2>&1; then
      echo "  $service sync server ready (${i}s)"
      return 0
    fi
    if [ "$VERBOSE" = true ]; then
      echo "  waiting for $service... (${i}/${TIMEOUT})"
    fi
    sleep 1
  done
  echo "ERROR: $service sync server did not become ready within ${TIMEOUT}s"
  echo "  Check logs: docker compose -f $COMPOSE_FILE logs $service"
  exit 1
}

wait_for_sync vm-a 8081
wait_for_sync vm-b 8082

# Step 4: Show content before push
if [ "$VERBOSE" = true ]; then
  echo ""
  echo "=== VM-A content store (before push) ==="
  compose exec -T vm-a mag sync status || true
  echo ""
  echo "=== VM-B content store (before push) ==="
  compose exec -T vm-b mag sync status || true
fi

# Step 5: Push code from VM-A to VM-B
# VM-A's maggie.toml has peers = ["vm-b:8082"], so bare `mag sync push` works.
echo ""
echo "=== Pushing DemoA::Greeter from VM-A to VM-B ==="
if [ "$VERBOSE" = true ]; then
  compose exec -T vm-a mag --verbose sync push
else
  compose exec -T vm-a mag sync push
fi

# Step 6: Verify VM-B received the content
echo ""
echo "=== VM-B content after sync ==="
compose exec -T vm-b mag sync list || true

# Step 7: Pull and rehydrate on VM-B (class resolution + compile)
echo ""
echo "=== VM-B pulling DemoA::Greeter from VM-A ==="
if [ "$VERBOSE" = true ]; then
  compose exec -T vm-b mag --verbose sync pull vm-a:8081 DemoA::Greeter
else
  compose exec -T vm-b mag sync pull vm-a:8081 DemoA::Greeter
fi

# Step 8: Show demo output from container logs
echo ""
echo "=== Demo output (last 50 lines) ==="
compose logs --tail=50

# Step 9: Summary
echo ""
echo "=== Demo complete ==="
echo "  VM-A (DemoA namespace) pushed Greeter class to VM-B"
echo "  VM-B received, verified content hashes, and rehydrated the class"
echo ""

if [ "$KEEP" = true ]; then
  echo "Containers still running. Explore with:"
  echo "  docker compose -f $COMPOSE_FILE exec vm-a bash"
  echo "  docker compose -f $COMPOSE_FILE exec vm-b bash"
  echo "  docker compose -f $COMPOSE_FILE exec vm-a mag sync status"
  echo "  docker compose -f $COMPOSE_FILE exec vm-b mag sync list"
  echo "  docker compose -f $COMPOSE_FILE exec vm-b mag sync diff vm-a:8081"
  echo ""
  echo "Tear down with:"
  echo "  docker compose -f $COMPOSE_FILE down"
else
  echo "Done. Use --keep to leave containers running for exploration."
fi
