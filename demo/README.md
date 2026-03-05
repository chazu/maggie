# Two-VM Code Sync Demo

Two Maggie VMs running in Docker containers demonstrate content-addressed code
distribution with sandboxed execution. VM-A defines a `DemoA::Greeter` class,
pushes it to VM-B over the sync protocol, where VM-B rehydrates it into a live
class, runs it in a restricted process, and sends the greeting result back via
HTTP.

## Prerequisites

- Docker (with Docker Compose v2)
- The `mag` binary builds from the project root Dockerfile

## Quick Start

```bash
./scripts/demo-two-vm.sh
```

Use `--keep` to leave containers running for manual exploration:

```bash
./scripts/demo-two-vm.sh --keep
```

Use `--verbose` for detailed output at each step:

```bash
./scripts/demo-two-vm.sh --keep --verbose
```

## What Happens

### Step by step

1. **Build** -- Docker images are built from the project root `Dockerfile`,
   producing a `mag` binary with the embedded standard image.

2. **Start** -- Two containers launch, each running `mag -m Main.start`:
   - **VM-A** (namespace `DemoA`) listens for sync on `:8081` and HTTP on `:9001`
   - **VM-B** (namespace `DemoB`) listens for sync on `:8082` and HTTP on `:9002`

3. **Push** -- VM-A pushes its compiled content (including `DemoA::Greeter`)
   to VM-B via the sync protocol. VM-A's `maggie.toml` lists `vm-b:8082` as a
   peer, so `mag sync push` sends to it automatically.

4. **Capability check** -- VM-B's sync server inspects the incoming content
   against its declared capabilities (`["HTTP"]`). Code requiring undeclared
   capabilities is rejected before any chunks are transferred.

5. **Hash verification** -- For each received chunk, VM-B recompiles from
   source and verifies the content hash matches. Tampered code is rejected.

6. **Rehydrate** -- `mag sync pull vm-a:8081 DemoA::Greeter` resolves the
   class name to a content hash on VM-A, pulls the transitive closure of
   chunks, and compiles them into live classes in VM-B's runtime.

7. **Sandboxed execution** -- VM-B runs the received `Greeter` class in a
   restricted process (`forkRestricted:`). The forked process cannot see
   globals like `File`, `Network`, or other sensitive classes -- they simply
   resolve to `nil`.

8. **Result callback** -- The sandboxed process sends its greeting result
   back to VM-A's HTTP server on port 9001 using the allowed `HTTP`
   capability. VM-A receives and prints the message.

### Architecture

```
+-------------------+                    +-------------------+
|       VM-A        |                    |       VM-B        |
|                   |  1. sync push      |                   |
|  DemoA::Greeter --+--------------------->  (rehydrate)     |
|                   |  capability check  |                   |
|                   |  hash verify       |  Sandbox run:     |
|  HttpServer:9001  <--------------------+  [Greeter greet]  |
|  "Hello, World!"  |  2. HTTP POST      |  HttpClient       |
+-------------------+                    +-------------------+
     :8081 sync                               :8082 sync
     :9001 http                               :9002 http
```

## Security Model

The demo showcases three layers of defense for receiving and running foreign
code:

### Layer 1: Capability Manifests

Before any code is transferred, the receiver checks the sender's declared
capabilities against its own policy. If the sender requires capabilities the
receiver has not enabled, the transfer is rejected immediately. This is a
cheap, early-reject filter.

Configuration in `maggie.toml`:
```toml
[sync]
capabilities = ["HTTP"]
```

### Layer 2: Content Hash Verification

Every method and class has a deterministic content hash (SHA-256 over a
normalized AST). When VM-B receives a chunk, it recompiles the source and
verifies the hash matches what the sender claimed. If the source was modified
in transit, the hash will not match and the chunk is rejected. This ensures
code integrity end-to-end.

### Layer 3: Process Restriction (Sandboxed Execution)

Even after code passes capability and integrity checks, it runs in a
restricted process. `forkRestricted:` creates a forked interpreter where
specified global names (classes, modules) are hidden -- they resolve to `nil`
rather than raising an error. Restrictions are inherited by child forks, so
a restricted process cannot escalate by forking again.

Writes to globals from a restricted process go to a process-local overlay
and do not affect the shared VM state.

## Manual Exploration

After running with `--keep`:

```bash
# Shell into a container
docker compose -f demo/docker-compose.yml exec vm-a bash

# Check sync server status
docker compose -f demo/docker-compose.yml exec vm-a mag sync status

# List all content in VM-B's store
docker compose -f demo/docker-compose.yml exec vm-b mag sync list

# Compare what VM-A and VM-B have
docker compose -f demo/docker-compose.yml exec vm-b mag sync diff vm-a:8081

# Inspect a specific content hash
docker compose -f demo/docker-compose.yml exec vm-b mag sync show <hash-prefix>

# View container logs
docker compose -f demo/docker-compose.yml logs -f
```

## Configuration

### VM-A (`demo/vm-a/maggie.toml`)

```toml
[project]
name = "demo-vm-a"
namespace = "DemoA"

[source]
dirs = ["src"]
entry = "Main.start"

[sync]
listen = ":8081"
peers = ["vm-b:8082"]
capabilities = ["HTTP"]
```

### VM-B (`demo/vm-b/maggie.toml`)

```toml
[project]
name = "demo-vm-b"
namespace = "DemoB"

[source]
dirs = ["src"]
entry = "Main.start"

[sync]
listen = ":8082"
peers = ["vm-a:8081"]
capabilities = ["HTTP"]
```

## Ports

| Service | Sync Port | HTTP Port | Description                  |
|---------|-----------|-----------|------------------------------|
| vm-a    | 8081      | 9001      | Source VM, defines Greeter   |
| vm-b    | 8082      | 9002      | Receiver VM, runs sandboxed  |

## Troubleshooting

**Containers fail to start:**
Check that ports 8081, 8082, 9001, and 9002 are not already in use on the
host. Use `docker compose -f demo/docker-compose.yml logs` to see error
output.

**Sync server not ready:**
The script waits up to 30 seconds for each sync server. If this is not
enough (e.g., on a slow machine or cold Docker cache), the build step may
still be running. Check with `docker compose -f demo/docker-compose.yml ps`.

**Push reports "Nothing to push":**
VM-A's source directory (`demo/vm-a/src/`) may be empty. The Greeter class
source files need to be present there.

**Hash verification failures:**
This indicates the `mag` binary in the container was built from different
source than expected. Rebuild with `docker compose -f demo/docker-compose.yml
build --no-cache`.
