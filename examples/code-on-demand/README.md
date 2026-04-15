# Code-on-Demand

A minimal example demonstrating Maggie's automatic code-on-demand feature.
Node A defines a `Calculator` class and spawns a block on Node B that uses it.
Node B does not have `Calculator` locally -- it is automatically pulled,
compiled, and executed.

## Architecture

```
+---------------------------+                      +---------------------------+
|  Node A (client)          |                      |  Node B (server)          |
|                           |                      |                           |
|  Defines Calculator with  |   SpawnProcess RPC   |  Does NOT have Calculator |
|  factorial: method        | -------------------> |                           |
|                           |                      |  1. Looks up content hash |
|  Sends:                   |   Serve RPC (pull)   |     -> not found          |
|  [:n | Calculator         | <------------------- |  2. Pulls from Node A     |
|    factorial: n]          |                      |  3. Compiles + verifies   |
|    forkOn: nodeB          |   __spawn_result__   |  4. Executes block        |
|    with: 20               | <------------------- |  5. Returns result        |
|                           |                      |                           |
|  future await             |                      |                           |
|  => 2432902008176640000   |                      |                           |
+---------------------------+                      +---------------------------+
```

## Server (server.mag)

The server is a minimal node that listens for incoming spawn requests.
It does not define any application classes -- all code arrives on demand.

## Client (client.mag)

The client defines a `Calculator` class with a `factorial:` method, then
spawns a block on the server that uses it. The server automatically pulls
the `Calculator` class before executing the block.

## Running

```bash
# Terminal 1: Start the server node (empty -- no application code)
mag --serve --port 9200 examples/code-on-demand/server.mag -m Server.start

# Terminal 2: Run the client (defines Calculator, spawns on server)
mag --serve --port 9201 examples/code-on-demand/client.mag -m Client.run
```

Expected output:

```
# Server terminal:
Server listening on :9200, waiting for spawn requests...

# Client terminal:
Connecting to server at localhost:9200...
Spawning factorial computation on remote node...
Remote result: 2432902008176640000
Done.
```

## What this demonstrates

- **Code-on-demand** -- the server has no `Calculator` class; it is pulled
  automatically from the client when the spawn request arrives.
- **Content-addressed transfer** -- the block is identified by its content
  hash. The server pulls exactly the methods it needs.
- **Rehydration** -- pulled source text is compiled into runnable classes
  on the server, verified against content hashes, and installed in the
  ClassTable.
- **forkOn: with Future** -- the client gets a Future that resolves when
  the remote block completes.
- **Transparent** -- no explicit sync commands needed. The runtime handles
  code transfer automatically.

## Configuration

Both nodes need trust configuration to allow spawning. Example `maggie.toml`:

```toml
[sync]
listen = ":9200"

[trust]
default = "sync,spawn"
spawn-restrictions = ["File", "ExternalProcess", "HTTP"]
```

The `spawn-restrictions` ensure that remotely-spawned code cannot access
the filesystem, run external processes, or make HTTP requests.
