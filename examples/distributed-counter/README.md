# Distributed Counter

A minimal example of two Maggie nodes communicating via process mailboxes.

## Architecture

```
┌──────────────────┐       gRPC/HTTP2       ┌──────────────────┐
│  Client Node     │ ───────────────────>   │  Server Node     │
│                  │                        │                  │
│  Sends 5         │   cast: #increment:    │  "counter"       │
│  increment msgs  │   with: value          │  process reads   │
│                  │                        │  from mailbox    │
└──────────────────┘                        └──────────────────┘
```

## Server (server.mag)

```smalltalk
"Register as a counter service and process increment messages"
Counter subclass: Object
  instanceVars: total

  classMethod: start [
      | counter |
      counter := self new.
      counter run
  ]

  method: initialize [
      total := 0
  ]

  method: run [
      Process current registerAs: 'counter'.
      'Counter service started' printString.

      "Process messages until told to stop"
      [true] whileTrue: [
          | msg |
          msg := Process receive.
          msg selector = #increment:
              ifTrue: [
                  total := total + msg payload.
                  ('Total: ', total printString) printString
              ].
          msg selector = #getTotal
              ifTrue: [
                  ('Final total: ', total printString) printString
              ]
      ]
  ]
```

## Client (client.mag)

```smalltalk
"Connect to the server and send increment messages"
Client subclass: Object
  classMethod: run [
      | node counter |
      node := Node connect: 'localhost:8081'.

      node ping ifFalse: [
          'Cannot reach server' printString. ^nil
      ].

      counter := node processNamed: 'counter'.

      "Send 5 increment messages"
      1 to: 5 do: [:i |
          counter cast: #increment: with: i * 10.
          ('Sent increment: ', (i * 10) printString) printString
      ].

      'All messages sent' printString
  ]
```

## Running

```bash
# Terminal 1: Start the server (serves on port 9200)
mag --serve --port 9200 examples/distributed-counter/server.mag -m Counter.start

# Terminal 2: Run the client (connects to localhost:9200)
mag examples/distributed-counter/client.mag -m Client.run
```

Expected output:

```
# Server terminal:
Counter service ready
Total: 10
Total: 30
Total: 60
Total: 100
Total: 150

# Client terminal:
Connected to server
Sent increment: 10
Sent increment: 20
Sent increment: 30
Sent increment: 40
Sent increment: 50
All messages sent and delivered
```

## What this demonstrates

- **Node connect:** — establishing a connection to a remote VM
- **processNamed:** — obtaining a reference to a registered remote process
- **cast:with:** — fire-and-forget message delivery (serialized with CBOR, signed with Ed25519)
- **Process receive** — blocking mailbox receive on the server side
- **MailboxMessage** — structured messages with selector and payload
- **registerAs:** — process name registration for discovery
