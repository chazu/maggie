# Maggie Conventions Doctrine

This document is the ruling on how Maggie's language surface behaves. It
exists because inconsistency is a tax on every user: when File returns a
Result, HttpClient returns nil-and-discards-the-reason, Sqlite raises, and
Future stashes its error in a side channel, nobody can predict the failure
mode of an API without reading its source.

Where current code violates a ruling, the ruling wins: the code is the bug.
CI enforcement lives in `cmd/bootstrap/conventions_test.go`.

## 1. Failure doctrine

- **Expected failures return a Result** (`Success`/`Failure`). "Expected"
  means the caller can reasonably anticipate it: file not found, network
  error, parse failure, no rows.
- **Programmer errors signal exceptions** (catchable with `on:do:`):
  subscript out of bounds, type errors, message-not-understood, assigning
  garbage.
- **nil is never a failure signal.** An API that returns nil on error
  discards the reason and moves the crash to a distant nil-DNU.

Migration schedule (worst offenders first) — **completed 2026-07-16**:
- `HttpClient` — ~~returns nil on error and discards the reason~~ DONE: all
  request methods return `Success` wrapping the response body or `Failure`
  carrying the reason.
- `Future` — ~~`await` returns nil on error with a side-channel `error`
  accessor~~ DONE: `await` signals (typed remote exceptions re-signal;
  plain remote errors raise a catchable `Error`); `await:ifTimeout:` added.
  The `error`/`exception` accessors remain for non-blocking inspection.
- `File readFileContents:` — ~~returns an untagged `String | Failure`
  union~~ DONE: returns `Success with: contents`, matching
  `writeFileContents:contents:`.

## 2. nil semantics

nil is a value, not a signal. Absence, closure, and timeout get explicit
variants or exceptions:

- absence: `at:ifAbsent:`, `detect:ifNone:`, `remove:ifAbsent:`
- channels: `receiveIfClosed:`, `tryReceiveIfEmpty:` (added 2026-07-16 —
  plain `receive`/`tryReceive` still answer nil for closed/empty, so use
  the variants whenever a legitimately-sent nil must be distinguishable)
- futures: signal on error; `await:ifTimeout:` for deadlines (both
  implemented 2026-07-16)

**Sanctioned rule, no exceptions:** restricted globals (`forkRestricted:`)
signal a catchable `RestrictedGlobal` error instead of silently resolving
to nil (implemented 2026-07-16; Guide09 documents the pattern).

## 3. Naming

- **One blessed selector per concept.** Aliases teach users the library has
  no doctrine. Deprecated aliases are removed after one release.
- Conversions are `as*`: `asArray`, `asString`, `asStream`. (`toArray` is
  deprecated.)
- Smalltalk-80 conventions hold: `isEmpty`/`notEmpty`, `printString`,
  `hash`/`=` contract (equal objects have equal hashes — `String>>hash`
  is content-based as of 2026-07-16; `hashCode` is gone, `identityHash`
  is the identity escape hatch).
- Go primitives use the `prim` prefix when a lib `.mag` wrapper exists,
  else the wrapper overwrites the primitive on image load.

## 4. Selector honesty

Every selector in `lib/` must be backed by a real implementation; every
VM-registered selector on a lib class must be declared in that class's
`.mag` file (as a `<primitive>` stub or a wrapper). Violations in both
directions shipped real lies:

- `forkAt:` / `priority` / `priority:` — phantom API. `forkAt:` silently
  returned nil (`Process fork:at:` never existed); the priority accessors
  were stubs returning constants. Deleted 2026-07-16 — goroutines have no
  priorities, and an honest surface beats a familiar one.
- `Channel>>onSend:do:` — implemented in the VM but invisible: never
  declared in Channel.mag, never documented. Declared 2026-07-16.

Enforcement (`cmd/bootstrap/conventions_test.go`):
- **Hard gate:** every `<primitive>` stub in `lib/` resolves to a
  Go-registered method on that class.
- **Ratchet:** VM-registered selectors NOT declared in lib
  (`testdata/undeclared_selectors_baseline.txt`) may only shrink.
- **Ratchet:** the count of lib methods without a doctest
  (```example block) may only shrink
  (`testdata/doctest_ratchet.txt`). New code ships with doctests.

## Amending this document

Rulings change by editing this file in the same commit as the code that
implements the change, with the reasoning captured here or in
`docs/reviews/`. A ruling nobody enforces is a suggestion; keep the CI
checks in lockstep.
