---
task: maggie-kwy
title: "Research: Existing Go Primitives for Missing Stdlib"
author: Blip
date: 2026-03-14
status: complete
tags: [stdlib, primitives, regex, datetime, math, random, gowrap]
---

# Existing Go Primitives for Missing Stdlib

## Executive Summary

Maggie currently has **no** dedicated classes or primitives for regex, date/time, math functions (trig/log/exp), or random numbers. The Float class has basic `sqrt`, `floor`, `ceiling`, `rounded`, `truncated` but no trig/log/exp. System has `epochSeconds` for Unix timestamps but no date parsing/formatting. String has no regex or split support. There are zero random-number primitives anywhere in the VM.

## 1. Regex

### Current State
- **No regex class, no regex primitives.** Grepping `vm/` for `regex|Regex|regexp|Regexp` returns zero results.
- String has `includes:`, `indexOf:`, `replaceAll:with:` — all literal substring matching, no pattern support.
- The Go stdlib `regexp` package is not imported anywhere in `vm/`.

### What's Needed
- A `Regex` class wrapping Go's `regexp.Regexp`
- Core operations: `match:`, `findIn:`, `findAllIn:`, `replaceIn:with:`, `splitOn:`
- String convenience methods: `matchesRegex:`, `split:` (using regex or literal)

### Implementation Approach
- **New Go primitives required.** `regexp.Compile` returns a `*regexp.Regexp`; this needs to be stored as a Go object.
- **GoObjectWrapper is available** (Phase 5a): `vm.RegisterGoType` + `GoToValue`/`ValueToGo` can wrap `*regexp.Regexp`.
- Alternatively, a hand-written `regex_primitives.go` (like `json_primitives.go`) would be simpler and more ergonomic.
- **gowrap is NOT suitable** for `regexp` — the package uses interfaces and functional patterns that don't map cleanly to Smalltalk classes. Hand-written primitives are better.

### Priority: **HIGH**
- Regex is essential for any real text processing (log parsing, input validation, data extraction).
- An agent writing a real system would need this almost immediately.

---

## 2. Date/Time

### Current State
- `System epochSeconds` — returns `time.Now().Unix()` as SmallInt. That's it.
- `Process sleep:` — takes milliseconds, uses `time.Sleep`. Not a date/time API.
- HTTP server uses `time.Second` for timeouts internally but doesn't expose it.
- **No Date, Time, or DateTime class.** No parsing, formatting, arithmetic, or timezone support.

### What's Needed
- A `DateTime` class wrapping Go's `time.Time`
- Core operations: `now`, `parse:format:`, `format:`, `year`, `month`, `day`, `hour`, `minute`, `second`
- Arithmetic: `addSeconds:`, `addMinutes:`, `addHours:`, `addDays:`, `differenceFrom:`
- Formatting: Go-style format strings (`2006-01-02 15:04:05`) or ISO 8601
- A `Duration` class wrapping `time.Duration` (optional, lower priority)

### Implementation Approach
- **New Go primitives required.** `time.Time` is a struct; needs GoObjectWrapper or dedicated wrapper.
- **GoObjectWrapper approach**: Register `time.Time` as a Go type, wrap methods. This is cleaner than NaN-boxing.
- **gowrap could partially work** for `time` package (exported functions like `time.Now()`, `time.Parse()`), but the `time.Time` methods are all value-receiver methods on a struct — gowrap handles these.
- **Recommendation**: Hand-written `datetime_primitives.go` for ergonomic Smalltalk API. gowrap output would be too Go-idiomatic.

### Priority: **HIGH**
- Any real application needs timestamps, date formatting, scheduling.
- Currently impossible to format a date or compute time differences.

---

## 3. Math Functions

### Current State
- **Float class** (`vm/float_primitives.go`): `+`, `-`, `*`, `/`, `<`, `>`, `negated`, `abs`, `truncated`, `rounded`, `floor`, `ceiling`, `sqrt`, `printString`
- **SmallInteger class** (`vm/integer_primitives.go`): arithmetic, comparisons, bit ops, `negated`, `abs`, `timesRepeat:`, `to:do:`, `to:by:do:`
- **SmallInteger.mag**: `sign`, `max:`, `min:`, `between:and:`, `even`, `odd`, `positive`, `negative`, `isZero`, `factorial`, `gcd:`, `lcm:`
- **Float.mag**: `sign`, `max:`, `min:`, `positive`, `negative`, `isZero`, `floor`, `ceiling`, `rounded`, `truncated`, `printString`
- **Missing**: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2:`, `log`, `log:` (arbitrary base), `exp`, `pow:`, `pi`, `e`, `ln`
- The Go `math` package is imported in `float_primitives.go` but only used for `Floor`, `Ceil`, `Sqrt`.

### What's Needed
- Trigonometric: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2:`
- Logarithmic: `ln` (natural log), `log:` (arbitrary base), `log10`
- Exponential: `exp`, `pow:`
- Constants: `Float pi`, `Float e`, `Float infinity`, `Float nan`
- Miscellaneous: `min:`, `max:` on Float (already in .mag), `clamp:to:`

### Implementation Approach
- **Trivial to add as Go primitives.** All functions exist in Go's `math` package. Each is a one-liner.
- Add to `vm/float_primitives.go` — no new files needed.
- Constants can be class methods on Float (e.g., `Float pi` returns `math.Pi`).
- **gowrap is overkill** — these are simple `math.Sin(recv.Float64())` calls.

### Priority: **MEDIUM-HIGH**
- Essential for any scientific, geometric, or physics computation.
- An agent building a graphical application or game would need trig immediately.
- Very cheap to implement — each is 3 lines of Go.

---

## 4. Random Numbers

### Current State
- **Zero random number support.** No `Random` class, no random primitives anywhere.
- Grepping for `rand` in `vm/*.go` returns zero relevant results (only `Random` in test file names).

### What's Needed
- A `Random` class with:
  - `Random next` — float in [0, 1)
  - `Random nextInt:` — integer in [0, n)
  - `Random nextBetween:and:` — integer in [min, max]
  - `Random seed:` — set seed for reproducibility
- Optionally: `Random new` (seeded), `Random new:` (with seed), instance-level state

### Implementation Approach
- **New Go primitives required.** Use `math/rand/v2` (Go 1.22+) or `math/rand`.
- Simple class-method API for global RNG: `Random next`, `Random nextInt:`.
- Instance-level RNG (with `rand.New(rand.NewSource(seed))`) for reproducibility.
- **gowrap is NOT suitable** — `math/rand` uses package-level functions and `*rand.Rand` struct, but the API needs Maggie-idiomatic wrapping.
- **Hand-written `random_primitives.go`** is best — ~50 lines of Go.

### Priority: **HIGH**
- Random numbers are needed for: testing, simulation, games, UUID generation, sampling.
- An agent building any non-trivial system would hit this gap quickly.

---

## 5. Additional Gaps (Lower Priority)

### String Split/Join
- No `split:` method on String (neither literal nor regex).
- No `Array join:` or `Array joinWith:` method.
- Both could be added as Go primitives on String/Array using `strings.Split`/`strings.Join`.
- **Priority: MEDIUM** — very common operations.

### String Formatting / Interpolation
- No `format:` or string interpolation support.
- Would need either a printf-style primitive or template expansion.
- **Priority: LOW** — can be worked around with concatenation.

### Sorting
- No `Array sort` or `Array sort:` primitive.
- Would need a Go primitive that calls back into Maggie for comparison blocks.
- **Priority: MEDIUM** — needed for any data processing.

---

## 6. gowrap Feasibility Assessment

The `gowrap` tool (`cmd/mag/wrap.go`, `gowrap/`) can introspect Go packages and generate VM bindings automatically. Assessment per category:

| Category | gowrap Feasible? | Notes |
|----------|-----------------|-------|
| `regexp` | Partial | Could wrap `*Regexp` struct methods, but `Compile()` returns `(*Regexp, error)` — needs error handling. Class creation API not idiomatic. |
| `time` | Partial | Could wrap `time.Time` methods. But `time.Now()`, `time.Parse()` are package functions, not methods. Mixed approach needed. |
| `math` | No benefit | All are `func(float64) float64` — faster to hand-write 1-line primitives on Float class. |
| `math/rand` | No benefit | Need Maggie-idiomatic `Random` class, not a Go struct wrapper. |
| `strings` (split/join) | No benefit | Better as String/Array primitive methods. |

**Verdict**: gowrap is designed for wrapping user libraries with struct-heavy APIs. For Go stdlib packages with functional APIs (math, strings) or complex error handling (regexp, time), hand-written primitives are faster to write and more ergonomic.

---

## 7. Prioritized Implementation Roadmap

### Tier 1 — Essential (agent can't build real systems without these)
1. **Random numbers** — ~50 lines of Go, immediate unblock for testing/simulation
2. **Math functions** — ~80 lines of Go added to `float_primitives.go`, unblocks geometry/science
3. **Regex** — ~150 lines of Go, unblocks text processing/validation

### Tier 2 — Important (needed for production-quality apps)
4. **DateTime** — ~200 lines of Go, unblocks scheduling/logging/timestamps
5. **String split/join** — ~30 lines of Go, unblocks data parsing

### Tier 3 — Nice to Have
6. **Array sort** — ~50 lines of Go, needs block callback mechanism
7. **String format** — ~40 lines of Go, convenience over concatenation
8. **Duration class** — ~80 lines of Go, companion to DateTime

### Effort Estimates
- Tier 1: ~280 lines of Go total, ~1-2 hours
- Tier 2: ~230 lines of Go total, ~1-2 hours
- Tier 3: ~170 lines of Go total, ~1 hour

All items need **new Go primitives** — none can be implemented in pure Maggie (.mag files) because the underlying functionality (regex engine, system clock, trig functions, PRNG) lives in Go stdlib.
