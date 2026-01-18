// Package bytecode provides a stack-based virtual machine for executing
// Trashtalk blocks natively in Go. This enables blocks to execute without
// shelling out to Bash, providing significant performance improvements.
//
// The bytecode format is designed for:
//   - Compact representation (typically 2-4 bytes per instruction)
//   - Fast decoding (fixed-width opcodes, simple operand formats)
//   - Easy serialization (can be stored in SQLite or passed between processes)
//
// Key features:
//   - Reference capture for closures (changes visible in block)
//   - Cross-plugin invocation via shared memory or IPC
//   - Full compatibility with existing Bash-based blocks
//
// # Architecture Overview
//
// The bytecode system consists of several components:
//
//   - Opcodes: ~50 stack-based instructions covering arithmetic, control flow,
//     variable access, message sends, and block operations
//
//   - Chunk: A compiled bytecode unit containing code, constants, parameter info,
//     and capture descriptors. Chunks can be serialized to bytes using the "TTBC"
//     format (TrashTalk ByteCode) for storage or cross-process transport.
//
//   - Compiler: Converts parser AST (parser.BlockExpr) to bytecode chunks,
//     performing capture analysis to identify free variables.
//
//   - VM: Stack-based interpreter that executes bytecode chunks. Supports
//     reference capture semantics where changes to captured variables are
//     visible outside the block.
//
// # Capture Semantics
//
// Blocks capture variables by reference, not by value. When a block modifies
// a captured variable:
//
//   - Local variables: Changes are visible in the enclosing scope
//   - Instance variables: Changes are written back to SQLite immediately
//   - Nested captures: Propagate through the capture chain
//
// This matches Smalltalk semantics where blocks are closures with full access
// to their lexical environment.
//
// # Integration with Procyon
//
// The bytecode system integrates with Procyon's code generator:
//
//   - Static blocks (known at compile time) are pre-compiled to bytecode and
//     embedded as constants in the generated Go code
//
//   - Dynamic blocks (created at runtime) are compiled on-demand and registered
//     with the bytecode VM
//
//   - The trashtalk-daemon maintains a cross-process block registry for
//     plugin-to-plugin block invocation
//
// # Fallback Behavior
//
// If bytecode compilation fails or a block uses unsupported features, the
// system falls back to Bash-based execution transparently. This ensures
// backward compatibility while enabling performance improvements where possible.
//
// See docs/BYTECODE_BLOCK_IMPLEMENTATION_PLAN.md for the full implementation
// plan and design decisions.
package bytecode
