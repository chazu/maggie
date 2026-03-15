---
title: "File I/O Primitives Inventory"
task: maggie-jvq
agent: Marble
date: 2026-03-15
type: research
tags: [file-io, primitives, stdlib, gaps]
---

# File I/O Primitives Inventory

## Current State

All File I/O is implemented as **class-side methods** on a Go-created `File` class
(`vm/file_primitives.go`). There is no `File.mag` in `lib/` — no Maggie-side wrapper
exists. The class is registered in `registerFilePrimitives()`, called from `vm.go:294`.

### Existing Primitives (all class methods on `File`)

| Category | Selector | Description | Returns |
|----------|----------|-------------|---------|
| **Reading** | `readFileContents:` | Read entire file as string | String or Failure |
| **Writing** | `writeFileContents:contents:` | Write string to file (overwrite) | Success/Failure |
| **Writing** | `appendToFile:contents:` | Append string to file (create if needed) | Success/Failure |
| **Existence** | `exists:` | Check if path exists | true/false |
| **Existence** | `isDirectory:` | Check if path is a directory | true/false |
| **Existence** | `isFile:` | Check if path is a regular file | true/false |
| **Directory** | `listDirectory:` | List entries in directory | Array of strings or Failure |
| **Directory** | `createDirectory:` | Create directory (mkdir -p) | Success/Failure |
| **Manipulation** | `delete:` | Delete file or empty directory | Success/Failure |
| **Manipulation** | `rename:to:` | Rename/move file or directory | Success/Failure |
| **Manipulation** | `copy:to:` | Copy a file | Success/Failure |
| **Path** | `basename:` | Last path element | String |
| **Path** | `dirname:` | Directory portion | String |
| **Path** | `extension:` | File extension (with dot) | String |
| **Path** | `join:with:` | Join two path components | String |
| **Path** | `absolutePath:` | Resolve to absolute path | String |
| **Path** | `workingDirectory` | Get CWD | String or Failure |
| **Path** | `homeDirectory` | Get user home dir | String or Failure |

**Total: 18 class methods, 0 instance methods.**

### Design Characteristics

- **Stateless / class-only**: No file handle objects. Every operation opens, acts, closes.
- **String-based**: All paths are strings, all content is strings.
- **Result pattern**: Mutating operations return Success/Failure. Queries return values directly.
- **Error handling**: Failures wrap Go error messages as strings.
- **No File class in bootstrap** (`cmd/bootstrap/main.go`): The File class is created entirely
  in Go, not compiled from `.mag` source.
- **Sandbox integration**: `File` is one of the default restricted globals in sandboxed processes.

### Helper Functions (Go-side, internal)

- `valueToString(Value) string` — converts String/Symbol values to Go strings
- `newSuccessResult(Value) Value` — wraps value in Success result
- `newFailureResult(string) Value` — creates Failure from error string

## Comparison with Other Smalltalk File I/O

### Pharo/Squeak

- **FileStream** — instance-based, open/read/write/close lifecycle
- **FileReference** — path-oriented API (like pathlib): `'/tmp/foo.txt' asFileReference`
  - `readStream`, `writeStream`, `binaryReadStream`, `binaryWriteStream`
  - `contents`, `readStreamDo:`, `writeStreamDo:`
  - `exists`, `isFile`, `isDirectory`, `size`, `modificationTime`
  - `delete`, `renameTo:`, `copyTo:`, `moveTo:`
  - `parent`, `children`, `allChildren`, `resolve:`, `/` (path join operator)
  - `ensureCreateDirectory`, `ensureDelete` (recursive)
  - `extension`, `basename`, `basenameWithoutExtension`
- **FileSystem** — pluggable backend (disk, memory, zip)
- **FileLocator** — symbolic roots (`FileLocator home`, `FileLocator documents`)

### GNU Smalltalk

- **FileStream** — `FileStream open: 'file.txt' mode: FileStream read`
- **File** — `File name: '/tmp/foo'` returns a File object
  - `exists`, `isDirectory`, `isReadable`, `isWritable`, `isExecutable`
  - `size`, `lastModifyTime`, `lastAccessTime`
  - `remove`, `renameTo:`, `copyTo:`
  - `directory` (parent), `stripPath`, `stripFileName`

### VisualWorks

- **Filename** — path object with `readStream`, `writeStream`, `appendStream`
- Supports `#contentsOfEntireFile`, `#fileIn`, `#fileOut`
- `tail`, `head`, `directory`, `withoutExtension`

### Common Patterns Across Smalltalk Implementations

1. **Instance-based file handles** — open → read/write → close lifecycle
2. **Stream integration** — files produce ReadStream/WriteStream objects
3. **Path as object** (not just string) — FileReference, Filename, etc.
4. **Metadata access** — size, timestamps, permissions
5. **Recursive operations** — allChildren, ensureDelete (rm -rf)
6. **Binary vs text mode** — separate streams for binary data
7. **Glob/pattern matching** — `directory allFiles select: [:f | f extension = 'mag']`
8. **Temp file support** — `FileReference newTempFilePrefix: 'mag'`
9. **Block-scoped streams** — `file readStreamDo: [:s | s contents]` (auto-close)

## Gap Analysis

### Critical Gaps (needed for basic agent file operations)

| Gap | Description | Priority |
|-----|-------------|----------|
| **File metadata** | `size:`, `modificationTime:`, `permissions:` — no way to get file size or timestamps | High |
| **Recursive delete** | `deleteAll:` (rm -rf) — current `delete:` only handles empty dirs | High |
| **Glob/pattern matching** | `glob:` or `glob:in:` — no way to find files by pattern | High |
| **Temp files** | `tempFile` or `tempFileWithPrefix:` — no temp file creation | Medium |
| **Binary I/O** | All current I/O is string-only; no byte array support | Medium |
| **Streaming reads** | No way to read large files incrementally (line-by-line, chunk-by-chunk) | Medium |

### Moderate Gaps (useful but not blocking)

| Gap | Description | Priority |
|-----|-------------|----------|
| **Path object** | Files are bare strings, no FileReference-style wrapping | Low |
| **File permissions** | No `chmod:to:`, `isReadable:`, `isWritable:`, `isExecutable:` | Low |
| **Symlink support** | No `createSymlink:to:`, `isSymlink:`, `readLink:` | Low |
| **Change directory** | No `setWorkingDirectory:` (CWD change) | Low |
| **Watch/notify** | No filesystem event watching (inotify/kqueue) | Low |
| **File locking** | No advisory locking (flock) | Low |
| **Maggie-side wrapper** | No `File.mag` — all primitives are Go-only, no Maggie convenience methods | Low |

### Not Gaps (already covered elsewhere)

- **Process execution**: Covered by `ExternalProcess` class (`vm/exec_primitives.go`)
- **HTTP I/O**: Covered by `HttpClient`/`HttpServer` classes
- **JSON/TOML serialization**: Covered by `Json`/`Toml` classes
- **SQLite/DuckDB**: Covered by dedicated primitive files

## Recommendations

For an agent performing basic file operations, the **minimum viable additions** are:

1. **`size:`** — file size in bytes (trivial: `os.Stat` → `info.Size()`)
2. **`modificationTime:`** — mtime as epoch milliseconds (trivial: `info.ModTime()`)
3. **`deleteAll:`** — recursive delete (`os.RemoveAll`)
4. **`glob:in:`** — find files by pattern (`filepath.Glob` or `filepath.WalkDir` + match)
5. **`tempFile`** / **`tempFileWithPrefix:`** — create temp files (`os.CreateTemp`)

These are all straightforward 5-15 line Go primitives following the existing pattern.

A streaming/handle-based API (FileStream) would be a larger design effort and is
not strictly needed if files fit in memory — the current slurp-and-dump approach
works for config files, source code, and typical agent workloads.
