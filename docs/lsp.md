# Maggie LSP Server

The Maggie LSP server provides standard editor features for `.mag` files over the Language Server Protocol. It runs over stdio in the same process as the VM.

## Starting the Server

```bash
mag --lsp
mag lsp
```

Both forms are equivalent. The server reads JSON-RPC on stdin and writes responses to stdout.

You can load libraries before starting the LSP server:

```bash
mag ./lib/... --lsp
```

## Supported Features

| Feature | LSP Method | What It Does |
|---|---|---|
| Completion | `textDocument/completion` | Classes, globals, selectors. Triggers on `.` and `:` |
| Hover | `textDocument/hover` | Class info (hierarchy, ivars, method counts) or selector implementor list |
| Go to Definition | `textDocument/definition` | Jump to class or selector implementors |
| Find References | `textDocument/references` | Find all senders of a selector (bytecode scan) |
| Diagnostics | `textDocument/publishDiagnostics` | Syntax errors on open/change |

### Completion

Prefix-matches against:
- Class names (shown as Class kind)
- Global variables (shown as Variable kind)
- Selectors (shown as Function kind)

Limited to 100 results per request.

### Hover

- **Uppercase word** (e.g. `Array`): Shows class name, superclass, instance variables, method counts, and full hierarchy chain.
- **Lowercase word** (e.g. `collect`): Looks up the selector (tries appending `:` for keyword messages) and lists all implementing classes.

### Go to Definition

- **Class name**: Returns `maggie://class/ClassName`
- **Selector**: Returns `maggie://class/ClassName/selector` for each implementor

These are virtual URIs since methods live in the VM image, not source files.

### Find References

Scans bytecode of every compiled method for sends of the given selector, including specialized opcodes (`+`, `-`, `at:`, `value`, etc.).

### Diagnostics

On every `didOpen` and `didChange`, the full document text is compiled as an expression via `VM.CompileExpression`. Parse errors are published as LSP diagnostics.

## Emacs Setup

### lsp-mode

Add to your config:

```elisp
(require 'maggie-mode)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(maggie-mode . "maggie"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("mag" "--lsp"))
    :activation-fn (lsp-activate-on "maggie")
    :server-id 'maggie-lsp
    :major-modes '(maggie-mode))))

(add-hook 'maggie-mode-hook #'lsp-deferred)
```

If `mag` is not on your `exec-path`, use the full path:

```elisp
:new-connection (lsp-stdio-connection '("/path/to/mag" "--lsp"))
```

To load libraries into the VM before serving LSP requests:

```elisp
:new-connection (lsp-stdio-connection '("mag" "./lib/..." "--lsp"))
```

### eglot

The `emacs/maggie-lsp.el` file in this repo provides a ready-made eglot configuration:

```elisp
(add-to-list 'load-path "/path/to/maggie/emacs")
(require 'maggie-lsp)
(add-hook 'maggie-mode-hook #'maggie-lsp-ensure)
```

Or configure manually:

```elisp
(require 'eglot)
(add-to-list 'eglot-server-programs '(maggie-mode . ("mag" "--lsp")))
(add-hook 'maggie-mode-hook #'eglot-ensure)
```

## Architecture

```
Editor (Emacs / VS Code / etc.)
    |  JSON-RPC over stdio
    v
mag --lsp
    |
    +-- GLSP handler callbacks
    |     +-- initialize -> declare capabilities
    |     +-- didOpen/didChange/didClose -> document store + diagnostics
    |     +-- completion -> prefix match classes/globals/selectors
    |     +-- hover -> class info or implementor list
    |     +-- definition -> find implementors
    |     +-- references -> find senders (bytecode scan)
    |
    +-- VMWorker (serializes VM access)
    +-- VM (single-threaded interpreter)
```

The LSP server runs in-process with the VM. It accesses `VMWorker` directly (no network hop), sharing the same VM instance. All VM operations are serialized through the worker goroutine.

## Troubleshooting

**Server doesn't start**: Verify `mag --lsp` works from the command line:

```bash
echo -ne 'Content-Length: 77\r\n\r\n{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}' | mag --lsp
```

You should see a JSON response with `capabilities`.

**No completions**: Completions require a prefix of at least one character. Check that trigger characters (`.` and `:`) are configured in your client.

**Diagnostics not updating**: The server uses full document sync. Every change sends the complete file contents. If your client is configured for incremental sync, diagnostics may not fire.

**Virtual URIs**: Go-to-definition returns `maggie://` URIs since methods are stored in the VM image. Your editor may not know how to open these. This is expected for image-based methods; source-file methods will get file URIs in a future version.
