;;; maggie-lsp.el --- LSP support for Maggie via eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "29.1") (eglot "1.9"))

;;; Commentary:

;; Configures eglot to use `mag --lsp' as the language server for
;; maggie-mode.  Load this file and call `maggie-lsp-ensure' in a
;; .mag buffer (or add it to `maggie-mode-hook').

;;; Code:

(require 'eglot)
(require 'maggie-mode)

(add-to-list 'eglot-server-programs
             '(maggie-mode . ("mag" "--lsp")))

;;;###autoload
(defun maggie-lsp-ensure ()
  "Enable eglot LSP support in the current Maggie buffer."
  (interactive)
  (eglot-ensure))

(provide 'maggie-lsp)
;;; maggie-lsp.el ends here
