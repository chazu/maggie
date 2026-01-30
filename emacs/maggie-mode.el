;;; maggie-mode.el --- Major mode for Maggie/Trashtalk source -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Major mode for editing Maggie (.mag) source files.
;; Provides syntax highlighting and keybindings for the language server.

;;; Code:

(require 'maggie-eval)

(defgroup maggie nil
  "Maggie language support."
  :group 'languages
  :prefix "maggie-")

;; Syntax table
(defvar maggie-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Strings use single quotes
    (modify-syntax-entry ?' "\"" st)
    ;; Double-quote for comments in Smalltalk tradition
    (modify-syntax-entry ?\" "!" st)
    ;; Parentheses
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Symbol constituents
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?# "'" st)
    (modify-syntax-entry ?^ "." st)
    (modify-syntax-entry ?| "." st)
    st)
  "Syntax table for `maggie-mode'.")

;; Font-lock keywords
(defvar maggie-font-lock-keywords
  `(
    ;; Class definitions: "ClassName < SuperClass"
    ("^\\([A-Z][A-Za-z0-9_]*\\)\\s-*<\\s-*\\([A-Z][A-Za-z0-9_]*\\)"
     (1 font-lock-type-face)
     (2 font-lock-type-face))

    ;; Class name at start of line (standalone)
    ("^\\([A-Z][A-Za-z0-9_]*\\)$"
     (1 font-lock-type-face))

    ;; Instance variable declarations: "| foo bar baz |"
    ("|\\([A-Za-z0-9_ ]+\\)|"
     (1 font-lock-variable-name-face))

    ;; Keyword message selectors (word:)
    ("\\([A-Za-z_][A-Za-z0-9_]*:\\)"
     (1 font-lock-function-name-face))

    ;; Symbols (#foo, #foo:bar:)
    ("#\\([A-Za-z_][A-Za-z0-9_:]*\\)"
     (0 font-lock-constant-face))

    ;; Block arguments ([:arg | ...])
    (":\\([A-Za-z_][A-Za-z0-9_]*\\)"
     (0 font-lock-variable-name-face))

    ;; Return operator
    ("\\^" (0 font-lock-keyword-face))

    ;; Assignment
    ("\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*:="
     (1 font-lock-variable-name-face))

    ;; Built-in pseudo-variables
    ("\\_<\\(self\\|super\\|true\\|false\\|nil\\|thisContext\\)\\_>"
     (1 font-lock-builtin-face))

    ;; Numbers
    ("\\_<-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e[+-]?[0-9]+\\)?"
     (0 font-lock-constant-face))

    ;; Class references (capitalized words in code)
    ("\\_<\\([A-Z][A-Za-z0-9_]*\\)\\_>"
     (1 font-lock-type-face))

    ;; Comments (double-quoted in Smalltalk)
    ("\"[^\"]*\""
     (0 font-lock-comment-face t))
    )
  "Font-lock keywords for `maggie-mode'.")

;; Indentation (simple: 4-space tab stops)
(defun maggie-indent-line ()
  "Indent current line of Maggie code."
  (interactive)
  (let ((indent 4)
        (cur-indent 0))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq cur-indent 0)
        ;; Look at previous line to determine indentation
        (forward-line -1)
        (setq cur-indent (current-indentation))
        (forward-line 1)
        (beginning-of-line)
        ;; Increase indent after class header or method header
        (save-excursion
          (forward-line -1)
          (beginning-of-line)
          (when (or (looking-at "\\s-*[A-Z][A-Za-z0-9_]*\\s-*\\(<\\|$\\)")
                    (looking-at "^[A-Za-z_:]+\\s-*$"))
            (setq cur-indent (+ cur-indent indent))))
        ;; Decrease indent for lines that start with dedent tokens
        (when (looking-at "\\s-*[]})]")
          (setq cur-indent (max 0 (- cur-indent indent))))))
    (indent-line-to cur-indent)))

;;;###autoload
(define-derived-mode maggie-mode prog-mode "Maggie"
  "Major mode for editing Maggie/Trashtalk source files."
  :syntax-table maggie-mode-syntax-table
  (setq-local font-lock-defaults '(maggie-font-lock-keywords))
  (setq-local indent-line-function #'maggie-indent-line)
  (setq-local comment-start "\"")
  (setq-local comment-end "\"")
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mag\\'" . maggie-mode))

(provide 'maggie-mode)
;;; maggie-mode.el ends here
