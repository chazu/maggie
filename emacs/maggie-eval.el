;;; maggie-eval.el --- Evaluate Maggie expressions from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Commands for evaluating Maggie expressions via the language server:
;;   maggie-do-it     (C-c C-d) - evaluate, discard result
;;   maggie-print-it  (C-c C-p) - evaluate, print result after selection
;;   maggie-inspect-it (C-c C-i) - evaluate, open inspector (Phase 4)

;;; Code:

(require 'maggie-connection)
(require 'maggie-inspector)

(defun maggie--get-expression ()
  "Get the expression to evaluate.
Uses the active region if set, otherwise the current line."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (string-trim (thing-at-point 'line t))))

(defun maggie--evaluate (source callback)
  "Evaluate SOURCE on the Maggie server, calling CALLBACK with the response."
  (unless (maggie-connected-p)
    (error "Not connected to Maggie server (run M-x maggie-connect)"))
  (maggie--connect-call
   "maggie.v1.EvaluationService" "Evaluate"
   `((source . ,source)
     (sessionId . ,maggie--session-id))
   callback))

(defun maggie--evaluate-sync (source)
  "Evaluate SOURCE synchronously and return the response alist."
  (unless (maggie-connected-p)
    (error "Not connected to Maggie server (run M-x maggie-connect)"))
  (maggie--connect-call-sync
   "maggie.v1.EvaluationService" "Evaluate"
   `((source . ,source)
     (sessionId . ,maggie--session-id))))

;;;###autoload
(defun maggie-do-it ()
  "Evaluate the selected expression (or current line). Result is discarded.
Bound to C-c C-d in `maggie-mode'."
  (interactive)
  (let ((source (maggie--get-expression)))
    (maggie--evaluate
     source
     (lambda (resp)
       (if (eq (alist-get 'success resp) :json-false)
           (message "Error: %s" (alist-get 'errorMessage resp))
         (message "Done."))))))

;;;###autoload
(defun maggie-print-it ()
  "Evaluate the selected expression and insert the result after point/selection.
Bound to C-c C-p in `maggie-mode'."
  (interactive)
  (let ((source (maggie--get-expression)))
    (let ((resp (maggie--evaluate-sync source)))
      (if (eq (alist-get 'success resp) :json-false)
          (message "Error: %s" (alist-get 'errorMessage resp))
        (let ((result (alist-get 'result resp)))
          (when (use-region-p)
            (goto-char (region-end)))
          (insert " " result))))))

;;;###autoload
(defun maggie-inspect-it ()
  "Evaluate the selected expression and open an inspector buffer.
Bound to C-c C-i in `maggie-mode'."
  (interactive)
  (let ((source (maggie--get-expression)))
    (let ((resp (maggie--evaluate-sync source)))
      (if (eq (alist-get 'success resp) :json-false)
          (message "Error: %s" (alist-get 'errorMessage resp))
        (let* ((handle (alist-get 'handle resp))
               (id (alist-get 'id handle)))
          (maggie-inspect-object id))))))

;;;###autoload
(defun maggie-check-syntax ()
  "Check syntax of the selected expression or current line."
  (interactive)
  (let* ((source (maggie--get-expression))
         (resp (maggie--connect-call-sync
                "maggie.v1.EvaluationService" "CheckSyntax"
                `((source . ,source)))))
    (if (eq (alist-get 'valid resp) t)
        (message "Syntax OK")
      (let ((diags (alist-get 'diagnostics resp)))
        (message "Syntax error: %s"
                 (mapconcat (lambda (d) (alist-get 'message d))
                            diags "; "))))))

;; Keybindings (set up by maggie-mode)
(defvar maggie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'maggie-do-it)
    (define-key map (kbd "C-c C-p") #'maggie-print-it)
    (define-key map (kbd "C-c C-i") #'maggie-inspect-it)
    (define-key map (kbd "C-c C-s") #'maggie-check-syntax)
    map)
  "Keymap for `maggie-mode'.")

(provide 'maggie-eval)
;;; maggie-eval.el ends here
