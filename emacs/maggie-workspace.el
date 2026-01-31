;;; maggie-workspace.el --- Workspace buffers with persistent locals -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Workspace buffers for the Maggie language server.
;; Variables assigned in evaluated expressions persist across evaluations
;; within the same workspace.  Each workspace maintains its own set of
;; local variables (independent of the global namespace).

;;; Code:

(require 'maggie-connection)

;; --- State (buffer-local) ---

(defvar-local maggie-workspace--locals nil
  "Alist of local variables for this workspace.
Each entry is (NAME . HANDLE-ALIST) where HANDLE-ALIST has
`id', `className', `displayString' keys.")

(defvar-local maggie-workspace--name nil
  "Name of this workspace.")

;; --- Evaluate with locals ---

(defun maggie-workspace--build-locals ()
  "Build the locals list for an EvaluateWithLocals request."
  (let (locals)
    (dolist (entry maggie-workspace--locals)
      (let* ((name (car entry))
             (handle (cdr entry)))
        (push `((name . ,name)
                (value . ,(alist-get 'displayString handle))
                (handle . ((id . ,(alist-get 'id handle))
                           (className . ,(alist-get 'className handle))
                           (displayString . ,(alist-get 'displayString handle)))))
              locals)))
    (vconcat (nreverse locals))))

(defun maggie-workspace--update-locals (updated)
  "Update workspace locals from UPDATED (vector of local-variable alists)."
  (when updated
    (let ((vec (if (vectorp updated) updated (vconcat updated))))
      (dotimes (i (length vec))
        (let* ((local (aref vec i))
               (name (alist-get 'name local))
               (handle (alist-get 'handle local)))
          (when (and name handle)
            (setf (alist-get name maggie-workspace--locals nil nil #'equal)
                  handle)))))))

(defun maggie-workspace-eval ()
  "Evaluate selected expression (or current line) with workspace locals.
Variables assigned persist across evaluations within this workspace."
  (interactive)
  (unless (maggie-connected-p)
    (error "Not connected to Maggie server (run M-x maggie-connect)"))
  (let* ((source (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (string-trim (thing-at-point 'line t))))
         (locals (maggie-workspace--build-locals))
         (resp (maggie--connect-call-sync
                "maggie.v1.ModificationService" "EvaluateWithLocals"
                `((source . ,source)
                  (sessionId . ,(or maggie--session-id ""))
                  (locals . ,locals)))))
    (if (eq (alist-get 'success resp) :json-false)
        (message "Error: %s" (alist-get 'errorMessage resp))
      ;; Update locals with any new/modified variables
      (maggie-workspace--update-locals (alist-get 'updatedLocals resp))
      (let ((result (alist-get 'result resp)))
        (message "=> %s" result)))))

(defun maggie-workspace-eval-and-insert ()
  "Evaluate and insert result after point/selection.
Variables persist across evaluations."
  (interactive)
  (unless (maggie-connected-p)
    (error "Not connected to Maggie server (run M-x maggie-connect)"))
  (let* ((source (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (string-trim (thing-at-point 'line t))))
         (locals (maggie-workspace--build-locals))
         (resp (maggie--connect-call-sync
                "maggie.v1.ModificationService" "EvaluateWithLocals"
                `((source . ,source)
                  (sessionId . ,(or maggie--session-id ""))
                  (locals . ,locals)))))
    (if (eq (alist-get 'success resp) :json-false)
        (message "Error: %s" (alist-get 'errorMessage resp))
      (maggie-workspace--update-locals (alist-get 'updatedLocals resp))
      (let ((result (alist-get 'result resp)))
        (when (use-region-p)
          (goto-char (region-end)))
        (insert " " result)))))

(defun maggie-workspace-show-locals ()
  "Display the current workspace local variables."
  (interactive)
  (if (null maggie-workspace--locals)
      (message "No workspace locals")
    (let ((buf (get-buffer-create (format "*Maggie Locals: %s*"
                                         (or maggie-workspace--name "workspace")))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Workspace Locals\n" 'face 'bold))
          (insert (make-string 40 ?-) "\n")
          (dolist (entry maggie-workspace--locals)
            (let* ((name (car entry))
                   (handle (cdr entry))
                   (class (alist-get 'className handle))
                   (display (alist-get 'displayString handle)))
              (insert (format "%-20s %-15s %s\n" name class display))))
          (goto-char (point-min))
          (special-mode)))
      (display-buffer buf))))

(defun maggie-workspace-clear-locals ()
  "Clear all workspace local variables."
  (interactive)
  (setq maggie-workspace--locals nil)
  (message "Workspace locals cleared"))

;; --- Mode ---

(defvar maggie-workspace-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map maggie-mode-map)
    (define-key map (kbd "C-c C-d") #'maggie-workspace-eval)
    (define-key map (kbd "C-c C-p") #'maggie-workspace-eval-and-insert)
    (define-key map (kbd "C-c C-l") #'maggie-workspace-show-locals)
    (define-key map (kbd "C-c C-k") #'maggie-workspace-clear-locals)
    map))

(define-derived-mode maggie-workspace-mode maggie-mode "Maggie-Workspace"
  "Major mode for Maggie workspace buffers with persistent locals.
Variables assigned during evaluation persist across evaluations.

\\{maggie-workspace-mode-map}")

;; --- Entry point ---

;;;###autoload
(defun maggie-workspace (&optional name)
  "Open a new Maggie workspace buffer.
With prefix arg, prompt for workspace NAME."
  (interactive
   (list (when current-prefix-arg
           (read-string "Workspace name: "))))
  (unless (maggie-connected-p)
    (call-interactively #'maggie-connect))
  (let* ((ws-name (or name "scratch"))
         (buf-name (format "*Maggie Workspace: %s*" ws-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'maggie-workspace-mode)
        (maggie-workspace-mode)
        (setq maggie-workspace--name ws-name)
        (insert (format "\"Maggie Workspace: %s\"\n" ws-name))
        (insert "\"C-c C-d: evaluate | C-c C-p: print | C-c C-l: locals | C-c C-k: clear\"\n\n")))
    (switch-to-buffer buf)))

(provide 'maggie-workspace)
;;; maggie-workspace.el ends here
