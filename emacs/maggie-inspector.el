;;; maggie-inspector.el --- Object inspector for Maggie -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Interactive object inspector for the Maggie language server (Phase 4).
;; Talks to InspectionService via Connect HTTP/JSON to inspect live objects,
;; drill into slots, send messages, and navigate object graphs.
;;
;; Entry point: `maggie-inspect-object' (called by `maggie-inspect-it').

;;; Code:

(require 'maggie-connection)

;; --- Buffer name ---

(defconst maggie-inspector-buffer "*Maggie Inspector*"
  "Buffer name for the Maggie object inspector.")

;; --- Buffer-local state ---

(defvar-local maggie-inspector--handle-id nil
  "Current object's handle ID.")

(defvar-local maggie-inspector--class-name nil
  "Current object's class name.")

(defvar-local maggie-inspector--display nil
  "Current object's display string.")

(defvar-local maggie-inspector--slots nil
  "Vector of slot alists from the server response.")

(defvar-local maggie-inspector--history nil
  "List of previous handle IDs for back navigation.")

;; --- Server calls ---

(defun maggie-inspector--fetch (handle-id)
  "Call InspectionService/Inspect for HANDLE-ID and return the response alist."
  (maggie--connect-call-sync
   "maggie.v1.InspectionService" "Inspect"
   `((handleId . ,handle-id))))

(defun maggie-inspector--fetch-slot (handle-id slot-name)
  "Call InspectionService/InspectSlot for HANDLE-ID and SLOT-NAME.
Return the response alist."
  (maggie--connect-call-sync
   "maggie.v1.InspectionService" "InspectSlot"
   `((handleId . ,handle-id)
     (slotName . ,slot-name))))

(defun maggie-inspector--fetch-index (handle-id index)
  "Call InspectionService/InspectIndex for HANDLE-ID at INDEX.
Return the response alist."
  (maggie--connect-call-sync
   "maggie.v1.InspectionService" "InspectIndex"
   `((handleId . ,handle-id)
     (index . ,index))))

(defun maggie-inspector--send-message (handle-id selector &optional arguments)
  "Call InspectionService/SendMessage on HANDLE-ID with SELECTOR and ARGUMENTS.
ARGUMENTS is a list of source expression strings.
Return the response alist."
  (maggie--connect-call-sync
   "maggie.v1.InspectionService" "SendMessage"
   `((handleId . ,handle-id)
     (selector . ,selector)
     (arguments . ,(vconcat (or arguments '()))))))

(defun maggie-inspector--release-handle (handle-id)
  "Call InspectionService/ReleaseHandle for HANDLE-ID.
Return the response alist."
  (maggie--connect-call-sync
   "maggie.v1.InspectionService" "ReleaseHandle"
   `((handleId . ,handle-id))))

;; --- Rendering ---

(defun maggie-inspector--render (resp)
  "Render the inspector buffer from RESP, an InspectResponse alist."
  (let ((class-name (alist-get 'className resp))
        (display (alist-get 'displayString resp))
        (slots (alist-get 'slots resp))
        (handle (alist-get 'handle resp))
        (is-indexable (alist-get 'isIndexable resp))
        (indexable-size (alist-get 'indexableSize resp)))
    (let ((handle-id (alist-get 'id handle)))
      ;; Update buffer-local state
      (setq maggie-inspector--handle-id handle-id)
      (setq maggie-inspector--class-name class-name)
      (setq maggie-inspector--display display)
      (setq maggie-inspector--slots slots)
      ;; Render
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header: class name and handle ID
        (insert (propertize (format "ClassName: %s" (or class-name "?"))
                            'face 'bold))
        (insert (propertize (format "%s[handle: %s]"
                                    (make-string (max 1 (- 40 (length (or class-name "?"))))
                                                 ?\s)
                                    (or handle-id "?"))
                            'face 'font-lock-comment-face))
        (insert "\n")
        ;; Display string
        (insert (or display "") "\n")
        ;; Separator
        (insert (make-string 50 ?-) "\n")
        ;; Slots header
        (insert (propertize "Slots:" 'face 'bold) "\n")
        ;; Slot lines
        (if (or (null slots) (and (sequencep slots) (= (length slots) 0)))
            (insert "  (no slots)\n")
          (dolist (slot slots)
            (let* ((name (alist-get 'name slot))
                   (value-class (alist-get 'valueClass slot))
                   (value-display (alist-get 'valueDisplay slot))
                   (value-handle (alist-get 'valueHandle slot))
                   (slot-handle-id (alist-get 'id value-handle)))
              (insert (format "  %-20s %-15s %s  "
                              (or name "")
                              (or value-class "")
                              (or value-display "")))
              (when slot-handle-id
                (insert-text-button "[drill]"
                                    'action (lambda (_)
                                              (maggie-inspector--drill-slot slot-handle-id))
                                    'handle-id slot-handle-id
                                    'follow-link t
                                    'face 'link))
              (insert "\n"))))
        ;; Indexable info
        (when (eq is-indexable t)
          (insert "\n")
          (insert (propertize (format "Indexable: %d elements"
                                      (or indexable-size 0))
                              'face 'font-lock-comment-face))
          (insert "\n"))
        (goto-char (point-min))))))

;; --- Navigation ---

(defun maggie-inspector--drill-slot (handle-id)
  "Push current handle onto history and inspect the object at HANDLE-ID."
  (when maggie-inspector--handle-id
    (push maggie-inspector--handle-id maggie-inspector--history))
  (let ((resp (maggie-inspector--fetch handle-id)))
    (maggie-inspector--render resp)))

(defun maggie-inspector--drill-index (index)
  "Call InspectIndex for INDEX, push current handle onto history, render result."
  (when maggie-inspector--handle-id
    (push maggie-inspector--handle-id maggie-inspector--history))
  (let ((resp (maggie-inspector--fetch-index maggie-inspector--handle-id index)))
    (maggie-inspector--render resp)))

;; --- Interactive commands ---

(defun maggie-inspector-back ()
  "Pop from history and inspect the previous handle."
  (interactive)
  (if (null maggie-inspector--history)
      (message "No previous object in history")
    (let* ((prev-id (pop maggie-inspector--history))
           (resp (maggie-inspector--fetch prev-id)))
      (maggie-inspector--render resp))))

(defun maggie-inspector-refresh ()
  "Re-fetch and re-render the current inspected object."
  (interactive)
  (if (null maggie-inspector--handle-id)
      (message "No object to refresh")
    (let ((resp (maggie-inspector--fetch maggie-inspector--handle-id)))
      (maggie-inspector--render resp))))

(defun maggie-inspector-eval (expr)
  "Send message EXPR to the current inspected object and display the result.
Prompts for a selector expression interactively."
  (interactive "sEval on object (selector): ")
  (unless maggie-inspector--handle-id
    (error "No object being inspected"))
  (let ((resp (maggie-inspector--send-message maggie-inspector--handle-id expr)))
    (if (eq (alist-get 'success resp) :json-false)
        (message "Error: %s" (alist-get 'errorMessage resp))
      (message "=> %s" (alist-get 'result resp)))))

(defun maggie-inspector-inspect-result (expr)
  "Send message EXPR to the current object and drill into the result.
Prompts for a selector expression interactively."
  (interactive "sEval and inspect (selector): ")
  (unless maggie-inspector--handle-id
    (error "No object being inspected"))
  (let ((resp (maggie-inspector--send-message maggie-inspector--handle-id expr)))
    (if (eq (alist-get 'success resp) :json-false)
        (message "Error: %s" (alist-get 'errorMessage resp))
      (let* ((result-handle (alist-get 'resultHandle resp))
             (result-id (alist-get 'id result-handle)))
        (if (null result-id)
            (message "No result handle returned (result: %s)" (alist-get 'result resp))
          (maggie-inspector--drill-slot result-id))))))

(defun maggie-inspector-release ()
  "Release the current object handle without closing the buffer."
  (interactive)
  (if (null maggie-inspector--handle-id)
      (message "No handle to release")
    (let ((resp (maggie-inspector--release-handle maggie-inspector--handle-id)))
      (if (eq (alist-get 'success resp) t)
          (message "Released handle %s" maggie-inspector--handle-id)
        (message "Failed to release handle %s" maggie-inspector--handle-id)))))

(defun maggie-inspector-quit ()
  "Release the current handle and kill the inspector buffer."
  (interactive)
  (when maggie-inspector--handle-id
    (ignore-errors
      (maggie-inspector--release-handle maggie-inspector--handle-id)))
  (kill-buffer (current-buffer)))

;; --- Mode ---

(defvar maggie-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'maggie-inspector-back)
    (define-key map (kbd "g") #'maggie-inspector-refresh)
    (define-key map (kbd "e") #'maggie-inspector-eval)
    (define-key map (kbd "E") #'maggie-inspector-inspect-result)
    (define-key map (kbd "q") #'maggie-inspector-quit)
    (define-key map (kbd "r") #'maggie-inspector-release)
    map)
  "Keymap for `maggie-inspector-mode'.")

(define-derived-mode maggie-inspector-mode special-mode "Maggie-Inspector"
  "Major mode for the Maggie object inspector.

\\{maggie-inspector-mode-map}")

;; --- Entry point ---

;;;###autoload
(defun maggie-inspect-object (handle-id)
  "Create an inspector buffer and inspect the object at HANDLE-ID."
  (unless (maggie-connected-p)
    (error "Not connected to Maggie server (run M-x maggie-connect)"))
  (let ((resp (maggie-inspector--fetch handle-id))
        (buf (get-buffer-create maggie-inspector-buffer)))
    (with-current-buffer buf
      (maggie-inspector-mode)
      (setq maggie-inspector--history nil)
      (maggie-inspector--render resp))
    (pop-to-buffer buf)
    (message "Inspecting %s (%s)"
             (alist-get 'displayString resp)
             (alist-get 'className resp))))

(provide 'maggie-inspector)
;;; maggie-inspector.el ends here
