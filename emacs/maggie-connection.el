;;; maggie-connection.el --- Connect protocol HTTP client for Maggie -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; HTTP/JSON client for the Maggie language server using the Connect protocol.
;; Connect serves protobuf services over HTTP/1.1 with JSON bodies,
;; so Emacs can call them with `url-retrieve' — no gRPC or subprocess bridge needed.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

(defcustom maggie-server-host "localhost"
  "Hostname of the Maggie language server."
  :type 'string
  :group 'maggie)

(defcustom maggie-server-port 4567
  "Port of the Maggie language server."
  :type 'integer
  :group 'maggie)

(defvar maggie--session-id nil
  "Current session ID for the connected server.")

(defun maggie-server-url ()
  "Return the base URL for the Maggie language server."
  (format "http://%s:%d" maggie-server-host maggie-server-port))

(defun maggie--connect-call (service method payload callback)
  "Call SERVICE/METHOD on the Maggie server with JSON PAYLOAD.
CALLBACK is called with the parsed JSON response object.
SERVICE is the fully-qualified service name, e.g. \"maggie.v1.EvaluationService\".
METHOD is the RPC method name, e.g. \"Evaluate\"."
  (let* ((url (format "%s/%s/%s" (maggie-server-url) service method))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
    (url-retrieve url
                  (lambda (status cb)
                    (maggie--handle-response status cb))
                  (list callback)
                  t)))  ; silent

(defun maggie--connect-call-sync (service method payload)
  "Synchronous version of `maggie--connect-call'.
Returns the parsed JSON response or signals an error."
  (let* ((url (format "%s/%s/%s" (maggie-server-url) service method))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (buf (url-retrieve-synchronously url t t 10)))
    (if (not buf)
        (error "Failed to connect to Maggie server at %s" (maggie-server-url))
      (with-current-buffer buf
        (goto-char (point-min))
        ;; Skip HTTP headers
        (re-search-forward "\r?\n\r?\n" nil t)
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (condition-case err
              (prog1 (json-read)
                (kill-buffer))
            (error
             (kill-buffer)
             (error "Failed to parse server response: %s" (error-message-string err)))))))))

(defun maggie--handle-response (status callback)
  "Handle the HTTP response: parse JSON body and call CALLBACK."
  (if (plist-get status :error)
      (progn
        (message "Maggie server error: %s" (plist-get status :error))
        (kill-buffer))
    (goto-char (point-min))
    ;; Skip HTTP headers
    (re-search-forward "\r?\n\r?\n" nil t)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (condition-case err
          (let ((result (json-read)))
            (kill-buffer)
            (funcall callback result))
        (error
         (kill-buffer)
         (message "Maggie: failed to parse response: %s" (error-message-string err)))))))

;;;###autoload
(defun maggie-connect (&optional host port)
  "Connect to the Maggie language server at HOST:PORT."
  (interactive
   (list (read-string "Host: " maggie-server-host)
         (read-number "Port: " maggie-server-port)))
  (when host (setq maggie-server-host host))
  (when port (setq maggie-server-port port))
  ;; Create a session
  (let ((resp (maggie--connect-call-sync
               "maggie.v1.SessionService" "CreateSession"
               '((name . "emacs")))))
    (setq maggie--session-id (alist-get 'sessionId resp))
    (message "Connected to Maggie server at %s (session: %s)"
             (maggie-server-url) maggie--session-id)))

(defun maggie-disconnect ()
  "Disconnect from the Maggie language server."
  (interactive)
  (when maggie--session-id
    (ignore-errors
      (maggie--connect-call-sync
       "maggie.v1.SessionService" "DestroySession"
       `((sessionId . ,maggie--session-id))))
    (setq maggie--session-id nil)
    (message "Disconnected from Maggie server")))

(defun maggie-connected-p ()
  "Return non-nil if connected to the Maggie server."
  (not (null maggie--session-id)))

(provide 'maggie-connection)
;;; maggie-connection.el ends here
