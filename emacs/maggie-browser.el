;;; maggie-browser.el --- System browser for Maggie -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Maggie Authors
;; Author: Maggie Authors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Three-pane system browser for the Maggie language server.
;; - Left pane: class list
;; - Top-right pane: method list (with instance/class toggle)
;; - Bottom-right pane: method source code

;;; Code:

(require 'maggie-connection)

;; --- State ---

(defvar maggie-browser--current-class nil "Currently selected class name.")
(defvar maggie-browser--current-method nil "Currently selected method selector.")
(defvar maggie-browser--class-side nil "Non-nil if browsing class-side methods.")
(defvar maggie-browser--class-list nil "Cached list of class info alists.")
(defvar maggie-browser--method-list nil "Cached list of method info alists.")

;; --- Buffer names ---

(defconst maggie-browser-classes-buffer "*Maggie Classes*")
(defconst maggie-browser-methods-buffer "*Maggie Methods*")
(defconst maggie-browser-source-buffer "*Maggie Source*")

;; --- Class list ---

(defun maggie-browser--fetch-classes ()
  "Fetch all classes from the server."
  (let ((resp (maggie--connect-call-sync
               "maggie.v1.BrowsingService" "ListClasses" '())))
    (setq maggie-browser--class-list (alist-get 'classes resp))))

(defun maggie-browser--render-classes ()
  "Render the class list buffer."
  (with-current-buffer (get-buffer-create maggie-browser-classes-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Classes" 'face 'bold) "\n")
      (insert (make-string 40 ?-) "\n")
      (dolist (cls maggie-browser--class-list)
        (let* ((name (alist-get 'name cls))
               (super (alist-get 'superclassName cls))
               (imethods (or (alist-get 'instanceMethodCount cls) 0))
               (cmethods (or (alist-get 'classMethodCount cls) 0))
               (line (format "%-30s %3d/%d" name imethods cmethods)))
          (insert-text-button line
                              'action (lambda (_) (maggie-browser--select-class name))
                              'class-name name
                              'follow-link t
                              'face (if (equal name maggie-browser--current-class)
                                        'highlight nil))
          (when super
            (insert (propertize (format "  < %s" super) 'face 'font-lock-comment-face)))
          (insert "\n")))
      (goto-char (point-min))
      (maggie-browser-classes-mode))))

(defun maggie-browser--select-class (name)
  "Select a class and load its methods."
  (setq maggie-browser--current-class name)
  (setq maggie-browser--current-method nil)
  (maggie-browser--render-classes)
  (maggie-browser--fetch-and-render-methods))

;; --- Method list ---

(defun maggie-browser--fetch-methods ()
  "Fetch methods for the current class."
  (when maggie-browser--current-class
    (let* ((side (if maggie-browser--class-side "CLASS" "INSTANCE"))
           (resp (maggie--connect-call-sync
                  "maggie.v1.BrowsingService" "ListMethods"
                  `((className . ,maggie-browser--current-class)
                    (side . ,side)))))
      (setq maggie-browser--method-list (alist-get 'methods resp)))))

(defun maggie-browser--render-methods ()
  "Render the method list buffer."
  (with-current-buffer (get-buffer-create maggie-browser-methods-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize
               (format "%s %s"
                       (or maggie-browser--current-class "No class selected")
                       (if maggie-browser--class-side "[class]" "[instance]"))
               'face 'bold)
              "  ")
      (insert-text-button (if maggie-browser--class-side "[instance]" "[class]")
                          'action (lambda (_) (maggie-browser--toggle-side))
                          'follow-link t
                          'face 'link)
      (insert "\n")
      (insert (make-string 40 ?-) "\n")
      (if (not maggie-browser--method-list)
          (insert "(no methods)\n")
        (dolist (m maggie-browser--method-list)
          (let* ((sel (alist-get 'selector m))
                 (prim (eq (alist-get 'isPrimitive m) t)))
            (insert-text-button
             (format "%s%s" sel (if prim " [prim]" ""))
             'action (lambda (_) (maggie-browser--select-method sel))
             'method-selector sel
             'follow-link t
             'face (if (equal sel maggie-browser--current-method)
                       'highlight nil))
            (insert "\n"))))
      (goto-char (point-min))
      (maggie-browser-methods-mode))))

(defun maggie-browser--fetch-and-render-methods ()
  "Fetch and render methods for the current class."
  (maggie-browser--fetch-methods)
  (maggie-browser--render-methods)
  ;; Clear source if no method selected
  (unless maggie-browser--current-method
    (maggie-browser--render-source nil)))

(defun maggie-browser--toggle-side ()
  "Toggle between instance and class methods."
  (setq maggie-browser--class-side (not maggie-browser--class-side))
  (setq maggie-browser--current-method nil)
  (maggie-browser--fetch-and-render-methods))

(defun maggie-browser--select-method (selector)
  "Select a method and show its source."
  (setq maggie-browser--current-method selector)
  (maggie-browser--render-methods)
  (maggie-browser--fetch-and-render-source))

;; --- Source view ---

(defun maggie-browser--fetch-and-render-source ()
  "Fetch and display source for the current method."
  (when (and maggie-browser--current-class maggie-browser--current-method)
    (let* ((resp (maggie--connect-call-sync
                  "maggie.v1.BrowsingService" "GetMethod"
                  `((className . ,maggie-browser--current-class)
                    (selector . ,maggie-browser--current-method)
                    (classSide . ,(if maggie-browser--class-side t :json-false)))))
           (method-info (alist-get 'method resp))
           (source (alist-get 'source method-info))
           (category (alist-get 'category resp))
           (arity (alist-get 'arity resp)))
      (maggie-browser--render-source
       (list (cons 'source source)
             (cons 'category category)
             (cons 'arity arity)
             (cons 'isPrimitive (alist-get 'isPrimitive method-info)))))))

(defun maggie-browser--render-source (info)
  "Render the source view buffer. INFO is an alist or nil."
  (with-current-buffer (get-buffer-create maggie-browser-source-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (not info)
          (insert "(select a method to view source)\n")
        (let ((source (alist-get 'source info))
              (category (alist-get 'category info))
              (arity (alist-get 'arity info))
              (prim (alist-get 'isPrimitive info)))
          (insert (propertize
                   (format "%s >> %s%s"
                           maggie-browser--current-class
                           (if maggie-browser--class-side "class " "")
                           maggie-browser--current-method)
                   'face 'bold)
                  "\n")
          (when category
            (insert (propertize (format "category: %s" category)
                                'face 'font-lock-comment-face)
                    "\n"))
          (when arity
            (insert (propertize (format "arity: %s" arity)
                                'face 'font-lock-comment-face)
                    "\n"))
          (insert (make-string 50 ?-) "\n")
          (cond
           ((eq prim t)
            (insert (propertize "<primitive method>" 'face 'font-lock-warning-face) "\n"))
           ((and source (not (string-empty-p source)))
            (insert source "\n"))
           (t
            (insert "(no source available)\n")))))
      (goto-char (point-min))
      (maggie-mode))))

;; --- Find senders / implementors ---

(defun maggie-browser-find-senders (selector)
  "Find all senders of SELECTOR."
  (interactive
   (list (read-string "Find senders of: "
                       (or maggie-browser--current-method ""))))
  (let* ((resp (maggie--connect-call-sync
                "maggie.v1.BrowsingService" "FindSenders"
                `((selector . ,selector))))
         (senders (alist-get 'senders resp))
         (buf (get-buffer-create "*Maggie Senders*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Senders of #%s (%d)\n" selector (length senders))
                            'face 'bold))
        (insert (make-string 50 ?-) "\n")
        (dolist (s senders)
          (let ((cls (alist-get 'className s))
                (msel (alist-get 'methodSelector s))
                (cside (eq (alist-get 'isClassSide s) t)))
            (insert-text-button
             (format "%s%s >> %s" cls (if cside " class" "") msel)
             'action (lambda (_)
                       (maggie-browser--select-class cls)
                       (setq maggie-browser--class-side cside)
                       (maggie-browser--fetch-and-render-methods)
                       (maggie-browser--select-method msel))
             'follow-link t)
            (insert "\n")))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun maggie-browser-find-implementors (selector)
  "Find all implementors of SELECTOR."
  (interactive
   (list (read-string "Find implementors of: "
                       (or maggie-browser--current-method ""))))
  (let* ((resp (maggie--connect-call-sync
                "maggie.v1.BrowsingService" "FindImplementors"
                `((selector . ,selector))))
         (impls (alist-get 'implementors resp))
         (buf (get-buffer-create "*Maggie Implementors*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Implementors of #%s (%d)\n" selector (length impls))
                            'face 'bold))
        (insert (make-string 50 ?-) "\n")
        (dolist (impl impls)
          (let ((cls (alist-get 'className impl))
                (cside (eq (alist-get 'isClassSide impl) t)))
            (insert-text-button
             (format "%s%s" cls (if cside " class" ""))
             'action (lambda (_)
                       (maggie-browser--select-class cls)
                       (setq maggie-browser--class-side cside)
                       (maggie-browser--fetch-and-render-methods)
                       (maggie-browser--select-method selector))
             'follow-link t)
            (insert "\n")))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;; --- Layout ---

(defun maggie-browser--setup-layout ()
  "Set up the three-pane browser layout."
  (delete-other-windows)
  ;; Left: class list (1/3 width)
  (let ((class-win (selected-window))
        (right-win (split-window-right (/ (frame-width) 3))))
    (set-window-buffer class-win (get-buffer-create maggie-browser-classes-buffer))
    ;; Right top: method list
    (select-window right-win)
    (let ((method-win (selected-window))
          (source-win (split-window-below (/ (frame-height) 3))))
      (set-window-buffer method-win (get-buffer-create maggie-browser-methods-buffer))
      ;; Right bottom: source view
      (set-window-buffer source-win (get-buffer-create maggie-browser-source-buffer)))
    ;; Focus on class list
    (select-window class-win)))

;; --- Modes ---

(defvar maggie-browser-classes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'maggie-browser-refresh)
    (define-key map (kbd "q") #'maggie-browser-quit)
    (define-key map (kbd "s") #'maggie-browser-find-senders)
    (define-key map (kbd "i") #'maggie-browser-find-implementors)
    (define-key map (kbd "/") #'maggie-browser-search-classes)
    map))

(define-derived-mode maggie-browser-classes-mode special-mode "Maggie-Classes"
  "Mode for the Maggie class list pane.")

(defvar maggie-browser-methods-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'maggie-browser-refresh)
    (define-key map (kbd "q") #'maggie-browser-quit)
    (define-key map (kbd "s") #'maggie-browser-find-senders)
    (define-key map (kbd "i") #'maggie-browser-find-implementors)
    (define-key map (kbd "t") #'maggie-browser--toggle-side)
    map))

(define-derived-mode maggie-browser-methods-mode special-mode "Maggie-Methods"
  "Mode for the Maggie method list pane.")

;; --- Commands ---

(defun maggie-browser-refresh ()
  "Refresh the browser (re-fetch classes and methods)."
  (interactive)
  (maggie-browser--fetch-classes)
  (maggie-browser--render-classes)
  (when maggie-browser--current-class
    (maggie-browser--fetch-and-render-methods)
    (when maggie-browser--current-method
      (maggie-browser--fetch-and-render-source))))

(defun maggie-browser-quit ()
  "Close the system browser."
  (interactive)
  (dolist (buf (list maggie-browser-classes-buffer
                     maggie-browser-methods-buffer
                     maggie-browser-source-buffer))
    (when (get-buffer buf)
      (kill-buffer buf))))

(defun maggie-browser-search-classes (query)
  "Search for classes matching QUERY."
  (interactive "sSearch classes: ")
  (let* ((resp (maggie--connect-call-sync
                "maggie.v1.BrowsingService" "SearchClasses"
                `((query . ,query))))
         (classes (alist-get 'classes resp)))
    (setq maggie-browser--class-list classes)
    (maggie-browser--render-classes)
    (message "Found %d classes matching %S" (length classes) query)))

;;;###autoload
(defun maggie-browse-system ()
  "Open the Maggie system browser."
  (interactive)
  (unless (maggie-connected-p)
    (call-interactively #'maggie-connect))
  (setq maggie-browser--current-class nil)
  (setq maggie-browser--current-method nil)
  (setq maggie-browser--class-side nil)
  (maggie-browser--fetch-classes)
  (maggie-browser--render-classes)
  (maggie-browser--render-methods)
  (maggie-browser--render-source nil)
  (maggie-browser--setup-layout)
  (message "Maggie System Browser: %d classes loaded" (length maggie-browser--class-list)))

(provide 'maggie-browser)
;;; maggie-browser.el ends here
