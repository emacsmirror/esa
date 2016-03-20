;;; esa.el --- Emacs integration for esa.io

;; Original Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Original Created: 21 Jul 2008
;; Original URL: https://github.com/mhayashi1120/yagist.el
;; Author: Nab Inno <nab@blahfe.com>
;; Created: 21 May 2016
;; Version: 0.8.13
;; Keywords: tools esa
;; Package-Requires: ((cl-lib "0.3"))
;; URL: https://github.com/nabinno/esa.el

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; TODO:
;; - Encrypt risky configs

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'derived)
(require 'easy-mmode)


;;; Configurations:

(defgroup esa nil
  "Simple esa application."
  :prefix "esa-"
  :group 'applications)
(defcustom esa-token nil
  "If non-nil, will be used as your Esa OAuth token."
  :group 'esa
  :type 'string)
(defcustom esa-team-name nil
  "If non-nil, will be used as your Esa team name."
  :group 'esa
  :type 'string)
(defcustom esa-view-esa nil
  "If non-nil, automatically use `browse-url' to view esas after they're
posted."
  :type 'boolean
  :group 'esa)
(defcustom esa-display-date-format "%Y-%m-%d %H:%M"
  "Date format displaying in `esa-list' buffer."
  :type 'string
  :group 'esa)
(defvar esa-authenticate-function nil
  "Authentication function symbol.")
(make-obsolete-variable 'esa-authenticate-function nil "0.8.13")
(defvar esa-list-items-per-page nil
  "Number of esa to retrieve a page.")
(defcustom esa-working-directory "~/.esa"
  "*Working directory where to go esa repository is."
  :type 'directory
  :group 'esa)
(defcustom esa-working-directory-alist nil
  "*Alist of esa id as key, value is directory path.
.
Example:
\(setq esa-working-directory-alist
      `((\"1080701\" . \"~/myesa/Emacs-nativechecker\")))
"
  :type '(alist :key-type string
                :value-type directory)
  :group 'esa)


;;; Dispatcher:

(defun esa--read-json (start end)
  (let* ((str (buffer-substring start end))
         (decoded (decode-coding-string str 'utf-8)))
    (json-read-from-string decoded)))
(defun esa-request-0 (auth method url callback &optional json-or-params)
  (let* ((json (and (member method '("POST" "PATCH")) json-or-params))
         (params (and (member method '("GET" "DELETE")) json-or-params))
         (url-request-data (and json (concat (json-encode json) "\n")))
         (url-request-extra-headers
          `(("Authorization" . ,auth)))
         (url-request-method method)
         (url-max-redirection -1)
         (url (if params
                  (concat url "?" (esa-make-query-string params))
                url)))
    (url-retrieve url callback (list url json-or-params))))
(defun esa-request (method url callback &optional json-or-params)
  (let ((token (esa-check-oauth-token)))
    (esa-request-0
     (format "Bearer %s" token)
     method url callback json-or-params)))
;; http://developer.github.com/v3/oauth/#non-web-application-flow
(defun esa-check-oauth-token ()
  (cond
   (esa-token)
   (t
    (browse-url (format "https://%s.esa.io/user/token" esa-team-name))
    (error "You need to get OAuth Access Token by your browser"))))


;;; Stores:

;; POST /v1/teams/%s/posts
;;;###autoload
(defun esa-region (begin end &optional wip name)
  "Post the current region as a new paste at yourteam.esa.io
Copies the URL into the kill ring.
.
With a prefix argument, makes a wip paste."
  (interactive "r\nP")
  (let* ((description (read-from-minibuffer "Description: "))
         ;; cause of privacy reason,
         ;; set filename as empty if call from esa-*-region function.
         ;; I think that highly expected upload just the region,
         ;; not a filename.
         (filename (or name (esa-anonymous-file-name))))
    (esa-request
     "POST"
     (format"https://api.esa.io/v1/teams/%s/posts" esa-team-name)
     'esa-created-callback
     `(("description" . ,description)
       ("wip" . ,(if wip 't :json-false))
       ("files" .
        ((,filename .
                    (("content" . ,(buffer-substring begin end))))))))))
(defun esa-single-file-name ()
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file)))
    name))
(defun esa-anonymous-file-name ()
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (ext (file-name-extension name)))
    (concat "anonymous-esa." ext)))
(defun esa-make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (let ((hexify
         (lambda (x)
           (url-hexify-string
            (with-output-to-string (princ x))))))
    (mapconcat
     (lambda (param)
       (concat (funcall hexify (car param))
               "="
               (funcall hexify (cdr param))))
     params "&")))
(defun esa-command-to-string (&rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (= (apply 'call-process "git" nil t nil args) 0)
        (error "git command fails %s" (buffer-string))))))
;;;###autoload
(defun esa-region-wip (begin end)
  "Post the current region as a new wip paste at yourteam.esa.io
Copies the URL into the kill ring."
  (interactive "r")
  (esa-region begin end t))
;;;###autoload
(defun esa-buffer (&optional wip)
  "Post the current buffer as a new paste at yourteam.esa.io.
Copies the URL into the kill ring.
.
With a prefix argument, makes a wip paste."
  (interactive "P")
  (esa-region (point-min) (point-max)
                 wip (esa-single-file-name)))
;;;###autoload
(defun esa-buffer-wip ()
  "Post the current buffer as a new wip paste at yourteam.esa.io.
Copies the URL into the kill ring."
  (interactive)
  (esa-region (point-min) (point-max)
                 t (esa-single-file-name)))
;;;###autoload
(defun esa-region-or-buffer (&optional wip)
  "Post either the current region, or if mark is not set, the
current buffer as a new paste at yourteam.esa.io Copies the URL
into the kill ring.
.
With a prefix argument, makes a wip paste."
  (interactive "P")
  (if (esa-region-active-p)
      (esa-region (region-beginning) (region-end) wip)
    (esa-buffer wip)))
;;;###autoload
(defun esa-region-or-buffer-wip ()
  "Post either the current region, or if mark is not set, the
current buffer as a new wip paste at yourteam.esa.io Copies
the URL into the kill ring."
  (interactive)
  (if (esa-region-active-p)
      (esa-region (region-beginning) (region-end) t)
    (esa-buffer t)))
(defun esa-created-callback (status url json)
  (let ((location (save-excursion
                    (goto-char (point-min))
                    (and (re-search-forward "^Location: \\(.*\\)" nil t)
                         (match-string 1))))
        (http-url))
    (cond
     ;; check redirected location indicate public/private esa url
     ((and (stringp location)
           (string-match "\\([0-9]+\\|[0-9a-zA-Z]\\{20\\}\\)$" location))
      (let ((id (match-string 1 location)))
        (setq http-url (format "https://%s.esa.io/posts/%s" esa-team-name id))
        (message "Paste created: %s" http-url)
        (when esa-view-esa
          (browse-url http-url))))
     (t
      (message "Paste is %s"
               (esa--err-propertize "failed"))))
    (when http-url
      (kill-new http-url))
    (url-mark-buffer-as-dead (current-buffer))))

;; GET /v1/teams/%s/posts
(defvar esa-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'revert-buffer)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'forward-line)
    (define-key map "q" 'esa-quit-window)
    map))
(defvar esa-list--paging-info nil)
(make-variable-buffer-local 'esa-list--paging-info)
(define-derived-mode esa-list-mode fundamental-mode "Esa"
  "Show your esa list"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'revert-buffer-function)
       'esa-list-revert-buffer)
  (add-hook 'post-command-hook 'esa-list--paging-retrieve nil t)
  (use-local-map esa-list-mode-map))
;;;###autoload
(defun esa-list ()
  "Displays a list of all of the current user's esas in a new buffer."
  (interactive)
  (message "Retrieving list of your esas...")
  (esa-list-draw-esas 1))
(defun esa-quit-window (&optional kill-buffer)
  "Bury the *esas* buffer and delete its window.
With a prefix argument, kill the buffer instead."
  (interactive "P")
  (quit-window kill-buffer))
(defun esa-list--paging-retrieve ()
  (cond
   ((null esa-list--paging-info))
   ((eq esa-list--paging-info t))
   (t
    (cl-destructuring-bind (page . max) esa-list--paging-info
      (cond
       ((or (not (numberp page))
            (not (numberp max))))       ; Now retrieving
       ((not (eobp)))
       ((= page max)
        (message "No more next page"))
       (t
        (esa-list-draw-esas (1+ page))))))))
(defun esa-list-draw-esas (page)
  (with-current-buffer (get-buffer-create "*esas*")
    (when (= page 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (esa-list-mode)
        (esa-insert-list-header)))
    ;; suppress multiple retrieving
    (setq esa-list--paging-info t))
  (esa-request
   "GET"
   (format "https://api.esa.io/v1/teams/%s/posts" esa-team-name)
   'esa-lists-retrieved-callback
   `(,@(and esa-list-items-per-page
            `(("per_page" . ,esa-list-items-per-page)))
     ("page" . ,page))))
(defun esa-list-revert-buffer (&rest ignore)
  ;; redraw esa list
  (esa-list))
(defun esa-region-active-p ()
  (if (functionp 'region-active-p)
      ;; trick for suppressing elint warning
      (funcall 'region-active-p)
    (and transient-mark-mode mark-active)))
(defun esa-lists-retrieved-callback (status url params)
  "Called when the list of esas has been retrieved. Parses the result
and displays the list."
  (goto-char (point-min))
  (let ((max-page
         ;; search http headaer
         (and (re-search-forward "<\\([^>]+\\)>; *rel=\"last\"" nil t)
              (let ((url (match-string 1)))
                (and (string-match "\\?\\(.*\\)" url)
                     (let* ((query (match-string 1 url))
                            (params (url-parse-query-string query))
                            (max-page (cadr (assoc "page" params))))
                       (when (string-match "\\`[0-9]+\\'" max-page)
                         (string-to-number max-page))))))))
    (when (re-search-forward "^\r?$" nil t)
      (let* ((json (esa--read-json (point) (point-max)))
             (page (cdr (assoc "page" params))))
        (with-current-buffer (get-buffer-create "*esas*")
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (mapc 'esa-insert-esa-link json)))
          ;; no max-page means last-page
          (setq esa-list--paging-info
                (cons page (or max-page page)))
          ;; skip header
          (forward-line)
          (set-window-buffer nil (current-buffer)))))
    (url-mark-buffer-as-dead (current-buffer))))

;; DELETE /v1/teams/%s/posts/%s
(defun esa-delete (id)
  (esa-request
   "DELETE"
   (format "https://api.esa.io/v1/%s/posts/%s" esa-team-name id)
   (esa-simple-receiver "Delete")))

;; PATCH /v1/teams/%s/posts/%s
(defun esa-update (id description)
  (esa-request
   "PATCH"
   (format "https://api.esa.io/v1/teams/%s/posts/%s" esa-team-name id)
   (esa-simple-receiver "Update")
   `(,@(and description
            `(("description" . ,description))))))


;;; Components:

(defun esa-insert-list-header ()
  "Creates the header line in the esa list buffer."
  (save-excursion
    (insert "  ID           Updated                "
            "  Visibility  Description             "
            (esa-fill-string "" (frame-width))
            "\n"))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'header-line))
  (forward-line))
(defun esa-insert-esa-link (esa)
  "Inserts a button that will open the given esa when pressed."
  (let* ((data (esa-parse-esa esa))
         (repo (car data)))
    (dolist (x (cdr data))
      (insert (format "  %s   " x)))
    (make-text-button (line-beginning-position) (line-end-position)
                      'repo repo
                      'action 'esa-describe-button
                      'face 'default
                      'esa-json esa))
  (insert "\n"))
(defun esa-describe-button (button)
  (let ((json (button-get button 'esa-json)))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (esa-describe-esa-1 json)))))
(defun esa-describe-insert-button (text action json)
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box (:line-width 2 :color "dark grey")
                                :background "light grey"
                                :foreground "black")
                       'link))
        (id (cdr (assq 'id json))))
    (insert-text-button button-text
                        'face button-face
                        'follow-link t
                        'action action
                        'repo id
                        'esa-json json)
    (insert " ")))
(defun esa-describe-esa-1 (esa)
  (require 'lisp-mnt)
  (let ((id (cdr (assq 'id esa)))
        (description (cdr (assq 'description esa)))
        (url (cdr (assq 'html_url esa)))
        (updated (cdr (assq 'updated_at esa)))
        (publicp (eq (cdr (assq 'public esa)) t)))
    (insert
     (if publicp
    (propertize "Public Esa"
                     'font-lock-face `(bold underline ,font-lock-warning-face))
       (propertize "Private Esa"
                   'font-lock-face '(bold underline)))
     "\n")
    (insert "  " (propertize "Description: " 'font-lock-face 'bold)
            (or description "") "\n")
    (insert "          " (propertize "URL: " 'font-lock-face 'bold) url "\n")
    (insert "      " (propertize "Updated: " 'font-lock-face 'bold)
            (format-time-string
             esa-display-date-format
             (esa-parse-time-string updated)) "\n")
    (insert "\n\n")
    (esa-describe-insert-button
     "Fetch Repository" 'esa-fetch-button esa)
    (esa-describe-insert-button
     "Browse" 'esa-open-web-button esa)
    (insert "\n\n")
    (esa-describe-insert-button
     "Edit Description" 'esa-update-button esa)
    (esa-describe-insert-button
     "Delete Esa" 'esa-delete-button esa)))
(defun esa-fetch-button (button)
  "Called when a esa [Fetch] button has been pressed.
Fetche esa repository and open the directory.
.
See `esa-working-directory-alist' document to fetch repository
into the user selected directory."
  (esa-fetch (button-get button 'repo)))
(defun esa-delete-button (button)
  "Called when a esa [Delete] button has been pressed.
Confirm and delete the esa."
  (when (y-or-n-p "Really delete this esa? ")
    (esa-delete (button-get button 'repo))))
(defun esa-update-button (button)
  "Called when a esa [Edit] button has been pressed.
Edit the esa description."
  (let* ((json (button-get button 'esa-json))
         (desc (read-from-minibuffer
                "Description: "
                (cdr (assq 'description json)))))
    (esa-update (button-get button 'repo) desc)))
(defun esa-open-web-button (button)
  "Called when a esa [Browse] button has been pressed."
  (let* ((json (button-get button 'esa-json))
         (url (cdr (assq 'html_url json))))
    (browse-url url)))
(defun esa-parse-esa (esa)
  "Returns a list of the esa's attributes for display, given the xml list
for the esa."
  (let ((repo (cdr (assq 'id esa)))
        (updated-at (cdr (assq 'updated_at esa)))
        (description (cdr (assq 'description esa)))
        (visibility (if (eq (cdr (assq 'public esa)) 't)
                        "public"
                      "private")))
    (list repo
          (esa-fill-string repo 8)
          (esa-fill-string
           (format-time-string
            esa-display-date-format (esa-parse-time-string updated-at))
           20)
          (esa-fill-string visibility 7)
          (or description ""))))
(defun esa-parse-time-string (string)
  (let* ((times (split-string string "[-T:Z]" t))
         (getter (lambda (x) (string-to-number (nth x times))))
         (year (funcall getter 0))
         (month (funcall getter 1))
         (day (funcall getter 2))
         (hour (funcall getter 3))
         (min (funcall getter 4))
         (sec (funcall getter 5)))
    (encode-time sec min hour day month year 0)))
(defun esa-fill-string (string width)
  (truncate-string-to-width string width nil ?\s "..."))
(defconst esa-repository-url-format "https://%s.esa.io/posts/%s")
(defun esa-fetch (id)
  (let* ((url (format esa-repository-url-format esa-team-name id))
         (working-copy (esa-working-copy-directory id)))
    (cond
     ((not (file-directory-p (expand-file-name ".git" working-copy)))
      (message "Cloning %s into working copy..." url)
      (esa-start-git-for-local `("clone" ,url ".") working-copy))
     (t
      (message "Fetching %s into working copy... " url)
      (esa-start-git-for-local `("pull" ,url) working-copy)))
    (dired working-copy)))
(defun esa-start-git-for-local (args &optional directory)
  (let* ((buffer (generate-new-buffer " *gist git* "))
         (default-directory
           (or (and directory (file-name-as-directory directory))
               default-directory))
         (proc (apply 'start-process "Gist" buffer "git" args)))
    (set-process-sentinel
     proc `(lambda (p e)
             (when (memq (process-status p) '(exit signal))
               (let ((code (process-exit-status p)))
                 (cond
                  ((eq code 0)
                   (message "Done fetching gist repository."))
                  (t
                   (message "Gist git process finished with %d" code)))
                 (let ((buf (dired-find-buffer-nocreate ,default-directory)))
                   (when (and buf (buffer-live-p buf))
                     (with-current-buffer buf
                       (revert-buffer)))))
               (kill-buffer (process-buffer p)))))
    proc))
(defun esa-working-copy-directory (id)
  (let* ((pair (assoc id esa-working-directory-alist))
         (dir (cond
               (pair
                (cdr pair))
               (t
                (expand-file-name id esa-working-directory)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))


;;; Utilities:

(defun esa-simple-receiver (message)
  ;; Create a receiver of `esa-request-0'
  `(lambda (status url json-or-params)
     (goto-char (point-min))
     (when (re-search-forward "^Status: \\([0-9]+\\)" nil t)
       (let ((code (string-to-number (match-string 1))))
         (if (and (<= 200 code) (< code 300))
             (message "%s succeeded" ,message)
           (message "%s %s"
                    ,message
                    (esa--err-propertize "failed")))))
     (url-mark-buffer-as-dead (current-buffer))))
(defun esa--err-propertize (string)
  (propertize string 'face 'font-lock-warning-face))



(provide 'esa)
;;; esa.el ends here
