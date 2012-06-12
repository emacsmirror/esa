;;; gist.el --- Emacs integration for gist.github.com

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Contributors:
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Dan McKinley
;; Version: 0.7.0
;; Created: 21 Jul 2008
;; Keywords: gist git github paste pastie pastebin
;; Package-Requires: ((json "1.2.0"))

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

;; Uses your local GitHub config if it can find it.
;; See http://github.com/blog/180-local-github-config

;; If you are using Emacs 22 or earlier, download the json.el from following url
;;
;; http://bzr.savannah.gnu.org/lh/emacs/emacs-23/annotate/head:/lisp/json.el

;; If you want to save encrypted token to ~/.gitconfig download following url.
;;
;; https://github.com/mhayashi1120/Emacs-cipher/raw/master/cipher/aes.el
;; 
;; (setq gist-encrypt-risky-config t)

;;; TODO;

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

(defgroup gist nil
  "Simple gist application."
  :group 'applications)

(defvar github-user nil
  "If non-nil, will be used as your GitHub username without checking
git-config(1).")
(defvar github-token nil
  "If non-nil, will be used as your GitHub token without checking
git-config(1).")

(defcustom gist-user-password nil
  "If non-nil, will be used as your GitHub password without reading."
  :type 'string
  :group 'gist)

(defcustom gist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after they're
posted."
  :type 'boolean
  :group 'gist)

(defcustom gist-display-date-format "%Y-%m-%d %H:%M"
  "Date format displaying in `gist-list' buffer."
  :type 'string
  :group 'gist)

(defcustom gist-authenticate-function 'gist-basic-authentication
  "Authentication function symbol."
  :type 'function
  :group 'gist)

(defvar gist-list-items-per-page 20
  "Number of gist to retrieve a page.")

(defcustom gist-working-directory "~/.gist"
  "*Working directory where to go gist repository is."
  :type 'directory
  :group 'gist)

(defcustom gist-working-directory-alist nil
  "*Alist of gist id as key, value is directory path.

Example:
\(setq gist-working-directory-alist
      `((\"1080701\" . \"~/mygist/Emacs-nativechecker\")))
"
  :type '(alist :key-type string
                :value-type directory)
  :group 'gist)

(defvar gist-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'revert-buffer)
    (define-key map "p" 'gist-list-prev-gist)
    (define-key map "n" 'gist-list-next-gist)
    map))

(defvar gist-list--paging-info nil)
(make-variable-buffer-local 'gist-list--paging-info)

(define-derived-mode gist-list-mode fundamental-mode "Gists"
  "Show your gist list"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq revert-buffer-function 'gist-list-revert-buffer)
  (use-local-map gist-list-mode-map))

;; TODO http://developer.github.com/v3/oauth/
;; * "Desktop Application Flow" says that using the basic authentication...
;; * `gist-region' not works
(defun gist-basic-authentication ()
  (destructuring-bind (user . pass) (github-auth-info-basic)
    (format "Basic %s"
            (base64-encode-string (format "%s:%s" user pass)))))

(defun gist-oauth2-authentication ()
  (let ((token (github-auth-info-oauth2)))
    (format "Bearer %s" token)))

(defun gist-request (method url callback &optional json-or-params)
  (let* ((json (and (member method '("POST" "PATCH")) json-or-params))
         (params (and (member method '("GET" "DELETE")) json-or-params))
         (url-request-data (and json (concat (json-encode json) "\n")))
         (url-request-extra-headers
          `(("Authorization" . ,(funcall gist-authenticate-function))))
         (url-request-method method)
         (url-max-redirection -1)
         (url (if params (concat url "?" (gist-make-query-string params)) url)))
    (url-retrieve url callback (list url json-or-params))))

;;;###autoload
(defun gist-region (begin end &optional private name)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (let* ((description (read-from-minibuffer "Description: "))
         ;; cause of privacy reason,
         ;; set filename as empty if call from gist-*-region function.
         ;; I think that highly expected upload just the region,
         ;; not a filename.
         (filename (or name "")))
    (gist-request
     "POST"
     "https://api.github.com/gists"
     'gist-created-callback
     `(("description" . ,description)
       ("public" . ,(if private :json-false 't))
       ("files" .
        ((,filename .
                    (("content" . ,(buffer-substring begin end))))))))))

(defun gist-single-file-name ()
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file)))
    name))

(defun gist-make-query-string (params)
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

(defun gist-command-to-string (&rest args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process "git" nil t nil args))))

(defcustom gist-encrypt-risky-config nil
  "*Encrypt your token by using `cipher/aes' package."
  :type 'boolean
  :group 'gist)

(defvar github-risky-config-keys
  '("oauth-token"))

(defun gist-decrypt-string (key string)
  (let ((cipher/aes-decrypt-prompt
         (format "Password to decrypt %s: " key)))
    (cipher/aes-decrypt-string
     (base64-decode-string string))))

(defun gist-encrypt-string (key string)
  (let ((cipher/aes-encrypt-prompt
         (format "Password to encrypt %s: " key)))
    (base64-encode-string
     (cipher/aes-encrypt-string string) t)))

(defun github-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((raw-val (github-read-config key)))
    (cond
     ((and (require 'cipher/aes nil t)
           gist-encrypt-risky-config
           (member key github-risky-config-keys))
      (let* ((real-key (concat "encrypted." key))
             (enc-val (github-read-config real-key)))
        (when raw-val
          ;; destroy unencrypted value.
          (github-write-config key "")
          ;; translate raw value to encrypted value
          (github-set-config key raw-val))
        (let ((real-val (and enc-val
                             (gist-decrypt-string key enc-val))))
          (or real-val raw-val))))
     (t
      raw-val))))

(defun github-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (cond
   ((and (require 'cipher/aes nil t)
         gist-encrypt-risky-config
         (member key github-risky-config-keys))
    (let* ((raw-val (github-read-config key))
           (real-key (concat "encrypted." key))
           (enc-val (gist-encrypt-string key value)))
      (when raw-val
        ;; destroy unencrypted value.
        (github-write-config key ""))
      (github-write-config real-key enc-val)))
   (t
    (github-write-config key value))))

(defun github-write-config (key value)
  (gist-command-to-string
   "config" "--global" (format "github.%s" key) value))

(defun github-read-config (key)
  (let ((val (gist-command-to-string
              "config" "--global" (format "github.%s" key))))
    (cond
     ((string-match "\\`[\n]*\\'" val) nil)
     ((string-match "\n+\\'" val)
      (substring val 0 (match-beginning 0)))
     (t
      val))))

;; 1. Register a oauth application
;;   https://github.com/settings/applications
;; 2. Open url build by following code with web-browser, and replace URL with registered callback url
;;    and client-id with CLIENT-ID
;; (concat
;;  "https://github.com/login/oauth/authorize?"
;;  (gist-make-query-string
;;   '(("redirect_uri" . "**CALLBACK-URL**")
;;     ("client_id" . "**CLIENT-ID**"))))
;; 3. Copy the code in the redirected url in query string.
;;    ex: http://www.example.com/?code=SOME-CODE
;; 4. Open url like following by web-browser, and replace query-string like step 2.
;; (concat
;;  "https://github.com/login/oauth/access_token?"
;;  (gist-make-query-string
;;   '(("redirect_uri" . "**CALLBACK-URL**")
;;     ("client_id" . "**CLIENT-ID**")
;;     ("client_secret" . "**CLIENT-SECRET**")
;;     ("code" . "**CODE**"))))

(defun github-auth-info-oauth2 ()
  (let* ((token (or github-token (github-config "oauth-token"))))

    (when (not token)
      (setq token (read-string "GitHub OAuth token: "))
      (github-set-config "oauth-token" token))

    token))

(defun github-auth-info-basic ()
  (let* ((user (or github-user (github-config "user")))
         pass)

    (when (not user)
      (setq user (read-string "GitHub username: "))
      (github-set-config "user" user))

    (setq pass (gist-get-user-password))

    (cons user pass)))

(defun gist-get-user-password ()
  (or gist-user-password
      (read-passwd "Password: ")))

;;;###autoload
(defun gist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region begin end t))

;;;###autoload
(defun gist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (gist-region (point-min) (point-max)
               private (gist-single-file-name)))

;;;###autoload
(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region (point-min) (point-max)
               t (gist-single-file-name)))

;;;###autoload
(defun gist-region-or-buffer (&optional private)
  "Post either the current region, or if mark is not set, the current buffer as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (if (gist-region-active-p)
      (gist-region (region-beginning) (region-end) private)
    (gist-buffer private)))

;;;###autoload
(defun gist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the current buffer as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive)
  (if (gist-region-active-p)
      (gist-region (region-beginning) (region-end) t)
    (gist-buffer t)))

;;;###autoload
(defun gist-list ()
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (gist-list-draw-gists 1))

(defun gist-list-next-gist ()
  "Move to next line or to retrieve next page."
  (interactive)
  (forward-line 1)
  (destructuring-bind (page . max) gist-list--paging-info
    (cond
     ((not (eobp)))
     ((= page max)
      (message "No more next page"))
     (t
      (gist-list-draw-gists (1+ page))))))

(defun gist-list-prev-gist ()
  "Move to previous line."
  (interactive)
  (forward-line -1))

(defun gist-list-draw-gists (page)
  ;;TODO control multiple asyncronous retrieving
  (with-current-buffer (get-buffer-create "*gists*")
    (when (= page 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gist-list-mode)
        (gist-insert-list-header))))
  (gist-request
   "GET"
   "https://api.github.com/gists"
   'gist-lists-retrieved-callback
   `(("per_page" . ,gist-list-items-per-page)
     ("page" . ,page))))

(defun gist-list-revert-buffer (&rest ignore)
  ;; redraw gist list
  (gist-list))

(defun gist-region-active-p ()
  (if (functionp 'region-active-p)
      ;; trick for suppressing elint warning
      (funcall 'region-active-p)
    (and transient-mark-mode mark-active)))

(defun gist-insert-list-header ()
  "Creates the header line in the gist list buffer."
  (save-excursion
    (insert "  ID           Updated                "
            "  Visibility  Description             "
            (gist-fill-string "" (frame-width))
            "\n"))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'header-line))
  (forward-line))

(defun gist-insert-gist-link (gist)
  "Inserts a button that will open the given gist when pressed."
  (let* ((data (gist-parse-gist gist))
         (repo (car data)))
    (dolist (x (cdr data))
      (insert (format "  %s   " x)))
    (make-text-button (line-beginning-position) (line-end-position)
                      'repo repo
                      'action 'gist-describe-button
                      'face 'default
                      'gist-json gist))
  (insert "\n"))

(defun gist-describe-button (button)
  (let ((json (button-get button 'gist-json)))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (gist-describe-gist-1 json)))))

(defun gist-describe-insert-button (text action json)
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
                        'gist-json json)
    (insert " ")))

(defun gist-describe-gist-1 (gist)
  (require 'lisp-mnt)
  (let ((id (cdr (assq 'id gist)))
        (description (cdr (assq 'description gist)))
        (url (cdr (assq 'html_url gist)))
        (updated (cdr (assq 'updated_at gist)))
        (publicp (eq (cdr (assq 'public gist)) t)))

    (insert
     (if publicp
         (propertize "Public Gist"
                     'font-lock-face `(bold underline ,font-lock-warning-face))
       (propertize "Private Gist"
                   'font-lock-face '(bold underline)))
     "\n")
    (insert "  " (propertize "Description: " 'font-lock-face 'bold) (or description "") "\n")
    (insert "          " (propertize "URL: " 'font-lock-face 'bold) url "\n")
    (insert "      " (propertize "Updated: " 'font-lock-face 'bold)
            (format-time-string
             gist-display-date-format
             (gist-parse-time-string updated)) "\n")

    (insert "\n\n")

    (gist-describe-insert-button "Fetch Repository" 'gist-fetch-button gist)
    (gist-describe-insert-button "Browse" 'gist-open-web-button gist)

    (insert "\n\n")

    (gist-describe-insert-button "Edit Description" 'gist-update-button gist)
    (gist-describe-insert-button "Delete Gist" 'gist-delete-button gist)))

(defun gist-fetch-button (button)
  "Called when a gist [Fetch] button has been pressed.
Fetche gist repository and open the directory.

See `gist-working-directory-alist' document to fetch repository
into the user selected directory."
  (gist-fetch (button-get button 'repo)))

(defun gist-delete-button (button)
  "Called when a gist [Delete] button has been pressed.
Confirm and delete the gist."
  (when (y-or-n-p "Really delete this gist? ")
    (gist-delete (button-get button 'repo))))

(defun gist-update-button (button)
  "Called when a gist [Edit] button has been pressed.
Edit the gist description."
  (let* ((json (button-get button 'gist-json))
         (desc (read-from-minibuffer
                "Description: "
                (cdr (assq 'description json)))))
    (gist-update (button-get button 'repo) desc)))

(defun gist-open-web-button (button)
  "Called when a gist [Browse] button has been pressed."
  (let* ((json (button-get button 'gist-json))
         (url (cdr (assq 'html_url json))))
    (browse-url url)))

(defun gist-parse-gist (gist)
  "Returns a list of the gist's attributes for display, given the xml list
for the gist."
  (let ((repo (cdr (assq 'id gist)))
        (updated-at (cdr (assq 'updated_at gist)))
        (description (cdr (assq 'description gist)))
        (visibility (if (eq (cdr (assq 'public gist)) 't)
                        "public"
                      "private")))
    (list repo
          (gist-fill-string repo 8)
          (gist-fill-string
           (format-time-string
            gist-display-date-format (gist-parse-time-string updated-at))
           20)
          (gist-fill-string visibility 7)
          (or description ""))))

(defun gist-parse-time-string (string)
  (let* ((times (split-string string "[-T:Z]" t))
         (getter (lambda (x) (string-to-number (nth x times))))
         (year (funcall getter 0))
         (month (funcall getter 1))
         (day (funcall getter 2))
         (hour (funcall getter 3))
         (min (funcall getter 4))
         (sec (funcall getter 5)))
    (encode-time sec min hour day month year 0)))

(defun gist-fill-string (string width)
  (truncate-string-to-width string width nil ?\s "..."))

(defconst gist-repository-url-format "git@gist.github.com:%s.git")

(defun gist-fetch (id)
  (let* ((url (format gist-repository-url-format id))
         (working-copy (gist-working-copy-directory id)))
    (cond
     ((not (file-directory-p (expand-file-name ".git" working-copy)))
      (message "Cloning %s into working copy..." url)
      (gist-start-git `("clone" ,url ".") working-copy))
     (t
      (message "Fetching %s into working copy... " url)
      (gist-start-git `("pull" ,url) working-copy)))
    (dired working-copy)))

(defun gist-delete (id)
  (gist-request
   "DELETE"
   (format "https://api.github.com/gists/%s" id)
   (gist-simple-receiver "Delete")))

(defun gist-update (id description)
  (gist-request
   "PATCH"
   (format "https://api.github.com/gists/%s" id)
   (gist-simple-receiver "Update")
   `(,@(and description
            `(("description" . ,description))))))

(defun gist-working-copy-directory (id)
  (let* ((pair (assoc id gist-working-directory-alist))
         (dir (cond
               (pair
                (cdr pair))
               (t
                (expand-file-name id gist-working-directory)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun gist-start-git (args &optional directory)
  (let* ((buffer (generate-new-buffer " *gist git* "))
         (default-directory
           (or (and directory (file-name-as-directory directory))
               default-directory))
         (proc (apply 'start-process "Gist" buffer "git" args)))
    (set-process-sentinel
     proc (lambda (p e)
            (when (memq (process-status p) '(exit signal))
              (let ((code (process-exit-status p)))
                (cond
                 ((eq code 0)
                  (message "Done fetching gist repository."))
                 (t
                  (message "Gist git process finished with %d" code))))
              (kill-buffer (process-buffer p)))))
    proc))

(defun gist-simple-receiver (message)
  ;; Create a receiver of `gist-request'
  `(lambda (status url json-or-params)
     (goto-char (point-min))
     (when (re-search-forward "^Status: \\([0-9]+\\)" nil t)
       (let ((code (string-to-number (match-string 1))))
         (if (and (<= 200 code) (< code 300))
             (message "%s succeeded" ,message)
           (message "%s failed" ,message))))
     (url-mark-buffer-as-dead (current-buffer))))

(defun gist-created-callback (status url json)
  (let ((location (save-excursion
                    (goto-char (point-min))
                    (and (re-search-forward "^Location: \\(.*\\)" nil t)
                         (match-string 1))))
        (http-url))
    (cond
     ;; check redirected location indicate public/private gist url
     ((and (stringp location)
           (string-match "\\([0-9]+\\|[0-9a-zA-Z]\\{20\\}\\)$" location))
      (let ((id (match-string 1 location)))
        (setq http-url (format "https://gist.github.com/%s" id))
        (message "Paste created: %s" http-url)
        (when gist-view-gist
          (browse-url http-url))))
     (t
      (message "Paste is failed")))
    (when http-url
      (kill-new http-url))
    (url-mark-buffer-as-dead (current-buffer))))

(defun gist-lists-retrieved-callback (status url params)
  "Called when the list of gists has been retrieved. Parses the result
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
      (let* ((str (buffer-substring (point) (point-max)))
             (decoded (decode-coding-string str 'utf-8))
             (json (json-read-from-string decoded))
             (page (cdr (assoc "page" params))))
        (with-current-buffer (get-buffer-create "*gists*")
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (mapc 'gist-insert-gist-link json)))
          ;; no max-page means last-page
          (setq gist-list--paging-info
                (cons page (or max-page page)))

          ;; skip header
          (forward-line)
          (set-window-buffer nil (current-buffer)))))
    (url-mark-buffer-as-dead (current-buffer))))

(provide 'gist)

;;; gist.el ends here.
