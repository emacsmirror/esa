yagist.el -- Yet Another Emacs integration for gist.github.com
================================================

Uses your local GitHub config if it can find it.
See <http://github.com/blog/180-local-github-config>

Install
=======

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/mhayashi1120/yagist.el.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/yagist.el")
    (require 'yagist)

If you want to save encrypted token to ~/.gitconfig download following url.

    https://github.com/mhayashi1120/Emacs-cipher/raw/master/cipher/aes.el

    (setq yagist-encrypt-risky-config t)

Functions
=========

    M-x yagist-list - Lists your gists in a new buffer. Use arrow keys
    to browse, RET to open one in the other buffer.

    M-x yagist-region - Copies Gist URL into the kill ring.
    With a prefix argument, makes a private gist.

    M-x yagist-region-private - Explicitly create a private gist.

    M-x yagist-buffer - Copies Gist URL into the kill ring.
    With a prefix argument, makes a private gist.

    M-x yagist-buffer-private - Explicitly create a private gist.

    M-x yagist-region-or-buffer - Post either the current region, or if mark
    is not set, the current buffer as a new paste at gist.github.com .
    Copies the URL into the kill ring.
    With a prefix argument, makes a private paste.

    M-x yagist-region-or-buffer-private - Explicitly create a gist from the
    region or buffer.

    M-x yagist-minor-mode - Automated POST current buffer contents to gist 
	after saving.

    M-x yagist-global-minor-mode - Open the file that under gist repository
    automatically activate `yagist-minor-mode'.

Config
======

Set `yagist-view-gist` to non-nil if you want to view your Gist using
`browse-url` after it is created.

Set `yagist-github-user` to your GitHub basic authentication to avoid
checking `git-config`.

Set `yagist-github-token` to your GitHub credentials to avoid checking 
`git-config`.

Please try following step to get the Github OAuth token:

1. Register a oauth application
  https://github.com/settings/applications

2. Open url build by following code with web-browser, and replace URL with 
   registered callback url and client-id with CLIENT-ID
<pre>
   (concat
    "https://github.com/login/oauth/authorize?"
    (yagist-make-query-string
     '(("redirect_uri" . "**CALLBACK-URL**")
       ("client_id" . "**CLIENT-ID**")
       ("scope" . "gist"))))
</pre>

  NOTE: Scopes are defined here.
  http://developer.github.com/v3/oauth/#scopes

3. Copy the code in the redirected url in query string.
   e.g. http://www.example.com/?code=SOME-CODE

4. Open url build by follwing expression with web-browser.
<pre>
   (concat
    "https://github.com/login/oauth/access_token?"
    (yagist-make-query-string
     '(("redirect_uri" . "**CALLBACK-URL**")
       ("client_id" . "**CLIENT-ID**")
       ("client_secret" . "**CLIENT-SECRET**")
       ("code" . "**CODE**"))))
</pre>

Meta
====

* Code: `git clone git://github.com/mhayashi1120/yagist.el.git`
* Home: <http://github.com/mhayashi1120/yagist.el>
* Bugs: <http://github.com/mhayashi1120/yagist.el/issues>
