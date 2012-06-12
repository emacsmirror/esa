gist.el -- Emacs integration for gist.github.com
================================================

Uses your local GitHub config if it can find it.
See <http://github.com/blog/180-local-github-config>

Install
=======

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/mhayashi1120/gist.el.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/gist.el")
    (require 'gist)

If you want to save encrypted token to ~/.gitconfig download following url.

    https://github.com/mhayashi1120/Emacs-cipher/raw/master/cipher/aes.el

    (setq gist-encrypt-risky-config t)

Functions
=========

    M-x gist-list - Lists your gists in a new buffer. Use arrow keys
    to browse, RET to open one in the other buffer.

    M-x gist-region - Copies Gist URL into the kill ring.
    With a prefix argument, makes a private gist.

    M-x gist-region-private - Explicitly create a private gist.

    M-x gist-buffer - Copies Gist URL into the kill ring.
    With a prefix argument, makes a private gist.

    M-x gist-buffer-private - Explicitly create a private gist.

    M-x gist-region-or-buffer - Post either the current region, or if mark
    is not set, the current buffer as a new paste at gist.github.com .
    Copies the URL into the kill ring.
    With a prefix argument, makes a private paste.

    M-x gist-region-or-buffer-private - Explicitly create a gist from the
    region or buffer.

Config
======

Set `gist-view-gist` to non-nil if you want to view your Gist using
`browse-url` after it is created.

Set `github-user` and `github-token` to your GitHub credentials to
avoid checking `git-config`.

See <http://github.com/blog/180-local-github-config>

Meta
====

* Code: `git clone git://github.com/mhayashi1120/gist.el.git`
* Home: <http://github.com/mhayashi1120/gist.el>
* Bugs: <http://github.com/mhayashi1120/gist.el/issues>
