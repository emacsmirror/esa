# esa.el --- Emacs integration for esa.io
## Installation

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/nabinno/esa.el.git

In your emacs config:

```lisp
(add-to-list 'load-path "~/.emacs.d/vendor/esa.el")
(require 'esa)
```

## Functions

- `M-x esa-list` - Lists your gists in a new buffer. Use arrow keys to
  browse, RET to open one in the other buffer.
- `M-x esa-region` - Copies Gist URL into the kill ring.  With a prefix
  argument, makes a private gist.
- `M-x esa-region-private` - Explicitly create a private gist.
- `M-x esa-buffer` - Copies Gist URL into the kill ring.  With a
  prefix argument, makes a private gist.
- `M-x esa-buffer-private` - Explicitly create a private gist.
- `M-x esa-region-or-buffer` - Post either the current region, or if
  mark is not set, the current buffer as a new paste at
  yourteam.esa.io.  Copies the URL into the kill ring.  With a prefix
  argument, makes a private paste.
- `M-x esa-region-or-buffer-private` - Explicitly create a gist from the
  region or buffer.
- `M-x esa-minor-mode` - Automated POST current buffer contents to
  gist after saving.
- `M-x esa-global-minor-mode` - Open the file that under gist
  repository automatically activate `esa-minor-mode'.

## Configuration

Set `esa-view-gist` to non-nil if you want to view your Gist using
`browse-url` after it is created.

### OAuth authentication

Get the `Personal API Access Token` from:

https://yourteam.esa.io/user/token

Put following to your .emacs:

```lisp
(setq esa-token "******************************")
(setq esa-team-name "foomanchu")
```

### Other customizations

     M-x customize-group (esa)

---

## Meta

* Code: `git clone git://github.com/nabinno/esa.el.git`
* Home: <https://github.com/nabinno/esa.el>
* Bugs: <https://github.com/nabinno/esa.el/issues>
* Hosting: melpa
## EPILOGUE
>     A whale!
>     Down it goes, and more, and more
>     Up goes its tail!
>
>     -Buson Yosa
