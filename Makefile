EMACS = emacs

check: compile

compile:
	$(EMACS) -q -batch -f batch-byte-compile esa.el
