EMACS   ?= emacs
ORG     ?= README.org
ELFILES = early-init.el init.el README.el

.PHONY: all tangle compile clean

all: tangle compile

tangle:
	$(EMACS) --batch -Q \
	  --eval "(require 'org)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"

compile: $(ELFILES)
	$(EMACS) --batch -Q \
	  --eval "(progn \
	    (require 'package) \
	    (setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\") \
	                             (\"melpa\" . \"https://melpa.org/packages/\")) \
	          package-vc-allow-clone t) \
	    (package-initialize) \
	    (unless (package-installed-p 'leaf) \
	      (package-refresh-contents) \
	      (package-install 'leaf)))" \
	  --eval "(mapc #'byte-compile-file '($(patsubst %,\"%\",$(ELFILES))))"

clean:
	rm -f *.elc
