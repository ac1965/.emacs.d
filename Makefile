# Makefile for Emacs config build (safe clean)

EMACS       ?= emacs
ORG         ?= README.org
LISPDIR     ?= lisp
PERSONALDIR ?= personal
ELFILES     = $(wildcard $(LISPDIR)/*.el) $(wildcard $(PERSONALDIR)/*.el)
ELCFILES    = $(ELFILES:.el=.elc)

all: tangle

tangle: $(ORG)
	$(EMACS) --batch -Q \
		--eval "(require 'org)" \
		--eval "(org-babel-tangle-file \"$(ORG)\")"

compile: tangle $(ELCFILES)

$(LISPDIR)/%.elc: $(LISPDIR)/%.el
	$(EMACS) --batch -Q --eval "(byte-compile-file \"$<\")"

$(PERSONALDIR)/%.elc: $(PERSONALDIR)/%.el
	$(EMACS) --batch -Q --eval "(byte-compile-file \"$<\")"

clean:
	find $(LISPDIR) $(PERSONALDIR) -name "*.elc" -delete

.PHONY: all tangle compile clean
