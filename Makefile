# Makefile for Emacs config build
# Emacs 30+, Org Babel (tangle), byte-compilation
# Safe, minimal, maintainable

# Variables
EMACS       ?= emacs
ORG         ?= README.org
LISPDIR     ?= lisp
PERSONALDIR ?= personal
ELFILES     = $(wildcard $(LISPDIR)/*.el) $(wildcard $(PERSONALDIR)/*.el)
ELCFILES    = $(ELFILES:.el=.elc)

# Default target
all: tangle

# Tangle all .el files from README.org
tangle: $(ORG)
	$(EMACS) --batch -Q \
		--eval "(require 'org)" \
		--eval "(org-babel-tangle-file \"$(ORG)\")"

# Byte-compile all tangled .el files
compile: tangle $(ELCFILES)

$(LISPDIR)/%.elc: $(LISPDIR)/%.el
	$(EMACS) --batch -Q \
		--eval "(byte-compile-file \"$<\")"

$(PERSONALDIR)/%.elc: $(PERSONALDIR)/%.el
	$(EMACS) --batch -Q \
		--eval "(byte-compile-file \"$<\")"

# Remove compilation artifacts
clean:
	rm -f $(LISPDIR)/*.elc $(PERSONALDIR)/*.elc

.PHONY: all tangle compile clean
