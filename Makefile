EMACS     ?= emacs
ORG       ?= README.org
LISP_DIR  ?= lisp

.PHONY: all tangle clean

all: tangle

tangle:
	@mkdir -p $(LISP_DIR)
	$(EMACS) --batch -Q \
	  --eval "(require 'org)" \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"

clean:
	rm -f *.el $(LISP_DIR)/*.el
