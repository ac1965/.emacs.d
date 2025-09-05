EMACS     ?= emacs
ORG       ?= README.org
LISP_DIR  ?= lisp
ELFILES   := early-init.el init.el $(LISP_DIR)/README.el

.PHONY: all tangle compile clean

all: tangle compile

tangle:
	@mkdir -p $(LISP_DIR)
	$(EMACS) --batch -Q \
	  --eval "(require 'org)" \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"

compile: $(ELFILES)
	$(EMACS) --batch -Q -L $(LISP_DIR) \
	  --eval "(require 'package)" \
	  --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\") \
	                                   (\"melpa\" . \"https://melpa.org/packages/\")) \
	                  package-vc-allow-clone t)" \
	  --eval "(unless (package-installed-p 'leaf) \
	            (package-refresh-contents) \
	            (package-install 'leaf))" \
	  --eval "(mapc #'byte-compile-file '($(patsubst %,\"%\",$(ELFILES))))"

clean:
	rm -f *.elc $(LISP_DIR)/*.elc
