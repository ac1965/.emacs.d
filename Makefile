EMACS ?= emacs
README_EL = README.el
README_ORG = README.org

.PHONY: all tangle clean

all: tangle

tangle: $(README_ORG)
	$(EMACS) --batch -Q \
	      --eval "(require 'org)" \
	      --eval "(org-babel-tangle-file \"$(README_ORG)\")"

clean:
	rm -f $(README_EL)
