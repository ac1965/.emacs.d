# Makefile --- Configuration build targets
# Copyright (c) 2021-2026, YAMASHITA, Takao
# License: GNU GPL v3 or later

EMACS ?= emacs
EMACS_FLAGS = --batch --no-init-file
ORG_FILE = README.org
TANGLE_CMD = (require 'ob-tangle) (org-babel-tangle-file "$(ORG_FILE)")

.PHONY: all tangle clean help

all: tangle

tangle: $(ORG_FILE)
	$(EMACS) $(EMACS_FLAGS) \
	  --eval "$(TANGLE_CMD)"
	@echo "Tangling complete."

clean:
	find . -name "*.elc" -delete
	@echo "Cleaned compiled files."

help:
	@echo "make tangle  — tangle all source blocks from README.org"
	@echo "make clean   — remove compiled .elc files"
