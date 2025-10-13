# Makefile for Emacs config build
# - Resolves EMACSD and STRAIGHT_BASE_DIR from early-init.el
# - Honors project-local definitions/ helpers provided by early-init
# ------------------------------------------------------------------------------
# Override via: make EMACS=/path/to/emacs EARLY=/path/to/early-init.el INIT=/path/to/init.el ORG=README.org
EMACS  ?= emacs
ORG    ?= README.org
EARLY  ?= early-init.el
INIT   ?= init.el

# Project dirs (relative to repo)
LISPDIR      ?= lisp
PERSONALDIR  ?= personal

# ------------------------------------------------------------------------------
# Resolve EMACSD by loading EARLY and consulting helpers. Fallback to user-emacs-directory.
EMACSD := $(shell \
  if [ -f "$(EARLY)" ]; then \
    "$(EMACS)" --batch -Q -l "$(EARLY)" \
      --eval "(princ (expand-file-name (cond ((fboundp 'my:d) (my:d)) (t user-emacs-directory))))"; \
  else \
    "$(EMACS)" --batch -Q --eval "(princ (expand-file-name user-emacs-directory))"; \
  fi)

# Resolve STRAIGHT_BASE_DIR:
# 1) If EARLY defines the variable STRAIGHT_BASE_DIR, use it.
# 2) Else, if helper (my:straight-base-dir) exists, call it.
# 3) Else, if straight-base-dir is bound, use it.
# 4) Else, fallback to EMACSD/straight.
STRAIGHT_BASE_DIR := $(shell \
  if [ -f "$(EARLY)" ]; then \
    "$(EMACS)" --batch -Q -l "$(EARLY)" \
      --eval "(princ (expand-file-name \"straight\" (cond ((boundp 'STRAIGHT_BASE_DIR) STRAIGHT_BASE_DIR) \
                                             ((fboundp 'my:straight-base-dir) (my:straight-base-dir)) \
                                             ((boundp 'straight-base-dir) straight-base-dir) \
                                             (t user-emacs-directory))))"; \
  else \
    printf "%s" "$(EMACSD)/straight"; \
  fi)

LEAF_DIR   := $(STRAIGHT_BASE_DIR)/repos/leaf
LEAFKW_DIR := $(STRAIGHT_BASE_DIR)/repos/leaf-keywords

# Discover files recursively
ELFILES  := $(shell find $(LISPDIR) $(PERSONALDIR) -type f -name '*.el' 2>/dev/null)
ELCFILES := $(patsubst %.el,%.elc,$(ELFILES))

# Common eval snippets
EVAL_REQ_ORG := --eval "(require 'org)" \
                --eval "(require 'ob-core)" \
                --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))"
EVAL_LEAF    := --eval "(add-to-list 'load-path (expand-file-name \"$(LEAF_DIR)\"))" \
                --eval "(add-to-list 'load-path (expand-file-name \"$(LEAFKW_DIR)\"))" \
                --eval "(require 'leaf)" \
                --eval "(require 'leaf-keywords)" \
                --eval "(leaf-keywords-init)"

# ------------------------------------------------------------------------------
.PHONY: all tangle compile compile-q compile-personal compile-lisp clean distclean \
        check-init echo-paths echo-myd echo-sbd

all: tangle compile

# 1) Tangle README.org -> *.el (works with -Q; org only)
tangle: $(ORG)
	@echo "[tangle] $(ORG)"
	$(EMACS) --batch -Q \
	  $(EVAL_REQ_ORG) \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"

# 2) Compile using full init (safest: leaf & straight initialized)
compile: check-init $(ELCFILES)
	@echo "[compile] done"

# Pattern: .el -> .elc via init.el
%.elc: %.el
	@echo "[byte-compile:init] $<"
	$(EMACS) --batch -l "$(EARLY)" -l "$(INIT)" --eval "(byte-compile-file \"$<\")"

# 3) Compile under -Q by injecting leaf paths derived from EARLY's STRAIGHT_BASE_DIR
compile-q:
	@echo "[compile-q] -Q with EMACSD=$(EMACSD)"
	@echo "[compile-q] -Q with STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"
	@for f in $(ELFILES); do \
	  echo "  [byte-compile:-Q] $$f"; \
	  $(EMACS) --batch -Q \
	    $(EVAL_LEAF) \
	    --eval "(byte-compile-file \"$$f\")"; \
	done
	@echo "[compile-q] done"

# Optional split targets (init-based)
compile-personal: check-init
	@echo "[compile:init] personal/"
	@find $(PERSONALDIR) -type f -name '*.el' -print0 | xargs -0 -r -I{} \
	  $(EMACS) --batch -l "$(EARLY)" -l "$(INIT)" --eval "(byte-compile-file \"{}\")"

compile-lisp: check-init
	@echo "[compile:init] lisp/"
	@find $(LISPDIR) -type f -name '*.el' -print0 | xargs -0 -r -I{} \
	  $(EMACS) --batch -l "$(EARLY)" -l "$(INIT)" --eval "(byte-compile-file \"{}\")"

# Sanity check before init-based compile
check-init:
	@test -f "$(INIT)" || (echo "[error] init.el not found at: $(INIT)"; echo "         -> set INIT=/path/to/init.el or use 'make compile-q'"; exit 1)

# Debug helpers
echo-paths:
	@echo "EMACSD=$(EMACSD)"; \
	echo "STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"; \
	echo "LEAF_DIR=$(LEAF_DIR)"; \
	echo "LEAFKW_DIR=$(LEAFKW_DIR)"; \
	echo "EARLY=$(EARLY)"; \
	echo "INIT=$(INIT)"

echo-myd:  ## print resolved my:d (EMACSD)
	@echo $(EMACSD)

echo-sbd:  ## print resolved STRAIGHT_BASE_DIR
	@echo $(STRAIGHT_BASE_DIR)

# Cleanup
clean:
	@echo "[clean] remove *.elc under $(LISPDIR) and $(PERSONALDIR)"
	@test -n "$(LISPDIR)" && find $(LISPDIR) -type f -name '*.elc' -delete || true
	@test -n "$(PERSONALDIR)" && find $(PERSONALDIR) -type f -name '*.elc' -delete || true

distclean: clean
	@echo "[distclean] remove stray *.eln under project tree (if any)"
	@find . -type f -name '*.eln' -delete
