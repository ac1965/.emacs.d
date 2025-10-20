# Makefile for Emacs config build
# - One-shot `make all`: tangle -> byte-compile (sequential)
# - Resolves EMACSD / STRAIGHT_BASE_DIR from early-init.el when available
# - Does not require a build/ directory

SHELL := /bin/sh

EMACS  ?= emacs
ORG    ?= README.org
EARLY  ?= early-init.el
INIT   ?= init.el

# Project directories (relative to repo root)
LISPDIR     ?= lisp
PERSONALDIR ?= personal

# Marker file indicating tangle completion
TANGLE_STAMP := .tangle-stamp

# ---------------------------------------------------------------------
# Resolve EMACSD by loading EARLY; fallback to user-emacs-directory
EMACSD := $(shell \
  if [ -f "$(EARLY)" ]; then \
    "$(EMACS)" --batch -Q -l "$(EARLY)" \
      --eval "(princ (expand-file-name (cond ((fboundp 'my:d) (my:d)) (t user-emacs-directory))))"; \
  else \
    "$(EMACS)" --batch -Q --eval "(princ (expand-file-name user-emacs-directory))"; \
  fi)

# Resolve STRAIGHT_BASE_DIR with several fallbacks
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

# (Reference only; not used as dependencies to avoid stale lists)
ELFILES  = $(shell find $(LISPDIR) $(PERSONALDIR) -type f -name '*.el' 2>/dev/null)
ELCFILES = $(patsubst %.el,%.elc,$(ELFILES))

# Minimal Org loading under -Q
EVAL_REQ_ORG := --eval "(require 'org)" \
                --eval "(require 'ob-core)" \
                --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))"

# Optional: inject leaf paths under -Q (used by `compile-q`)
EVAL_LEAF := --eval "(add-to-list 'load-path (expand-file-name \"$(LEAF_DIR)\"))" \
             --eval "(add-to-list 'load-path (expand-file-name \"$(LEAFKW_DIR)\"))" \
             --eval "(require 'leaf)" \
             --eval "(require 'leaf-keywords)" \
             --eval "(leaf-keywords-init)"

# ---------------------------------------------------------------------
.PHONY: all tangle compile compile-q compile-personal compile-lisp \
        clean distclean show-files echo-paths echo-myd echo-sbd check-init

# Top-level: always run tangle before compile
all: tangle compile

# 1) Tangle README.org -> *.el and drop a completion stamp
$(TANGLE_STAMP): $(ORG)
	@echo "[tangle] $(ORG)"
	@"$(EMACS)" --batch -Q \
	  $(EVAL_REQ_ORG) \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"
	@touch "$(TANGLE_STAMP)"

tangle: $(TANGLE_STAMP)

# 2) Compile using full init; enumerate files after tangle to avoid stale lists
compile: check-init $(TANGLE_STAMP)
	@echo "Byte-compiling .el under '$(LISPDIR)' and '$(PERSONALDIR)'..."
	@FILES=$$( { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' | sort; } 2>/dev/null; \
	            { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' | sort; } 2>/dev/null ); \
	if [ -z "$$FILES" ]; then \
	  echo "[compile] no .el files found; did tangle write to '$(LISPDIR)' or '$(PERSONALDIR)'?"; \
	  exit 2; \
	else \
	  for f in $$FILES; do \
	    echo "[byte-compile:init] $$f"; \
	    "$(EMACS)" --batch -l "$(EARLY)" -l "$(INIT)" \
	      --eval "(setq byte-compile-verbose t debug-on-error t)" \
	      --eval "(byte-compile-file \"$$f\")"; \
	  done; \
	  echo "[compile] done"; \
	fi

# Optional: compile under -Q by injecting leaf (does not load init.el)
compile-q: $(TANGLE_STAMP)
	@echo "[compile-q] -Q with STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"
	@FILES=$$( { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' | sort; } 2>/dev/null; \
	            { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' | sort; } 2>/dev/null ); \
	if [ -z "$$FILES" ]; then \
	  echo "[compile-q] no .el files found; did tangle write to '$(LISPDIR)' or '$(PERSONALDIR)'?"; \
	  exit 2; \
	else \
	  for f in $$FILES; do \
	    echo "  [byte-compile:-Q] $$f"; \
	    "$(EMACS)" --batch -Q \
	      $(EVAL_LEAF) \
	      --eval "(setq byte-compile-verbose t debug-on-error t)" \
	      --eval "(byte-compile-file \"$$f\")"; \
	  done; \
	  echo "[compile-q] done"; \
	fi

# Optional: directory-scoped compile with init.el
compile-personal: check-init $(TANGLE_STAMP)
	@echo "[compile:init] personal/"
	@find "$(PERSONALDIR)" -type f -name '*.el' -print0 2>/dev/null | xargs -0 -r -I{} \
	  "$(EMACS)" --batch -l "$(EARLY)" -l "$(INIT)" \
	    --eval "(setq byte-compile-verbose t debug-on-error t)" \
	    --eval "(byte-compile-file \"{}\")"

compile-lisp: check-init $(TANGLE_STAMP)
	@echo "[compile:init] lisp/"
	@find "$(LISPDIR)" -type f -name '*.el' -print0 2>/dev/null | xargs -0 -r -I{} \
	  "$(EMACS)" --batch -l "$(EARLY)" -l "$(INIT)" \
	    --eval "(setq byte-compile-verbose t debug-on-error t)" \
	    --eval "(byte-compile-file \"{}\")"

# Sanity check before init-based compile
check-init:
	@test -f "$(INIT)" || (echo "[error] init.el not found at: $(INIT)"; echo "        -> set INIT=/path/to/init.el or use 'make compile-q'"; exit 1)

# Helpers
show-files:
	@echo "[list] $(LISPDIR)";    { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' | sort; } || true
	@echo "[list] $(PERSONALDIR)"; { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' | sort; } || true

echo-paths:
	@echo "EMACSD=$(EMACSD)"; \
	echo "STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"; \
	echo "LEAF_DIR=$(LEAF_DIR)"; \
	echo "LEAFKW_DIR=$(LEAFKW_DIR)"; \
	echo "EARLY=$(EARLY)"; \
	echo "INIT=$(INIT)"

echo-myd:
	@echo $(EMACSD)

echo-sbd:
	@echo $(STRAIGHT_BASE_DIR)

# Cleanup
clean:
	@echo "[clean] remove *.elc under $(LISPDIR) and $(PERSONALDIR)"
	@{ [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.elc' -delete; } 2>/dev/null || true
	@{ [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.elc' -delete; } 2>/dev/null || true

distclean: clean
	@echo "[distclean] remove $(TANGLE_STAMP) and stray *.eln"
	@rm -f "$(TANGLE_STAMP)"
	@find . -type f -name '*.eln' -delete
