# Makefile — One-pass builder for a modular Emacs config
# - Default / `make all` : onepass-init (tangle -> incremental byte-compile)
# - `make onepass-q`     : -Q (minimal env) tangle -> full byte-compile
# - Paths are absolutized from repo root to avoid "lisp/personal" confusion.

SHELL := /bin/sh

# ---- Repo-root & absolutized dirs --------------------------------------------
ROOT := $(CURDIR)

EMACS  ?= emacs
ORG    ?= README.org
EARLY  ?= early-init.el
INIT   ?= init.el

# Always treat these as top-level under repo root
LISPDIR_REL     ?= lisp
PERSONALDIR_REL ?= personal

LISPDIR     := $(abspath $(ROOT)/$(LISPDIR_REL))
PERSONALDIR := $(abspath $(ROOT)/$(PERSONALDIR_REL))
ORG := $(abspath $(ROOT)/$(ORG))
EARLY := $(abspath $(ROOT)/$(EARLY))
INIT  := $(abspath $(ROOT)/$(INIT))

STRICT_BYTE_WARN ?= 0   # Treat byte-compile warnings as errors
NATIVE_COMPILE   ?= 1   # Prefer native-compile if available

# ---- Emacs runners & common eval snippets ------------------------------------
EMACS_BATCH := "$(EMACS)" --batch
EMACS_Q     := $(EMACS_BATCH) -Q

EVAL_STRICT := $(if $(filter 1,$(STRICT_BYTE_WARN)),--eval "(setq byte-compile-error-on-warn t)",)
EVAL_NATIVE := $(if $(filter 1,$(NATIVE_COMPILE)),--eval "(setq comp-deferred-compilation t)",)

# Optional leaf injection for -Q
STRAIGHT_BASE_DIR ?= $(shell \
  if [ -f "$(EARLY)" ]; then \
    $(EMACS_Q) -l "$(EARLY)" \
      --eval "(princ (expand-file-name (or (ignore-errors STRAIGHT_BASE_DIR) \
                                           (ignore-errors (and (boundp 'straight-base-dir) straight-base-dir)) \
                                           (expand-file-name \"straight\" user-emacs-directory))))"; \
  else \
    printf "%s" "$$HOME/.emacs.d/straight"; \
  fi)
LEAF_DIR   := $(STRAIGHT_BASE_DIR)/repos/leaf
LEAFKW_DIR := $(STRAIGHT_BASE_DIR)/repos/leaf-keywords

EVAL_LEAF := \
  --eval "(let* ((ldir \"$(LEAF_DIR)\") (kwdir \"$(LEAFKW_DIR)\")) \
            (when (file-directory-p ldir)  (add-to-list 'load-path ldir)) \
            (when (file-directory-p kwdir) (add-to-list 'load-path kwdir)) \
            (ignore-errors (require 'leaf)) \
            (ignore-errors (require 'leaf-keywords)) \
            (when (featurep 'leaf-keywords) (leaf-keywords-init)))"

# ---- Default target (no args) ------------------------------------------------
.PHONY: all onepass-init onepass-q clean distclean show-files echo-paths tangle
all: onepass-init

# ---- One-pass (early+init env) : tangle -> incremental compile ---------------
onepass-init: $(ORG)
	@echo "[onepass-init] tangle -> incremental byte-compile (init loaded)"
	@$(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
	  $(EVAL_STRICT) $(EVAL_NATIVE) \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(require 'org)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
	  --eval "(let* ((dirs (delq nil (list (and (file-directory-p \"$(LISPDIR)\") \"$(LISPDIR)\") \
	                                        (and (file-directory-p \"$(PERSONALDIR)\") \"$(PERSONALDIR)\"))))) \
	            (dolist (d dirs) (byte-recompile-directory d 0)) \
	            (when (and (featurep 'comp) (bound-and-true-p comp-deferred-compilation)) \
	              (dolist (d dirs) (ignore-errors (native-compile-async d 'recursively)))))" \
	  --eval "(message \"[onepass-init] done\")"

# ---- One-pass (-Q minimal env) : tangle -> full compile ----------------------
onepass-q: $(ORG)
	@echo "[onepass-q] -Q tangle -> full byte-compile (init not loaded)"
	@$(EMACS_Q) \
	  $(EVAL_LEAF) $(EVAL_STRICT) $(EVAL_NATIVE) \
	  --eval "(setq org-confirm-babel-evaluate nil)" \
	  --eval "(require 'org)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")" \
	  --eval "(let* ((dirs (delq nil (list (and (file-directory-p \"$(LISPDIR)\") \"$(LISPDIR)\") \
	                                        (and (file-directory-p \"$(PERSONALDIR)\") \"$(PERSONALDIR)\"))))) \
	            (dolist (d dirs) (byte-recompile-directory d t)) \
	            (when (and (featurep 'comp) (bound-and-true-p comp-deferred-compilation)) \
	              (dolist (d dirs) (ignore-errors (native-compile-async d 'recursively)))))" \
	  --eval "(message \"[onepass-q] done\")"

# ---- Utilities ---------------------------------------------------------------
show-files:
	@echo "[list] $(LISPDIR)";    { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' | sort; } || true
	@echo "[list] $(PERSONALDIR)"; { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' | sort; } || true

echo-paths:
	@echo "ROOT=$(ROOT)"; \
	echo "EARLY=$(EARLY)"; \
	echo "INIT=$(INIT)"; \
	echo "LISPDIR=$(LISPDIR)"; \
	echo "PERSONALDIR=$(PERSONALDIR)"; \
	echo "STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"; \
	echo "LEAF_DIR=$(LEAF_DIR)"; \
	echo "LEAFKW_DIR=$(LEAFKW_DIR)"

clean:
	@echo "[clean] remove *.elc under $(LISPDIR) and $(PERSONALDIR)"
	@{ [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.elc' -delete; } 2>/dev/null || true
	@{ [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.elc' -delete; } 2>/dev/null || true

distclean: clean
	@echo "[distclean] remove stray *.eln"
	@find "$(ROOT)" -type f -name '*.eln' -delete

tangle:
	@echo "[tangle] $(ORG)"
	@$(EMACS_Q) \
	  --eval "(require 'org)" \
	  --eval "(require 'ob-core)" \
	  --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))" \
	  --eval "(setq org-confirm-babel-evaluate nil noninteractive t)" \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"
