# Makefile for Emacs config build (portable / robust)
# - One-shot `make all`: tangle -> byte-compile (sequential)
# - Resolves EMACSD / STRAIGHT_BASE_DIR from early-init.el when available
# - No build/ dir required
# - Space-safe, BSD/macOS/GNU friendly

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

# Strictness toggles (set to 1 to enable)
STRICT_BYTE_WARN ?= 0   # -> (setq byte-compile-error-on-warn t)
NATIVE_COMPILE   ?= 1   # -> use native-compile for .eln if available

# ---------------------------------------------------------------------
# Common Emacs command fragments
EMACS_BATCH := "$(EMACS)" --batch
EMACS_Q     := $(EMACS_BATCH) -Q

# Minimal Org loading under -Q
EVAL_REQ_ORG := \
  --eval "(require 'org)" \
  --eval "(require 'ob-core)" \
  --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)))" \
  --eval "(setq org-confirm-babel-evaluate nil noninteractive t)"

# Optional: inject leaf paths under -Q (portable: ignore-errors)
EVAL_LEAF := \
  --eval "(let* ((ldir (expand-file-name \"$(LEAF_DIR)\")) (kwdir (expand-file-name \"$(LEAFKW_DIR)\"))) \
           (when (file-directory-p ldir)  (add-to-list 'load-path ldir)) \
           (when (file-directory-p kwdir) (add-to-list 'load-path kwdir)) \
           (ignore-errors (require 'leaf)) \
           (ignore-errors (require 'leaf-keywords)) \
           (when (featurep 'leaf-keywords) (leaf-keywords-init)))"

# Optional: strict compile errors on warnings
EVAL_STRICT := $(if $(filter 1,$(STRICT_BYTE_WARN)),--eval "(setq byte-compile-error-on-warn t)",)

# Optional: prefer native-compile when available (no-op if not)
EVAL_NATIVE := $(if $(filter 1,$(NATIVE_COMPILE)),--eval "(setq comp-deferred-compilation t)",)

# ---------------------------------------------------------------------
# Resolve EMACSD by loading EARLY; fallback to user-emacs-directory
EMACSD := $(shell \
  if [ -f "$(EARLY)" ]; then \
    $(EMACS_Q) -l "$(EARLY)" \
      --eval "(princ (expand-file-name (cond ((fboundp 'my:d) (my:d)) (t user-emacs-directory))))"; \
  else \
    $(EMACS_Q) --eval "(princ (expand-file-name user-emacs-directory))"; \
  fi)

# Resolve STRAIGHT_BASE_DIR with several fallbacks
STRAIGHT_BASE_DIR := $(shell \
  if [ -f "$(EARLY)" ]; then \
    $(EMACS_Q) -l "$(EARLY)" \
      --eval "(princ (expand-file-name \"straight\" (cond ((boundp 'STRAIGHT_BASE_DIR) STRAIGHT_BASE_DIR) \
                                             ((fboundp 'my:straight-base-dir) (my:straight-base-dir)) \
                                             ((boundp 'straight-base-dir) straight-base-dir) \
                                             (t user-emacs-directory))))"; \
  else \
    printf "%s" "$(EMACSD)/straight"; \
  fi)

LEAF_DIR   := $(STRAIGHT_BASE_DIR)/repos/leaf
LEAFKW_DIR := $(STRAIGHT_BASE_DIR)/repos/leaf-keywords

# Reference only (not deps)
ELFILES  = $(shell { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' 2>/dev/null; } ; \
                    { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' 2>/dev/null; } )

# ---------------------------------------------------------------------
.PHONY: all help tangle compile compile-q compile-personal compile-lisp \
        compile-incremental clean distclean show-files echo-paths echo-myd echo-sbd check-init

# Default help
help:
	@printf '%s\n' \
'Targets:' \
'  all                 Tangle then byte-compile with init.el' \
'  tangle              org-babel-tangle-file $(ORG) -> *.el' \
'  compile             Byte-compile under init.el (always recompile)' \
'  compile-incremental Byte-compile only when .el is newer than .elc' \
'  compile-q           Byte-compile under -Q (inject leaf if available)' \
'  compile-personal    Byte-compile personal/*.el under init.el' \
'  compile-lisp        Byte-compile lisp/*.el under init.el' \
'  clean               Remove *.elc under lisp/ and personal/' \
'  distclean           clean + remove $(TANGLE_STAMP) + delete stray *.eln' \
'  show-files          List .el under lisp/ and personal/' \
'  echo-paths          Show resolved paths (EMACSD, STRAIGHT_BASE_DIR, etc.)' \
'' \
'Options (env):' \
'  EMACS=<cmd> ORG=<file> EARLY=<file> INIT=<file>' \
'  STRICT_BYTE_WARN=1     Treat byte-compile warnings as errors' \
'  NATIVE_COMPILE=1       Prefer native-compile (when available)'

# Top-level: always run tangle before compile
all: tangle compile

# 1) Tangle README.org -> *.el and drop a completion stamp
$(TANGLE_STAMP): $(ORG)
	@echo "[tangle] $(ORG)"
	@$(EMACS_Q) \
	  $(EVAL_REQ_ORG) \
	  --eval "(org-babel-tangle-file \"$(ORG)\")"
	@touch "$(TANGLE_STAMP)"

tangle: $(TANGLE_STAMP)

# Sanity check before init-based compile
check-init:
	@test -f "$(INIT)" || { \
	  echo "[error] init.el not found at: $(INIT)"; \
	  echo "        -> set INIT=/path/to/init.el or use 'make compile-q'"; \
	  exit 1; }

# Helper: count .el files safely (for friendly message)
define COUNT_EL
( { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' -print0; } 2>/dev/null; \
  { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' -print0; } 2>/dev/null ) \
| tr -cd '\000' | wc -c | awk '{print $$1}'
endef

# 2) Compile using full init (NUL-safe while loop; no xargs)
compile: check-init $(TANGLE_STAMP)
	@echo "Byte-compiling .el under '$(LISPDIR)' and '$(PERSONALDIR)'..."
	@CNT=`$(COUNT_EL)` ; \
	if [ $$CNT -eq 0 ]; then \
	  echo "[compile] no .el files found; did tangle write to '$(LISPDIR)' or '$(PERSONALDIR)'?"; \
	  exit 2; \
	fi ; \
	( \
	  { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' -print0; } 2>/dev/null; \
	  { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' -print0; } 2>/dev/null \
	) | while IFS= read -r -d '' f; do \
	    echo "[byte-compile:init] $$f"; \
	    $(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
	      $(EVAL_STRICT) $(EVAL_NATIVE) \
	      --eval "(setq byte-compile-verbose t debug-on-error t)" \
	      --eval "(byte-compile-file \"$$f\")"; \
	  done ; \
	echo "[compile] done"

# 2') Incremental compile (only when .el > .elc or .elc missing)
compile-incremental: check-init $(TANGLE_STAMP)
	@echo "[compile-incremental] recompile only when .el is newer than .elc"
	@$(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
	  $(EVAL_STRICT) $(EVAL_NATIVE) \
	  --eval "(setq byte-compile-verbose t debug-on-error t)" \
	  --eval "(dolist (d '(\"$(LISPDIR)\" \"$(PERSONALDIR)\")) \
	            (when (and (stringp d) (> (length d) 0) (file-directory-p d)) \
	              ;; FLAG=0 -> only when .el is newer or .elc missing
	              (byte-recompile-directory d 0)))"
	@echo "[compile-incremental] done"

# Optional: compile under -Q by injecting leaf (does not load init.el)
compile-q: $(TANGLE_STAMP)
	@echo "[compile-q] -Q with STRAIGHT_BASE_DIR=$(STRAIGHT_BASE_DIR)"
	@CNT=`$(COUNT_EL)` ; \
	if [ $$CNT -eq 0 ]; then \
	  echo "[compile-q] no .el files found; did tangle write to '$(LISPDIR)' or '$(PERSONALDIR)'?"; \
	  exit 2; \
	fi ; \
	( \
	  { [ -d "$(LISPDIR)" ] && find "$(LISPDIR)" -type f -name '*.el' -print0; } 2>/dev/null; \
	  { [ -d "$(PERSONALDIR)" ] && find "$(PERSONALDIR)" -type f -name '*.el' -print0; } 2>/dev/null \
	) | while IFS= read -r -d '' f; do \
	    echo "  [byte-compile:-Q] $$f"; \
	    $(EMACS_Q) \
	      $(EVAL_LEAF) $(EVAL_STRICT) $(EVAL_NATIVE) \
	      --eval "(setq byte-compile-verbose t debug-on-error t)" \
	      --eval "(byte-compile-file \"$$f\")"; \
	  done ; \
	echo "[compile-q] done"

# Optional: directory-scoped compile with init.el
compile-personal: check-init $(TANGLE_STAMP)
	@echo "[compile:init] personal/"
	@if [ -d "$(PERSONALDIR)" ]; then \
	  find "$(PERSONALDIR)" -type f -name '*.el' -print0 \
	  | while IFS= read -r -d '' f; do \
	      echo "[byte-compile:init] $$f"; \
	      $(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
	        $(EVAL_STRICT) $(EVAL_NATIVE) \
	        --eval "(setq byte-compile-verbose t debug-on-error t)" \
	        --eval "(byte-compile-file \"$$f\")"; \
	    done ; \
	fi

compile-lisp: check-init $(TANGLE_STAMP)
	@echo "[compile:init] lisp/"
	@if [ -d "$(LISPDIR)" ]; then \
	  find "$(LISPDIR)" -type f -name '*.el' -print0 \
	  | while IFS= read -r -d '' f; do \
	      echo "[byte-compile:init] $$f"; \
	      $(EMACS_BATCH) -l "$(EARLY)" -l "$(INIT)" \
	        $(EVAL_STRICT) $(EVAL_NATIVE) \
	        --eval "(setq byte-compile-verbose t debug-on-error t)" \
	        --eval "(byte-compile-file \"$$f\")"; \
	    done ; \
	fi

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
