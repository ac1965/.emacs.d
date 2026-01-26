README.org part 1/3
--------------------
# -*- mode: org; coding: utf-8; -*-
#+TITLE: Modern Emacs Configuration
#+AUTHOR: YAMASHITA, Takao
#+EMAIL: tjy1965@gmail.com
#+LANGUAGE: en
#+OPTIONS: toc:3 num:t
#+STARTUP: overview
#+PROPERTY: header-args :results silent :exports code :mkdirp yes :padline no :tangle no
#+PROPERTY: header-args:emacs-lisp :lexical t :noweb no-export

* Introduction
:PROPERTIES:
  :CUSTOM_ID: introduction
  :END:

A modern, literate Emacs configuration using Org Mode's Babel format, emphasizing performance, language server integration, AI assistance, and productivity.

** Features
:PROPERTIES:
:CUSTOM_ID: features
:END:

-  🚀 *Performance & Native Compilation* — Early-init moves ELN under `.cache/`, silences async warnings, widens GC at startup and restores sane values later, and uses GCMH for idle GC.
-  🧩 *Language Server Protocol* — Backend-agnostic helpers in `core/general.el`; choose *Eglot* or *lsp-mode* via `core/switches.el` with presence checks and auto-enable logic.
-  🤖 *AI Integration* — Aidermacs (vterm backend). Prefers OpenRouter when `OPENROUTER_API_KEY` is set; otherwise uses OpenAI with `OPENAI_API_KEY`.
-  🎨 *Modern UI & Editing* — Tree-sitter remaps (`*-ts-mode`), ef-themes + spacious-padding, Nerd Icons, Vertico/Orderless/Corfu/CAPE/Embark/Consult, Doom/Nano modeline switchers.
-  🗂️ *Productivity Tools* — Opinionated Org stack (agenda, capture, journal, roam, download, TOC), Magit + diff-hl/Forge, REST client, Docker/dev helpers, tidy backups/autosave-visited.

[[file:demo.png]]

** Coding Rules
:PROPERTIES:
:CUSTOM_ID: conventions
:END:

- ✅ `lexical-binding: t` is *mandatory* for all files.
- ✅ The `(provide 'FEATURE)` symbol *must match the file’s logical feature name*
  (usually derived from the file path).
- 📦 Built-in packages MUST explicitly declare `:straight nil`.
- 🌿 Each `leaf` form follows a stable, readable structure:
  `:straight` → `:bind` → `:hook` → `:custom` → `:config`
- 📚 Only documented, public APIs are used.
  - Private, internal, or speculative APIs are intentionally avoided.
- 🧠 *Compatibility & Forward-Safety Policy*
  - This configuration targets **Emacs 30+**.
  - Code is written with **Emacs 31 and later** in mind.
  - Obsolete APIs are avoided even if still functional.
    - Prefer `if-let*`, `when-let*`, `and-let*` over deprecated forms.
  - New compiler or runtime warnings are treated as *actionable signals*.
  - The codebase aims to remain warning-free under the latest stable Emacs
    with default `byte-compile-warnings`.

See also: [[#modular-loader-and-core-suite][Modular Loader & Core/Utils design]]
for how these rules are enforced structurally.

** Installation
:PROPERTIES:
   :CUSTOM_ID: installation
   :END:

*** Prerequisites
:PROPERTIES:
:CUSTOM_ID: prerequisites
:END:

- *Required*
  - Emacs *30.0+* with native compilation (`--with-native-compilation`)
  - Git
  - GNU Make
  - GCC *10+* with `libgccjit`

- *Optional but Recommended*
  - ripgrep (`rg`) → faster project-wide search
  - aspell or hunspell → spell checking
  - pass + GnuPG → password and auth-source integration
  - Homebrew (macOS only) → for consistent toolchain installation

*** Building Emacs

Use the provided build script:
[[https://github.com/ac1965/dotfiles/blob/master/.local/bin/build-emacs.sh][build-emacs.sh]]

#+begin_src shell
  build-emacs.sh
#+end_src

*** Quick Start

1. Clone the repository:
   #+begin_src shell
     git clone --depth 1 https://github.com/ac1965/.emacs.d ~/.emacs.d/
   #+end_src

2. Tangle configuration:
   #+begin_src shell
     EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs make -C ~/.emacs.d/ tangle
   #+end_src

*** Makefile

#+begin_src text :tangle Makefile :comments no
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
#+end_src

*** System Information

**** Apple Silicon (Primary)
- GNU Emacs *31.0.50*

|Property|Value|
|--------|-----|
|Commit|6287637ccd9f66a219844231380ab9873d049c6e|
|Branch|master|
|System|aarch64-apple-darwin25.2.0|
|Date|2026-01-17 15:41:46 (JST)|
|Patch|N/A ns-inline.patch|
|Features|ACL DBUS GLIB GNUTLS IMAGEMAGICK LCMS2 LIBXML2 MODULES NATIVE_COMP NOTIFY KQUEUE NS PDUMPER PNG RSVG SQLITE3 THREADS TOOLKIT_SCROLL_BARS TREE_SITTER WEBP XIM XWIDGETS ZLIB|
|Options|--with-ns --enable-mac-app=yes --with-xwidgets --with-native-compilation --with-json --with-tree-sitter --with-imagemagick --with-gnutls --prefix=/Users/ac1965/.local CPPFLAGS=-I/opt/homebrew/opt/llvm/include 'LDFLAGS=-L/opt/homebrew/opt/llvm/lib -L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++'|

**** Intel (Secondary)
- GNU Emacs *31.0.50*

|Property|Value|
|--------|-----|
|Commit|63ea5e5b3a57e7660ece022ba1834002ca2f206d|
|Branch|master|
|System|x86_64-apple-darwin25.1.0|
|Date|2025-11-01 12:05:25 (JST)|
|Patch|N/A ns-inline.patch|
|Features|ACL DBUS GIF GLIB GMP GNUTLS JPEG LCMS2 LIBXML2 MODULES NATIVE_COMP NOTIFY KQUEUE NS PDUMPER PNG RSVG SQLITE3 THREADS TIFF TOOLKIT_SCROLL_BARS TREE_SITTER WEBP XIM ZLIB|
|Options|--with-native-compilation --with-gnutls=ifavailable --with-json --with-modules --with-tree-sitter --with-xml2 --with-librsvg --with-mailutils --with-native-image-api --with-ns CPPFLAGS=-I/usr/local/opt/llvm/include 'LDFLAGS=-L/usr/local/opt/llvm/lib -L/usr/local/opt/llvm/lib/c++ -Wl,-rpath,/usr/local/opt/llvm/lib/c++'|

** Tools
:PROPERTIES:
   :CUSTOM_ID: tools
:END:

*** Graph Capture (Require Dependency Visualization)
:PROPERTIES:
:CUSTOM_ID: tools-graph-capture
:header-args:emacs-lisp: :tangle lisp/tools/graph.el
:END:

**** Purpose
Capture and visualize `require` relationships between Emacs Lisp features
during startup or module loading.
This helps understanding implicit dependencies, load order, and unwanted
coupling between modules.

**** What it does
- Advises `require` to record *from → to* edges between features
- Stores edges in a hash table (no duplicates)
- Exports the dependency graph as:
  - Graphviz DOT
  - Mermaid (for Org / Markdown)
- Provides interactive commands to enable/disable capture at runtime

**** Notes
- Intended for **diagnostics only**, not for normal startup
- Enable capture *before* loading modules
- Disable capture after use to avoid overhead
- Feature names are derived from file names or `require` symbols

**** Implementation (tangled to lisp/tools/graph.el)

#+begin_src emacs-lisp
;;; tools/graph.el --- Require dependency graph capture -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021-2025
;; Author: YAMASHITA, Takao
;; License: GNU GPL v3 or later
;;;
;;; Commentary:
;; Diagnostic tool to capture and visualize `require` dependencies
;; between Emacs Lisp features.  Outputs Graphviz DOT or Mermaid graphs.
;; Category: tools
;;;
;;; Code:

(defvar graph/require-edges (make-hash-table :test 'equal)
  "Hash table storing recorded require edges as FROM->TO keys.")

(defun graph--record (from to)
  "Record a require edge FROM -> TO if both are symbols."
  (when (and (symbolp from) (symbolp to))
    (puthash (format "%s->%s" from to) t graph/require-edges)))

(defun graph/require-advice (orig feature &optional filename noerror)
  "Advice around `require` to record dependency edges."
  (let* ((from-file (or load-file-name (buffer-file-name)))
         (from-sym (if from-file
                       (intern (file-name-sans-extension
                                (file-name-nondirectory from-file)))
                     'init))
         (to-sym (if (symbolp feature)
                     feature
                   (intern (format "%s" feature)))))
    (graph--record from-sym to-sym)
    (funcall orig feature filename noerror)))

(defun graph/edge-list ()
  "Return a sorted list of recorded edges."
  (sort (hash-table-keys graph/require-edges) #'string<))

(defun graph/export-dot ()
  "Render recorded require edges as Graphviz DOT."
  (interactive)
  (let ((buf (get-buffer-create "*Require Graph (DOT)*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "digraph G {\n  rankdir=LR;\n  node [shape=box, fontsize=10];\n")
      (dolist (e (graph/edge-list))
        (when (string-match "^\\([^->]+\\)->\\(.+\\)$" e)
          (insert (format "  \"%s\" -> \"%s\";\n"
                          (match-string 1 e)
                          (match-string 2 e)))))
      (insert "}\n"))
    (pop-to-buffer buf)))

(defun graph/export-mermaid ()
  "Render recorded require edges as Mermaid flowchart."
  (interactive)
  (let ((buf (get-buffer-create "*Require Graph (Mermaid)*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "```mermaid\nflowchart LR\n")
      (dolist (e (graph/edge-list))
        (when (string-match "^\\([^->]+\\)->\\(.+\\)$" e)
          (insert
           (format "  %s --> %s\n"
                   (replace-regexp-in-string "[./-]" "_" (match-string 1 e))
                   (replace-regexp-in-string "[./-]" "_" (match-string 2 e))))))
      (insert "```\n"))
    (pop-to-buffer buf)))

(defun graph/enable-require-capture ()
  "Enable require dependency capture."
  (interactive)
  (clrhash graph/require-edges)
  (advice-add 'require :around #'graph/require-advice)
  (message "[graph] require capture enabled"))

(defun graph/disable-require-capture ()
  "Disable require dependency capture."
  (interactive)
  (advice-remove 'require #'graph/require-advice)
  (message "[graph] require capture disabled"))

(provide 'tools/graph)
;;; tools/graph.el ends here
#+end_src

**** Module Load Summary Helper

#+begin_src emacs-lisp :tangle lisp/tools/graph.el
(defun my/modules-summary-line ()
  "Display a concise summary of module loading and captured require edges."
  (interactive)
  (let* ((edges (if (and (boundp 'graph/require-edges)
                         (hash-table-p graph/require-edges))
                    (hash-table-count graph/require-edges)
                  "N/A"))
         (skip  (if (boundp 'my:modules-skip)
                    (length (or my:modules-skip '()))
                  0))
         (extra (if (boundp 'my:modules-extra)
                    (length (or my:modules-extra '()))
                  0)))
    (message "[modules] edges=%s, skip=%d, extra=%d"
             edges skip extra)))
#+end_src

**** Usage

1. Restart Emacs
2. Enable capture:
   - `M-: (require 'tools/graph)`
   - `M-: (graph/enable-require-capture)`
3. Load modules (e.g. `(require 'modules)` or normal startup)
4. Export graph:
   - `M-: (graph/export-mermaid)`
   - or `M-: (graph/export-dot)`
5. Paste the result into Org / Markdown
6. (Optional) `M-: (my/modules-summary-line)`

**** Mermaid Example

#+begin_src mermaid :file ./graphs/require-graph.svg :results file
%% Paste the Mermaid text generated by (graph/export-mermaid) below:
flowchart LR
  modules --> core_general
  core_general --> utils_misc
#+end_src

#+RESULTS:
[[file:./graphs/require-graph.svg]]

* Configuration Files
:PROPERTIES:
:CUSTOM_ID: structure
:END:

This Emacs configuration is *modular by design* and targets **Emacs 30+**.
Each layer has a clearly defined responsibility to keep behavior predictable,
UI replaceable, and personal customizations isolated.

- =early-init.el= → earliest bootstrap (performance, paths, UI defaults)
- =init.el=       → package bootstrap, global defaults, module entrypoint
- =lisp/=         → shared, versioned modules (core, ui, completion, orgx, dev, vcs, utils)
- =personal/=     → user- and device-specific overlays (not shared policy)

** Core Bootstrap — early-init.el & init.el
:PROPERTIES:
:CUSTOM_ID: core-bootstrap
:END:

*** Overview

**** Purpose
Provide a *clean, fast, and conservative* bootstrap sequence that prepares
Emacs before regular initialization.

The bootstrap is split into two explicit stages:

- =early-init.el= runs **before package initialization** and establishes
  directories, performance guards, and flicker-free UI defaults.
- =init.el= completes package bootstrapping (*straight.el + leaf*), imports
  the login environment on macOS, applies runtime performance knobs, and
  exposes a deterministic module loader entrypoint.

This separation keeps early startup minimal and infrastructure-focused,
while deferring all feature logic to later stages.

**** What this configuration does

- Disables =package.el= early; *straight.el* and *leaf* are the only package managers.
- Speeds up startup by temporarily widening GC limits and clearing
  =file-name-handler-alist=, then restoring sane runtime values.
- Normalizes all state under predictable directories:
  =.cache/=, =.etc/=, and =.var/= (including native-comp artifacts).
- On macOS, prefers the Homebrew toolchain by preparing PATH-related variables
  (e.g. =PATH=, =LIBRARY_PATH=, =CC=) *before* native compilation is triggered.
- Disables classic backups and auto-save early; higher-level modules may enable
  =auto-save-visited-mode= later in a controlled way.
- Applies early UI defaults (no menu/tool/scroll bars, stable frame parameters)
  to avoid startup flicker.
- Bootstraps *straight.el* robustly, with guarded network access and explicit
  error reporting.
- Initializes *leaf* and its keywords, and enables conservative performance
  helpers (e.g. GCMH, =read-process-output-max=).
- Sets URL-related state paths *before* =url.el= loads, so downstream consumers
  (including *straight*) inherit them.
- Provides two stable entrypoints:
  - a per-user personal override file (=personal/<login-name>.el=)
  - a shared module loader (=lisp/modules.el=)

**** Reproducibility Note (Personal Tangling)

This configuration prioritizes reproducibility for all *shared* layers:

- =early-init.el=
- =init.el=
- =lisp/=

Personal files are intentionally tangled to user-specific paths:

: personal/<login-name>.el

This design explicitly trades strict reproducibility for:
- Per-user isolation
- Safe multi-user sharing of the same repository
- Zero-conflict personal overrides

This behavior is intentional and by design.

**** Module map (where things live)

| File            | Role |
|-----------------+------|
| =early-init.el= | Pre-init bootstrap (dirs, performance guards, package.el off, macOS toolchain, early UI) |
| =init.el=       | Main init (URL state, straight bootstrap, env import, runtime knobs, module loader) |

**** How it works (boot flow)

1. Emacs loads =early-init.el=:
   - Directory paths are established.
   - =package.el= is disabled.
   - GC and file-handler pressure is relaxed.
   - Early UI defaults are applied.
   - macOS toolchain variables are prepared when applicable.

2. Emacs loads =init.el=:
   - URL state directories are set *before* =url.el=.
   - *straight.el* is bootstrapped.
   - On macOS GUI/daemon sessions, the login environment is imported.
   - *leaf* is initialized and a minimal base of packages is ensured.
   - Runtime performance knobs (GCMH, IO buffers) are applied.
   - A per-user personal file is loaded safely.
   - =modules.el= is required as the canonical feature entrypoint.

3. After initialization completes, a concise startup summary
   (elapsed time and GC count) is printed.

**** Key settings (reference)

- =package-enable-at-startup= :: =nil= — rely exclusively on *straight.el*.
- =straight-base-dir= :: Located under =.cache/= to keep the config root clean.
- =native-comp-eln-load-path= :: Centralized under =.cache/eln-cache=.
- =read-process-output-max= :: Temporarily raised (4 MiB) for better LSP/IO throughput.
- =gcmh-high-cons-threshold= :: 16 MiB; =gcmh-mode= enabled for smoother long sessions.

**** Usage tips

- Treat =early-init.el= as infrastructure only; avoid user behavior or feature logic.
- Put shared behavior in modules loaded via =modules.el=.
- Put identity, device-specific glue, and workflow integrations in
  =personal/<login-name>.el= or related personal modules.
- After installing or upgrading Homebrew toolchains, restart Emacs so native
  compilation sees updated paths.
- To relocate the entire setup, move the config directory; Emacs will regenerate
  =.cache/=, =.etc/=, and =.var/= automatically.

**** Troubleshooting

- *“Native compilation can’t find libgccjit on macOS”* →
  Ensure Homebrew’s =libgccjit= is installed and visible. The early bootstrap
  prepares =LIBRARY_PATH= when possible.
- *“Straight bootstrap failed”* →
  A transient network issue during =url-retrieve-synchronously=.
  Re-run; failures are reported with a clear =[straight] bootstrap failed= message.
- *“Inhibit startup echo warning”* →
  =inhibit-startup-echo-area-message= is set to the actual user name string
  to satisfy Emacs’ type requirements.

*** early-init.el
:PROPERTIES:
:header-args:emacs-lisp: :tangle early-init.el
:END:

#+begin_src emacs-lisp
  ;;; early-init.el --- Early initialization (core) -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;;; Commentary:
  ;; Early bootstrap executed before regular init.el.
  ;;
  ;; - Disable package.el
  ;; - Startup optimization (GC / file-name-handlers)
  ;; - Define base directories (.cache / .etc / .var)
  ;; - Native compilation cache setup
  ;; - macOS Homebrew toolchain environment
  ;; - Early UI defaults
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'subr-x))

  (require 'seq)


  ;;; Internal utilities ----------------------------------------------------------

  (defun core--ensure-directory (dir)
    "Ensure DIR exists, creating it recursively if needed."
    (unless (file-directory-p dir)
      (condition-case err
          (make-directory dir t)
        (error
         (warn "early-init: failed to create %s (%s)"
               dir (error-message-string err))))))

  (defun core--login-username ()
    "Return login username or nil."
    (ignore-errors (user-login-name)))

  (defvar core--orig-file-name-handler-alist nil
    "Original `file-name-handler-alist' saved for restoration.")

  (defun core--restore-startup-state ()
    "Restore GC and file handler settings after startup."
    (setq file-name-handler-alist core--orig-file-name-handler-alist
          gc-cons-threshold 16777216
          gc-cons-percentage 0.1))

  (defalias 'my/ensure-directory-exists #'core--ensure-directory)


  ;;; Disable package.el ----------------------------------------------------------

  (setq package-enable-at-startup nil
        package-quickstart nil)


  ;;; Base directories ------------------------------------------------------------

  (defvar my:d
    (file-name-as-directory
     (or (and load-file-name
              (file-name-directory (file-chase-links load-file-name)))
         user-emacs-directory))
    "Root directory of this Emacs configuration.")

  (setq user-emacs-directory my:d)

  (defconst my:d:var       (expand-file-name ".var/" my:d))
  (defconst my:d:etc       (expand-file-name ".etc/" my:d))
  (defconst my:d:lisp      (expand-file-name "lisp/" my:d))
  (defconst my:d:cache
    (expand-file-name
     "emacs/"
     (or (getenv "XDG_CACHE_HOME")
         (expand-file-name ".cache/" my:d))))
  (defconst my:d:eln-cache (expand-file-name "eln-cache/" my:d:cache))
  (defconst my:d:treesit   (expand-file-name "tree-sitter/" my:d:var))
  (defconst my:d:url       (expand-file-name "url/" my:d:var))
  (defconst my:d:eww       (expand-file-name "eww/" my:d:var))

  (dolist (dir (list my:d:var my:d:etc my:d:lisp my:d:cache
                     my:d:eln-cache my:d:treesit my:d:url my:d:eww))
    (core--ensure-directory dir))


  ;;; macOS Homebrew toolchain ----------------------------------------------------

  (when (eq system-type 'darwin)
    (when-let* ((brew (or (getenv "HOMEBREW_PREFIX")
                          (and (file-directory-p "/opt/homebrew") "/opt/homebrew")
                          (and (file-directory-p "/usr/local")   "/usr/local")))
                (bin  (expand-file-name "bin" brew)))
      ;; PATH
      (when (file-directory-p bin)
        (let* ((path  (or (getenv "PATH") ""))
               (parts (split-string path ":" t)))
          (unless (member bin parts)
            (setenv "PATH" (concat bin ":" path)))))

      ;; LIBRARY_PATH (libgccjit)
      (let* ((libgccjit (expand-file-name "opt/libgccjit" brew))
             (gcc       (expand-file-name "opt/gcc" brew))
             (candidates
              (seq-filter
               #'file-directory-p
               (list (expand-file-name "lib/gcc/current" libgccjit)
                     (expand-file-name "lib" libgccjit)
                     (expand-file-name "lib/gcc/current" gcc)))))
        (when candidates
          (setenv "LIBRARY_PATH"
                  (string-join
                   (delete-dups
                    (append candidates
                            (when-let* ((old (getenv "LIBRARY_PATH")))
                              (split-string old ":" t))))
                   ":"))))

      ;; CC
      (when-let* ((gcc-bin
                   (seq-find
                    #'file-exists-p
                    (mapcar
                     (lambda (n)
                       (expand-file-name (format "gcc-%d" n) bin))
                     (number-sequence 20 10 -1)))))
        (setenv "CC" gcc-bin))))


  ;;; Native compilation ----------------------------------------------------------

  (when (and (boundp 'native-comp-eln-load-path)
             (listp native-comp-eln-load-path))
    (setopt native-comp-eln-load-path
            (cons my:d:eln-cache
                  (delq my:d:eln-cache native-comp-eln-load-path))
            native-comp-async-report-warnings-errors 'silent))


  ;;; no-littering compatibility -------------------------------------------------

  (defvar no-littering-etc-directory (file-name-as-directory my:d:etc))
  (defvar no-littering-var-directory (file-name-as-directory my:d:var))


  ;;; straight.el base ------------------------------------------------------------

  (setopt straight-base-dir my:d:cache
          straight-use-package-by-default t
          straight-vc-git-default-clone-depth 1
          straight-build-dir
          (format "build-%d.%d" emacs-major-version emacs-minor-version)
          straight-profiles '((nil . "default.el")))


  ;;; Startup performance --------------------------------------------------------

  (setq core--orig-file-name-handler-alist file-name-handler-alist)

  (setq file-name-handler-alist
        (seq-remove
         (lambda (h)
           (let ((fn (cdr h)))
             (and (symbolp fn)
                  (string-match-p "\\`\\(tramp\\|jka-compr\\)"
                                  (symbol-name fn)))))
         file-name-handler-alist))

  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)

  (add-hook 'emacs-startup-hook #'core--restore-startup-state)


  ;;; Backups / auto-save --------------------------------------------------------

  (setq make-backup-files nil
        version-control nil
        delete-old-versions nil
        backup-by-copying nil
        auto-save-default nil
        auto-save-list-file-prefix nil)


  ;;; Early UI defaults ----------------------------------------------------------

  (setopt frame-resize-pixelwise t
          frame-inhibit-implied-resize t
          cursor-in-non-selected-windows nil
          x-underline-at-descent-line t
          window-divider-default-right-width 16
          window-divider-default-places 'right-only)

  (dolist (it '((fullscreen . fullboth)
                (internal-border-width . 8)
                (tool-bar-lines . 0)))
    (add-to-list 'default-frame-alist it)
    (add-to-list 'initial-frame-alist it))

  (when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


  ;;; Startup echo ---------------------------------------------------------------

  (when-let* ((u (core--login-username)))
    (setq inhibit-startup-echo-area-message u))

  (provide 'early-init)
  ;;; early-init.el ends here
#+end_src

*** init.el
:PROPERTIES:
:header-args:emacs-lisp: :tangle init.el
:END:

#+begin_src emacs-lisp
  ;;; init.el --- Main initialization (core) -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;;; Commentary:
  ;; Entry point for Emacs 30+ configuration.
  ;;
  ;;; Code:

  (require 'subr-x)
  (require 'seq)
  (require 'cl-lib)

  ;; ;; Enable debugger on error (temporary)
  ;; (setq debug-on-error t)

  ;;; Internal helpers -----------------------------------------------------------

  (defun utils--safe-load-file (file &optional noerror)
    "Load FILE safely.
  If NOERROR is non-nil, log instead of raising."
    (when (and (stringp file) (file-exists-p file))
      (condition-case err
          (load file nil 'nomessage)
        (error
         (funcall (if noerror #'message #'user-error)
                  "[load] failed: %s (%s)"
                  file (error-message-string err))))))

  (defalias 'my/safe-load-file #'utils--safe-load-file)


  ;;; 0) URL state BEFORE url.el -------------------------------------------------

  (defvar core--url-state-dir
    (file-name-as-directory
     (or (bound-and-true-p my:d:url)
         (expand-file-name "url/" user-emacs-directory))))

  (setopt url-configuration-directory core--url-state-dir
          url-cookie-file (expand-file-name "cookies" core--url-state-dir)
          url-history-file (expand-file-name "history" core--url-state-dir)
          url-cache-directory (expand-file-name "cache/" core--url-state-dir))

  (dolist (d (list url-configuration-directory url-cache-directory))
    (make-directory d t))

  (require 'url)


  ;;; 1) Bootstrap straight.el ---------------------------------------------------

  (defvar bootstrap-version 7)

  (let* ((base (or (bound-and-true-p straight-base-dir)
                   user-emacs-directory))
         (bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" base)))
    (unless (file-exists-p bootstrap-file)
      (let ((buf
             (url-retrieve-synchronously
              "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
              'silent 'inhibit-cookies)))
        (unless (buffer-live-p buf)
          (user-error "[straight] failed to retrieve install.el"))
        (with-current-buffer buf
          (goto-char (point-max))
          (eval-print-last-sexp))))
    (load bootstrap-file nil 'nomessage))


  ;;; 1.1) leaf / org ------------------------------------------------------------

  (dolist (pkg '(leaf leaf-keywords))
    (straight-use-package pkg))

  (require 'leaf)

  (eval-when-compile
    (require 'leaf-keywords))

  (when (fboundp 'leaf-keywords-init)
    (leaf-keywords-init))

  (straight-use-package 'org)
  (require 'org)


  ;;; 1.2) macOS environment -----------------------------------------------------

  (leaf exec-path-from-shell
    :straight t
    :when (and (eq system-type 'darwin)
               (or (daemonp) (memq window-system '(mac ns))))
    :config
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-arguments '("-l" "-i"))
    (exec-path-from-shell-copy-envs
     '("PATH" "LANG"
       "PASSWORD_STORE_DIR"
       "GPG_KEY_ID"
       "OPENROUTER_API_KEY"
       "OPENAI_API_KEY"))
    (exec-path-from-shell-initialize))


  ;;; 2) Performance -------------------------------------------------------------

  (defvar core--orig-read-process-output-max
    (and (boundp 'read-process-output-max)
         read-process-output-max))

  (when (boundp 'read-process-output-max)
    (setq read-process-output-max (* 4 1024 1024)))

  (add-hook 'after-init-hook
            (lambda ()
              (when (boundp 'read-process-output-max)
                (setq read-process-output-max
                      core--orig-read-process-output-max))))

  (leaf gcmh
    :straight t
    :custom
    ((gcmh-idle-delay . 5)
     (gcmh-high-cons-threshold . 16777216))
    :config
    (gcmh-mode 1))


  ;;; 3) Core built-ins ----------------------------------------------------------

  (leaf emacs
    :straight nil
    :hook
    ((prog-mode . display-line-numbers-mode))
    :custom
    ((inhibit-startup-screen . t)
     (inhibit-startup-message . t)
     (initial-scratch-message . nil)
     (initial-major-mode . 'fundamental-mode)
     (use-short-answers . t)
     (create-lockfiles . nil)
     (idle-update-delay . 0.2)
     (ring-bell-function . #'ignore)
     (display-line-numbers-type . 'relative)
     (frame-title-format . t)
     (confirm-kill-emacs . #'y-or-n-p))
    :config
    (when (fboundp 'window-divider-mode)
      (window-divider-mode 1))
    (when (fboundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode 1))
    (when (fboundp 'electric-pair-mode)
      (electric-pair-mode 1))
    (dolist (k '("C-z" "C-x C-z" "C-x C-c"))
      (keymap-global-unset k)))


  ;;; 4) Modifier keys -----------------------------------------------------------

  (leaf my:modifier
    :straight nil
    :config
    (pcase system-type
      ('darwin
       (setq mac-option-modifier 'meta
             mac-command-modifier 'super
             mac-control-modifier 'control))
      ('windows-nt
       (setq w32-lwindow-modifier 'super
             w32-rwindow-modifier 'super))))


  ;;; 5) Personal overlay --------------------------------------------------------

  (let* ((root (or (bound-and-true-p my:d) user-emacs-directory))
         (personal (expand-file-name "personal/" root))
         (user (ignore-errors (user-login-name))))
    ;; Add personal directory to load-path
    (when (file-directory-p personal)
      (add-to-list 'load-path personal))
    ;; Load personal files
    (my/safe-load-file (expand-file-name "user.el" personal) t)
    (when user
      (my/safe-load-file
       (expand-file-name (concat user ".el") personal) t)))


  ;;; 6) Modules entrypoint ------------------------------------------------------

  (defvar my:modules-extra nil
    "Extra module list appended by optional layers.")

  (let* ((root (or (bound-and-true-p my:d) user-emacs-directory))
         (lisp-dir (expand-file-name "lisp/" root)))
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))
    (require 'core/custom-ui-extras nil t)
    (require 'modules nil t))


  ;;; 7) Startup message ---------------------------------------------------------

  (defun core--announce-startup ()
    "Report startup time and GC count."
    (message "Emacs ready in %.2f seconds with %d GCs."
             (float-time
              (time-subtract after-init-time before-init-time))
             gcs-done))

  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 0 nil #'core--announce-startup)))


  (provide 'init)
  ;;; init.el ends here
#+end_src

** Modular Loader & Core Module Suite — `modules.el` + core/ui/completion/orgx/dev/utils
:PROPERTIES:
:CUSTOM_ID: modular-loader-and-core-suite
:END:

*** Overview

**** Purpose
Provide a *deterministic, maintainable, and debuggable Emacs 30+ configuration*
with a **strict separation of responsibility and execution**.

This configuration is designed around the following principles:

- *Composable*: all shared modules live under =lisp/= and are loaded in a fixed,
  explicit order via a single entrypoint.
- *Selectable*: UI, LSP, and optional feature stacks are resolved centrally and
  can be switched without cross-module edits.
- *Low-noise*: Customize output is isolated under =.etc/=; writable runtime state
  lives under =.var/= and is never mixed with policy code.
- *Forward-compatible*: the codebase follows **Emacs 31+ compatibility guidance**
  and avoids obsolete, transitional, or undocumented APIs even when still
  technically functional.

The loader and core layers define *policy and orchestration*.
Execution, presentation, and personal overlays are kept physically and
conceptually separate.

**** What this configuration does

- *Bootstraps modules deterministically* via =lisp/modules.el=, honoring:
  - =my:modules-verbose= for per-module timing and summary reporting,
  - =my:modules-skip= to omit selected features safely,
  - =my:modules-extra= to append optional or experimental modules last.
- *Resolves replaceable stacks centrally* via =core/switches.el=:
  - UI backends: =none= | =nano= | =doom=
  - LSP backends: =eglot= | =lsp-mode=
- *Defines core policy layers* for daily editing:
  - Editing and Dired defaults,
  - Undo and history policy (=undo-fu= / =vundo=),
  - Window and buffer navigation foundations.
- *Defines session policy explicitly*:
  - saveplace, recentf, savehist, desktop, winner,
  - autosave and recovery strategy.
- *Delegates execution to the utilities layer* for long-running sessions:
  - Garbage collection at safe boundaries,
  - Temporary buffer and dead-process housekeeping,
  - LSP lifecycle cleanup on project switches.
- *Exposes UI as replaceable bundles*:
  - Fonts and typography normalization,
  - Theming via nano-theme + palette normalization,
  - Optional modeline bundles (=nano-modeline= or =doom-modeline=),
  - Icons, clock/battery, and visual aids.
- *Defines development surfaces*:
  - Tree-sitter grammars and major-mode remapping,
  - Project and editorconfig integration,
  - Eglot or lsp-mode (+ lsp-ui),
  - Formatting via Apheleia,
  - Docker, REST, SQL, terminal, and build helpers,
  - AI-assisted development via Aidermacs with all state isolated under =.var/=.
- *Builds a layered completion surface*:
  - Minibuffer: Orderless + Vertico (+ Marginalia),
  - In-buffer: Corfu (+ kind-icon) driven by CAPF,
  - CAPF sources composed explicitly (CAPE, mode-specific, Org SRC–aware),
  - Navigation and actions via Consult + Embark (+ embark-consult).

**** Utilities Layer Philosophy

The =utils/= layer provides **operational executors**, not policy.

It exists to *do work* requested implicitly by higher-level policy defined in
=core/=, never to decide *why* or *when* something should happen.

Characteristics:
- May install guarded hooks or timers.
- May execute GC, buffer/process cleanup, or LSP lifecycle actions.
- Must remain safe to disable entirely for debugging or bisection.
- Must never impose UI, logging, notifications, or user preferences.

The =utils/= layer contains *no policy*.
All decisions belong to =core/= or =personal/=.

**** Utilities Layer Design Principles

- *Operational helpers*:
  Cross-cutting executors for long-running sessions
  (GC strategy, housekeeping, lifecycle cleanup).
- *Opt-in and non-intrusive*:
  All utilities degrade to no-ops when disabled and never affect global policy.
- *Future-proof*:
  Implementations follow Emacs 31+ guidance and avoid obsolete APIs.

**** Module map (where things live)

This table documents **where responsibilities live** and **what must not live
there**.
The goal is **stable core behavior**, **replaceable UI**, and
**fully isolated personal code**.

| Path / Module | Responsibility | Must NOT contain |
|---------------+----------------+------------------|
| lisp/modules.el | Canonical module load order | Configuration or policy logic |
| lisp/core/ | *Foundation & policy* (paths, startup, sessions, switches) | UI, personal taste, platform glue |
| lisp/core/session*.el | Session health, thresholds, timers | UI, notifications |
| lisp/core/general.el | Global keybinding foundation | Mode-specific bindings |
| lisp/ui/ | *Presentation only* (theme, fonts, modeline) | Timers, GC, cleanup logic |
| lisp/completion/ | Completion stack & CAPF composition | Session or lifecycle policy |
| lisp/orgx/ | Org extensions & workflows (stable, practical) | Experimental Org features |
| lisp/dev/ | Developer tooling (LSP, treesit, formatters) | UI or personal identity |
| lisp/vcs/ | Version control entry points | Project or session policy |
| lisp/utils/ | *Operational executors* (GC, housekeeping, LSP hygiene) | Policy or startup logic |
| personal/user.el | *Personal overlays* (identity, switches, paths) | Shared policy |
| personal/device-*.el | Device / OS–specific glue | Cross-platform code |
| personal/*-integration.el | External service integrations | Core or utils dependencies |

**** Design rules enforced by this structure

- *Core is policy, not appearance*
  No UI, no personal taste, no platform specifics.
- *UI is replaceable*
  nano / doom / none can be switched without touching core.
- *Utils are executors, not decision-makers*
  Safe to disable and never authoritative.
- *Personal code is isolated*
  User- and device-specific glue only.
- *Platform-specific code never leaks upward*
  OS-dependent logic stays under =personal/=.

*** modules.el
:PROPERTIES:
:CUSTOM_ID: core-modules
:header-args:emacs-lisp: :tangle lisp/modules.el
:END:

**** Purpose
Serve as the **single authoritative entry point** for loading all modular
configuration files under `lisp/`.

This module defines *load order*, *error handling policy*, and *diagnostics*
for the entire Emacs configuration, while remaining agnostic of individual
module contents.

**** What it does
- Defines the canonical ordered module list (`my:modules`)
- Supports selective loading via:
  - `my:modules-skip`
  - `my:modules-extra`
- Loads each module safely using `require`
- Measures and reports per-module load time
- Produces a final summary including:
  - Loaded modules
  - Skipped modules
  - Failed modules (non-fatal)

**** Notes
- This module **must load early**
- It does not configure any editor behavior directly
- A failure in one module never aborts the full startup
- Feature symbols must exactly match each module’s `provide` form
- Acts as the backbone for all higher-level systems (UI, completion, org, etc.)

**** Implementation

#+begin_src emacs-lisp
  ;;; modules.el --- Modular config loader -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;;; Commentary:
  ;; Central entry point to load modular configs placed under lisp/.
  ;; Category: core
  ;;
  ;;; Code:

  (eval-when-compile (require 'subr-x))
  (require 'seq)
  (defgroup my:modules nil
    "Loader options for modular Emacs configuration."
    :group 'convenience)

  (defcustom my:modules-verbose t
    "If non-nil, print per-module load time and a summary."
    :type 'boolean
    :group 'my:modules)

  (defcustom my:modules-skip nil
    "List of module features to skip during loading."
    :type '(repeat symbol)
    :group 'my:modules)

  (defcustom my:modules-extra nil
    "List of extra module features to append after `my:modules'."
    :type '(repeat symbol)
    :group 'my:modules)

  (defconst my:modules
    '(
      ;; Core
      core/fixes
      core/core-session
      core/general
      core/tools
      core/utils
      core/core-treesit
      core/history
      core/editing
      core/switches
      core/custom

      ;; UI
      ui/ui-font
      ui/ui-theme
      ui/ui-window
      ui/ui-utils
      ui/ui-health-modeline

      ;; Completion
      completion/completion-core
      completion/completion-vertico
      completion/completion-consult
      completion/completion-embark
      completion/completion-corfu
      completion/completion-icons
      completion/completion-capf
      completion/completion-capf-org-src
      completion/completion-capf-org-src-lang
      completion/completion-corfu-org-src
      completion/completion-orderless-org-src

      ;; Org ecosystem (module namespace = orgx/)
      orgx/org-core
      orgx/org-visual
      orgx/org-extensions
      orgx/org-export

      ;; VCS (uncomment when needed)
      vcs/vcs-magit
      vcs/vcs-gutter
      vcs/vcs-forge

      ;; Development
      dev/dev-ai
      dev/dev-term
      dev/dev-build
      dev/dev-docker

      ;; Web/Infra
      dev/dev-web-core
      dev/dev-format
      dev/dev-infra-modes
      dev/dev-sql
      dev/dev-rest

      ;; Utils
      utils/utils-functions
      utils/utils-scratch
      utils/utils-backup
      utils/utils-async
      utils/utils-org-agenda
      utils/utils-notes-markdown
      utils/utils-search-nav
      utils/utils-gc
      utils/utils-buffers
      utils/utils-lsp
      )
    "Default set of modules to load in order.")

  (defun my/modules--should-load-p (feature)
    "Return non-nil if FEATURE should be loaded (i.e., not in skip list)."
    (not (memq feature my:modules-skip)))

  (defun my/modules--require-safe (feature)
    "Require FEATURE with error trapping. Return non-nil on success.
  Errors are reported via `message' but do not abort the whole loader."
    (condition-case err
        (progn (require feature) t)
      (error
       (message "[modules] Failed to load %s: %s"
                feature (error-message-string err))
       nil)))

  (defun my:modules--format-seconds (sec)
    "Format SEC (float seconds) in a compact human-readable form."
    (cond
     ((< sec 0.001) (format "%.3fms" (* sec 1000.0)))
     ((< sec 1.0)   (format "%.1fms"  (* sec 1000.0)))
     (t             (format "%.2fs"   sec))))

  (defun my/modules-load ()
    "Load all modules defined by `my:modules', respecting options.
  - Honors `my:modules-skip' and `my:modules-extra'.
  - Prints per-module timing when `my:modules-verbose' is non-nil.
  - Prints a final summary including counts *and* the lists of skipped/failed."
    (let* ((all (append my:modules my:modules-extra))
           (final (seq-remove (lambda (m) (not (my/modules--should-load-p m))) all))
           (skipped (seq-remove (lambda (m) (memq m final)) all))
           (ok 0) (ng 0)
           (failed '())
           (loaded '())
           (t0 (and my:modules-verbose (current-time))))
      (dolist (mod final)
        (let ((m0 (and my:modules-verbose (current-time))))
          (if (my/modules--require-safe mod)
              (progn (setq ok (1+ ok)) (push mod loaded))
            (setq ng (1+ ng)) (push mod failed))
          (when my:modules-verbose
            (message "[modules] %-24s %s"
                     mod (my:modules--format-seconds
                          (float-time (time-subtract (current-time) m0)))))))
      (when my:modules-verbose
        ;; Main summary (backward-compatible)
        (message "[modules] loaded=%d skipped=%d failed=%d total=%s"
                 ok (length skipped) ng
                 (my:modules--format-seconds
                  (float-time (time-subtract (current-time) t0))))
        ;; Detail: skipped targets
        (when skipped
          (message "[modules] skipped (%d): %s"
                   (length skipped)
                   (mapconcat #'symbol-name (nreverse skipped) " ")))
        ;; Detail: failed targets
        (when failed
          (message "[modules] failed  (%d): %s"
                   (length failed)
                   (mapconcat #'symbol-name (nreverse failed) " "))))
      ok))

  (my/modules-load)

  (provide 'modules)
  ;;; modules.el ends here
#+end_src

*** core/fixes.el
:PROPERTIES:
:CUSTOM_ID: core-fixes
:header-args:emacs-lisp: :tangle lisp/core/fixes.el
:END:

**** Purpose
Provide a minimal and strictly scoped compatibility and hotfix layer to
keep the configuration stable across Emacs versions.

This module exists to absorb **version-specific breakage** without
changing architecture or refactoring existing code.

**** What it does
- Defines version comparison helpers
- Adds defensive advice guards for Emacs 30.1+
- Prevents duplicate advice installation on reload
- Limits fixes to clearly justified, version-scoped cases

**** Notes
- This is **not** a general tweaks file
- No new features or behavior changes are allowed here
- All fixes must be:
  - Explicitly justified
  - Version-scoped
  - Minimal and reversible

**** Implementation

#+begin_src emacs-lisp
  ;;; core/fixes.el --- Compatibility & hotfix layer -*- lexical-binding: t; -*-

  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later

  ;;; Commentary:
  ;;
  ;; This file contains *minimal and conditional fixes* to keep the
  ;; existing configuration stable across Emacs versions.
  ;;
  ;; Design principles:
  ;; - Do NOT change architecture or module structure
  ;; - Do NOT refactor existing code
  ;; - Add guards ONLY where real breakage is observed or expected
  ;; - Keep fixes version-scoped and explicit
  ;;
  ;; This is NOT a dumping ground for tweaks.
  ;; Everything here must have a concrete reason to exist.
  ;;
  ;; Current scope:
  ;; - Emacs 30.1+ compatibility guards
  ;; - Advice safety fences (load / require)
  ;; - Loader stability fixes
  ;;
  ;;; Code:

  ;;;; Utilities -------------------------------------------------------------

  (defun core/fixes--emacs>= (major minor)
    "Return non-nil if running Emacs version is >= MAJOR.MINOR."
    (or (> emacs-major-version major)
        (and (= emacs-major-version major)
             (>= emacs-minor-version minor))))

  ;;;; thisfile advice guard (Emacs 30.1+) ----------------------------------

  (when (core/fixes--emacs>= 30 1)

    ;; Defensive check: do not add advice unless the functions exist.
    (when (and (fboundp 'my:with-thisfile--load)
               (fboundp 'my:with-thisfile--require))

      ;; Avoid duplicate advice application (reload-safe).
      (unless (advice-member-p #'my:with-thisfile--load 'load)
        (advice-add 'load :around #'my:with-thisfile--load))

      (unless (advice-member-p #'my:with-thisfile--require 'require)
        (advice-add 'require :around #'my:with-thisfile--require))))

  ;;;; Forward-compatibility notes ------------------------------------------
  ;;
  ;; - If Emacs 30.2+ resolves the underlying issue, this block can be
  ;;   narrowed or removed by adjusting the version guard above.
  ;; - Do NOT expand the version range casually.
  ;; - Any new fix must justify:
  ;;     * Why here?
  ;;     * Why version-scoped?
  ;;     * Why advice (and not local change)?
  ;;

  (provide 'core/fixes)

  ;;; core/fixes.el ends here
#+end_src

*** core/core-session.el
:PROPERTIES:
:CUSTOM_ID: core-session
:header-args:emacs-lisp: :tangle lisp/core/core-session.el
:END:

**** Purpose
Provide a central orchestration layer for long-running Emacs sessions.
This module defines *policy*, *timing*, and *coordination* while delegating
actual work to utils modules.

**** What it does
- Coordinates GC, buffer cleanup, and LSP lifecycle helpers
- Defines safe defaults and thresholds
- Owns timers and idle hooks
- Exposes customization knobs for personal overrides

**** Notes
- This module does NOT provide UI, visualization, or notifications.
- Actual implementations live in utils/*.
- Personal configuration should only override defcustom values.

**** Implementation

#+begin_src emacs-lisp
  ;;; core-session.el --- Long-running session orchestration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Commentary:
  ;; Central policy and orchestration layer for long-running Emacs sessions.
  ;;
  ;; This module coordinates:
  ;; - Safe garbage collection
  ;; - Periodic buffer housekeeping
  ;; - LSP lifecycle cleanup
  ;;
  ;; Actual implementations are delegated to utils modules.
  ;; This file defines *when* and *under what conditions* they run.
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  ;; ---------------------------------------------------------------------------
  ;; Customization
  ;; ---------------------------------------------------------------------------

  (defgroup core-session nil
    "Long-running Emacs session orchestration."
    :group 'convenience)

  (defcustom core-session-enable-p t
    "Enable long-running session orchestration."
    :type 'boolean
    :group 'core-session)

  (defcustom core-session-idle-delay
    (* 30 60)
    "Seconds of idle time before running lightweight maintenance."
    :type 'integer
    :group 'core-session)

  (defcustom core-session-periodic-interval
    600
    "Interval in seconds for periodic maintenance tasks."
    :type 'integer
    :group 'core-session)

  (defcustom core-session-buffer-threshold
    300
    "Soft threshold for number of live buffers considered risky."
    :type 'integer
    :group 'core-session)

  (defcustom core-session-process-threshold
    8
    "Soft threshold for number of live processes considered risky."
    :type 'integer
    :group 'core-session)

  ;; ---------------------------------------------------------------------------
  ;; Internal helpers
  ;; ---------------------------------------------------------------------------

  (defvar core-session--idle-timer nil
    "Idle timer for lightweight session maintenance.")

  (defvar core-session--periodic-timer nil
    "Periodic timer for session health checks.")

  (defun core-session--buffers-count ()
    "Return the number of live buffers."
    (length (buffer-list)))

  (defun core-session--processes-count ()
    "Return the number of live processes."
    (length (process-list)))

  (defun core-session--risky-state-p ()
    "Return non-nil if the current session looks risky."
    (or (> (core-session--buffers-count)
           core-session-buffer-threshold)
        (> (core-session--processes-count)
           core-session-process-threshold)))

  ;; ---------------------------------------------------------------------------
  ;; Maintenance actions (delegation only)
  ;; ---------------------------------------------------------------------------

  (defun core-session--lightweight-maintenance ()
    "Run lightweight maintenance tasks.

  This function delegates actual work to utils modules and must remain safe."
    (when core-session-enable-p
      ;; GC helpers
      (when (fboundp 'utils-gc--collect)
        (utils-gc--collect))

      ;; Buffer housekeeping
      (when (fboundp 'utils-buffers-cleanup)
        (utils-buffers-cleanup))))

  (defun core-session--periodic-check ()
    "Run periodic session health checks.

  Currently this only performs maintenance when the session looks risky."
    (when (and core-session-enable-p
               (core-session--risky-state-p))
      (core-session--lightweight-maintenance)))

  ;; ---------------------------------------------------------------------------
  ;; Public commands
  ;; ---------------------------------------------------------------------------

  ;;;###autoload
  (defun core-session-run-health-check ()
    "Run a manual session health check."
    (interactive)
    (core-session--lightweight-maintenance)
    (message "Core session health check completed"))

  ;;;###autoload
  (defun core-session-lightweight-restart ()
    "Perform a safe lightweight restart of the current Emacs session.

  This shuts down obsolete LSP servers, cleans buffers, and runs GC.
  No buffers with unsaved changes are touched."
    (interactive)
    ;; LSP lifecycle cleanup
    (when (and core-session-enable-p
               (fboundp 'utils-lsp-on-project-switch))
      (ignore-errors
        (utils-lsp-on-project-switch)))

    ;; Buffers and GC
    (core-session--lightweight-maintenance)

    (clear-image-cache)
    (message "Core session lightweight restart completed"))

  ;; ---------------------------------------------------------------------------
  ;; Activation
  ;; ---------------------------------------------------------------------------

  (defun core-session--enable ()
    "Enable core session orchestration."
    ;; Idle maintenance
    (setq core-session--idle-timer
          (run-with-idle-timer
           core-session-idle-delay
           t
           #'core-session--lightweight-maintenance))

    ;; Periodic checks
    (setq core-session--periodic-timer
          (run-with-timer
           core-session-periodic-interval
           core-session-periodic-interval
           #'core-session--periodic-check)))

  (defun core-session--disable ()
    "Disable core session orchestration."
    (when (timerp core-session--idle-timer)
      (cancel-timer core-session--idle-timer))
    (when (timerp core-session--periodic-timer)
      (cancel-timer core-session--periodic-timer))
    (setq core-session--idle-timer nil
          core-session--periodic-timer nil))

  (when core-session-enable-p
    (core-session--enable))

  (provide 'core/core-session)
  ;;; core/core-session.el ends here
#+end_src

*** core/general.el
:PROPERTIES:
:CUSTOM_ID: core-general
:header-args:emacs-lisp: :tangle lisp/core/general.el
:END:

**** Purpose
Provide global, non-modal keybindings and general-purpose helpers that form
the ergonomic foundation of the entire configuration.

This module defines *interaction policy* (leader keys, navigation, commands)
without depending on any specific UI, completion framework, or LSP backend.

**** What it does
- Defines a **global leader key** (`C-c SPC`) and structured sub-maps
- Establishes a **local leader** for major-mode–specific commands
- Provides IDE-agnostic helpers (rename, format, code actions)
- Integrates Which-Key labels for discoverability
- Adds safe global keybindings (macOS-like and terminal-friendly)
- Wires local leader bindings for common major modes (Org, Dired, Magit, etc.)

**** Notes
- This module intentionally avoids modal editing (no Meow / Evil)
- No completion or UI-specific logic is implemented here
- Leader layout is stable and meant to be muscle-memory friendly
- Major-mode bindings are added lazily via `with-eval-after-load`

**** Implementation

#+begin_src emacs-lisp
  ;;; core/general.el --- General settings & keybindings (NO Meow) -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Commentary:
  ;; - Drop Meow: provide a non-modal, global leader-key layout.
  ;; - Keep useful global bindings for macOS-like shortcuts and muscle memory.
  ;; - Provide LSP-agnostic helpers (code actions, rename, format).
  ;; - Authentication helpers (GPG + pass) remain as-is.
  ;;
  ;; Design notes:
  ;; - We define a global leader key (C-c SPC).
  ;; - Under the leader, we expose groups: b(buffers), w(windows), p(project), g(git),
  ;; c(code), e(errors), t(toggles), o(org/roam), m(mode-specific), a(ai), q(session), h(help).
  ;; - "m" is reserved as a local leader prefix for major-mode specific commands.
  ;; - Which-Key labels reflect the chosen leader key at runtime.
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'leaf-keywords)
    (require 'subr-x))

  (defvar plstore-secret-keys)
  (defvar plstore-encrypt-to)

  (autoload 'magit-status "magit")
  (autoload 'magit-blame-addition "magit")
  (autoload 'magit-log-current "magit")
  (autoload 'magit-diff-buffer-file "magit")
  (autoload 'magit-commit "magit")

  (autoload 'flymake-goto-next-error "flymake")
  (autoload 'flymake-goto-prev-error "flymake")
  (autoload 'flymake-show-buffer-diagnostics "flymake")

  (autoload 'project-roots "project")

  ;;;; Text scaling hydra --------------------------------------------------------
  (leaf hydra
    :straight t
    :config
    (defhydra core-hydra-text-scale (:hint nil :color red)
      "
  ^Text Scaling^
  [_+_] increase   [_-_] decrease   [_0_] reset   [_q_] quit
  "
      ("+" text-scale-increase)
      ("-" text-scale-decrease)
      ("0" (text-scale-set 0) :color blue)
      ("q" nil "quit" :color blue)))

  ;;;; Small utilities -----------------------------------------------------------
  (leaf my:utils
    :straight nil
    :init
    (defun my/new-frame-with-scratch ()
      "Create a new frame and switch to a fresh buffer."
      (interactive)
      (let ((frame (make-frame)))
        (with-selected-frame frame
          (switch-to-buffer (generate-new-buffer "untitled")))))

    (defun my/restart-or-exit ()
      "Restart Emacs if `restart-emacs' exists; otherwise save & exit."
      (interactive)
      (if (fboundp 'restart-emacs)
          (restart-emacs)
        (save-buffers-kill-emacs)))

    ;; Arrow-based window motions (keeps default muscle memory).
    (windmove-default-keybindings))

  ;;;; IDE-agnostic helpers (Eglot / lsp-mode) -----------------------------------
  (defun my/code-actions ()
    "Run code actions via Eglot or lsp-mode."
    (interactive)
    (cond
     ((fboundp 'eglot-code-actions) (eglot-code-actions))
     ((fboundp 'lsp-execute-code-action) (lsp-execute-code-action))
     (t (user-error "No code action backend (Eglot/LSP) available"))))

  (defun my/rename-symbol ()
    "Rename symbol via Eglot or lsp-mode."
    (interactive)
    (cond
     ((fboundp 'eglot-rename) (eglot-rename))
     ((fboundp 'lsp-rename) (lsp-rename))
     (t (user-error "No rename backend (Eglot/LSP) available"))))

  (defun my/format-buffer ()
    "Format buffer via Eglot/LSP; fallback to `indent-region'."
    (interactive)
    (cond
     ((fboundp 'eglot-format-buffer) (eglot-format-buffer))
     ((fboundp 'lsp-format-buffer) (lsp-format-buffer))
     ((fboundp 'indent-region) (indent-region (point-min) (point-max)))
     (t (user-error "No formatter available"))))

  (defun my/consult-ripgrep-project ()
    "Run ripgrep in current project; fallback to prompting."
    (interactive)
    (let* ((pr (when (fboundp 'project-current) (project-current)))
           (root (when pr (car (project-roots pr)))))
      (if (and root (fboundp 'consult-ripgrep))
          (consult-ripgrep root)
        (call-interactively 'consult-ripgrep))))

  (defun my/toggle-transient-line-numbers ()
    "Toggle line numbers, preserving buffer-local overrides."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode 0)
      (display-line-numbers-mode 1)))

  ;;;; Global LEADER (non-modal) -------------------------------------------------
  ;; Define leader keys and prefix maps:
  (defconst my:leader-key "C-c SPC"
    "Key sequence used as the global leader key.")

  (defconst my:leader-which-prefix "C-c SPC"
    "Human-readable leader prefix string for which-key labels.")

  ;; Define a local leader key sequence for major mode commands (contextual).
  (defconst my:local-leader-key (concat my:leader-key " m")
    "Key sequence used as the local (major-mode) leader key.")

  ;; Top-level leader prefix map and subgroup prefix maps.
  (define-prefix-command 'my/leader-map)
  (define-prefix-command 'my/leader-b-map) ;; buffers
  (define-prefix-command 'my/leader-w-map) ;; windows
  (define-prefix-command 'my/leader-p-map) ;; project
  (define-prefix-command 'my/leader-g-map) ;; git
  (define-prefix-command 'my/leader-c-map) ;; code
  (define-prefix-command 'my/leader-e-map) ;; errors/diagnostics
  (define-prefix-command 'my/leader-t-map) ;; toggles
  (define-prefix-command 'my/leader-o-map) ;; org/roam
  (define-prefix-command 'my/leader-m-map) ;; mode-specific (local leader)
  (define-prefix-command 'my/leader-a-map) ;; ai
  (define-prefix-command 'my/leader-q-map) ;; session/quit
  (define-prefix-command 'my/leader-h-map) ;; help

  ;; Bind the global leader key to its prefix map.
  (when (fboundp 'keymap-global-set)
    (keymap-global-set my:leader-key 'my/leader-map)
    ;; In older Emacs, use (global-set-key (kbd my:leader-key) 'my/leader-map)
    )

  ;; Bind group prefixes under the leader map.
  (define-key my/leader-map (kbd "b") 'my/leader-b-map)
  (define-key my/leader-map (kbd "w") 'my/leader-w-map)
  (define-key my/leader-map (kbd "p") 'my/leader-p-map)
  (define-key my/leader-map (kbd "g") 'my/leader-g-map)
  (define-key my/leader-map (kbd "c") 'my/leader-c-map)
  (define-key my/leader-map (kbd "e") 'my/leader-e-map)
  (define-key my/leader-map (kbd "t") 'my/leader-t-map)
  (define-key my/leader-map (kbd "o") 'my/leader-o-map)
  (define-key my/leader-map (kbd "m") 'my/leader-m-map)  ;; "m" for major-mode leader
  (define-key my/leader-map (kbd "a") 'my/leader-a-map)
  (define-key my/leader-map (kbd "q") 'my/leader-q-map)
  (define-key my/leader-map (kbd "h") 'my/leader-h-map)

  ;; 1) Top-level leader bindings (LEADER <key>)
  (define-key my/leader-map (kbd "SPC") #'execute-extended-command) ;; M-x
  (define-key my/leader-map (kbd "/")   #'consult-line)
  (define-key my/leader-map (kbd ";")   #'comment-or-uncomment-region)
  (define-key my/leader-map (kbd "=")   #'er/expand-region)
  (define-key my/leader-map (kbd "`")   #'eval-expression)
  (define-key my/leader-map (kbd "z")   #'core-hydra-text-scale/body)
  ;; frequent file and buffer helpers
  (define-key my/leader-map (kbd ".")   #'other-window)
  (define-key my/leader-map (kbd "f")   #'find-file)
  (define-key my/leader-map (kbd "F")   #'find-file-other-window)
  (define-key my/leader-map (kbd "O")   #'find-file-other-frame)
  (define-key my/leader-map (kbd "r")   #'consult-recent-file)

  ;; 2) Buffers (LEADER b ...)
  (define-key my/leader-b-map (kbd "b") #'consult-buffer)
  (define-key my/leader-b-map (kbd "B") #'consult-project-buffer)
  (define-key my/leader-b-map (kbd "k") #'my/kill-buffer-smart)
  (define-key my/leader-b-map (kbd "n") #'next-buffer)
  (define-key my/leader-b-map (kbd "p") #'previous-buffer)
  (define-key my/leader-b-map (kbd "r") #'revert-buffer)

  ;; 3) Windows (LEADER w ...)
  (define-key my/leader-w-map (kbd "w") #'ace-window)
  (define-key my/leader-w-map (kbd "s") #'split-window-below)
  (define-key my/leader-w-map (kbd "v") #'split-window-right)
  (define-key my/leader-w-map (kbd "d") #'delete-window)
  (define-key my/leader-w-map (kbd "o") #'delete-other-windows)
  (define-key my/leader-w-map (kbd "=") #'balance-windows)
  (define-key my/leader-w-map (kbd "2") #'my/toggle-window-split)

  ;; 4) Project (LEADER p ...)
  (define-key my/leader-p-map (kbd "p") #'project-switch-project)
  (define-key my/leader-p-map (kbd "f") #'project-find-file)
  (define-key my/leader-p-map (kbd "s") #'my/consult-ripgrep-project)
  (define-key my/leader-p-map (kbd "b") #'consult-project-buffer)
  (define-key my/leader-p-map (kbd "r") #'project-query-replace-regexp)
  (define-key my/leader-p-map (kbd "d") #'project-dired)

  ;; 5) Search (LEADER s ...) – (placed under main map for convenience)
  (define-key my/leader-map (kbd "s s") #'consult-line)
  (define-key my/leader-map (kbd "s r") #'consult-ripgrep)
  (define-key my/leader-map (kbd "s g") #'my/consult-ripgrep-project)
  (define-key my/leader-map (kbd "s m") #'consult-imenu)

  ;; 6) Git (LEADER g ...)
  (define-key my/leader-g-map (kbd "s") #'magit-status)
  (define-key my/leader-g-map (kbd "b") #'magit-blame-addition)
  (define-key my/leader-g-map (kbd "l") #'magit-log-current)
  (define-key my/leader-g-map (kbd "d") #'magit-diff-buffer-file)
  (define-key my/leader-g-map (kbd "c") #'magit-commit)

  ;; 7) Code (LEADER c ...) – LSP-agnostic helpers
  (define-key my/leader-c-map (kbd "a") #'my/code-actions)
  (define-key my/leader-c-map (kbd "r") #'my/rename-symbol)
  (define-key my/leader-c-map (kbd "f") #'my/format-buffer)
  (define-key my/leader-c-map (kbd "d") #'xref-find-definitions)
  (define-key my/leader-c-map (kbd "D") #'xref-find-definitions-other-window)
  (define-key my/leader-c-map (kbd "R") #'xref-find-references)
  (define-key my/leader-c-map (kbd "i") #'completion-at-point)

  ;; 8) Errors/diagnostics (LEADER e ...)
  (define-key my/leader-e-map (kbd "n") #'flymake-goto-next-error)
  (define-key my/leader-e-map (kbd "p") #'flymake-goto-prev-error)
  (define-key my/leader-e-map (kbd "l") #'flymake-show-buffer-diagnostics)

  ;; 9) Toggles (LEADER t ...)
  (define-key my/leader-t-map (kbd "l") #'my/toggle-transient-line-numbers)
  (define-key my/leader-t-map (kbd "w") #'whitespace-mode)
  (define-key my/leader-t-map (kbd "r") #'read-only-mode)
  (define-key my/leader-t-map (kbd "z") #'core-hydra-text-scale/body)
  ;; Removed toggle for images here, as it's specific to EWW (now local leader in eww-mode).

  ;; 10) Org & Roam (LEADER o ...)
  (define-key my/leader-o-map (kbd "a") #'org-agenda)
  (define-key my/leader-o-map (kbd "c") #'org-capture)
  (define-key my/leader-o-map (kbd "i") #'org-roam-node-insert)
  (define-key my/leader-o-map (kbd "f") #'org-roam-node-find)
  (define-key my/leader-o-map (kbd "s") #'my/org-sidebar)
  (define-key my/leader-o-map (kbd "t") #'my/org-sidebar-toggle)

  ;; 11) Misc/Web – **(Moved to local leader or global C-c w)**
  ;; (LEADER m ...) previously held EWW (web) commands.
  ;; We leave my/leader-m-map defined for local leader usage, but no global bindings here now.
  ;; EWW commands are accessible via global "C-c w" prefix or when in eww-mode via local leader.

  ;; 12) AI (LEADER a ...)
  (define-key my/leader-a-map (kbd "a") #'aidermacs-transient-menu)

  ;; 13) Session/quit (LEADER q ...)
  (define-key my/leader-q-map (kbd "n") #'my/new-frame-with-scratch)
  (define-key my/leader-q-map (kbd "r") #'my/restart-or-exit)
  (define-key my/leader-q-map (kbd "q") #'save-buffers-kill-emacs)

  ;; 14) Help (LEADER h ...)
  (define-key my/leader-h-map (kbd "k") #'describe-key)
  (define-key my/leader-h-map (kbd "f") #'describe-function)
  (define-key my/leader-h-map (kbd "v") #'describe-variable)

  ;;;; Which-Key integration for leader groups -----------------------------------
  (leaf which-key
    :straight t
    :hook (after-init-hook . which-key-mode)
    :custom ((which-key-idle-delay . 0.4))
    :config
    ;; Label leader groups dynamically according to `my:leader-which-prefix`.
    (dolist (it `((,(concat my:leader-which-prefix " b") . "buffers")
                  (,(concat my:leader-which-prefix " w") . "windows")
                  (,(concat my:leader-which-prefix " p") . "project")
                  (,(concat my:leader-which-prefix " s") . "search")
                  (,(concat my:leader-which-prefix " g") . "git")
                  (,(concat my:leader-which-prefix " c") . "code")
                  (,(concat my:leader-which-prefix " e") . "errors")
                  (,(concat my:leader-which-prefix " t") . "toggles")
                  (,(concat my:leader-which-prefix " o") . "org/roam")
                  (,(concat my:leader-which-prefix " m") . "mode")   ;; updated label
  		(,(concat my:leader-which-prefix " a") . "ai")
                  (,(concat my:leader-which-prefix " q") . "session")
                  (,(concat my:leader-which-prefix " h") . "help")))
      (which-key-add-key-based-replacements (car it) (cdr it))))

  ;;;; Major-mode specific (local leader) bindings -------------------------------
  (with-eval-after-load 'dired
    (require 'dired-filter nil t)
    (require 'dired-subtree nil t)
    (add-hook 'dired-mode-hook #'dired-filter-mode)
    (setq dired-subtree-use-backgrounds t)
    ;; ---- Dired Local Keybindings ----
    (define-key dired-mode-map (kbd "TAB") #'dired-subtree-toggle)
    (define-key dired-mode-map (kbd "i")   #'dired-subtree-insert)
    (define-key dired-mode-map (kbd ";")   #'dired-subtree-remove)
    (define-key dired-mode-map (kbd "f")   #'dired-filter-mode)
    (define-key dired-mode-map (kbd "/")   #'dired-filter-map)
    (define-key dired-mode-map (kbd "z")   #'my/dired-view-file-other-window)
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c SPC m TAB" "toggle subtree"
        "C-c SPC m i"   "insert subtree"
        "C-c SPC m ;"   "remove subtree"
        "C-c SPC m f"   "filter mode"
        "C-c SPC m /"   "filter map"
        "C-c SPC m z"   "view in other window")))

  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "C-c SPC m c") #'vterm-send-C-c)
    (define-key vterm-mode-map (kbd "C-c SPC m r") #'vterm-send-return)
    (define-key vterm-mode-map (kbd "C-c SPC m k") #'vterm-reset-cursor-point)
    (define-key vterm-mode-map (kbd "C-c SPC m q") #'vterm-quit)
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c SPC m c" "send C-c"
        "C-c SPC m r" "send RET"
        "C-c SPC m q" "quit vterm")))

  (with-eval-after-load 'eww
    (define-key eww-mode-map (kbd "s") #'my/eww-search)
    (define-key eww-mode-map (kbd "o") #'eww-open-file)
    (define-key eww-mode-map (kbd "b") #'eww-list-bookmarks)
    (define-key eww-mode-map (kbd "r") #'eww-readable)
    (define-key eww-mode-map (kbd "u") #'my/eww-toggle-images)
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c SPC m s" "search"
        "C-c SPC m b" "bookmarks"
        "C-c SPC m u" "toggle images")))

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c SPC m t") #'org-todo)
    (define-key org-mode-map (kbd "C-c SPC m a") #'org-archive-subtree)
    (define-key org-mode-map (kbd "C-c SPC m s") #'org-schedule)
    (define-key org-mode-map (kbd "C-c SPC m d") #'org-deadline)
    (define-key org-mode-map (kbd "C-c SPC m p") #'org-priority)
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c SPC m t" "todo"
        "C-c SPC m s" "schedule"
        "C-c SPC m d" "deadline"
        "C-c SPC m p" "priority")))

  (with-eval-after-load 'magit
    (define-key magit-mode-map (kbd "C-c SPC m c") #'magit-commit)
    (define-key magit-mode-map (kbd "C-c SPC m p") #'magit-push-current)
    (define-key magit-mode-map (kbd "C-c SPC m f") #'magit-fetch)
    (define-key magit-mode-map (kbd "C-c SPC m l") #'magit-log-buffer-file)
    (define-key magit-mode-map (kbd "C-c SPC m s") #'magit-stage)
    (define-key magit-mode-map (kbd "C-c SPC m u") #'magit-unstage)
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c SPC m c" "commit"
        "C-c SPC m p" "push"
        "C-c SPC m f" "fetch"
        "C-c SPC m s" "stage"
        "C-c SPC m u" "unstage")))

  ;;;; Notes / Markdown knowledge (C-c n ...) ------------------------------------
  ;;
  ;; Markdown-based personal notes (Inkdrop-like workflow).
  ;;
  ;; Design:
  ;; - Kept outside the global leader (C-c SPC)
  ;; - Lightweight, prose-oriented notes (not tasks)
  ;; - notes/ is excluded from org-agenda-files (see orgx/org-core.el)
  ;; - Never override existing user/global bindings

  (with-eval-after-load 'utils/utils-notes-markdown
    ;; Define prefix only if it is free
    (unless (lookup-key global-map (kbd "C-c n"))
      (define-prefix-command 'my/notes-prefix)
      (global-set-key (kbd "C-c n") 'my/notes-prefix))

    ;; Sub bindings (guarded)
    (unless (lookup-key global-map (kbd "C-c n f"))
      (define-key my/notes-prefix (kbd "f") #'consult-notes))
    (unless (lookup-key global-map (kbd "C-c n r"))
      (define-key my/notes-prefix (kbd "r") #'my/notes-consult-ripgrep))
    (unless (lookup-key global-map (kbd "C-c n n"))
      (define-key my/notes-prefix (kbd "n") #'my/notes-new-note))
    (unless (lookup-key global-map (kbd "C-c n d"))
      (define-key my/notes-prefix (kbd "d") #'my/notes-open-root)))

  ;; which-key label (optional, non-fatal)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c n" "notes / markdown"))

  ;;;; Global keybindings (outside leader) ---------------------------------------
  ;; Define global keys (macOS-like shortcuts, function keys, etc.)
  (global-set-key (kbd "<f1>") #'help-command)
  (global-set-key (kbd "<f5>") #'my/revert-buffer-quick)   ;; quick revert buffer (if defined elsewhere)
  (global-set-key (kbd "<f8>") #'treemacs)
  (global-set-key (kbd "C-.") #'other-window)
  (global-set-key (kbd "C-/") #'undo-fu-only-undo)
  (global-set-key (kbd "C-=") #'er/expand-region)
  (global-set-key (kbd "C-?") #'undo-fu-only-redo)
  (global-set-key (kbd "C-c 0") #'delete-window)
  (global-set-key (kbd "C-c 1") #'delete-other-windows)
  (global-set-key (kbd "C-c 2") #'my/toggle-window-split)
  (global-set-key (kbd "C-c ;") #'comment-or-uncomment-region)
  (global-set-key (kbd "C-c M-a") #'align-regexp)
  (global-set-key (kbd "C-c V") #'view-file-other-window)
  (global-set-key (kbd "C-c a a") #'aidermacs-transient-menu)   ;; global AI menu (duplicate of LEADER a a)
  (global-set-key (kbd "C-c b") #'consult-buffer)
  ;; "C-c d ..." org/roam bindings (these may be redundant with leader o):
  (global-set-key (kbd "C-c d a") #'org-agenda)
  (global-set-key (kbd "C-c d c") #'org-capture)
  (global-set-key (kbd "C-c d f") #'org-roam-node-find)
  (global-set-key (kbd "C-c d i") #'org-roam-node-insert)
  (global-set-key (kbd "C-c d s") #'my/org-sidebar)
  (global-set-key (kbd "C-c d t") #'my/org-sidebar-toggle)
  (global-set-key (kbd "C-c k") #'my/kill-buffer-smart)
  (global-set-key (kbd "C-c l") #'display-line-numbers-mode)    ;; quick toggle line numbers
  (global-set-key (kbd "C-c o") #'find-file)
  (global-set-key (kbd "C-c r") #'consult-ripgrep)
  (global-set-key (kbd "C-c v") #'find-file-read-only)
  ;; Web/EWW global prefix keys:
  (global-set-key (kbd "C-c w b") #'eww-list-bookmarks)
  (global-set-key (kbd "C-c w o") #'eww-open-file)
  (global-set-key (kbd "C-c w r") #'eww-readable)
  (global-set-key (kbd "C-c w s") #'my/eww-search)
  (global-set-key (kbd "C-c w u") #'my/eww-toggle-images)
  (global-set-key (kbd "C-c w w") #'eww)
  (global-set-key (kbd "C-c z") #'core-hydra-text-scale/body)
  ;; macOS-like Super (s-) keys:
  (global-set-key (kbd "C-h") #'backward-delete-char)   ;; Make C-h backspace (like in terminals/mac)
  (global-set-key (kbd "C-s") #'consult-line)           ;; Search in buffer (override isearch)
  (global-set-key (kbd "s-.") #'ace-window)
  (global-set-key (kbd "s-<down>") #'end-of-buffer)
  (global-set-key (kbd "s-<left>") #'previous-buffer)
  (global-set-key (kbd "s-<right>") #'next-buffer)
  (global-set-key (kbd "s-<up>") #'beginning-of-buffer)
  (global-set-key (kbd "s-b") #'consult-buffer)
  (global-set-key (kbd "s-j") #'find-file-other-window)
  (global-set-key (kbd "s-m") #'my/new-frame-with-scratch)
  (global-set-key (kbd "s-o") #'find-file-other-frame)
  (global-set-key (kbd "s-r") #'my/restart-or-exit)
  (global-set-key (kbd "s-w") #'ace-swap-window)
  (global-set-key (kbd "M-x") #'execute-extended-command)

  ;;;; Auth / secrets ------------------------------------------------------------
  (defvar my:d:password-store
    (or (getenv "PASSWORD_STORE_DIR")
        (concat no-littering-var-directory "password-store/"))
    "Path to the password store.")

  (defun my/auth-check-env ()
    "Validate authentication environment and warn if misconfigured."
    (unless (getenv "GPG_KEY_ID")
      (display-warning 'auth "GPG_KEY_ID is not set." :level 'debug))
    (unless (file-directory-p my:d:password-store)
      (display-warning 'auth
                       (format "Password store directory does not exist: %s"
                               my:d:password-store)
                       :level 'warning)))

  (leaf *authentication
    :straight nil
    :init
    (my/auth-check-env)

    (leaf epa-file
      :straight nil
      :commands (epa-file-enable)
      :init
      (setq epa-pinentry-mode
            (if (getenv "USE_GPG_LOOPBACK") 'loopback 'default))
      (add-hook 'emacs-startup-hook #'epa-file-enable))

    (leaf auth-source
      :straight nil
      :init
      (with-eval-after-load 'auth-source
        (let ((key (getenv "GPG_KEY_ID")))
          (if key
              (setq auth-source-gpg-encrypt-to key)
            (display-warning 'auth-source
                             "GPG_KEY_ID is not set. Authentication backends may be limited.")))))

    (leaf password-store :straight t)

    (leaf auth-source-pass
      :straight t
      :commands (auth-source-pass-enable)
      :hook (emacs-startup-hook . (lambda ()
                                    (when (executable-find "pass")
                                      (auth-source-pass-enable)))))

    (leaf plstore
      :straight nil
      :init
      (with-eval-after-load 'plstore
        (setq plstore-secret-keys 'silent
              plstore-encrypt-to (getenv "GPG_KEY_ID")))))

  (provide 'core/general)
  ;;; core/general.el ends here
#+end_src

*** core/tools.el
:PROPERTIES:
:CUSTOM_ID: core-tools
:header-args:emacs-lisp: :tangle lisp/core/tools.el
:END:

**** Purpose
Provide developer-oriented helper commands and low-level tooling that do
not fit into other core domains.

**** What it does
- Window and buffer inspection helpers
- Keybinding conflict detection
- Dired and Org navigation helpers
- Tree-sitter grammar management
- Misc developer utilities

**** Notes
- Commands here are mostly interactive
- No persistent state is stored
- Safe to load unconditionally

**** Implementation

#+begin_src emacs-lisp
  ;;; core/tools.el --- Developer & UI helper tools -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Code:

  ;; --- Declarations for native-comp / byte-compile ---
  (declare-function dired-get-file-for-visit "dired")
  (declare-function dired-goto-subdir "dired")

  (declare-function org-back-to-heading "org")
  (declare-function org-show-subtree "org")

  (declare-function org-fold-subtree "org-fold")
  (declare-function org-fold-folded-p "org-fold")

  (declare-function my/org-fold-subtree "core/tools")
  (declare-function my/org-unfold-subtree "core/tools")
  (declare-function my/org-toggle-fold "core/tools")

  (defun my/toggle-linum-lines () (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode 0)
      (display-line-numbers-mode 1)))

  (defun my/toggle-window-split ()
    "Toggle between horizontal and vertical split for two windows."
    (interactive)
    (when (= (count-windows) 2)
      (let* ((this-buf (window-buffer))
             (next-buf (window-buffer (next-window)))
             (this-edges (window-edges))
             (next-edges (window-edges (next-window)))
             (split-vert (= (car this-edges) (car next-edges)))
             (split-fn (if split-vert #'split-window-horizontally #'split-window-vertically)))
        (delete-other-windows)
        (funcall split-fn)
        (set-window-buffer (selected-window) this-buf)
        (set-window-buffer (next-window) next-buf)
        (select-window (selected-window)))))

  (defun my/find-keybinding-conflicts ()
    "Find and display conflicting keybindings in active keymaps."
    (interactive)
    (let ((conflicts (make-hash-table :test 'equal))
          (maps (current-active-maps t))
          (buffer-name "*Keybinding Conflicts*"))
      (dolist (map maps)
        (map-keymap
         (lambda (key cmd)
           (when (commandp cmd)
             (let ((desc (key-description (vector key)))
                   (existing (gethash desc conflicts)))
               (puthash desc (delete-dups (cons cmd existing)) conflicts))))
         map))
      (with-current-buffer (get-buffer-create buffer-name)
        (read-only-mode -1)
        (erase-buffer)
        (insert "* Keybinding Conflicts *\n\n")
        (maphash (lambda (key cmds)
                   (when (> (length cmds) 1)
                     (insert (format "%s => %s\n" key (mapconcat #'symbol-name cmds ", ")))))
                 conflicts)
        (read-only-mode 1))
      (pop-to-buffer buffer-name)))

  (defun my/dired-view-file-other-window ()
    "Open selected Dired file or directory in another window."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (or (and (cdr dired-subdir-alist) (dired-goto-subdir file)) (dired file))
        (view-file-other-window file))))

  ;; --------- Tree-sitter helpers -------------------------------------------------
  ;;;; Tree-sitter (modern API; grammars under my:d:var/tree-sitter) --------------
  ;; We store/look up compiled grammars (.so/.dylib) under my:d:var/tree-sitter.

  (defvar my:d:var
    (or (bound-and-true-p my:d:var)
        ;; Prefer no-littering var dir if available
        (and (boundp 'no-littering-var-directory) no-littering-var-directory)
        ;; Fallback: ~/.emacs.d/.var/
        (expand-file-name ".var/" user-emacs-directory))
    "Base directory for variable (writable) runtime data.")

  (defun my/ensure-directory-exists (dir)
    "Create DIR if it does not exist."
    (unless (file-directory-p dir)
      (make-directory dir t)))

  (defconst my:d:treesit
    (expand-file-name "tree-sitter/" my:d:var)
    "Directory to store and load Tree-sitter grammars.")

  ;; Ensure directory exists and register it as an extra search path.
  (my/ensure-directory-exists my:d:treesit)
  (add-to-list 'treesit-extra-load-path my:d:treesit)

  ;; Declare language sources. Keys are language symbols used by treesit.
  (setq treesit-language-source-alist
        '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (c          "https://github.com/tree-sitter/tree-sitter-c")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
          (css        "https://github.com/tree-sitter/tree-sitter-css")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust       "https://github.com/tree-sitter/tree-sitter-rust")
          (toml       "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun my/treesit--language-installed-p (lang)
    "Return non-nil if LANG grammar library can be loaded."
    (and (fboundp 'treesit-language-available-p)
         (ignore-errors (treesit-language-available-p lang))))

  (defun my/treesit-install-languages (&optional langs)
    "Install LANGS via `treesit-install-language-grammar` (0–1 arg API).
  If LANGS is nil, install everything from `treesit-language-source-alist`."
    (unless (fboundp 'treesit-install-language-grammar)
      (user-error "This Emacs is not built with tree-sitter support"))
    (let ((targets (or langs (mapcar #'car treesit-language-source-alist))))
      (dolist (lang targets)
        (unless (my/treesit--language-installed-p lang)
  	(message "[treesit] Installing %s …" lang)
          (treesit-install-language-grammar lang)))))

  (defun my/treesit-ensure (&optional langs)
    "Ensure LANGS grammars are available; install missing ones."
    (interactive)
    (my/treesit-install-languages langs)
    (message "[treesit] All requested grammars are available."))

  ;; Legacy shim preserved for compatibility with old sites passing 7 args, etc.
  (defun my/treesit--call-with-outdir (&rest maybe-legacy-args)
    "Compatibility wrapper. Accept LANG or list of LANG symbols, ignore extras."
    (let ((head (car maybe-legacy-args)))
      (cond
       ((symbolp head) (my/treesit-ensure (list head)))
       ((and (listp head) (cl-every #'symbolp head)) (my/treesit-ensure head))
       (t (user-error "Unsupported call form: %S" maybe-legacy-args)))))
  ;; --------- Misc dev / UI helpers ----------------------------------------------

  (defun my/open-by-vscode () (interactive)
    (when (buffer-file-name)
      (async-shell-command
       (format "code -r -g %s:%d:%d" (buffer-file-name) (line-number-at-pos) (current-column)))))

  (defun my/show-env-variable (var) (interactive "sEnvironment variable: ")
    (message "%s = %s" var (or (getenv var) "Not set")))

  (defun my/print-build-info () (interactive)
    (let ((buf (get-buffer-create "*Build Info*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "- GNU Emacs *%s*\n\n" emacs-version))
          (insert "|Property|Value|\n|--------|-----|\n")
          (insert (format "|Commit|%s|\n" (if (fboundp 'emacs-repository-get-version)
                                              (emacs-repository-get-version) "N/A")))
          (insert (format "|Branch|%s|\n" (if (fboundp 'emacs-repository-get-branch)
                                              (emacs-repository-get-branch) "N/A")))
          (insert (format "|System|%s|\n" system-configuration))
          (insert (format "|Date|%s|\n" (format-time-string "%Y-%m-%d %T (%Z)" emacs-build-time)))
          (insert (format "|Patch|%s ns-inline.patch|\n" (if (boundp 'mac-ime--cursor-type) "with" "N/A")))
          (insert (format "|Features|%s|\n" system-configuration-features))
          (insert (format "|Options|%s|\n" system-configuration-options)))
        (view-mode 1))
      (switch-to-buffer buf)))

  (with-eval-after-load 'org
    (require 'org-fold)
    (defun my/org-fold-subtree ()   (interactive) (org-fold-subtree t))
    (defun my/org-unfold-subtree () (interactive) (org-show-subtree))
    (defun my/org-toggle-fold () (interactive)
      (save-excursion
        (org-back-to-heading t)
        (if (org-fold-folded-p (point)) (org-show-subtree) (org-fold-subtree t))))
    (define-key org-mode-map (kbd "C-c f") #'my/org-fold-subtree)
    (define-key org-mode-map (kbd "C-c e") #'my/org-unfold-subtree)
    (define-key org-mode-map (kbd "C-c t") #'my/org-toggle-fold))

  (provide 'core/tools)
  ;;; core/tools.el ends here
#+end_src

*** core/utils.el
:PROPERTIES:
:CUSTOM_ID: core-utils
:header-args:emacs-lisp: :tangle lisp/core/utils.el
:END:

**** Purpose
Provide small, reusable utility helpers shared across core modules.

This module contains **infrastructure-level glue** that does not belong
to any single feature domain.

**** What it does
- Ensures directory creation helpers exist
- Adds save-time timestamp updates
- Implements automatic Org Babel tangling on save
- Provides lightweight buffer helpers
- Adds safety helpers for read-only buffers

**** Notes
- Functions here must be generic and dependency-free
- No UI, no mode-specific behavior
- Hooks are kept simple and local in effect

**** Implementation

#+begin_src emacs-lisp
  ;;; core/utils.el --- Core utility helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Code:

  (unless (fboundp 'my/ensure-directory-exists)
    (defun my/ensure-directory-exists (dir)
      "Create DIR if it does not exist."
      (unless (file-directory-p dir)
        (make-directory dir t))))

  ;; Insert timestamp on save
  (defun my/save-buffer-wrapper ()
    "Insert or update a `$Lastupdate` timestamp at the top of the buffer."
    (interactive)
    (let ((timestamp (concat "$Lastupdate: " (format-time-string "%Y/%m/%d %H:%M:%S") " $")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\$Lastupdate: [0-9/: ]*\\$" nil t)
          (replace-match timestamp t nil)))))
  (add-hook 'before-save-hook #'my/save-buffer-wrapper)

  (defun my/auto-tangle-updated-src-blocks ()
    "Automatically tangle updated Org source blocks when saving `README.org`."
    (when (and buffer-file-name (string= (file-name-nondirectory buffer-file-name) "README.org"))
      (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        #'my/auto-tangle-updated-src-blocks
                        nil 'make-it-local)))

  (defun my/revert-buffer-quick ()
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun my/auto-insert-lexical-binding ()
    "Insert `lexical-binding: t` in Emacs Lisp files under `no-littering-var-directory`."
    (when (and (stringp buffer-file-name)
               (boundp 'no-littering-var-directory)
               (string-prefix-p (expand-file-name no-littering-var-directory)
                                (expand-file-name buffer-file-name))
               (string-match-p "\\.el\\'" buffer-file-name)
               (not (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "lexical-binding" (line-end-position 5) t))))
      (save-excursion
        (goto-char (point-min))
        (insert ";; -*- lexical-binding: t; -*- \n"))))
  (add-hook 'find-file-hook #'my/auto-insert-lexical-binding)

  (defun my/enable-view-mode-on-read-only ()
    (if buffer-read-only
        (view-mode 1)
      (view-mode -1)))
  (add-hook 'read-only-mode-hook #'my/enable-view-mode-on-read-only)

  (provide 'core/utils)
  ;;; core/utils.el ends here
#+end_src

*** core/core-treesit.el
:PROPERTIES:
:CUSTOM_ID: core-utils
:header-args:emacs-lisp: :tangle lisp/core/core-treesit.el
:END:

**** Purpose
Provide **optional Tree-sitter (treesit) integration** as a core capability
without changing default major-mode behavior.

**** What it does
- Detects Tree-sitter availability safely
- Optionally remaps legacy major-modes to `*-ts-mode`
- Optionally installs missing grammars
- Does nothing unless explicitly enabled

**** Notes
- Disabled by default
- Safe on non-treesit builds
- No dependency on UI, completion, LSP, or Org

**** Implementation

#+begin_src emacs-lisp
  ;;; core/core-treesit.el --- Optional Tree-sitter integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Commentary:
  ;; Optional Tree-sitter (treesit) integration for Emacs 30+ / 31+.
  ;;
  ;; This module is intentionally conservative:
  ;; - Disabled by default
  ;; - Never forces *-ts-mode
  ;; - Safe on non-treesit builds
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf))

  ;; ---------------------------------------------------------------------------
  ;; Customization
  ;; ---------------------------------------------------------------------------

  (defgroup core-treesit nil
    "Optional Tree-sitter integration."
    :group 'core)

  (defcustom core-treesit-enable-p nil
    "Enable Tree-sitter integration."
    :type 'boolean
    :group 'core-treesit)

  (defcustom core-treesit-auto-install-p nil
    "Automatically install missing Tree-sitter grammars."
    :type 'boolean
    :group 'core-treesit)

  ;; ---------------------------------------------------------------------------
  ;; Capability detection
  ;; ---------------------------------------------------------------------------

  (defun core-treesit-available-p ()
    "Return non-nil if Tree-sitter is available in this Emacs."
    (and (fboundp 'treesit-available-p)
         (treesit-available-p)))

  ;; ---------------------------------------------------------------------------
  ;; Mode mapping (explicit & conservative)
  ;; ---------------------------------------------------------------------------

  (defconst core-treesit--mode-alist
    '((python-mode . python-ts-mode)
      (js-mode     . js-ts-mode)
      (js2-mode    . js-ts-mode)
      (typescript-mode . typescript-ts-mode)
      (css-mode    . css-ts-mode)
      (json-mode   . json-ts-mode)
      (yaml-mode   . yaml-ts-mode))
    "Mapping from legacy modes to Tree-sitter modes.")

  (defun core-treesit--install-grammar (lang)
    "Install Tree-sitter grammar for LANG if missing."
    (when (and core-treesit-auto-install-p
               (fboundp 'treesit-install-language-grammar))
      (ignore-errors
        (treesit-install-language-grammar lang))))

  (defun core-treesit--setup-remap ()
    "Populate `major-mode-remap-alist` conservatively."
    (dolist (entry core-treesit--mode-alist)
      (let* ((legacy (car entry))
             (ts     (cdr entry))
             (lang   (intern (string-remove-suffix "-mode"
                                                   (symbol-name legacy)))))
        (when (and (fboundp ts)
                   (fboundp legacy))
          (add-to-list 'major-mode-remap-alist
                       (cons legacy ts))
          (core-treesit--install-grammar lang)))))

  ;; ---------------------------------------------------------------------------
  ;; Public entry point
  ;; ---------------------------------------------------------------------------

  (defun core-treesit-setup ()
    "Enable Tree-sitter integration if configured and available."
    (when (and core-treesit-enable-p
               (core-treesit-available-p))
      (core-treesit--setup-remap)))

  ;; Run after init so user policy is known
  (add-hook 'after-init-hook #'core-treesit-setup)

  (provide 'core/core-treesit)
  ;;; core/core-treesit.el ends here
#+end_src
