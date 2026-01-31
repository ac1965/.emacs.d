README.org part 2/3
--------------------

*** core/core-treesit.el
:PROPERTIES:
:CUSTOM_ID: core-treesit
:header-args:emacs-lisp: :tangle lisp/core/core-treesit.el
:END:

**** Purpose
Provide a **centralized and deterministic Tree-sitter infrastructure layer**
for Emacs 29–31+, with strict separation between:

- availability detection
- major-mode remapping
- grammar source definition
- grammar installation (explicit only)

This module guarantees that Tree-sitter grammars are **never installed
implicitly** and are always placed under the directory defined in
=early-init.el=.

**** What it does
- Detects Tree-sitter availability safely
- Applies explicit =major-mode-remap-alist= entries
- Defines and owns grammar source metadata
- Provides a **single explicit API** for grammar installation
- Forces grammar installation directory via dynamic rebinding

**** What it intentionally does NOT do
- Does NOT auto-install grammars at init time
- Does NOT rely on =treesit-auto-install=
- Does NOT install grammars during:
  - init
  - byte-compilation
  - native-compilation
  - major-mode activation

**** Design principles (important)
- Grammar installation is **explicit and user-driven**
- =treesit-install-language-grammar= is never called implicitly
- Installation directory is rebound with =let= at call time
- All Tree-sitter side effects are centralized in this module

**** Notes (Emacs 29–31 behavior)
- Emacs may attempt to auto-install grammars when entering =*-ts-mode=
- This module disables that path and replaces it with a controlled API
- Rebinding =treesit-install-dir= is required to avoid installation under
  =~/.emacs.d/tree-sitter/=

**** Implementation

#+begin_src emacs-lisp
  ;;; core/core-treesit.el --- Tree-sitter infrastructure layer -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Commentary:
  ;; Centralized Tree-sitter integration.
  ;;
  ;; Policy:
  ;; - early-init.el defines directory policy (my:d:treesit) and treesit-extra-load-path.
  ;; - This module owns:
  ;;   - availability checks
  ;;   - major-mode remapping
  ;;   - grammar source registration
  ;;   - explicit install commands (interactive)
  ;;   - optional auto-install at startup
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  ;; ---------------------------------------------------------------------------
  ;; Customization
  ;; ---------------------------------------------------------------------------

  (defgroup core-treesit nil
    "Tree-sitter infrastructure layer."
    :group 'convenience)

  (defcustom core-treesit-enable-p t
    "Enable Tree-sitter based major modes when available."
    :type 'boolean
    :group 'core-treesit)

  (defcustom core-treesit-auto-install-p nil
    "Automatically install missing Tree-sitter grammars on startup."
    :type 'boolean
    :group 'core-treesit)

  (defcustom core-treesit-install-languages
    '(python javascript typescript tsx json css yaml bash toml)
    "Languages considered by `core-treesit-install-all` and auto-install."
    :type '(repeat symbol)
    :group 'core-treesit)

  ;; ---------------------------------------------------------------------------
  ;; Availability
  ;; ---------------------------------------------------------------------------

  (defun core-treesit--available-p ()
    "Return non-nil if Tree-sitter is available and enabled."
    (and core-treesit-enable-p
         (fboundp 'treesit-available-p)
         (ignore-errors (treesit-available-p))))

  (defun core-treesit--language-available-p (lang)
    "Return non-nil if grammar for LANG is available."
    (and (fboundp 'treesit-language-available-p)
         (ignore-errors (treesit-language-available-p lang))))

  ;; ---------------------------------------------------------------------------
  ;; Mode remapping (single source of truth)
  ;; ---------------------------------------------------------------------------

  (defvar core-treesit--mode-remap-alist
    '((python-mode      . python-ts-mode)
      (js-mode          . js-ts-mode)
      (js-json-mode     . json-ts-mode)
      (json-mode        . json-ts-mode)
      (css-mode         . css-ts-mode)
      (typescript-mode  . typescript-ts-mode)
      (tsx-mode         . tsx-ts-mode)
      (yaml-mode        . yaml-ts-mode)
      (sh-mode          . bash-ts-mode)
      (toml-mode        . toml-ts-mode))
    "Explicit major-mode remapping for Tree-sitter.")

  (defun core-treesit-apply-remap ()
    "Apply Tree-sitter major-mode remapping when reopening buffers."
    (interactive)
    (when (core-treesit--available-p)
      (dolist (pair core-treesit--mode-remap-alist)
        (add-to-list 'major-mode-remap-alist pair))))

  ;; ---------------------------------------------------------------------------
  ;; Grammar sources (single source of truth)
  ;; ---------------------------------------------------------------------------

  (defvar core-treesit--language-sources
    '((python     . ("https://github.com/tree-sitter/tree-sitter-python"))
      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
      (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
      (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
      (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
      (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
      (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
      (toml       . ("https://github.com/tree-sitter/tree-sitter-toml")))
    "Alist of (LANG . (REPO [REV] [DIR])) for `treesit-language-source-alist`.")

  (defun core-treesit-register-sources ()
    "Register grammar sources into `treesit-language-source-alist`."
    (interactive)
    (when (boundp 'treesit-language-source-alist)
      (dolist (it core-treesit--language-sources)
        (let* ((lang (car it))
               (spec (cdr it))
               (entry (cons lang spec)))
          (add-to-list 'treesit-language-source-alist entry)))))

  ;; ---------------------------------------------------------------------------
  ;; Installation (explicit commands)
  ;; ---------------------------------------------------------------------------

  (defun core-treesit-install-grammar (lang)
    "Install Tree-sitter grammar for LANG using registered sources."
    (interactive
     (list
      (intern
       (completing-read
        "Install grammar: "
        (mapcar (lambda (x) (symbol-name (car x))) core-treesit--language-sources)
        nil t))))
    (unless (core-treesit--available-p)
      (user-error "Tree-sitter is not available or disabled"))
    (unless (fboundp 'treesit-install-language-grammar)
      (user-error "treesit-install-language-grammar is not available in this Emacs"))
    (core-treesit-register-sources)
    (if (core-treesit--language-available-p lang)
        (message "[treesit] already available: %s" lang)
      (message "[treesit] installing: %s" lang)
      (treesit-install-language-grammar lang)
      (if (core-treesit--language-available-p lang)
          (message "[treesit] installed: %s" lang)
        (message "[treesit] install finished but not detected: %s (check treesit-extra-load-path)" lang))))

  (defun core-treesit-install-all (&optional force)
    "Install all grammars in `core-treesit-install-languages`.
  If FORCE is non-nil, attempt installation even if Emacs reports available."
    (interactive "P")
    (unless (core-treesit--available-p)
      (user-error "Tree-sitter is not available or disabled"))
    (core-treesit-register-sources)
    (dolist (lang core-treesit-install-languages)
      (when (or force (not (core-treesit--language-available-p lang)))
        (ignore-errors
          (core-treesit-install-grammar lang)))))

  (defun core-treesit-report ()
    "Report Tree-sitter availability and language status."
    (interactive)
    (messagen
     "[treesit] available=%s, extra-load-path=%S"
     (if (core-treesit--available-p) "yes" "no")
     (when (boundp 'treesit-extra-load-path) treesit-extra-load-path))
    (when (core-treesit--available-p)
      (dolist (lang core-treesit-install-languages)
        (message "[treesit] %s: %s"
                 lang
                 (if (core-treesit--language-available-p lang) "ok" "missing")))))

  ;; ---------------------------------------------------------------------------
  ;; Startup behavior
  ;; ---------------------------------------------------------------------------

  (when (core-treesit--available-p)
    (core-treesit-apply-remap)
    (when core-treesit-auto-install-p
      (ignore-errors (core-treesit-install-all))))

  (provide 'core-treesit)
  ;;; core/core-treesit.el ends here
#+end_src

*** core/core-history.el
:PROPERTIES:
:CUSTOM_ID: core-history
:header-args:emacs-lisp: :tangle lisp/core/core-history.el
:END:

**** Purpose
Provide persistent session history and autorevert behavior.

This module ensures that buffer positions, recent files, and minibuffer
history survive Emacs restarts.

**** What it does
- Enables save-place for cursor restoration
- Configures recentf for file history
- Enables savehist for minibuffer persistence
- Stores all state under no-littering directories

**** Notes
- No UI elements are defined here
- Safe to enable globally
- All persistence is file-based and explicit

**** Implementation

#+begin_src emacs-lisp
  ;;; core/history.el --- Session persistence & autorevert -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf saveplace :straight nil
    :init
    (setq save-place-file (concat no-littering-var-directory "saveplace"))
    (save-place-mode +1))

  (leaf recentf :straight nil
    :init
    (setq recentf-max-saved-items 100
          recentf-save-file (concat no-littering-var-directory "recentf"))
    (recentf-mode +1))

  (leaf savehist
    :straight nil
    :global-minor-mode savehist-mode
    :config
    (setq savehist-file (concat no-littering-var-directory "history"))
    (my/ensure-directory-exists (file-name-directory savehist-file))
    (add-to-list 'savehist-additional-variables 'my:desktop-ask-on-restore))

  (provide 'core-history)
  ;;; core/history.el ends here
#+end_src

*** core/editing.el
:PROPERTIES:
:CUSTOM_ID: core-editing
:header-args:emacs-lisp: :tangle lisp/core/editing.el
:END:

**** Purpose
Provide editing helpers and UX improvements that apply broadly across
buffers without imposing workflow constraints.

**** What it does
- Configures TRAMP and autosave behavior
- Enables structural editing (paredit, puni)
- Adds undo, window, and navigation helpers
- Centralizes Dired and autorevert behavior

**** Notes
- No mode-specific keymaps are defined here
- Features are globally safe and reversible
- Heavy logic is avoided

**** Implementation

#+begin_src emacs-lisp
  ;;; core/editing.el --- Editing helpers & UX aids -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Code:

  (leaf tramp
    :straight nil
    :pre-setq
    `((tramp-persistency-file-name . ,(concat no-littering-var-directory "tramp"))
      (tramp-auto-save-directory   . ,(concat no-littering-var-directory "tramp-autosave")))
    :custom
    '((tramp-default-method . "scp")
      (tramp-verbose        . 3)))

  (setopt auto-save-visited-interval 1
          auto-save-default        nil)
  (when (fboundp 'auto-save-visited-mode)
    (auto-save-visited-mode 1))

  (leaf paredit :straight t
    :hook (emacs-lisp-mode . (lambda ()
                               (enable-paredit-mode)
                               (electric-pair-local-mode -1))))

  (leaf paren :straight nil
    :custom ((show-paren-delay . 0)
             (show-paren-style . 'expression)
             (show-paren-highlight-openparen . t))
    :global-minor-mode show-paren-mode)

  (leaf puni :straight t
    :global-minor-mode puni-global-mode
    :hook ((minibuffer-setup . (lambda () (puni-global-mode -1)))))

  (leaf undo-fu :straight t
    :custom ((undo-fu-allow-undo-in-region . t)))

  (leaf vundo :straight t)

  (leaf ace-window :straight t
    :custom ((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
             (aw-scope . 'frame)
             (aw-background . t))
    :config (ace-window-display-mode 1))

  (leaf visual-line-mode :straight nil
    :hook (text-mode . visual-line-mode))

  (leaf dired-filter :straight t)
  (leaf dired-subtree :straight t :after dired)

  (leaf dired :straight nil
    :config
    (if (and (eq system-type 'darwin) (executable-find "gls"))
        (progn
          (setq insert-directory-program "gls"
                dired-use-ls-dired t
                dired-listing-switches "-aBhl --group-directories-first"))
      (setq dired-use-ls-dired nil
            dired-listing-switches "-alh")))

  (leaf expand-region :straight t :after treesit)
  (leaf aggressive-indent :straight t :hook (prog-mode . aggressive-indent-mode))
  (leaf delsel :straight nil :global-minor-mode delete-selection-mode)

  (leaf autorevert :straight nil
    :custom ((auto-revert-interval . 2)
             (auto-revert-verbose . nil))
    :global-minor-mode global-auto-revert-mode)

  (leaf transient
    :straight t
    :config
    (setq transient-history-file (concat no-littering-var-directory "transient/history.el")
          transient-levels-file  (concat no-littering-var-directory "transient/levels.el")
          transient-values-file  (concat no-littering-var-directory "transient/values.el"))
    (my/ensure-directory-exists (concat no-littering-var-directory "transient/")))

  (provide 'editing)
  ;;; core/editing.el ends here
#+end_src

*** core/switches.el
:PROPERTIES:
:CUSTOM_ID: core-switches
:header-args:emacs-lisp: :tangle lisp/core/switches.el
:END:

**** Purpose
Provide a unified switchboard for selecting UI bundles and LSP backends.

**** What it does
- Selects between Doom / Nano / none UI bundles
- Selects between Eglot and lsp-mode
- Performs presence checks before enabling
- Loads modules lazily and safely

**** Notes
- This module owns *policy*, not implementation
- Actual UI / LSP setup lives in other modules
- Safe to reload

**** Implementation

#+begin_src emacs-lisp
  ;;; core/switches.el --- Unified feature switches (UI/LSP) -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;;; Commentary:
  ;; Switcher for UI bundles (doom/nano) and LSP backends (eglot/lsp).
  ;;
  ;;; Code:

  (eval-when-compile (require 'subr-x))

  (when (boundp 'my:use:modules)
    (when (or (not (boundp 'my:use-ui)) (eq my:use-ui 'none))
      (setq my:use-ui my:use:modules)))
  (define-obsolete-variable-alias 'my:use:modules 'my:use-ui "2025-10-11")

  (defgroup my:switches nil "Unified switches for UI and LSP." :group 'convenience)

  (defcustom my:use-lsp 'eglot
    "Which LSP client to use. One of: `eglot`, `lsp`."
    :type '(choice (const eglot) (const lsp))
    :group 'my:switches)

  (defcustom my:use-ui 'none
    "Which UI bundle to use. One of: `none`, `doom`, `nano`."
    :type '(choice (const none) (const doom) (const nano))
    :group 'my:switches)

  (autoload 'my/ui-enable-doom  "ui/ui-doom-modeline" "Enable Doom UI bundle." t)
  (autoload 'my/ui-enable-nano  "ui/ui-nano-modeline" "Enable Nano UI bundle." t)
  (autoload 'my/lsp-enable-eglot   "dev/dev-lsp-eglot" "Enable Eglot LSP." t)
  (autoload 'my/lsp-enable-lspmode "dev/dev-lsp-mode"  "Enable lsp-mode LSP." t)

  (defun my/sw--present-p (kind choice)
    (pcase kind
      ('ui (pcase choice
             ('doom (or (fboundp 'my/ui-enable-doom)
                        (locate-library "ui/ui-doom-modeline")
                        (locate-library "doom-modeline")))
             ('nano (or (fboundp 'my/ui-enable-nano)
                        (locate-library "ui/ui-nano-modeline")
                        (locate-library "nano-modeline")))
             (_ t)))
      ('lsp (pcase choice
              ('eglot (or (fboundp 'my/lsp-enable-eglot)
                          (locate-library "dev/dev-lsp-eglot")
                          (locate-library "eglot")))
              ('lsp   (or (fboundp 'my/lsp-enable-lspmode)
                          (locate-library "dev/dev-lsp-mode")
                          (locate-library "lsp-mode")))
              (_ nil)))
      (_ nil)))

  (defun my/sw--enable-ui (choice)
    (pcase choice
      ('doom (cond
              ((fboundp 'my/ui-enable-doom) (my/ui-enable-doom) t)
              ((locate-library "ui/ui-doom-modeline")
               (load (locate-library "ui/ui-doom-modeline") nil 'nomessage)
               (when (fboundp 'my/ui-enable-doom) (my/ui-enable-doom) t))
              (t (message "[switches] Doom UI not found.") nil)))
      ('nano (cond
              ((fboundp 'my/ui-enable-nano) (my/ui-enable-nano) t)
              ((locate-library "ui/ui-nano-modeline")
               (load (locate-library "ui/ui-nano-modeline") nil 'nomessage)
               (when (fboundp 'my/ui-enable-nano) (my/ui-enable-nano) t))
              (t (message "[switches] Nano UI not found.") nil)))
      ('none (message "[switches] UI bundle disabled.") t)
      (_ (message "[switches] Unknown UI choice: %s" choice) nil)))

  (defun my/sw--enable-lsp (choice)
    (pcase choice
      ('eglot (cond
               ((fboundp 'my/lsp-enable-eglot) (my/lsp-enable-eglot) t)
               ((locate-library "dev/dev-lsp-eglot")
                (load (locate-library "dev/dev-lsp-eglot") nil 'nomessage)
                (when (fboundp 'my/lsp-enable-eglot) (my/lsp-enable-eglot) t))
               (t (message "[switches] Eglot setup not found.") nil)))
      ('lsp (cond
             ((fboundp 'my/lsp-enable-lspmode) (my/lsp-enable-lspmode) t)
             ((locate-library "dev/dev-lsp-mode")
              (load (locate-library "dev/dev-lsp-mode") nil 'nomessage)
              (when (fboundp 'my/lsp-enable-lspmode) (my/lsp-enable-lspmode) t))
             (t (message "[switches] lsp-mode setup not found.") nil)))
      (_ (message "[switches] Unknown LSP choice: %s" choice) nil)))

  (when (not (eq my:use-ui 'none))
    (let ((present (my/sw--present-p 'ui my:use-ui)))
      (cond
       ((my/sw--enable-ui my:use-ui) (message "[switches] UI bundle: %s" my:use-ui))
       (present (message "[switches] UI seems present but could not enable: %s" my:use-ui))
       (t (message "[switches] UI bundle unavailable: %s" my:use-ui)))))

  (let ((present (my/sw--present-p 'lsp my:use-lsp)))
    (cond
     ((my/sw--enable-lsp my:use-lsp) (message "[switches] LSP backend: %s" my:use-lsp))
     (present (message "[switches] LSP seems present but could not enable: %s" my:use-lsp))
     (t (message "[switches] LSP backend unavailable: %s" my:use-lsp))))

  (provide 'switches)
  ;;; core/switches.el ends here
#+end_src

*** core/custom.el
:PROPERTIES:
:CUSTOM_ID: custom-file
:header-args:emacs-lisp: :tangle lisp/core/custom.el
:END:

**** Purpose
Centralize Emacs Customize output into a dedicated, non-intrusive file and
provide helper commands to inspect and snapshot selected customization state.

This module isolates Customize side effects from the main configuration.

**** What it does
- Redirects `custom-file` to `.etc/custom.el`
- Ensures the custom file and directory exist
- Loads the custom file safely at startup
- Provides commands to:
  - Open the custom file
  - Dump a curated snapshot of current variables and faces

**** Notes
- Customize output is *not* evaluated inline in init files
- Snapshot dumping is manual and explicit
- This module owns **routing and helpers**, not UI policy

**** Implementation

#+begin_src emacs-lisp
  ;;; core/custom.el --- custom-file helpers -*- lexical-binding: t; -*-
  ;;
  ;; Category: core
  ;;
  ;;; Commentary:
  ;; - Route customize output to .etc/custom.el
  ;; - Provide helpers to open and (optionally) dump current values/faces.

  (eval-when-compile (require 'subr-x))

  (defconst my:f:custom
    (or (bound-and-true-p my:f:custom)
        (expand-file-name "custom.el"
                          (or (bound-and-true-p my:d:etc)
                              (expand-file-name ".etc" user-emacs-directory))))
    "Path to the custom-file (Customize output).")

  (defun my/custom--ensure-file ()
    "Ensure `custom-file` exists and has a small header."
    (let* ((dir (file-name-directory my:f:custom)))
      (unless (file-directory-p dir)
        (condition-case err
            (make-directory dir t)
          (error
           (warn "[custom] failed to create %s: %s"
                 dir (error-message-string err)))))
      (unless (file-exists-p my:f:custom)
        (with-temp-file my:f:custom
          (insert
           ";;; custom.el --- Customize output -*- lexical-binding: t; -*-\n"
           ";; This file is generated by Customize. Edit with care.\n\n")))))

  ;; Route Customize output
  (setq custom-file my:f:custom)
  (my/custom--ensure-file)

  (when (file-readable-p custom-file)
    (ignore-errors
      (load custom-file nil 'nomessage)))

  ;;;###autoload
  (defun my/custom-open ()
    "Open the `custom-file`."
    (interactive)
    (my/custom--ensure-file)
    (find-file my:f:custom))

  ;;;###autoload
  (defun my/custom-dump-current ()
    "Persist a curated snapshot of current settings/faces into `custom-file`.
  This does not run automatically."
    (interactive)
    (my/custom--ensure-file)

    ;; Variables to persist
    (dolist (pair
             `((inhibit-startup-screen        . ,inhibit-startup-screen)
               (frame-resize-pixelwise        . ,(bound-and-true-p frame-resize-pixelwise))
               (completion-styles             . ,(and (boundp 'completion-styles)
                                                     completion-styles))
               (completion-category-overrides . ,(and (boundp 'completion-category-overrides)
                                                     completion-category-overrides))
               (org-startup-indented           . ,(and (boundp 'org-startup-indented)
                                                     org-startup-indented))
               (org-hide-leading-stars         . ,(and (boundp 'org-hide-leading-stars)
                                                     org-hide-leading-stars))
               (org-tags-column                . ,(and (boundp 'org-tags-column)
                                                     org-tags-column))
               (org-agenda-tags-column         . ,(and (boundp 'org-agenda-tags-column)
                                                     org-agenda-tags-column))))
      (when (car (last pair))
        (customize-save-variable (car pair) (cdr pair))))

    ;; Faces to persist
    (let ((faces
           '((org-modern-date-active
              ((t (:background "#373844" :foreground "#f8f8f2"
                               :height 0.75 :weight light :width condensed))))
             (org-modern-time-active
              ((t (:background "#44475a" :foreground "#f8f8f2"
                               :height 0.75 :weight light :width condensed))))
             (org-modern-tag
              ((t (:background "#44475a" :foreground "#b0b8d1"
                               :height 0.75 :weight light :width condensed)))))))
      (dolist (f faces)
        (custom-set-faces `(,(car f) ,(cadr f)))))

    (custom-save-all)
    (message "[custom] Wrote snapshot to %s" my:f:custom))

  (provide 'custom)
  ;;; core/custom.el ends here
#+end_src

*** core/custom-ui-extras.el
:PROPERTIES:
:CUSTOM_ID: core-custom-ui-extras
:header-args:emacs-lisp: :tangle lisp/core/custom-ui-extras.el
:END:

**** Purpose
Allow users to enable optional UI-related modules **without modifying**
the default module list.

This file exists as a safe extension point.

**** What it does
- Appends UI-related modules to `my:modules-extra`
- Avoids duplication
- Leaves default module ordering untouched

**** Notes
- Loaded only when explicitly added
- Intended for personal / experimental UI layers
- Zero side effects outside module selection

**** Implementation

#+begin_src emacs-lisp
  ;;; core/custom-ui-extras.el --- user extras -*- lexical-binding: t; -*-

  ;; Append without touching your default module list.
  (setq my:modules-extra
        (delete-dups
         (append my:modules-extra
                 '(ui-visual-aids
                   org-typography
  		 ui-macos))))

  (provide 'custom-ui-extras)
  ;;; core/custom-ui-extras.el ends here
#+end_src

*** ui/ui-font.el
:PROPERTIES:
:CUSTOM_ID: ui-font
:header-args:emacs-lisp: :tangle lisp/ui/ui-font.el
:END:

**** Purpose
Define a stable and portable font configuration for graphical Emacs,
including default, variable-pitch, and emoji fonts, with optional ligature support.

**** What it does
- Selects reasonable default fonts depending on the operating system.
- Applies font settings only in graphical frames.
- Supports daemon and non-daemon startup correctly.
- Provides an interactive command to report the current font.
- Optionally enables programming ligatures via ligature.el.

**** Notes
- All user-customizable options are exposed via `defcustom`.
- This module does not force fonts if Emacs is running in terminal mode.
- Ligatures are enabled only in `prog-mode` buffers.

**** Implementation

#+begin_src emacs-lisp
  ;;; ui-font.el --- Font configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;;
  ;;; Commentary:
  ;; Font configuration for UI (ui category).
  ;; This module sets default, variable-pitch, and emoji fonts
  ;; with OS-aware fallbacks and optional ligature support.
  ;;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf))

  ;;;; Custom variables

  (defgroup ui-font nil
    "Font configuration for UI."
    :group 'ui)

  (defcustom ui-font-default nil
    "Default monospace font family.
  If nil, an OS-dependent fallback is used."
    :type '(choice (const :tag "Auto" nil)
                   (string :tag "Font family"))
    :group 'ui-font)

  (defcustom ui-font-variable-pitch nil
    "Variable-pitch font family.
  If nil, the default monospace font is reused."
    :type '(choice (const :tag "Auto" nil)
                   (string :tag "Font family"))
    :group 'ui-font)

  (defcustom ui-font-emoji nil
    "Emoji font family.
  If nil, an OS-dependent fallback is used."
    :type '(choice (const :tag "Auto" nil)
                   (string :tag "Font family"))
    :group 'ui-font)

  (defcustom ui-font-size 18
    "Default font size in points."
    :type 'integer
    :group 'ui-font)

  ;;;; Internal helpers

  (defun ui-font--system-default ()
    "Return a default monospace font family depending on OS."
    (cond
     ((eq system-type 'darwin)     "Menlo")
     ((eq system-type 'gnu/linux)  "Monospace")
     ((eq system-type 'windows-nt) "Consolas")
     (t "Monospace")))

  (defun ui-font--system-emoji ()
    "Return a default emoji font family depending on OS."
    (cond
     ((eq system-type 'darwin)     "Apple Color Emoji")
     ((eq system-type 'gnu/linux)  "Noto Color Emoji")
     ((eq system-type 'windows-nt) "Segoe UI Emoji")
     (t "Noto Color Emoji")))

  ;;;; Core setup

  (defun ui-font-apply ()
    "Apply font settings to the current frame."
    (when (display-graphic-p)
      (set-face-attribute
       'default nil
       :family (or ui-font-default (ui-font--system-default))
       :height (* 10 ui-font-size))
      (set-face-attribute
       'variable-pitch nil
       :family (or ui-font-variable-pitch
                   ui-font-default
                   (ui-font--system-default)))
      (set-fontset-font
       t 'emoji
       (font-spec :family
                  (or ui-font-emoji (ui-font--system-emoji))))))

  ;;;###autoload
  (defun ui-font-show-current ()
    "Echo the current default font family and point size."
    (interactive)
    (let ((family (face-attribute 'default :family))
          (height (face-attribute 'default :height)))
      (message "Current font: %s, %.1f pt"
               family (/ height 10.0))))

  ;;;###autoload
  (defun ui-font-describe ()
    "Display font information applied by `ui-font-apply`."
    (interactive)
    (unless (display-graphic-p)
      (user-error "Fonts are only meaningful in graphical frames"))
    (let* ((default-family (face-attribute 'default :family))
           (default-height (face-attribute 'default :height))
           (variable-family (face-attribute 'variable-pitch :family))
           (emoji-font (font-get
                        (font-spec :script 'emoji)
                        :family)))
      (message
       (concat
        "Fonts applied:\n"
        "  Default        : %s (%.1f pt)\n"
        "  Variable-pitch : %s\n"
        "  Emoji          : %s")
       default-family
       (/ default-height 10.0)
       variable-family
       (or emoji-font "unknown"))))

  ;;;; Startup handling

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (ui-font-apply))))
    (add-hook 'after-init-hook #'ui-font-apply))

  ;;;; Optional ligatures

  (leaf ligature
    :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
    :when (display-graphic-p)
    :hook (prog-mode-hook . global-ligature-mode)
    :config
    (ligature-set-ligatures
     'prog-mode
     '("->" "=>" "::" "===" "!=" "&&" "||")))

  (provide 'ui-font)
  ;;; ui-font.el ends here
#+end_src

*** ui/ui-nano-palette.el
:PROPERTIES:
:CUSTOM_ID: ui-nano-palette
:header-args:emacs-lisp: :tangle lisp/ui/ui-nano-palette.el
:END:

**** Purpose
Provide a **single source of truth** for a nano-style light color palette
and apply it consistently across Emacs faces.

This module centralizes all literal color values and exposes them as
customizable variables.

**** What it does
- Defines a minimal nano-style palette via `defcustom`
- Applies core UI, syntax, and mode-line faces from the palette
- Provides helper functions to:
  - Apply faces as-is
  - Override a subset of colors and re-apply

**** Notes
- Faces should be applied *after* a theme is enabled
- No theme logic lives here
- Intended to normalize faces across different themes

**** Implementation

#+begin_src emacs-lisp
  ;;; ui-nano-palette.el --- Nano-style palette -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; One source of truth:
  ;; - Define the palette once (defcustom → easy to tweak via M-x customize)
  ;; - Apply all faces from a single function
  ;; - No duplicated literal colors scattered around
  ;;
  ;;; Code:

  (defgroup my:nano nil
    "Minimal nano-style light palette."
    :group 'faces)

  (defcustom nano-color-background "#fafafa"
    "UI background for buffers/panels."
    :type 'string :group 'my:nano)

  (defcustom nano-color-foreground "#374151"
    "Default body text color."
    :type 'string :group 'my:nano)

  (defcustom nano-color-salient "#2563eb"
    "Accent for links/keywords/standout choices."
    :type 'string :group 'my:nano)

  (defcustom nano-color-popout "#6b7280"
    "Neutral notice / muted highlight."
    :type 'string :group 'my:nano)

  (defcustom nano-color-critical "#dc2626"
    "Critical error / danger color."
    :type 'string :group 'my:nano)

  (defcustom nano-color-strong "#111827"
    "Strong emphasis (headings, key mode-line parts)."
    :type 'string :group 'my:nano)

  (defcustom nano-color-faded "#9ca3af"
    "De-emphasized info (comments/secondary/disabled)."
    :type 'string :group 'my:nano)

  (defcustom nano-color-subtle "#e5e7eb"
    "Subtle backgrounds (mode/header lines, gentle selections)."
    :type 'string :group 'my:nano)

  (defun my/nano-apply-faces ()
    "Apply faces based on the nano-style palette defined above."
    (set-face-attribute 'default nil
                        :background nano-color-background
                        :foreground nano-color-foreground)
    (set-face-attribute 'bold nil :foreground nano-color-strong :weight 'bold)
    (set-face-attribute 'italic nil :slant 'italic)

    (set-face-attribute 'font-lock-comment-face nil :foreground nano-color-faded)
    (set-face-attribute 'font-lock-keyword-face nil
                        :foreground nano-color-salient :weight 'semi-bold)
    (set-face-attribute 'font-lock-string-face nil :foreground nano-color-popout)
    (set-face-attribute 'font-lock-warning-face nil
                        :foreground nano-color-popout :weight 'bold)

    (set-face-attribute 'link nil :foreground nano-color-salient :underline t)
    (set-face-attribute 'button nil :foreground nano-color-salient :underline t)

    (set-face-attribute 'error nil
                        :foreground nano-color-critical :weight 'bold)
    (set-face-attribute 'warning nil
                        :foreground nano-color-popout :weight 'bold)
    (set-face-attribute 'success nil :foreground "#10b981")

    (set-face-attribute 'region nil :background nano-color-subtle)

    (let ((ml-bg nano-color-subtle)
          (ml-fg nano-color-strong))
      (set-face-attribute 'mode-line nil
                          :background ml-bg :foreground ml-fg
                          :box `(:line-width 1 :color ,ml-bg))
      (set-face-attribute 'mode-line-inactive nil
                          :background nano-color-background
                          :foreground nano-color-faded
                          :box `(:line-width 1 :color ,nano-color-background)))

    (set-face-attribute 'minibuffer-prompt nil
                        :foreground nano-color-salient :weight 'semi-bold))

  (defun my/nano-set-palette-and-apply (&rest plist)
    "Override palette entries via PLIST and apply faces."
    (when plist
      (let ((map '((:background . nano-color-background)
                   (:foreground . nano-color-foreground)
                   (:salient    . nano-color-salient)
                   (:popout     . nano-color-popout)
                   (:critical   . nano-color-critical)
                   (:strong     . nano-color-strong)
                   (:faded      . nano-color-faded)
                   (:subtle     . nano-color-subtle))))
        (while plist
          (let* ((k (pop plist))
                 (v (pop plist))
                 (sym (cdr (assq k map))))
            (when sym (set sym v))))))
    (my/nano-apply-faces))

  (provide 'ui-nano-palette)
  ;;; ui/ui-nano-palette.el ends here
#+end_src

*** ui/ui-theme.el
:PROPERTIES:
:CUSTOM_ID: ui-theme
:header-args:emacs-lisp: :tangle lisp/ui/ui-theme.el
:END:

**** Purpose
Define the **theme orchestration layer** for the UI.

This module wires together nano-emacs, nano-theme, spacing, and the
nano palette to produce a consistent visual system.

**** What it does
- Loads the nano palette as the color authority
- Enables nano-emacs and nano-theme
- Re-applies palette-based face normalization after theme changes
- Adds subtle GUI padding

**** Notes
- Face normalization always runs *after* theme activation
- Palette logic lives in `ui-nano-palette.el`
- Safe across Emacs 29+ theme APIs

**** Implementation

#+begin_src emacs-lisp
  ;;; ui/ui-theme.el --- Theme configuration -*- lexical-binding: t; -*-
  ;;
  ;; Category: ui
  ;;
  (eval-when-compile (require 'leaf))

  (require 'ui-nano-palette)

  (setq-default line-spacing 0.24)

  (defun my/nano--reapply-after-theme (&rest _)
    "Re-apply nano-style face normalization after a theme change."
    (when (fboundp 'my/nano-apply-faces)
      (my/nano-apply-faces)))

  (with-eval-after-load 'cus-theme
    (if (boundp 'enable-theme-functions)
        (add-hook 'enable-theme-functions #'my/nano--reapply-after-theme)
      (advice-add 'enable-theme :after #'my/nano--reapply-after-theme)))

  (leaf nano-emacs
    :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
    :config
    (require 'nano-layout)
    (require 'nano-faces)
    (nano-faces)

    (set-face-attribute 'nano-face-strong nil
                        :foreground (face-foreground 'nano-face-default)
                        :weight 'bold)

    (leaf nano-theme
      :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
      :config
      (load-theme 'nano t)
      (my/nano-apply-faces)

      (set-face-attribute 'bold nil :weight 'bold)
      (set-face-attribute 'italic nil :slant 'italic)
      (with-eval-after-load 'elec-pair
        (custom-set-faces
         '(electric-pair-overlay-face ((t (:background "#e5e7eb"))))
         '(show-paren-match ((t (:background "#e5e7eb"
                                             :foreground "#111827"
                                             :weight bold))))
         '(show-paren-mismatch ((t (:background "#dc2626"
                                                :foreground "white"
                                                :weight bold))))))))

  (leaf spacious-padding
    :straight t
    :if (display-graphic-p)
    :custom
    ((spacious-padding-widths . '((left . 15) (right . 15)))
     (spacious-padding-subtle-mode-line . t))
    :config
    (spacious-padding-mode 1)
    (my/nano-apply-faces))

  (provide 'ui-theme)
  ;;; ui/ui-theme.el ends here
#+end_src

*** ui/ui-doom-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-doom-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-doom-modeline.el
:END:

**** Purpose
Provide an alternative **Doom-style modeline bundle**.

**** What it does
- Enables doom-modeline with Nerd Icons
- Configures a compact, informative modeline
- Exposes an interactive command to enable it

**** Notes
- Mutually exclusive with nano-modeline
- Purely UI; no session state logic

**** Implementation

#+begin_src emacs-lisp
  ;;; ui-doom-modeline.el --- Doom UI bundle entry -*- lexical-binding: t; -*-
  ;;
  ;; Category: ui
  ;;
  (eval-when-compile (require 'leaf))
  (declare-function doom-modeline-mode "doom-modeline")

  (leaf nerd-icons :straight t)

  (leaf doom-modeline
    :straight t
    :custom ((doom-modeline-height . 28)
             (doom-modeline-buffer-file-name-style
              . 'truncate-with-project)
             (doom-modeline-minor-modes . nil)
             (doom-modeline-enable-word-count . t))
    :config
    (doom-modeline-mode 1))

  ;;;###autoload
  (defun my/ui-enable-doom ()
    "Enable Doom UI modeline bundle."
    (interactive)
    (doom-modeline-mode 1)
    (message "[ui] Doom modeline enabled."))

  (provide 'ui-doom-modeline)
  ;;; ui/ui-doom-modeline.el ends here
#+end_src

*** ui/ui-nano-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-nano-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-nano-modeline.el
:END:

**** Purpose
Provide a **robust, guarded loader** for nano-modeline.

**** What it does
- Lazily enables nano-modeline when available
- Installs hooks for many major modes
- Guarantees idempotent initialization

**** Notes
- Safe even if nano-modeline is not installed
- Designed for long-running sessions

**** Implementation

#+begin_src emacs-lisp
  ;;; ui-nano-modeline.el --- Nano UI bundle entry -*- lexical-binding: t; -*-
  ;;
  ;; Category: ui
  ;;
  (eval-when-compile (require 'leaf))

  (defvar ui--nano-modeline-initialized nil)

  (defun my/ui--nano-available-p ()
    (require 'nano-modeline nil 'noerror))

  (defun my/ui--nano-setup ()
    (when (and (not ui--nano-modeline-initialized)
               (my/ui--nano-available-p))
      (setopt nano-modeline-padding '(0.20 . 0.25))
      (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
      (add-hook 'text-mode-hook #'nano-modeline-text-mode)
      (setq ui--nano-modeline-initialized t)
      (message "[ui] nano-modeline initialized.")))

  (leaf nano-modeline
    :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
    :after nano-emacs
    :require nil
    :init
    (add-hook 'after-init-hook #'my/ui--nano-setup))

  ;;;###autoload
  (defun my/ui-enable-nano ()
    "Enable Nano UI modeline bundle."
    (interactive)
    (my/ui--nano-setup))

  (provide 'ui-nano-modeline)
  ;;; ui/ui-nano-modeline.el ends here
#+end_src

*** ui/ui-health-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-health-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-health-modeline.el
:END:

**** Purpose
Provide **lightweight, read-only visibility** into Emacs session health
via the global mode-line.

**** What it does
- Displays the number of live buffers
- Displays the number of running processes
- Optionally displays active Eglot workspace count
- Reacts to buffer, process, and Eglot lifecycle events

**** Notes
- This module is **UI-only**
- It must not trigger GC, cleanup, or lifecycle actions
- All policy decisions belong to `core/core-session.el`

**** Implementation

#+begin_src emacs-lisp
  ;;; ui-health-modeline.el --- Session health indicators -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;;; Commentary:
  ;; Mode-line indicators for Emacs session health.
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  (defgroup ui-health-modeline nil
    "Mode-line indicators for session health."
    :group 'mode-line)

  (defcustom ui-health-show-buffers-p t
    "Show buffer count in the mode-line."
    :type 'boolean
    :group 'ui-health-modeline)

  (defcustom ui-health-show-processes-p t
    "Show process count in the mode-line."
    :type 'boolean
    :group 'ui-health-modeline)

  (defcustom ui-health-show-eglot-p t
    "Show Eglot workspace count in the mode-line when available."
    :type 'boolean
    :group 'ui-health-modeline)

  (defvar ui-health--mode-line-string ""
    "Mode-line string representing current session health.")

  (defun ui-health--buffers-count ()
    "Return the number of live buffers."
    (length (buffer-list)))

  (defun ui-health--processes-count ()
    "Return the number of live processes."
    (length (process-list)))

  (defun ui-health--eglot-count ()
    "Return the number of active Eglot workspaces, or nil."
    (when (and ui-health-show-eglot-p
               (featurep 'eglot)
               (boundp 'eglot--managed-buffers))
      (length eglot--managed-buffers)))

  (defun ui-health--update ()
    "Update `ui-health--mode-line-string`."
    (setq ui-health--mode-line-string
          (concat
           (when ui-health-show-buffers-p
             (format " B:%d" (ui-health--buffers-count)))
           (when ui-health-show-processes-p
             (format " P:%d" (ui-health--processes-count)))
           (when-let* ((n (ui-health--eglot-count)))
             (format " LSP:%d" n))))
    (force-mode-line-update))

  (add-hook 'buffer-list-update-hook #'ui-health--update)
  (add-hook 'process-status-change-hook #'ui-health--update)

  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'ui-health--update)
    (add-hook 'eglot-server-stopped-hook #'ui-health--update))

  (unless (member 'ui-health--mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(ui-health--mode-line-string))))

  (ui-health--update)

  (provide 'ui-health-modeline)
  ;;; ui/ui-health-modeline.el ends here
#+end_src

*** ui/ui-window.el
:PROPERTIES:
:CUSTOM_ID: ui-window
:header-args:emacs-lisp: :tangle lisp/ui/ui-window.el
:END:

**** Purpose
Provide **window and layout management utilities** for daily use.

**** What it does
- Automatically resizes windows using `zoom-mode`
- Persists window layouts via `desktop.el`
- Enables undo/redo for window configurations
- Provides simple commands to save and restore layouts

**** Notes
- Desktop data is stored under `no-littering` directories
- Layout save/restore is manual and explicit

**** Implementation

#+begin_src emacs-lisp
  ;;; ui/ui-window.el --- Window management -*- lexical-binding: t; -*-
  ;;
  ;; Category: ui
  ;;
  (eval-when-compile (require 'leaf))

  (leaf zoom
    :straight t
    :hook (after-init-hook . zoom-mode)
    :custom
    ((zoom-size . '(0.62 . 0.62))
     (zoom-ignored-major-modes . '(dired-mode treemacs-mode))
     (zoom-ignored-buffer-names . '("*Messages*" "*Help*"))))

  (leaf desktop
    :straight nil
    :config
    (let ((dir (concat no-littering-var-directory "desktop/")))
      (setq desktop-dirname dir
            desktop-path (list dir)
            desktop-base-file-name "desktop"
            desktop-base-lock-name "lock"
            desktop-restore-eager 10
            desktop-save t
            desktop-load-locked-desktop nil
            desktop-auto-save-timeout 300)
      (my/ensure-directory-exists dir)
      (desktop-save-mode 1)))

  (leaf winner
    :straight nil
    :global-minor-mode t)

  (defvar my:saved-window-config nil
    "Saved window configuration state.")

  (defun my/save-window-layout ()
    "Save current window layout."
    (interactive)
    (setq my:saved-window-config (window-state-get nil t))
    (message "Window configuration saved."))

  (defun my/restore-window-layout ()
    "Restore last saved window layout."
    (interactive)
    (if my:saved-window-config
        (window-state-put my:saved-window-config)
      (message "No saved window configuration found.")))

  (provide 'ui-window)
  ;;; ui/ui-window.el ends here
#+end_src

*** ui/ui-utils.el
:PROPERTIES:
:CUSTOM_ID: ui-utils
:header-args:emacs-lisp: :tangle lisp/ui/ui-utils.el
:END:

#+begin_src emacs-lisp
  ;;; ui/ui-utils.el --- Treemacs configuration & UI utils -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;;; Code:

  (leaf minions :straight t
    :custom ((minions-mode-line-lighter . "⚙"))
    :hook (after-init-hook . minions-mode))

  (setq display-time-interval 30
        display-time-day-and-date t
        display-time-24hr-format t)
  (display-time-mode 1)
  (when (fboundp 'display-battery-mode) (display-battery-mode 1))

  (leaf treemacs :straight t
    :if (display-graphic-p)
    :custom ((treemacs-filewatch-mode . t)
             (treemacs-follow-mode . t)
             (treemacs-indentation . 2)
             (treemacs-missing-project-action . 'remove)))

  (leaf nerd-icons-dired :straight t
    :hook (dired-mode . nerd-icons-dired-mode))

  (leaf pbcopy
    :if (memq window-system '(mac ns))
    :straight t
    :config (turn-on-pbcopy))

  (provide 'ui-utils)
  ;;; ui/ui-utils.el ends here
#+end_src

*** ui/ui-visual-aids.el
:PROPERTIES:
:CUSTOM_ID: ui-visual-aids
:header-args:emacs-lisp: :tangle lisp/ui/ui-visual-aids.el
:END:

**** Purpose
Add **subtle, non-intrusive visual aids** that improve readability and
orientation without clutter.

**** What it does
- Pulses the cursor after navigation jumps
- Highlights TODO/FIXME-style annotations
- Adds rainbow delimiters in programming modes
- Displays lightweight indentation guides

**** Notes
- Avoids Tree-sitter or heavy overlays
- Designed to coexist with existing UI features

**** Implementation

#+begin_src emacs-lisp
;;; ui/ui-visual-aids.el --- Subtle visual helpers -*- lexical-binding: t; -*-
;;
;; Category: ui
;;
(eval-when-compile (require 'leaf))

(leaf pulsar
  :straight t
  :init
  (setq pulsar-delay 0.04
        pulsar-pulse t
        pulsar-face 'pulsar-generic)
  :config
  (pulsar-global-mode 1)
  (dolist (cmd '(recenter-top-bottom
                 other-window
                 next-buffer
                 previous-buffer))
    (add-to-list 'pulsar-pulse-functions cmd)))

(leaf hl-todo
  :straight t
  :hook ((prog-mode-hook text-mode-hook) . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . "#d97706")
          ("FIXME" . "#dc2626")
          ("NOTE" . "#2563eb")
          ("HACK" . "#9333ea"))))

(leaf rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf indent-bars
  :straight t
  :hook (prog-mode-hook . indent-bars-mode)
  :init
  (setq indent-bars-prefer-character t
        indent-bars-spacing-override 2))

(provide 'ui-visual-aids)
;;; ui/ui-visual-aids.el ends here
#+end_src

*** ui/ui-macos.el
:PROPERTIES:
:CUSTOM_ID: ui-macos
:header-args:emacs-lisp: :tangle lisp/ui/ui-macos.el
:END:

**** Purpose
Provide **macOS-specific UI niceties**.

**** What it does
- Enables transparent title bars
- Uses native title bars where supported

**** Notes
- Guarded by `system-type`
- No effect on non-macOS platforms

**** Implementation

#+begin_src emacs-lisp
;;; ui/ui-macos.el --- macOS niceties -*- lexical-binding: t; -*-
;;
;; Category: ui
;;
(eval-when-compile (require 'leaf))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist
               '(use-title-bar . t)))

(provide 'ui-macos)
;;; ui/ui-macos.el ends here
#+end_src

*** completion/completion-core.el
:PROPERTIES:
:CUSTOM_ID: completion-core
:header-args:emacs-lisp: :tangle lisp/completion/completion-core.el
:END:

**** Purpose
Define the **global completion matching policy** used throughout Emacs.

This module establishes the default completion styles and category-specific
overrides that are shared by minibuffer completion, in-buffer completion,
and higher-level completion UIs.

**** What it does
- Enables `orderless` as the primary completion style
- Keeps `basic` completion as a fallback
- Customizes completion behavior per category:
  - Files use partial completion
  - Symbols and commands use orderless matching
- Acts as the foundation for all other completion modules

**** Notes
- This module does **not** enable any UI
- No Corfu / Vertico / Consult logic lives here
- Category overrides can be further refined by context-specific modules
  (e.g. Org SRC overrides)

**** Implementation

#+begin_src emacs-lisp
  ;;; completion-core.el --- Completion core settings -*- lexical-binding: t; -*-
  ;;
  ;; Category: completion
  ;;
  (eval-when-compile (require 'leaf))

  (leaf orderless
    :straight t
    :custom
    ((completion-styles . '(orderless basic))
     (completion-category-overrides
      . '((file    (styles . (partial-completion)))
          (symbol  (styles . (orderless)))
          (command (styles . (orderless)))))))

  (provide 'completion-core)
#+end_src

*** completion/completion-vertico.el
:PROPERTIES:
:CUSTOM_ID: completion-vertico
:header-args:emacs-lisp: :tangle lisp/completion/completion-vertico.el
:END:

**** Purpose
Provide a **minimal and fast minibuffer completion UI** based on Vertico.

This module focuses exclusively on minibuffer interaction and does not
affect in-buffer completion.

**** What it does
- Enables `vertico-mode` globally
- Limits the number of visible candidates for clarity
- Uses `vertico-posframe` in GUI sessions for a floating minibuffer
- Enables `marginalia` to enrich candidates with annotations

**** Notes
- Posframe support is enabled only in graphical sessions
- Candidate annotation is delegated to `marginalia`
- Styling and icons are handled in separate modules

**** Implementation

#+begin_src emacs-lisp
;;; completion-vertico.el --- Vertico minibuffer UI -*- lexical-binding: t; -*-
;;
;; Category: completion
;;
(eval-when-compile (require 'leaf))

(leaf vertico
  :straight t
  :global-minor-mode vertico-mode
  :custom ((vertico-count . 15)))

(leaf vertico-posframe
  :straight t
  :if (display-graphic-p)
  :after vertico
  :custom ((vertico-posframe-border-width . 2))
  :config
  (vertico-posframe-mode 1))

(leaf marginalia
  :straight t
  :global-minor-mode marginalia-mode)

(provide 'completion-vertico)
#+end_src

*** completion/completion-consult.el
:PROPERTIES:
:CUSTOM_ID: completion-consult
:header-args:emacs-lisp: :tangle lisp/completion/completion-consult.el
:END:

**** Purpose
Integrate `consult` as the primary **search, navigation, and narrowing**
backend.

This module enhances standard Emacs commands with previewable,
incremental narrowing.

**** What it does
- Installs `consult`
- Routes Xref lookups through Consult for a unified UI
- Provides consistent navigation across projects and buffers

**** Notes
- This module does not define keybindings
- Integration with Embark is handled separately
- Preview behavior depends on the active UI (Vertico / Corfu)

**** Implementation

#+begin_src emacs-lisp
;;; completion-consult.el --- Consult search/navigation -*- lexical-binding: t; -*-
;;
;; Category: completion
;;
(eval-when-compile (require 'leaf))

(leaf consult
  :straight t
  :custom
  ((xref-show-xrefs-function        . #'consult-xref)
   (xref-show-definitions-function . #'consult-xref)))

(provide 'completion-consult)
#+end_src

*** completion/completion-embark.el
:PROPERTIES:
:CUSTOM_ID: completion-embark
:header-args:emacs-lisp: :tangle lisp/completion/completion-embark.el
:END:

**** Purpose
Enable **context-aware actions** on completion candidates via Embark.

This module allows users to act on candidates without leaving the
completion context.

**** What it does
- Enables `embark` as the prefix help system
- Integrates Embark with Consult for live previews
- Adds action menus for files, buffers, symbols, and more

**** Notes
- Actions are discovered dynamically
- UI and keybindings are intentionally minimal
- Embark does not alter completion sources or styles

**** Implementation

#+begin_src emacs-lisp
;;; completion-embark.el --- Embark actions -*- lexical-binding: t; -*-
;;
;; Category: completion
;;
(eval-when-compile (require 'leaf))

(leaf embark
  :straight t
  :custom ((prefix-help-command . #'embark-prefix-help-command)))

(leaf embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'completion-embark)
#+end_src

*** completion/completion-corfu.el
:PROPERTIES:
:CUSTOM_ID: completion-corfu
:header-args:emacs-lisp: :tangle lisp/completion/completion-corfu.el
:END:

**** Purpose
Provide **in-buffer popup completion** using Corfu.

This module defines the default Corfu behavior and integrates Cape-based
completion sources.

**** What it does
- Enables `global-corfu-mode`
- Uses TAB for completion and indentation
- Enables automatic popup completion
- Integrates `kind-icon` for visual annotations
- Registers common Cape CAPFs globally

**** Notes
- Context-specific overrides (Org SRC, REPLs) are defined elsewhere
- CAPF ordering matters and is refined by other modules
- Corfu UI behavior is adjusted buffer-locally when needed

**** Implementation

#+begin_src emacs-lisp
;;; completion/completion-corfu.el --- Corfu popup completion module -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021-2026
;; Author: YAMASHITA, Takao
;; License: GNU GPL v3 or later
;;
;; Category: completion
;;
;;; Commentary:
;; Corfu-based in-buffer completion with sensible defaults.
;;
;;; Code:

(eval-when-compile (require 'leaf))

(leaf corfu
  :straight t
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  :custom
  ((corfu-auto  . t)
   (corfu-cycle . t)))

(leaf kind-icon
  :straight t
  :after corfu
  :require t
  :custom ((kind-icon-default-face . 'corfu-default))
  :config
  (add-to-list 'corfu-margin-formatters
               #'kind-icon-margin-formatter))

(leaf cape
  :straight t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'completion-corfu)
;;; completion/completion-corfu.el ends here
#+end_src

*** completion/completion-icons.el
:PROPERTIES:
:CUSTOM_ID: completion-icons
:header-args:emacs-lisp: :tangle lisp/completion/completion-icons.el
:END:

**** Purpose
Add **icon-based visual cues** to completion and buffer lists.

This module improves recognizability without changing completion logic.

**** What it does
- Adds Nerd Font icons to `ibuffer`
- Adds icons to Marginalia annotations
- Aligns Marginalia annotations to the right

**** Notes
- Requires Nerd Fonts to be installed
- Purely cosmetic; safe to disable
- Depends on Marginalia being enabled

**** Implementation

#+begin_src emacs-lisp
;;; completion-icons.el --- Nerd icons for completion -*- lexical-binding: t; -*-
;;
;; Category: completion
;;
(eval-when-compile (require 'leaf))

(leaf nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(leaf nerd-icons-completion
  :straight t
  :hook (marginalia-mode-hook
         . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(setq marginalia-align 'right)

(provide 'completion-icons)
#+end_src

*** completion/completion-capf.el
:PROPERTIES:
:CUSTOM_ID: completion-capf
:header-args:emacs-lisp: :tangle lisp/completion/completion-capf.el
:END:

**** Purpose
Provide a clear and predictable completion strategy by selecting
`completion-at-point-functions` (CAPFs) per major mode and defining
semantic completion behavior via `completion-category-overrides`.

This module separates *what to complete* (CAPFs) from *how to display*
(Corfu), ensuring stable behavior across programming, text, Org, shell,
and REPL buffers.

**** What it does
- Sets `completion-at-point-functions` **buffer-locally** per major mode
- Uses only **existing and stable Cape CAPFs**
- Keeps LSP / Eglot CAPFs intact in `prog-mode`
- Adds specialized presets for:
  - Emacs Lisp
  - Programming modes
  - Text modes
  - Org mode
  - Shell / Eshell
  - REPL buffers (IELM / comint)
- Defines `completion-category-overrides` to control matching styles
  by semantic category (file, symbol, keyword, etc.)

**** Notes
- This module intentionally avoids global mutation of CAPFs
- Corfu is treated purely as a UI layer
- Category overrides belong to the completion layer, not the UI
- Safe to load early; relies only on standard completion APIs and Cape

**** Implementation

#+begin_src emacs-lisp
  ;;; completion/capf.el --- Mode-specific CAPF & category configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;;
  ;;; Commentary:
  ;; Configure `completion-at-point-functions` per major mode and
  ;; `completion-category-overrides` for Corfu + Cape.
  ;; Category: completion
  ;;;
  ;;; Code:

  ;;;; Utilities

  (defun completion--set-capfs (capfs)
    "Set buffer-local CAPFs to CAPFS."
    (setq-local completion-at-point-functions capfs))

  ;;;; CAPF presets by mode

  (defun completion--elisp-capfs ()
    "CAPFs for Emacs Lisp buffers."
    (completion--set-capfs
     '(cape-elisp-symbol
       cape-file
       cape-dabbrev)))

  (defun completion--prog-capfs ()
    "CAPFs for programming modes (LSP-friendly)."
    (completion--set-capfs
     (append completion-at-point-functions
             '(cape-file
               cape-dabbrev))))

  (defun completion--text-capfs ()
    "CAPFs for generic text buffers."
    (completion--set-capfs
     '(cape-dabbrev
       cape-file)))

  (defun completion--org-capfs ()
    "CAPFs specialized for Org buffers."
    (completion--set-capfs
     '(cape-dabbrev
       cape-file
       cape-tex)))

  (defun completion--shell-capfs ()
    "CAPFs for shell and eshell buffers."
    (completion--set-capfs
     '(cape-file
       cape-dabbrev)))

  (defun completion--repl-capfs ()
    "CAPFs for REPL-like buffers (IELM, comint)."
    (completion--set-capfs
     (append completion-at-point-functions
             '(cape-dabbrev))))

  ;;;; Hooks

  (add-hook 'emacs-lisp-mode-hook #'completion--elisp-capfs)
  (add-hook 'lisp-interaction-mode-hook #'completion--elisp-capfs)

  (add-hook 'prog-mode-hook #'completion--prog-capfs)
  (add-hook 'text-mode-hook #'completion--text-capfs)

  (add-hook 'org-mode-hook #'completion--org-capfs)

  (add-hook 'sh-mode-hook #'completion--shell-capfs)
  (add-hook 'eshell-mode-hook #'completion--shell-capfs)

  (add-hook 'ielm-mode-hook #'completion--repl-capfs)
  (add-hook 'comint-mode-hook #'completion--repl-capfs)

  ;;;; completion-category-overrides

  (setq completion-category-overrides
        '((file (styles basic partial-completion))
          (symbol (styles orderless))
          (keyword (styles orderless))
          (tex (styles basic))
          (dabbrev (styles basic))))


  (provide 'completion-capf)
  ;;; completion/capf.el ends here
#+end_src

*** completion/completion-capf-org-src.el
:PROPERTIES:
:CUSTOM_ID: completion-capf-org-src
:header-args:emacs-lisp: :tangle lisp/completion/completion-capf-org-src.el
:END:

**** Purpose
Provide context-aware completion inside Org buffers by dynamically
switching `completion-at-point-functions` when entering and leaving
Org Babel source edit buffers.

This ensures that:
- Org text uses text-oriented CAPFs
- Source blocks use language-appropriate CAPFs
- Completion behavior remains predictable and non-intrusive

**** What it does
- Applies Org-specific CAPFs in normal Org buffers
- Detects entry into Org SRC edit buffers (`org-src-mode`)
- Delegates CAPF selection to the major mode of the SRC buffer
- Falls back safely for unknown or unsupported languages

**** Notes
- Uses `org-src-mode-hook`, which runs in the *edit buffer*
- Does not advise or override Org internals
- Respects existing CAPF logic defined in `completion/capf.el`
- No effect outside Org buffers

**** Implementation

#+begin_src emacs-lisp
  ;;; completion/completion-capf-org-src.el --- CAPF switching for Org SRC blocks -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;;
  ;;; Commentary:
  ;; Switch completion-at-point-functions automatically when entering
  ;; Org Babel source edit buffers.
  ;; Category: completion
  ;;;
  ;;; Code:

  (defun completion--org-src-capfs ()
    "Apply appropriate CAPFs inside Org SRC edit buffers.

  This function runs in `org-src-mode` buffers and selects CAPFs
  based on the detected major mode of the source block."
    (cond
     ;; Emacs Lisp blocks
     ((derived-mode-p 'emacs-lisp-mode)
      (completion--elisp-capfs))

     ;; Programming language blocks
     ((derived-mode-p 'prog-mode)
      (completion--prog-capfs))

     ;; Fallback for text-like blocks
     (t
      (completion--text-capfs))))

  (add-hook 'org-src-mode-hook #'completion--org-src-capfs)

  (provide 'completion-capf-org-src)
  ;;; completion/completion-capf-org-src.el ends here
#+end_src

*** completion/completion-capf-org-src-lang.el
:PROPERTIES:
:CUSTOM_ID: completion-capf-org-src-lang
:header-args:emacs-lisp: :tangle lisp/completion/completion-capf-org-src-lang.el
:END:

**** Purpose
Provide language-specific completion behavior inside Org Babel SRC edit
buffers by selecting appropriate CAPFs for each language.

This refines the generic Org SRC CAPF switching by applying tailored
completion strategies for shell, SQL, and Python source blocks.

**** What it does
- Detects the major mode of the Org SRC edit buffer
- Applies language-specific CAPF presets:
  - Shell / Eshell
  - SQL
  - Python
- Falls back to generic Org SRC CAPFs when no language match is found
- Does not affect non-Org buffers

**** Notes
- This module runs only inside `org-src-mode` buffers
- It extends (not replaces) `completion-capf-org-src`
- Language detection relies on `derived-mode-p`
- Safe to extend with additional languages

**** Implementation

#+begin_src emacs-lisp
  ;;; completion/completion-capf-org-src-lang.el --- Language-specific CAPFs for Org SRC -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;;
  ;;; Commentary:
  ;; Apply language-specific CAPF presets inside Org Babel SRC edit buffers.
  ;; Category: completion
  ;;;
  ;;; Code:

  (defun completion--org-src-shell-capfs ()
    "CAPFs for shell-like Org SRC blocks."
    (completion--set-capfs
     '(cape-file
       cape-dabbrev)))

  (defun completion--org-src-sql-capfs ()
    "CAPFs for SQL Org SRC blocks."
    (completion--set-capfs
     '(cape-dabbrev
       cape-file)))

  (defun completion--org-src-python-capfs ()
    "CAPFs for Python Org SRC blocks."
    (completion--set-capfs
     (append completion-at-point-functions
             '(cape-file
               cape-dabbrev))))

  (defun completion--org-src-lang-capfs ()
    "Dispatch language-specific CAPFs for Org SRC edit buffers."
    (cond
     ;; Shell blocks
     ((derived-mode-p 'sh-mode 'shell-mode 'eshell-mode)
      (completion--org-src-shell-capfs))

     ;; SQL blocks
     ((derived-mode-p 'sql-mode)
      (completion--org-src-sql-capfs))

     ;; Python blocks
     ((derived-mode-p 'python-mode)
      (completion--org-src-python-capfs))

     ;; Fallback: keep CAPFs set by completion-capf-org-src
     (t nil)))

  (add-hook 'org-src-mode-hook #'completion--org-src-lang-capfs)

  (provide 'completion-capf-org-src-lang)
  ;;; completion/completion-capf-org-src-lang.el ends here
#+end_src

*** completion/completion-corfu-org-src.el
:PROPERTIES:
:CUSTOM_ID: completion-corfu-org-src
:header-args:emacs-lisp: :tangle lisp/completion/completion-corfu-org-src.el
:END:

**** Purpose
Adjust Corfu popup behavior exclusively inside Org Babel SRC edit buffers
to better suit short-lived, context-focused code editing.

This avoids global UI changes while improving usability when editing
embedded source blocks.

**** What it does
- Enables Corfu auto completion only inside Org SRC buffers
- Reduces popup delay for faster feedback
- Disables cycling to avoid accidental selection
- Uses buffer-local overrides only
- Restores global Corfu behavior automatically when exiting SRC buffers

**** Notes
- Applies only to `org-src-mode` buffers
- Does not modify global Corfu configuration
- Safe with Corfu global mode enabled
- No advice or internal Corfu hooks used

**** Implementation

#+begin_src emacs-lisp
  ;;; completion/completion-corfu-org-src.el --- Corfu UI tweaks for Org SRC -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;;
  ;;; Commentary:
  ;; Adjust Corfu popup behavior specifically for Org Babel SRC edit buffers.
  ;; Category: completion
  ;;;
  ;;; Code:

  (defun completion--org-src-corfu-setup ()
    "Apply buffer-local Corfu settings for Org SRC edit buffers."
    (setq-local corfu-auto t)
    (setq-local corfu-auto-delay 0.1)
    (setq-local corfu-auto-prefix 1)
    (setq-local corfu-cycle nil))

  (add-hook 'org-src-mode-hook #'completion--org-src-corfu-setup)

  (provide 'completion-corfu-org-src)
  ;;; completion/completion-corfu-org-src.el ends here
#+end_src

*** completion/completion-orderless-org-src.el
:PROPERTIES:
:CUSTOM_ID: completion-orderless-org-src
:header-args:emacs-lisp: :tangle lisp/completion/completion-orderless-org-src.el
:END:

**** Purpose
Adjust `orderless` matching styles exclusively inside Org Babel SRC edit
buffers to provide more permissive and code-friendly completion behavior.

This allows aggressive symbol matching in source blocks while keeping
stricter or simpler styles in normal buffers.

**** What it does
- Applies buffer-local `completion-category-overrides` in Org SRC buffers
- Uses `orderless` for symbol- and keyword-like categories
- Keeps file and dabbrev completion conservative
- Leaves global completion styles untouched
- Automatically resets when leaving the SRC edit buffer

**** Notes
- Applied only in `org-src-mode` buffers
- Implemented via buffer-local variable binding
- No advice, no global mutation
- Designed to work with Corfu + Cape presets

**** Implementation

#+begin_src emacs-lisp
  ;;; completion/completion-orderless-org-src.el --- Orderless style tweaks for Org SRC -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;;
  ;;; Commentary:
  ;; Override completion-category-overrides buffer-locally
  ;; for Org Babel SRC edit buffers.
  ;; Category: completion
  ;;;
  ;;; Code:

  (defun completion--org-src-orderless-setup ()
    "Apply Org SRC-specific completion category overrides.

  Use more permissive orderless matching for code-oriented categories
  inside Org Babel source edit buffers."
    (setq-local completion-category-overrides
                '((symbol  (styles orderless))
                  (keyword (styles orderless))
                  (function (styles orderless))
                  (variable (styles orderless))
                  (file     (styles basic partial-completion))
                  (dabbrev  (styles basic)))))

  (add-hook 'org-src-mode-hook #'completion--org-src-orderless-setup)

  (provide 'completion-orderless-org-src)
  ;;; completion/completion-orderless-org-src.el ends here
#+end_src

*** orgx/org-core.el
:PROPERTIES:
:CUSTOM_ID: orgx-core
:header-args:emacs-lisp: :tangle lisp/orgx/org-core.el
:END:

#+begin_src emacs-lisp
  ;;; orgx/org-core.el --- Org Mode core configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'cl-lib))

  (defvar my:d:org (expand-file-name "org" my:d:var))
  (defvar my:d:org-journal (expand-file-name "journal" my:d:org))
  (defvar my:d:org-roam (expand-file-name "org-roam" my:d:org))
  (defvar my:d:org-pictures (expand-file-name "pictures" my:d:org))
  (defvar my:f:capture-blog-file (expand-file-name "blog.org" my:d:org))

  (my/ensure-directory-exists my:d:org)
  (my/ensure-directory-exists my:d:org-journal)
  (my/ensure-directory-exists my:d:org-roam)
  (my/ensure-directory-exists my:d:org-pictures)

  (defun my/org-buffer-files ()
    "Return a list of *.org files currently visited in live buffers."
    (cl-loop for buf in (buffer-list)
             for file = (buffer-file-name buf)
             when (and file (string-match-p "\\.org\\'" file))
             collect file))

  (leaf org
    :straight nil
    :custom
    ((org-directory . my:d:org)
     (org-default-notes-file . "notes.org")
     (org-log-done . 'time)
     (org-support-shift-select . t)
     (org-return-follows-link . t))
    :config
    ;; NOTE: my:d:org/notes/ is intentionally excluded from org-agenda-files (prose-only Markdown domain).
    (setq org-agenda-files
          (seq-filter (lambda (file)
                        (and (string-match-p "\\.org$" file)
                             (not (string-match-p "archives" file))))
                      (directory-files-recursively org-directory "\\.org$")))
    (unless org-agenda-files
      (setq org-agenda-files (list (expand-file-name "inbox.org" org-directory))))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
    (setq org-refile-targets
          '((nil :maxlevel . 3)
            (my/org-buffer-files :maxlevel . 1)
            (org-agenda-files :maxlevel . 3)))
    (setq org-capture-templates
          `(("t" "Todo" entry (file+headline ,(expand-file-name "gtd.org" org-directory) "Inbox")
             "* TODO %?\n %i\n %a")
            ("n" "Note" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Notes")
             "* %?\nEntered on %U\n %i\n %a")
            ("j" "Journal" entry (function org-journal-find-location)
             "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
            ("m" "Meeting" entry (file ,(expand-file-name "meetings.org" org-directory))
             "* MEETING with %? :meeting:\n  %U\n  %a"))))

  (with-eval-after-load 'org
    (let* ((central (expand-file-name "archive.org" (or (bound-and-true-p org-directory)
                                                       (expand-file-name "~/org")))))
      (setopt org-archive-location (concat central "::"))))

  (provide 'org-core)
  ;;; orgx/org-core.el ends here
#+end_src

*** orgx/org-visual.el
:PROPERTIES:
:CUSTOM_ID: orgx-visual
:header-args:emacs-lisp: :tangle lisp/orgx/org-visual.el
:END:

**** Purpose
Provide **modern, distraction-free visuals** for Org buffers.

**** What it does
- Enables `org-modern`
- Improves heading, list, checkbox, and priority visuals
- Beautifies agenda layout and time grid

**** Notes
- Purely visual; no semantic changes
- Typography extras live in `orgx/org-typography.el`

**** Implementation

#+begin_src emacs-lisp
;;; orgx/org-visual.el --- Org Mode visual enhancements -*- lexical-binding: t; -*-
;;
;; Category: org
;;
;;; Code:

(eval-when-compile (require 'leaf))

(leaf org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :custom
  ((org-startup-indented . t)
   (org-hide-leading-stars . t)
   (org-auto-align-tags . nil)
   (org-tags-column . 0)
   (org-catch-invisible-edits . 'show-and-error)
   (org-special-ctrl-a/e . t)
   (org-insert-heading-respect-content . t)
   (org-hide-emphasis-markers . t)
   (org-pretty-entities . t)
   (org-modern-todo-faces
    . '(("TODO"     :background "#673AB7" :foreground "#f8f8f2")
        ("SOMEDAY"  :background "#6b7280" :foreground "#f8f8f2")
        ("WAITING"  :background "#6272a4" :foreground "#f8f8f2")
        ("DONE"     :background "#373844" :foreground "#b0b8d1")
        ("CANCELED" :background "#4b5563" :foreground "#e5e7eb")))
   (org-modern-list . '((?+ . "◦") (?- . "–") (?* . "•")))
   (org-modern-checkbox . '((?X . "") (?- . "") (?\s . "")))
   (org-modern-priority . '((?A . "") (?B . "") (?C . "")))
   (org-modern-replace-stars . "")
   (org-agenda-tags-column . 0)
   (org-agenda-block-separator . ?─)
   (org-agenda-time-grid
    . '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " " ┄┄┄┄┄ "))
   (org-agenda-current-time-string
    . "⭠ now ─────────────────────────────────────────────────")))

(provide 'org-visual)
;;; orgx/org-visual.el ends here
#+end_src

*** orgx/org-extensions.el
:PROPERTIES:
:CUSTOM_ID: orgx-extensions
:header-args:emacs-lisp: :tangle lisp/orgx/org-extensions.el
:END:

**** Purpose
Provide **optional but production-safe extensions** for Org workflows.

**** What it does
- Enables org-journal and org-roam
- Adds defensive guards for org-roam hashing
- Delays org-roam autosync to avoid IO races
- Adds helpers for downloads, TOC, and link capture

**** Notes
- org-roam autosync is intentionally delayed
- Guards avoid crashes on network/iCloud filesystems

**** Implementation

#+begin_src emacs-lisp
  ;;; orgx/org-extensions.el --- Org Mode extensions -*- lexical-binding: t; -*-
  ;;
  ;; Category: org
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf org-journal
    :straight t
    :custom ((org-journal-dir . my:d:org-journal)))

  (leaf org-roam
    :straight t
    :custom ((org-roam-directory . my:d:org-roam))
    :config
    (setq org-roam-db-location
          (expand-file-name "org-roam.db" my:d:org-roam))

    (defun orgx--org-roam-file-hash-guard (orig file)
      "Guard `org-roam-db--file-hash' against file read errors."
      (condition-case err
          (funcall orig file)
        (error
         (message "[org-roam] skip hash: %s (%s)"
                  file (error-message-string err))
         nil)))

    (with-eval-after-load 'org-roam
      (when (fboundp 'org-roam-db--file-hash)
        (advice-add 'org-roam-db--file-hash
                    :around #'orgx--org-roam-file-hash-guard)))

    (run-with-idle-timer
     5 nil
     (lambda ()
       (when (fboundp 'org-roam-db-autosync-mode)
         (org-roam-db-autosync-mode 1)
         (message "[org-roam] autosync enabled (delayed)")))))

  (leaf org-download
    :straight t
    :custom ((org-download-image-dir . my:d:org-pictures)))

  (leaf toc-org
    :straight t
    :hook ((org-mode . toc-org-enable)
           (markdown-mode . toc-org-mode)))

  (leaf org-cliplink :straight t)

  (provide 'org-extensions)
  ;;; orgx/org-extensions.el ends here
#+end_src

*** orgx/org-export.el
:PROPERTIES:
:CUSTOM_ID: orgx-export
:header-args:emacs-lisp: :tangle lisp/orgx/org-export.el
:END:

**** Purpose
Provide a **robust Org export pipeline** for Hugo, Markdown, and diagrams.

**** What it does
- Configures ox-hugo and Hugo capture templates
- Enables Markdown editing and preview
- Enables Mermaid and Graphviz in Org Babel
- Provides Hugo draft review helpers

**** Notes
- Hugo filenames are generated safely and deterministically
- Mermaid requires `mmdc` executable

**** Implementation

#+begin_src emacs-lisp
;;; orgx/org-export.el --- Org export configuration -*- lexical-binding: t; -*-
;;
;; Category: org
;;
;;; Code:

(eval-when-compile (require 'leaf))

(leaf ox-hugo
  :straight t
  :after ox
  :custom ((org-hugo-front-matter-format . "toml")))

(leaf markdown-mode :straight t)
(leaf markdown-preview-mode :straight t)
(leaf edit-indirect :straight t)

(leaf ob-mermaid
  :straight t
  :after org
  :config
  (when (executable-find "mmdc")
    (setq ob-mermaid-cli-path (executable-find "mmdc")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((mermaid . t)))))

(leaf ob-dot
  :straight nil
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((dot . t)))))

(provide 'org-export)
;;; orgx/org-export.el ends here
#+end_src

*** orgx/org-typography.el
:PROPERTIES:
:CUSTOM_ID: orgx-typography
:header-args:emacs-lisp: :tangle lisp/orgx/org-typography.el
:END:

**** Purpose
Provide **modern typography enhancements** for Org buffers without
overlapping with structural or decorative visuals.

This module focuses on *readability and flow* of prose-heavy Org documents,
complementing `orgx/org-visual.el` (org-modern).

**** What it does
- Aligns Org tables and inline images visually using `valign`
- Reveals emphasis markers, links, and sub/superscripts contextually
  using `org-appear`
- Improves prose editing experience while keeping code blocks untouched

**** Notes
- This module is intentionally **orthogonal** to `org-modern`
- No faces, icons, or agenda styling are defined here
- Variable-pitch support is documented but disabled by default
- Safe to enable in long-running sessions

**** Implementation

#+begin_src emacs-lisp
;;; orgx/org-typography.el --- Org modern typography extras -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021-2026
;; License: GNU GPL v3 or later
;;
;; Category: org
;;
;;; Commentary:
;; Complements orgx/org-visual.el (org-modern) without overlap.
;; Focuses on typography and prose readability.
;;
;;; Code:

(eval-when-compile
  (require 'leaf))

;; Optional prose setting:
;; Use variable-pitch for text while keeping code blocks monospaced.
;; Disabled by default to avoid surprising font changes.
;;
;; (leaf org
;;   :straight nil
;;   :hook (org-mode-hook . variable-pitch-mode))

;; Align tables and inline images for better visual flow
(leaf valign
  :straight t
  :hook (org-mode-hook . valign-mode))

;; Contextual reveal of emphasis, links, and markers
(leaf org-appear
  :straight t
  :hook (org-mode-hook . org-appear-mode)
  :init
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(provide 'org-typography)
;;; orgx/org-typography.el ends here
#+end_src

*** vcs/vcs-magit.el
:PROPERTIES:
:CUSTOM_ID: vcs-magit
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-magit.el
:END:

**** Purpose
Provide **Git integration via Magit** with safe defaults.

**** What it does
- Lazily loads Magit entry points
- Keeps auto-revert opt-in
- Enables refined diff display

**** Notes
- Avoids eager refresh churn
- Plays well with long-running sessions

**** Implementation

#+begin_src emacs-lisp
;;; vcs/vcs-magit.el --- Git integration with Magit -*- lexical-binding: t; -*-
;;
;; Category: vcs
;;
;;; Code:

(eval-when-compile (require 'leaf))

(leaf magit
  :straight t
  :commands (magit-status magit-dispatch)
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (setq magit-refresh-status-buffer nil
        magit-diff-refine-hunk 'all))

(provide 'vcs-magit)
;;; vcs/vcs-magit.el ends here
#+end_src

*** vcs/vcs-gutter.el
:PROPERTIES:
:CUSTOM_ID: vcs-gutter
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-gutter.el
:END:

**** Purpose
Provide **lightweight Git change indicators** in the fringe.

**** What it does
- Enables diff-hl in code, text, and dired buffers
- Integrates with Magit refresh lifecycle

**** Notes
- Sole fringe indicator by design
- Borders are disabled for clarity

**** Implementation

#+begin_src emacs-lisp
;;; vcs/vcs-gutter.el --- Show Git changes in fringe -*- lexical-binding: t; -*-
;;
;; Category: vcs
;;
;;; Code:

(eval-when-compile (require 'leaf))

(leaf diff-hl
  :straight t
  :commands (diff-hl-mode diff-hl-dired-mode diff-hl-magit-post-refresh)
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh #'diff-hl-magit-post-refresh))
  (customize-set-variable 'diff-hl-draw-borders nil))

(provide 'vcs-gutter)
;;; vcs/vcs-gutter.el ends here
#+end_src

*** vcs/vcs-forge.el
:PROPERTIES:
:CUSTOM_ID: vcs-forge
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-forge.el
:END:

**** Purpose
Integrate **GitHub/GitLab forges** into Magit via Forge.

**** What it does
- Installs Forge with Magit integration
- Forces built-in SQLite backend on Emacs 29+
- Stores Forge DB under var directory

**** Notes
- Defensive fallbacks are included
- No-littering is respected when available

**** Implementation

#+begin_src emacs-lisp
;;; vcs/vcs-forge.el --- GitHub/GitLab integration via Forge -*- lexical-binding: t; -*-
;;
;; Category: vcs
;;
;;; Code:

(eval-when-compile (require 'leaf))

(defvar no-littering-var-directory
  (expand-file-name ".var/" user-emacs-directory))

(unless (file-directory-p no-littering-var-directory)
  (make-directory no-littering-var-directory t))

(unless (fboundp 'my/ensure-directory-exists)
  (defun my/ensure-directory-exists (dir)
    (unless (file-directory-p dir)
      (make-directory dir t))))

(leaf forge
  :straight t
  :after magit
  :init
  (with-eval-after-load 'emacsql
    (when (boundp 'emacsql-sqlite3-executable)
      (setq emacsql-sqlite3-executable nil)))
  :config
  (let* ((db-dir (expand-file-name "forge" no-littering-var-directory))
         (db (expand-file-name "forge-database.sqlite" db-dir)))
    (my/ensure-directory-exists db-dir)
    (setq forge-database-file db)))

(provide 'vcs-forge)
;;; vcs/vcs-forge.el ends here
#+end_src

*** dev/dev-lsp-eglot.el
:PROPERTIES:
:CUSTOM_ID: dev-lsp-eglot
:header-args:emacs-lisp: :tangle lisp/dev/dev-lsp-eglot.el
:END:

**** Purpose
Provide a **safe, minimal Eglot-based LSP setup** with automatic activation
only when a language server is likely available.

**** What it does
- Installs Eglot as the LSP client
- Detects whether an LSP server can be guessed for the current buffer
- Enables Eglot automatically in `prog-mode` buffers when safe
- Avoids hard dependency on Eglot internal APIs

**** Notes
- Never errors even if Eglot changes private helpers
- Does not force LSP startup when no server is detected
- Designed to coexist with alternative LSP stacks

**** Implementation

#+begin_src emacs-lisp
;;; dev-lsp-eglot.el --- Eglot setup -*- lexical-binding: t; -*-
;;
;; Category: dev
;;
;;; Commentary:
;; Eglot baseline with safe auto-enable.
;;
;;; Code:

(eval-when-compile (require 'leaf))

(defun my/eglot-guessable-p ()
  "Return non-nil if current buffer seems to have an LSP server we can start."
  (cond
   ((fboundp 'eglot--guess-contact)
    (ignore-errors (eglot--guess-contact)))
   ((fboundp 'eglot-guess-contact)
    (ignore-errors (eglot-guess-contact)))
   (t nil)))

(leaf eglot
  :straight t
  :commands (eglot eglot-ensure)
  :custom ((eglot-autoreconnect . t))
  :hook ((prog-mode . (lambda ()
                        (when (my/eglot-guessable-p)
                          (eglot-ensure))))))

;;;###autoload
(defun my/lsp-enable-eglot ()
  "Enable Eglot-based LSP setup."
  (interactive)
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (my/eglot-guessable-p)
                (eglot-ensure))))
  (message "[lsp] Eglot enabled."))

(provide 'dev-lsp-eglot)
;;; dev-lsp-eglot.el ends here
#+end_src

*** dev/dev-lsp-mode.el
:PROPERTIES:
:CUSTOM_ID: dev-lsp-mode
:header-args:emacs-lisp: :tangle lisp/dev/dev-lsp-mode.el
:END:

**** Purpose
Provide an **lsp-mode based LSP stack** (optionally with `lsp-ui`) while
delegating in-buffer completion to Corfu/CAPF.

**** What it does
- Enables `lsp-mode` for `prog-mode` via `lsp-deferred`
- Configures conservative watcher/timeouts for stability
- Disables `lsp-mode`’s own completion provider (`:none`)
- Enables `lsp-ui` as an optional UI layer
- Exposes an autoloaded entry point to enable this stack on demand

**** Notes
- Completion is intentionally delegated to Corfu/Cape (CAPF)
- Hooks are simple and predictable (no advice)
- Designed to coexist with Eglot stack selection via `core/switches.el`

**** Implementation

#+begin_src emacs-lisp
  ;;; dev-lsp-mode.el --- lsp-mode setup -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;;; Commentary:
  ;; lsp-mode baseline + lsp-ui. Completion is delegated to Corfu.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf lsp-mode
    :straight t
    :commands (lsp lsp-deferred)
    :custom ((lsp-keymap-prefix . "C-c l")
             (lsp-enable-file-watchers . t)
             (lsp-file-watch-threshold . 5000)
             (lsp-response-timeout . 5)
             (lsp-diagnostics-provider . :auto)
             (lsp-completion-provider . :none))
    :hook ((prog-mode . lsp-deferred)))

  (leaf lsp-ui
    :straight t
    :after lsp-mode
    :custom ((lsp-ui-doc-enable . t)
             (lsp-ui-doc-delay . 0.2)
             (lsp-ui-sideline-enable . t)))

  ;;;###autoload
  (defun my/lsp-enable-lspmode ()
    "Enable lsp-mode-based LSP setup."
    (interactive)
    (add-hook 'prog-mode-hook #'lsp-deferred)
    (message "[lsp] lsp-mode enabled."))

  (provide 'dev-lsp-mode)
  ;;; dev-lsp-mode.el ends here
#+end_src

*** dev/dev-ai.el
:PROPERTIES:
:CUSTOM_ID: dev-ai
:header-args:emacs-lisp: :tangle lisp/dev/dev-ai.el
:END:

**** Purpose
Centralize **AI-assisted development** configuration (Aidermacs) and
consolidate its runtime/history files into a stable per-user directory.

**** What it does
- Creates/uses `~/.var/aideremacs/` (or `my:d:var`-based) as a base dir
- Sets official `AIDER_*` environment variables for history/log files
- Optionally injects a per-user `.env` via Aidermacs extra args
- Selects model/provider based on environment variables
  - OpenRouter (`OPENROUTER_API_KEY`)
  - OpenAI (`OPENAI_API_KEY`)

**** Notes
- Safe to load early (only env + variables, no UI)
- Uses official Aider env vars so CLI and Aidermacs stay consistent
- If your Aidermacs build uses `aidermacs-args` instead of `aidermacs-extra-args`,
  you can rename locally (this module assumes the newer variable exists)

**** Implementation

#+begin_src emacs-lisp
  ;;; dev/dev-ai.el --- AI-assisted development -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; AI-assisted coding via Aidermacs.
  ;;
  ;; Purpose: Centralize aider/Aidermacs runtime files into ~/.var/aideremacs
  ;; Notes:
  ;; - Uses official AIDER_* env vars so both CLI and Aidermacs obey.
  ;; - Safe to load early (e.g. in personal/user.el or core/general.el).
  ;; - pip3 install aider-ce
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf)
  		   (require 'subr-x))

  ;; 1) Decide base directory: ~/.var/aideremacs
  (defvar my:d:aider
    (expand-file-name "aideremacs/" (or (bound-and-true-p my:d:var)
                                        (expand-file-name "~/.var/")))
    "Base directory to store aider runtime files.")

  ;; 2) Ensure directory exists
  (my/ensure-directory-exists my:d:aider)

  ;; 3) Point aider history files to this directory via env vars
  ;;    (aider recognizes these officially)
  (setenv "AIDER_INPUT_HISTORY_FILE" (expand-file-name "input.history" my:d:aider))
  (setenv "AIDER_CHAT_HISTORY_FILE"  (expand-file-name "chat.history.md" my:d:aider))
  (setenv "AIDER_LLM_HISTORY_FILE"   (expand-file-name "llm.history" my:d:aider))
  ;; Optional: analytics log location if you enable analytics logging
  (setenv "AIDER_ANALYTICS_LOG"      (expand-file-name "analytics.log" my:d:aider))

  ;; 4) [Optional] Keep a per-user .env here and make aider load it.
  ;;    You can create ~/.var/aideremacs/.env and put API keys / options there.
  ;;    If you use Aidermacs, pass --env-file via its extra args.
  (with-eval-after-load 'aidermacs
    ;; Aidermacs exposes an extra-args variable in recent builds
    ;; (see NonGNU ELPA diffs mentioning `aidermacs-extra-args`).
    ;; If your installed version still uses `aidermacs-args`, switch the name.
    (defvar aidermacs-extra-args nil)
    (let ((env-file (expand-file-name ".env" my:d:aider)))
      (when (file-exists-p env-file)
        (setq aidermacs-extra-args
              (append aidermacs-extra-args
                      (list "--env-file" env-file)))))

    ;; Redundancy is harmless: also force the same files via CLI flags,
    ;; in case your shell environment overrides Emacs' setenv.
    (setq aidermacs-extra-args
          (append aidermacs-extra-args
                  (list "--input-history-file" (getenv "AIDER_INPUT_HISTORY_FILE")
                        "--chat-history-file"  (getenv "AIDER_CHAT_HISTORY_FILE")
                        "--llm-history-file"   (getenv "AIDER_LLM_HISTORY_FILE")))))

  (leaf aidermacs :straight t
    :init
    (cond
     ((getenv "OPENROUTER_API_KEY")
      (setenv "OPENAI_API_BASE" "https://openrouter.ai/api/v1")
      (setenv "OPENAI_API_KEY"  (getenv "OPENROUTER_API_KEY"))
      (setopt aidermacs-default-model "openrouter/anthropic/claude-3.5-sonnet"))
     ((getenv "OPENAI_API_KEY")
      (setenv "OPENAI_API_BASE" "https://api.openai.com/v1")
      (setopt aidermacs-default-model "gpt-4o-mini"))
     (t
      (display-warning 'aidermacs
                       "No API keys set. Set OPENROUTER_API_KEY or OPENAI_API_KEY.")))
    (setopt aidermacs-retry-attempts 3
            aidermacs-retry-delay   2.0
            aidermacs-backend       'vterm))

  (provide 'dev-ai)
  ;;; dev/dev-ai.el ends here
#+end_src

*** dev/dev-term.el
:PROPERTIES:
:CUSTOM_ID: dev-term
:header-args:emacs-lisp: :tangle lisp/dev/dev-term.el
:END:

**** Purpose
Provide a **terminal workflow** inside Emacs via vterm/vterm-toggle, and
keep terminal colors consistent by reusing the nano palette.

**** What it does
- Loads `vterm` and applies `ui/ui-nano-palette` to vterm faces
- Works around vterm builds that lack `vterm-color-default`
- Re-applies palette after theme changes
- Adds `vterm-toggle` for quick toggling and project scoping
- Adds a `display-buffer-alist` rule to show vterm at the bottom

**** Notes
- Uses palette as a single source of truth (no :custom-face)
- Color application is guarded by `facep` checks
- Display rule is installed only after vterm is available

**** Implementation

#+begin_src emacs-lisp
  ;;; dev/dev-term.el --- Terminal integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Uses ui/ui-nano-palette as the single source of truth for colors.
  ;; Avoid :custom-face (can trigger “Attempt modify constant …” with leaf).
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'cl-lib))

  ;; Palette is provided by the caller's load-path setup.
  (require 'ui-nano-palette)

  (defun my/vterm-apply-palette ()
    "Apply nano-style palette to vterm faces safely and compatibly.
  Some vterm builds don't define `vterm-color-default`; use `vterm` face instead."
    ;; Fallbacks (in case palette wasn't set yet)
    (defvar nano-color-foreground "#374151")
    (defvar nano-color-background "#fafafa")
    (defvar nano-color-salient    "#2563eb")
    (defvar nano-color-popout     "#6b7280")
    (defvar nano-color-critical   "#dc2626")
    (defvar nano-color-strong     "#111827")
    (defvar nano-color-faded      "#9ca3af")
    (defvar nano-color-subtle     "#e5e7eb")

    (cl-labels
        ((safe-face-set (face &rest props)
           (when (facep face)
             (apply #'set-face-attribute face nil props))))

      ;; Default fg/bg: prefer vterm-color-default; fall back to vterm
      (if (facep 'vterm-color-default)
          (safe-face-set 'vterm-color-default
                         :foreground nano-color-foreground
                         :background nano-color-background)
        (safe-face-set 'vterm
                       :foreground nano-color-foreground
                       :background nano-color-background))

      ;; 8-color palette (keep backgrounds unobtrusive)
      (safe-face-set 'vterm-color-black   :foreground nano-color-strong   :background 'unspecified)
      (safe-face-set 'vterm-color-red     :foreground nano-color-critical :background 'unspecified)
      (safe-face-set 'vterm-color-green   :foreground "#10b981"           :background 'unspecified) ; emerald-500
      (safe-face-set 'vterm-color-yellow  :foreground nano-color-popout   :background 'unspecified)
      (safe-face-set 'vterm-color-blue    :foreground nano-color-salient  :background 'unspecified)
      (safe-face-set 'vterm-color-magenta :foreground "#a21caf"           :background 'unspecified) ; fuchsia-700
      (safe-face-set 'vterm-color-cyan    :foreground "#0891b2"           :background 'unspecified) ; cyan-600
      (safe-face-set 'vterm-color-white   :foreground nano-color-subtle   :background 'unspecified)))

  (defun my/vterm-buffer-p (buf)
    "Return non-nil if BUF is a vterm buffer."
    (with-current-buffer buf
      (or (eq major-mode 'vterm-mode)
          (string-prefix-p "*vterm" (buffer-name buf)))))

  (leaf vterm
    :doc  "Emacs libvterm integration"
    :url  "https://github.com/akermu/emacs-libvterm"
    :straight t
    :config
    ;; Apply once vterm is loaded
    (my/vterm-apply-palette)
    ;; Re-apply after theme activation to keep colors consistent
    (with-eval-after-load 'cus-theme
      (if (boundp 'enable-theme-functions)
          (add-hook 'enable-theme-functions #'my/vterm-apply-palette)
        (advice-add 'enable-theme :after (lambda (&rest _) (my/vterm-apply-palette))))))

  (leaf vterm-toggle
    :doc  "Toggle between vterm and the current buffer"
    :url  "https://github.com/jixiuf/vterm-toggle"
    :straight t
    :custom
    (vterm-toggle-cd-auto-create-buffer . t)
    (vterm-toggle-fullscreen-p          . nil)
    (vterm-toggle-scope                 . 'project))

  ;; Buffer display rule must be set after vterm is available
  (with-eval-after-load 'vterm
    (add-to-list 'display-buffer-alist
                 `(my/vterm-buffer-p
                   (display-buffer-reuse-window display-buffer-in-direction)
                   (direction       . bottom)
                   (dedicated       . t)
                   (reusable-frames . visible)
                   (window-height   . 0.3))))

  (provide 'dev-term)
  ;;; dev/dev-term.el ends here
#+end_src
