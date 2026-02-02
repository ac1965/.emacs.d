README.org part 2/3
--------------------

***** core/core-utils.el
:PROPERTIES:
:CUSTOM_ID: core-utils
:header-args:emacs-lisp: :tangle lisp/core/core-utils.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-utils.el --- Core utility helpers -*- lexical-binding: t; -*-
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

  (provide 'core-utils)
  ;;; core/core-utils.el ends here
#+end_src

***** core/core-treesit.el
:PROPERTIES:
:CUSTOM_ID: core-treesit
:header-args:emacs-lisp: :tangle lisp/core/core-treesit.el
:END:
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
    (message
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

***** core/core-history.el
:PROPERTIES:
:CUSTOM_ID: core-history
:header-args:emacs-lisp: :tangle lisp/core/core-history.el
:END:
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

***** core/core-editing.el
:PROPERTIES:
:CUSTOM_ID: core-editing
:header-args:emacs-lisp: :tangle lisp/core/core-editing.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-editing.el --- Editing helpers & UX aids -*- lexical-binding: t; -*-
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

  (provide 'core-editing)
  ;;; core/core-editing.el ends here
#+end_src

***** core/core-switches.el
:PROPERTIES:
:CUSTOM_ID: core-switches
:header-args:emacs-lisp: :tangle lisp/core/core-switches.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-switches.el --- Unified feature switches (UI/LSP) -*- lexical-binding: t; -*-
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

  (provide 'core-switches)
  ;;; core/core-switches.el ends here
#+end_src

***** core/core-custom.el
:PROPERTIES:
:CUSTOM_ID: custom-file
:header-args:emacs-lisp: :tangle lisp/core/core-custom.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-custom.el --- custom-file helpers -*- lexical-binding: t; -*-
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

  (provide 'core-custom)
  ;;; core/core-custom.el ends here
#+end_src

***** core/core-custom-ui-extras.el
:PROPERTIES:
:CUSTOM_ID: core-custom-ui-extras
:header-args:emacs-lisp: :tangle lisp/core/core-custom-ui-extras.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-custom-ui-extras.el --- user extras -*- lexical-binding: t; -*-

  ;; Append without touching your default module list.
  (setq my:modules-extra
        (delete-dups
         (append my:modules-extra
                 '(ui-visual-aids
                   orgx-typography
  		 ui-macos))))

  (provide 'core-custom-ui-extras)
  ;;; core/core-custom-ui-extras.el ends here
#+end_src

*** completion/
:PROPERTIES:
:CUSTOM_ID: completion-modules
:END:

**** Purpose
Provide a **coherent, explicit, and debuggable completion architecture**
for this Emacs configuration.

This layer centralizes completion behavior to prevent fragmentation
across modes and subsystems, and to make completion flow fully inspectable.

**** What this layer does

Completion modules are responsible for:

- Defining the primary completion frameworks and UI components
- Orchestrating completion-at-point functions (CAPFs)
- Managing precedence, fallback, and composition rules
- Establishing a consistent completion experience across major modes

Typical responsibilities include:

- Minibuffer completion (selection, narrowing, annotations)
- In-buffer completion UI and popup behavior
- Matching styles, sorting, and filtering policy
- Explicit wiring between completion frontends and backends

**** What this layer does *not* do

Completion modules intentionally do **not**:

- Define global keybinding schemes or leader layouts
- Introduce language- or domain-specific semantics
- Configure language servers or external tooling
- Extend Org-specific workflows or capture logic

Those responsibilities belong to other layers.

**** Design constraints

- Completion modules may depend on:
  - =core=
  - =ui=

- Completion modules must not depend on:
  - =orgx=
  - =dev=
  - =vcs=
  - =utils=

- Completion behavior must be:
  - deterministic
  - composable
  - debuggable through standard Emacs facilities
    (e.g. inspecting CAPFs and completion tables)

**** Design principles

- There is a **single completion flow** per context
  (minibuffer vs in-buffer), not multiple competing systems
- CAPF order and fallback rules are explicit and reviewable
- Frontend (UI) and backend (sources) are clearly separated
- Reasonable defaults are preferred over aggressive heuristics

**** Module map

| File | Responsibility |
|------+----------------|
| =completion/completion-core.el= | Core completion policy and shared primitives |
| =completion/completion-minibuffer.el= | Minibuffer completion framework and UX |
| =completion/completion-capf.el= | CAPF composition, ordering, and fallback rules |
| =completion/completion-in-buffer.el= | In-buffer completion UI (popups, previews) |
| =completion/completion-matching.el= | Matching styles, filtering, and sorting policy |
| =completion/completion-annotations.el= | Metadata, annotations, and candidate enrichment |

**** Implementation

- Loaded after =core= and =ui=
- Each file:
  - provides exactly one completion feature
  - documents its assumptions and non-goals
- Disabling this entire layer must leave Emacs functional,
  with only basic `completion-at-point` behavior

***** completion/completion-core.el
:PROPERTIES:
:CUSTOM_ID: completion-core
:header-args:emacs-lisp: :tangle lisp/completion/completion-core.el
:END:
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

***** completion/completion-vertico.el
:PROPERTIES:
:CUSTOM_ID: completion-vertico
:header-args:emacs-lisp: :tangle lisp/completion/completion-vertico.el
:END:
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

***** completion/completion-consult.el
:PROPERTIES:
:CUSTOM_ID: completion-consult
:header-args:emacs-lisp: :tangle lisp/completion/completion-consult.el
:END:
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

***** completion/completion-embark.el
:PROPERTIES:
:CUSTOM_ID: completion-embark
:header-args:emacs-lisp: :tangle lisp/completion/completion-embark.el
:END:
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

***** completion/completion-corfu.el
:PROPERTIES:
:CUSTOM_ID: completion-corfu
:header-args:emacs-lisp: :tangle lisp/completion/completion-corfu.el
:END:
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

***** completion/completion-icons.el
:PROPERTIES:
:CUSTOM_ID: completion-icons
:header-args:emacs-lisp: :tangle lisp/completion/completion-icons.el
:END:
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

***** completion/completion-capf.el
:PROPERTIES:
:CUSTOM_ID: completion-capf
:header-args:emacs-lisp: :tangle lisp/completion/completion-capf.el
:END:
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

***** completion/completion-capf-org-src.el
:PROPERTIES:
:CUSTOM_ID: completion-capf-org-src
:header-args:emacs-lisp: :tangle lisp/completion/completion-capf-org-src.el
:END:
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

***** completion/completion-capf-org-src-lang.el
:PROPERTIES:
:CUSTOM_ID: completion-capf-org-src-lang
:header-args:emacs-lisp: :tangle lisp/completion/completion-capf-org-src-lang.el
:END:
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

***** completion/completion-corfu-org-src.el
:PROPERTIES:
:CUSTOM_ID: completion-corfu-org-src
:header-args:emacs-lisp: :tangle lisp/completion/completion-corfu-org-src.el
:END:
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

***** completion/completion-orderless-org-src.el
:PROPERTIES:
:CUSTOM_ID: completion-orderless-org-src
:header-args:emacs-lisp: :tangle lisp/completion/completion-orderless-org-src.el
:END:
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

*** ui/
:PROPERTIES:
:CUSTOM_ID: ui-modules
:END:

**** Purpose
Define the **visual presentation and interaction layer** of this Emacs configuration.

This category controls *how Emacs looks and feels* while deliberately avoiding
changes to semantic behavior, data flow, or feature logic.

**** What it does
- Configures visual appearance:
  - themes and color palettes
  - faces and typography
  - spacing, margins, and layout hints
- Adjusts interaction-related presentation:
  - cursor style and visibility
  - modeline content and density
  - visual feedback for focus and state
- Applies UI customizations that are:
  - orthogonal to functionality
  - safe to enable or disable independently

**** What it does *not* do
- Define keybinding schemes or leader layouts
  - handled by =core/general.el=
- Select or configure functional backends
  - completion, LSP, VCS, Org logic, etc.
- Introduce workflow logic or mode-specific behavior

**** Notes
- ui modules must not introduce *functional dependencies*
  - they may depend on =core=, but never on =completion=, =orgx=, =dev=, or =utils=
- All changes must be:
  - reversible at runtime
  - localized to presentation concerns
  - safe in terminal, GUI, daemon, and batch contexts
- UI bundles (e.g. doom / nano) are treated as **optional overlays**
  selected and activated via =core/switches.el=

**** Design rationale
Separating UI concerns ensures that:
- visual experimentation does not destabilize core behavior
- alternative UI bundles can be compared or disabled safely
- debugging can be performed with a minimal, presentation-free setup

This layer exists to improve *clarity and comfort*, never correctness.

**** Module map

| File | Responsibility |
|------+----------------|
| =ui/ui-theme.el= | Theme selection and color palette coordination |
| =ui/ui-faces.el= | Face definitions and typography adjustments |
| =ui/ui-modeline.el= | Modeline layout, density, and visual signaling |
| =ui/ui-visual-aids.el= | Minor visual helpers (highlighting, focus, indicators) |
| =ui/ui-layout.el= | Frame, window, margin, and spacing presentation rules |
| =ui/ui-icons.el= | Icon and glyph integration (GUI-safe, optional) |
| =ui/ui-macos.el= | macOS-specific visual tuning (fonts, frames, rendering) |
| =ui/ui-doom-modeline.el= | Doom-style UI bundle (optional overlay) |
| =ui/ui-nano-modeline.el= | Nano-style UI bundle (optional overlay) |

**** Implementation
- ui modules are loaded **after =core= and before functional layers**
- Each file:
  - provides exactly one feature
  - contains no non-visual side effects
- UI bundle modules:
  - are never loaded implicitly
  - are activated explicitly through =core/switches.el=
- Removing or disabling all ui modules must leave Emacs fully functional,
  albeit visually minimal

***** ui/ui-font.el
:PROPERTIES:
:CUSTOM_ID: ui-font
:header-args:emacs-lisp: :tangle lisp/ui/ui-font.el
:END:
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
                  (or ui-font-emoji (ui-font--system-emoji))))
      (ui-font-describe)))

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

***** ui/ui-nano-palette.el
:PROPERTIES:
:CUSTOM_ID: ui-nano-palette
:header-args:emacs-lisp: :tangle lisp/ui/ui-nano-palette.el
:END:
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

***** ui/ui-theme.el
:PROPERTIES:
:CUSTOM_ID: ui-theme
:header-args:emacs-lisp: :tangle lisp/ui/ui-theme.el
:END:
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

  ;; Dired subtree faces (light green background)
  (with-eval-after-load 'dired-subtree
    (set-face-attribute 'dired-subtree-depth-1-face nil
                        :background "#e6f4ea")
    (set-face-attribute 'dired-subtree-depth-2-face nil
                        :background "#dff0e3")
    (set-face-attribute 'dired-subtree-depth-3-face nil
                        :background "#d8ecdc")
    (set-face-attribute 'dired-subtree-depth-4-face nil
                        :background "#d1e8d5"))

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

***** ui/ui-doom-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-doom-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-doom-modeline.el
:END:
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

***** ui/ui-nano-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-nano-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-nano-modeline.el
:END:
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

***** ui/ui-health-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-health-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-health-modeline.el
:END:
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

***** ui/ui-window.el
:PROPERTIES:
:CUSTOM_ID: ui-window
:header-args:emacs-lisp: :tangle lisp/ui/ui-window.el
:END:
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

***** ui/ui-utils.el
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

***** ui/ui-visual-aids.el
:PROPERTIES:
:CUSTOM_ID: ui-visual-aids
:header-args:emacs-lisp: :tangle lisp/ui/ui-visual-aids.el
:END:
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

***** ui/ui-macos.el
:PROPERTIES:
:CUSTOM_ID: ui-macos
:header-args:emacs-lisp: :tangle lisp/ui/ui-macos.el
:END:
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

*** orgx/
:PROPERTIES:
:CUSTOM_ID: orgx-modules
:END:

**** Purpose
Extend Org-mode **beyond upstream defaults** while preserving its
core semantics and data model.

This layer encapsulates all Org-specific customization, workflows,
and assumptions, keeping them isolated from non-Org contexts.

**** What this layer does

Orgx modules are responsible for:

- Extending Org-mode behavior in a structured and non-invasive way
- Providing Org-specific utilities, commands, and workflows
- Integrating Org with other subsystems (completion, UI, search)
- Defining agenda, capture, and navigation policy

Typical responsibilities include:

- Agenda file discovery and scoping
- Capture templates and inbox workflows
- Org-specific navigation, folding, and visual aids
- Integration with roam-like or graph-based note systems

**** What this layer does *not* do

Orgx modules intentionally do **not**:

- Reimplement or fork core Org functionality
- Affect behavior in non-Org buffers
- Define global editing or completion policy
- Encode user- or device-specific paths directly

Those responsibilities belong to other layers.

**** Design constraints

- Orgx modules may depend on:
  - =core=
  - =ui=
  - =completion=

- Orgx modules must not depend on:
  - =dev=
  - =vcs=
  - =utils=

- All Org-specific assumptions must be:
  - confined to Org buffers
  - discoverable via module boundaries
  - safe to disable as a group

**** Design principles

- Org remains **plain Org** on disk
  (no proprietary formats or irreversible transforms)
- Extensions are additive and reversible
- Agenda and capture behavior is explicit and auditable
- Org-specific UX must degrade gracefully when extensions are disabled

**** Module map

| File | Responsibility |
|------+----------------|
| =orgx/org-core.el= | Core Org defaults and shared Org policy |
| =orgx/org-agenda.el= | Agenda discovery, scoping, and views |
| =orgx/org-capture.el= | Capture templates and inbox workflows |
| =orgx/org-navigation.el= | Navigation, folding, and structural helpers |
| =orgx/org-roam.el= | Roam-style linking and graph-based notes |
| =orgx/org-export.el= | Export configuration and backends |
| =orgx/org-ui.el= | Org-specific visual enhancements |

**** Implementation

- Loaded after =completion= and before =dev=
- Each file:
  - provides exactly one Org-related feature
  - documents its scope and assumptions clearly
- Disabling this layer must leave:
  - basic Org editing functional
  - Org files readable and editable without loss

***** orgx/orgx-core.el
:PROPERTIES:
:CUSTOM_ID: orgx-core
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-core.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-core.el --- Org Mode core configuration -*- lexical-binding: t; -*-
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

  (provide 'orgx-core)
  ;;; orgx/org-core.el ends here
#+end_src

***** orgx/orgx-visual.el
:PROPERTIES:
:CUSTOM_ID: orgx-visual
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-visual.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-visual.el --- Org Mode visual enhancements -*- lexical-binding: t; -*-
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

  (provide 'orgx-visual)
  ;;; orgx/orgx-visual.el ends here
#+end_src

***** orgx/orgx-extensions.el
:PROPERTIES:
:CUSTOM_ID: orgx-extensions
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-extensions.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-extensions.el --- Org Mode extensions -*- lexical-binding: t; -*-
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

  (provide 'orgx-extensions)
  ;;; orgx/orgx-extensions.el ends here
#+end_src

***** orgx/orgx-fold.el
:PROPERTIES:
:CUSTOM_ID: orgx-fold
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-fold.el
:END:
#+begin_src emacs-lisp
  ;;; org/orgx-fold.el --- Extra Org helpers -*- lexical-binding: t; -*-
  ;;
  ;; Category: org
  ;;
  ;;; Code:

  (with-eval-after-load 'org
    (require 'org-fold)

    (defun my/org-fold-subtree ()
      "Fold current Org subtree."
      (interactive)
      (org-fold-subtree t))

    (defun my/org-unfold-subtree ()
      "Unfold current Org subtree."
      (interactive)
      (org-show-subtree))

    (defun my/org-toggle-fold ()
      "Toggle fold state of current Org subtree."
      (interactive)
      (save-excursion
        (org-back-to-heading t)
        (if (org-fold-folded-p (point))
            (org-show-subtree)
          (org-fold-subtree t))))

    ;; Local bindings only (no global pollution)
    (define-key org-mode-map (kbd "C-c f") #'my/org-fold-subtree)
    (define-key org-mode-map (kbd "C-c e") #'my/org-unfold-subtree)
    (define-key org-mode-map (kbd "C-c t") #'my/org-toggle-fold))

  (provide 'orgx-fold)
  ;;; org/orgx-fold.el ends here
#+end_src
***** orgx/orgx-export.el
:PROPERTIES:
:CUSTOM_ID: orgx-export
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-export.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-export.el --- Org export configuration -*- lexical-binding: t; -*-
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

  (provide 'orgx-export)
  ;;; orgx/orgx-export.el ends here
#+end_src

***** orgx/orgx-typography.el
:PROPERTIES:
:CUSTOM_ID: orgx-typography
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-typography.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-typography.el --- Org modern typography extras -*- lexical-binding: t; -*-
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

  (provide 'orgx-typography)
  ;;; orgx/orgx-typography.el ends here
#+end_src

*** dev/
:PROPERTIES:
:CUSTOM_ID: dev-modules
:END:

**** Purpose
Support **development, diagnostics, and build-related workflows**
for this Emacs configuration.

This layer exists primarily for maintainers and power users,
not for routine day-to-day editing.

**** What this layer does

Dev modules are responsible for:

- Providing tooling for configuration development and inspection
- Supporting build, compile, and debug workflows
- Enabling diagnostics and introspection of the running Emacs instance
- Assisting with maintenance of the configuration itself

Typical responsibilities include:

- Language Server Protocol (LSP) client configuration
- Debugging, profiling, and tracing helpers
- Compilation, test, and build orchestration
- Developer-facing inspection commands and reports

**** What this layer does *not* do

Dev modules intentionally do **not**:

- Define global editing behavior or keybinding policy
- Affect startup correctness or core invariants
- Introduce visual presentation concerns
- Encode user- or device-specific preferences

Those responsibilities belong to other layers.

**** Design constraints

- Dev modules may depend on:
  - =core=
  - =ui=
  - =completion=

- Dev modules must not depend on:
  - =orgx=
  - =vcs=
  - =utils=

- All side effects must be:
  - explicit
  - opt-in via customization or switches
  - safe to disable as a group

**** Design principles

- Dev tooling is **additive**, never required
- Failure in this layer must degrade gracefully
- Disabling this layer must not affect:
  - startup success
  - basic editing
  - Org workflows

**** Module map

| File | Responsibility |
|------+----------------|
| =dev/dev-lsp-eglot.el= | Eglot-based LSP configuration |
| =dev/dev-lsp-mode.el= | lsp-mode based LSP configuration |
| =dev/dev-dap.el= | Debug Adapter Protocol integration |
| =dev/dev-compile.el= | Compile, build, and task helpers |
| =dev/dev-profile.el= | Profiling and performance diagnostics |
| =dev/dev-inspect.el= | Runtime inspection and debug utilities |

**** Implementation

- Loaded after =orgx=
- Activation is controlled via:
  - =core/switches.el=
  - explicit user or maintainer intent
- Each file:
  - provides exactly one dev-oriented feature
  - documents activation conditions and non-goals

***** dev/dev-lsp-eglot.el
:PROPERTIES:
:CUSTOM_ID: dev-lsp-eglot
:header-args:emacs-lisp: :tangle lisp/dev/dev-lsp-eglot.el
:END:
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

***** dev/dev-lsp-mode.el
:PROPERTIES:
:CUSTOM_ID: dev-lsp-mode
:header-args:emacs-lisp: :tangle lisp/dev/dev-lsp-mode.el
:END:
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

***** dev/dev-ai.el
:PROPERTIES:
:CUSTOM_ID: dev-ai
:header-args:emacs-lisp: :tangle lisp/dev/dev-ai.el
:END:
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

***** dev/dev-term.el
:PROPERTIES:
:CUSTOM_ID: dev-term
:header-args:emacs-lisp: :tangle lisp/dev/dev-term.el
:END:
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

***** dev/dev-web-core.el
:PROPERTIES:
:CUSTOM_ID: dev-web-core
:header-args:emacs-lisp: :tangle lisp/dev/dev-web-core.el
:END:
#+begin_src emacs-lisp
  ;;; dev-web-core.el --- Treesit & project core -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Core configuration for project management and modern syntax highlighting.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf project :straight nil)
  (leaf files   :straight nil
    :custom ((require-final-newline . t)
             (delete-trailing-lines . t))
    :hook ((before-save-hook . delete-trailing-whitespace)))

  (leaf editorconfig
    :straight t
    :global-minor-mode t)

  (leaf treesit
    :straight nil
    :custom ((major-mode-remap-alist
              . '((typescript-mode . typescript-ts-mode)
                  (js-mode         . js-ts-mode)
                  (json-mode       . json-ts-mode)
                  (css-mode        . css-ts-mode)
                  (yaml-mode       . yaml-ts-mode)
                  (sh-mode         . bash-ts-mode)))))

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

  (provide 'dev-web-core)
  ;;; dev-web-core.el ends here
#+end_src

***** dev/dev-build.el
:PROPERTIES:
:CUSTOM_ID: dev-build
:header-args:emacs-lisp: :tangle lisp/dev/dev-build.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-build.el --- Build & Makefile tools -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Build system integration.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf make-mode :straight nil
    :mode (("\\`Makefile\\'"    . makefile-gmake-mode)
           ("\\`GNUmakefile\\'" . makefile-gmake-mode)
           ("\\`makefile\\'"    . makefile-gmake-mode))
    :hook ((makefile-mode . (lambda ()
                              (setq-local indent-tabs-mode t
                                          tab-width 8
                                          show-trailing-whitespace t))))
    :config
    (leaf compile :straight nil
      :custom ((compilation-scroll-output . t)
               (compilation-skip-threshold . 2))
      :init
      (defun my/set-make-compile-command ()
        "Use `make -k` when Makefile is present."
        (when (or (derived-mode-p 'makefile-mode)
                  (locate-dominating-file default-directory "Makefile")
                  (locate-dominating-file default-directory "GNUmakefile"))
          (setq-local compile-command "make -k")))
      (add-hook 'after-change-major-mode-hook #'my/set-make-compile-command))
    (leaf ansi-color :straight nil
      :hook (compilation-filter . (lambda ()
                                    (let ((inhibit-read-only t))
                                      (ansi-color-apply-on-region compilation-filter-start (point-max)))))))

  (provide 'dev-build)
  ;;; dev/dev-build.el ends here
#+end_src

***** dev/dev-format.el
:PROPERTIES:
:CUSTOM_ID: dev-format
:header-args:emacs-lisp: :tangle lisp/dev/dev-format.el
:END:
#+begin_src emacs-lisp
  ;;; dev-format.el --- Prettier formatting via Apheleia -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;;; Commentary:
  ;; Automatic code formatting for web-centric modes.
  ;; Uses Apheleia with prettierd/prettier backends.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf apheleia
    :straight t
    :require t
    :config
    (setf (alist-get 'prettierd apheleia-formatters)
          '("prettierd" filepath))
    (setf (alist-get 'prettier apheleia-formatters)
          '("npx" "prettier" "--stdin-filepath" filepath))
    (dolist (pair '((typescript-ts-mode . prettierd)
                    (tsx-ts-mode        . prettierd)
                    (json-ts-mode       . prettierd)
                    (css-ts-mode        . prettierd)
                    (markdown-mode      . prettierd)))
      (add-to-list 'apheleia-mode-alist pair))
    (apheleia-global-mode +1))

  (provide 'dev-format)
  ;;; dev-format.el ends here
#+end_src

***** dev/dev-infra-modes.el
:PROPERTIES:
:CUSTOM_ID: dev-infra-modes
:header-args:emacs-lisp: :tangle lisp/dev/dev-infra-modes.el
:END:
#+begin_src emacs-lisp
  ;;; dev-infra-modes.el --- Infra modes (.env / Compose / TOML / Make) -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Syntax support for infra files such as Docker Compose, .env, TOML, and Makefiles.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf docker-compose-mode :straight t :mode ("docker-compose\\.*ya?ml\\'"))
  (leaf dotenv-mode :straight t
    :mode (("\\.env\\..*\\'" . dotenv-mode)
           ("\\.env\\'"     . dotenv-mode)))
  (leaf toml-mode :straight t :mode ("\\.toml\\'" . toml-mode))

  (provide 'dev-infra-modes)
  ;;; dev/dev-infra-modes.el ends here
#+end_src

***** dev/dev-docker.el
:PROPERTIES:
:CUSTOM_ID: dev-docker
:header-args:emacs-lisp: :tangle lisp/dev/dev-docker.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-docker.el --- Docker integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Docker development support.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf dockerfile-mode :straight t
    :mode (("Dockerfile\\(\\..*\\)?\\'" . dockerfile-mode)
           ("\\.dockerfile\\'"         . dockerfile-mode))
    :custom ((dockerfile-mode-command . "docker")))

  (leaf yaml-mode :straight t
    :mode (("\\`docker-compose.*\\.ya?ml\\'" . yaml-mode)
           ("\\.ya?ml\\'"                   . yaml-mode)))

  (leaf docker :straight t
    :commands (docker docker-containers docker-images docker-volumes docker-networks))

  (leaf tramp-container :straight nil
    :after tramp
    :init
    (setq tramp-container-method "docker"))

  (leaf tempel :straight t
    :commands (tempel-insert)
    :init
    (with-eval-after-load 'tempel
      (defvar my:tempel-docker-templates
        '((dockerfile "FROM " p n
                      "WORKDIR /app" n
                      "COPY . /app" n
                      "RUN " p n
                      "CMD [" p "]" n)))
      (add-to-list 'tempel-user-elements my:tempel-docker-templates)))

  (provide 'dev-docker)
  ;;; dev/dev-docker.el ends here
#+end_src

***** dev/dev-sql.el
:PROPERTIES:
:CUSTOM_ID: dev-sql
:header-args:emacs-lisp: :tangle lisp/dev/dev-sql.el
:END:
#+begin_src emacs-lisp
  ;;; lisp/dev/dev-sql.el --- SQL/PostgreSQL helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  (eval-when-compile (require 'leaf))

  (leaf sql :straight nil
    :custom ((sql-product . 'postgres)))

  (leaf sql-indent :straight t
    :hook (sql-mode . sqlind-minor-mode))

  (leaf sqlformat
    :straight t
    :custom ((sqlformat-command . 'pgformatter)
             (sqlformat-args . '("--nostyle")))
    :hook (sql-mode . sqlformat-on-save-mode)
    :hook (sql-ts-mode . sqlformat-on-save-mode))

  (provide 'dev-sql)
  ;;; dev-sql.el ends here
#+end_src
