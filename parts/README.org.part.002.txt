README.org part 2/3
--------------------

***** core/core-tools.el
:PROPERTIES:
:CUSTOM_ID: core-tools
:header-args:emacs-lisp: :tangle lisp/core/core-tools.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-tools.el --- Internal core helper utilities -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;; Commentary:
  ;; Internal helper primitives for core modules.
  ;;
  ;; This module:
  ;; - provides non-interactive, side-effect-free helpers
  ;; - avoids hooks, keybindings, and UI concerns
  ;; - is safe to load at any stage of initialization
  ;;
  ;;; Code:

  ;;;; Version helpers ----------------------------------------------------------

  (defun core-tools-emacs>= (major minor)
    "Return non-nil if running Emacs version is >= MAJOR.MINOR."
    (or (> emacs-major-version major)
        (and (= emacs-major-version major)
             (>= emacs-minor-version minor))))

  ;;;; Filesystem helpers -------------------------------------------------------

  (defun core-tools-ensure-directory (dir)
    "Ensure directory DIR exists.
  Create it recursively if necessary."
    (when (and (stringp dir)
               (not (file-directory-p dir)))
      (make-directory dir t)))

  ;;;; Feature / function probes ------------------------------------------------

  (defun core-tools-feature-present-p (feature)
    "Return non-nil if FEATURE can be safely required."
    (or (featurep feature)
        (locate-library (symbol-name feature))))

  (defun core-tools-function-present-p (fn)
    "Return non-nil if FN is a callable function."
    (and (symbolp fn) (fboundp fn)))

  ;;;; Safe require / call -------------------------------------------------------

  (defun core-tools-require-if-present (feature)
    "Require FEATURE only if it is present.
  Return non-nil if successfully loaded."
    (when (core-tools-feature-present-p feature)
      (require feature nil t)))

  (defun core-tools-call-if-present (fn &rest args)
    "Call FN with ARGS if FN is defined.
  Return the result, or nil if FN is unavailable."
    (when (core-tools-function-present-p fn)
      (apply fn args)))

  ;;;; Diagnostics helpers ------------------------------------------------------

  (defun core-tools-check-provide (feature file)
    "Warn if FILE does not provide FEATURE.
  Intended for use from `after-load-functions'."
    (when (and feature file)
      (unless (featurep feature)
        (warn "[core-tools] %s loaded from %s but did not provide `%s`"
              feature file feature))))

  ;;;; Internal policy helpers --------------------------------------------------

  (defun core-tools-user-file-p (file)
    "Return non-nil if FILE belongs to the user configuration."
    (and (stringp file)
         (string-prefix-p
          (expand-file-name user-emacs-directory)
          (expand-file-name file))))

  (provide 'core-tools)
  ;;; core/core-tools.el ends here

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
  ;; Commentary:
  ;; Centralized Tree-sitter integration and policy.
  ;;
  ;; This module:
  ;; - owns Tree-sitter availability and configuration
  ;; - defines grammar sources and remapping rules
  ;; - guarantees explicit and non-implicit installation behavior
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

  (defun core-treesit--startup-init ()
    "Apply Tree-sitter remapping and optional installation after startup."
    (when (core-treesit--available-p)
      (core-treesit-apply-remap)
      (when core-treesit-auto-install-p
        (ignore-errors (core-treesit-install-all)))))

  (add-hook 'emacs-startup-hook #'core-treesit--startup-init)

  (provide 'core-treesit)
  ;;; core/core-treesit.el ends here
#+end_src

***** core/core-history.el
:PROPERTIES:
:CUSTOM_ID: core-history
:header-args:emacs-lisp: :tangle lisp/core/core-history.el
:END:
#+begin_src emacs-lisp
  ;;; core/core-history.el --- Session persistence & autorevert -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;; Commentary:
  ;; Persistence of navigation and buffer history.
  ;;
  ;; This module:
  ;; - enables saveplace, recentf, and savehist
  ;; - stores history under controlled directories
  ;; - avoids aggressive or implicit restoration
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  ;; ---------------------------------------------------------------------------
  ;; saveplace
  ;; ---------------------------------------------------------------------------

  (leaf saveplace :straight nil
    :init
    (setq save-place-file (concat no-littering-var-directory "saveplace")))

  ;; ---------------------------------------------------------------------------
  ;; recentf
  ;; ---------------------------------------------------------------------------

  (leaf recentf :straight nil
    :init
    (setq recentf-max-saved-items 100
          recentf-save-file (concat no-littering-var-directory "recentf")))

  ;; ---------------------------------------------------------------------------
  ;; savehist
  ;; ---------------------------------------------------------------------------

  (leaf savehist
    :straight nil
    :config
    (setq savehist-file (concat no-littering-var-directory "history"))
    (my/ensure-directory-exists (file-name-directory savehist-file))

    ;; `savehist-additional-variables' is defined in savehist.el
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables
                   'my:desktop-ask-on-restore)))

  ;; ---------------------------------------------------------------------------
  ;; Startup activation
  ;; ---------------------------------------------------------------------------

  (defun core-history--startup-init ()
    "Enable history-related minor modes after startup."
    (save-place-mode +1)
    (recentf-mode +1)
    (savehist-mode +1))

  (add-hook 'emacs-startup-hook #'core-history--startup-init)

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
  ;; Commentary:
  ;; Centralized feature switches for UI bundles and LSP backends.
  ;;
  ;; This module:
  ;; - defines mutually exclusive feature choices
  ;; - resolves availability at runtime
  ;; - avoids hard dependencies on optional subsystems
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
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;; Commentary:
  ;; Centralized handling of Customize output and helpers.
  ;;
  ;; This module:
  ;; - routes customize output to a controlled location
  ;; - provides explicit commands to inspect or dump state
  ;; - never applies automatic or implicit persistence
  ;;
  ;;; Code:

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
  ;;; core/core-custom-ui-extras.el --- User UI module extensions -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: core
  ;;
  ;; Commentary:
  ;; Append user-specific UI extension modules.
  ;;
  ;; This module:
  ;; - extends the active module list without mutation
  ;; - remains optional and non-intrusive
  ;; - performs no configuration itself
  ;;
  ;;; Code:

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

This layer centralizes completion behavior to avoid fragmentation across
modes and subsystems, and to keep completion flow *fully inspectable* via
standard Emacs mechanisms (CAPFs, completion categories, minibuffer UI).

**** What this layer does
Completion modules are responsible for defining and wiring together the
completion stack used by this configuration.

Specifically, this layer:

- Defines the minibuffer completion UI and navigation/search commands
- Defines in-buffer completion UI and its default behavior
- Establishes matching policy and completion category overrides
- Orchestrates `completion-at-point-functions` (CAPFs) per major mode
- Provides targeted, buffer-local tweaks for Org SRC edit buffers
- Provides optional session hygiene around project switches (Eglot lifecycle)

Typical responsibilities include:

- Minibuffer completion (selection UI, narrowing, annotations)
- In-buffer completion (popup UI, auto completion defaults)
- Matching styles (orderless vs basic, file partial completion)
- CAPF ordering and safe fallbacks across editing contexts

**** What this layer does *not* do
Completion modules intentionally do **not**:

- Define global keybinding schemes or leader layouts
- Implement UI themes or palette policy (owned by =ui=)
- Introduce language- or project-specific semantics
- Own LSP configuration or server selection policy (owned by =dev=)
- Extend Org workflows beyond completion mechanics (owned by =orgx=)
- Provide VCS features (owned by =vcs=) or general utilities (owned by =utils=)

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
  - debuggable using standard Emacs facilities
    (e.g. inspecting CAPFs, `completion-category-overrides`,
     and the active minibuffer frontend)

**** Design principles
- There is a **single completion flow** per context:
  - minibuffer completion (Vertico + Marginalia + Consult)
  - in-buffer completion (CAPF + Corfu)

- CAPF order and fallback rules are explicit and reviewable
- Frontend (UI) and backend (sources/CAPFs) are clearly separated
- Category-based policy is preferred over mode-specific special cases
- Org SRC edit buffers are treated as a controlled exception:
  buffer-local overrides apply, without changing global policy

**** Module map
| File | Responsibility |
|------+----------------|
| =completion/completion-core.el= | Global completion policy: `completion-styles` and `completion-category-overrides` |
| =completion/completion-vertico.el= | Minibuffer completion UI (Vertico) and annotations (Marginalia) |
| =completion/completion-consult.el= | Search/navigation integration, including xref display via Consult |
| =completion/completion-embark.el= | Contextual actions and prefix help integration (Embark + Embark-Consult) |
| =completion/completion-corfu.el= | In-buffer completion UI (Corfu) + candidate visuals (kind-icon) + CAPF providers (Cape) |
| =completion/completion-icons.el= | Nerd icons integration for minibuffer annotations (Marginalia) |
| =completion/completion-capf.el= | Mode-specific CAPF presets and shared category overrides for CAPF-based completion |
| =completion/completion-capf-org-src.el= | CAPF switching when entering Org SRC edit buffers (`org-src-mode`) |
| =completion/completion-capf-org-src-lang.el= | Language-specific CAPF presets inside Org SRC edit buffers (sh/sql/python) |
| =completion/completion-corfu-org-src.el= | Buffer-local Corfu behavior tuned for Org SRC edit buffers |
| =completion/completion-orderless-org-src.el= | Buffer-local category overrides for permissive Orderless matching in Org SRC |
| =completion/completion-lsp.el= | Optional cleanup helper: shut down obsolete Eglot servers on project switch |

**** Notes
- Matching policy is defined centrally via `completion-styles` and
  `completion-category-overrides`; Org SRC overrides are buffer-local only.
- CAPF presets are applied via major-mode hooks; exceptions are limited to
  Org SRC edit buffers where `org-src-mode-hook` controls local overrides.
- LSP server lifecycle policy is not implemented here; only safe cleanup
  is provided, and only when Eglot is present and enabled.

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
  ;;; completion/completion-core.el --- Completion core settings -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Core completion style and category configuration.
  ;;
  ;; This module:
  ;; - defines global completion styles
  ;; - sets category overrides for common symbols
  ;; - serves as the foundation for Corfu/CAPE
  ;;
  ;;; Code:

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
  ;;; completion/completion-vertico.el --- Vertico minibuffer UI -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Vertico-based minibuffer completion UI.
  ;;
  ;; This module:
  ;; - enables Vertico, Marginalia, and vertico-posframe
  ;;   after window geometry becomes stable
  ;; - guarantees posframe usage from the very first minibuffer
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf))

  ;; ---------------------------------------------------------------------------
  ;; Initialization state
  ;; ---------------------------------------------------------------------------

  (defvar completion-vertico--initialized nil
    "Non-nil once Vertico, Marginalia, and vertico-posframe are initialized.")

  ;; ---------------------------------------------------------------------------
  ;; Initialization
  ;; ---------------------------------------------------------------------------

  (defun completion-vertico--init ()
    "Initialize Vertico, Marginalia, and vertico-posframe once after window setup."
    (unless completion-vertico--initialized
      (setq completion-vertico--initialized t)

      (require 'vertico)
      (vertico-mode 1)

      (require 'marginalia)
      (marginalia-mode 1)

      (when (display-graphic-p)
        (require 'vertico-posframe)
        (vertico-posframe-mode 1))))

  (add-hook 'window-setup-hook #'completion-vertico--init)

  ;; ---------------------------------------------------------------------------
  ;; Packages
  ;; ---------------------------------------------------------------------------

  (leaf vertico
    :straight t
    :custom
    ((vertico-count . 15)))

  (leaf vertico-posframe
    :straight t
    :if (display-graphic-p)
    :after vertico
    :custom
    ((vertico-posframe-border-width . 2)))

  (leaf marginalia
    :straight t)

  (provide 'completion-vertico)
  ;;; completion/completion-vertico.el ends here
#+end_src

***** completion/completion-consult.el
:PROPERTIES:
:CUSTOM_ID: completion-consult
:header-args:emacs-lisp: :tangle lisp/completion/completion-consult.el
:END:
#+begin_src emacs-lisp
  ;;; completion/completion-consult.el --- Consult search and navigation -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Consult integration for xref-based navigation.
  ;;
  ;; This module:
  ;; - routes xref UI through Consult
  ;; - avoids redefining navigation commands
  ;; - remains backend-agnostic
  ;;
  ;;; Code:

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
  ;;; completion/completion-embark.el --- Embark actions -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Embark integration for contextual actions and previews.
  ;;
  ;; This module:
  ;; - enables Embark as the primary action framework
  ;; - integrates Embark collections with Consult previews
  ;; - introduces no global keybindings or policy
  ;;
  ;;; Code:

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
  ;; Commentary:
  ;; Corfu-based in-buffer completion with sensible defaults.
  ;;
  ;; This module:
  ;; - enables global Corfu completion
  ;; - integrates icon margins and CAPE backends
  ;; - provides a lightweight, non-intrusive UI
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  ;; ---------------------------------------------------------------------------
  ;; Lazy initialization
  ;; ---------------------------------------------------------------------------

  (defvar completion-corfu--initialized nil
    "Non-nil once Corfu, kind-icon, and CAPE have been initialized.")

  (defun completion-corfu--init ()
    "Initialize Corfu completion UI and related backends on first use."
    (unless completion-corfu--initialized
      (setq completion-corfu--initialized t)

      ;; corfu
      (require 'corfu)
      (setq tab-always-indent 'complete)
      (global-corfu-mode 1)

      ;; kind-icon
      (require 'kind-icon)
      (add-to-list 'corfu-margin-formatters
                   #'kind-icon-margin-formatter)

      ;; cape
      (require 'cape)
      (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions #'cape-dabbrev)

      ;; One-shot initialization
      (remove-hook 'completion-at-point-functions #'completion-corfu--init)))

  (add-hook 'completion-at-point-functions #'completion-corfu--init)

  ;; ---------------------------------------------------------------------------
  ;; Packages
  ;; ---------------------------------------------------------------------------

  (leaf corfu
    :straight t
    :custom
    ((corfu-auto  . t)
     (corfu-cycle . t)))

  (leaf kind-icon
    :straight t
    :after corfu
    :require t
    :custom ((kind-icon-default-face . 'corfu-default)))

  (leaf cape
    :straight t
    :after corfu)

  (provide 'completion-corfu)
  ;;; completion/completion-corfu.el ends here
#+end_src

***** completion/completion-icons.el
:PROPERTIES:
:CUSTOM_ID: completion-icons
:header-args:emacs-lisp: :tangle lisp/completion/completion-icons.el
:END:
#+begin_src emacs-lisp
  ;;; completion/completion-icons.el --- Icons for completion UI -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Icon integration for completion and buffer lists.
  ;;
  ;; This module:
  ;; - decorates completion candidates with Nerd Icons
  ;; - integrates with Marginalia and Ibuffer
  ;; - avoids altering completion behavior itself
  ;;
  ;;; Code:

  (eval-when-compile (require  'leaf))

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
  ;;; completion/completion-capf.el --- Mode-specific CAPF configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Central definition of completion-at-point-functions by major mode.
  ;;
  ;; This module:
  ;; - defines CAPF presets for code, text, Org, and REPL buffers
  ;; - installs mode hooks to activate presets
  ;; - configures completion-category-overrides for Corfu/CAPE
  ;;
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
  ;;; completion/completion-capf.el ends here
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
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Automatic CAPF selection inside Org Babel source edit buffers.
  ;;
  ;; This module:
  ;; - switches CAPFs based on detected source block major mode
  ;; - runs only in org-src-mode buffers
  ;; - delegates actual CAPF definitions to completion-capf
  ;;
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
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Language-aware CAPF dispatch inside Org Babel SRC buffers.
  ;;
  ;; This module:
  ;; - applies shell, SQL, and Python-specific CAPFs
  ;; - runs after generic Org SRC CAPF setup
  ;; - preserves fallback behavior when no match exists
  ;;
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
  ;;; completion/completion-corfu-org-src.el --- Corfu tweaks for Org SRC -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Buffer-local Corfu behavior adjustments for Org SRC edit buffers.
  ;;
  ;; This module:
  ;; - enables aggressive auto completion in SRC buffers
  ;; - keeps settings strictly buffer-local
  ;; - never affects normal Org or code buffers
  ;;
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
  ;;; completion/completion-orderless-org-src.el --- Orderless tweaks for Org SRC -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Buffer-local Orderless matching rules for Org SRC edit buffers.
  ;;
  ;; This module:
  ;; - overrides completion-category-overrides locally
  ;; - relaxes matching for code-oriented categories
  ;; - never affects non-Org buffers
  ;;
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

***** completion/completion-lsp.el
:PROPERTIES:
:CUSTOM_ID: completion-lsp
:header-args:emacs-lisp: :tangle lisp/completion/completion-lsp.el
:END:
#+begin_src emacs-lisp
  ;;; completion/completion-lsp.el --- LSP lifecycle cleanup helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: completion
  ;;
  ;; Commentary:
  ;; Defensive cleanup of obsolete Eglot servers on project switches.
  ;;
  ;; This module:
  ;; - tracks project root transitions
  ;; - shuts down stale LSP servers safely
  ;; - integrates with project.el hooks only
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf nil
    :straight nil
    :init
    (defcustom completion-lsp-enable-p t
      "Enable LSP lifecycle cleanup."
      :type 'boolean
      :group 'completion)

    (defvar completion-lsp--current-project-root nil)

    (defun completion-lsp--project-root ()
      (when-let* ((project (project-current nil))
  		(roots (project-roots project)))
        (car roots)))

    (defun completion-lsp-on-project-switch ()
      "Shutdown obsolete eglot servers on project switch."
      (when completion-lsp-enable-p
        (let ((new-root (completion-lsp--project-root)))
          (when (and completion-lsp--current-project-root
                     new-root
                     (not (string-equal completion-lsp--current-project-root new-root))
                     (featurep 'eglot))
            (dolist (server eglot--servers)
              (when (string-prefix-p completion-lsp--current-project-root
                                     (eglot--project-root (cdr server)))
                (ignore-errors
                  (eglot-shutdown (cdr server))))))
          (setq completion-lsp--current-project-root new-root))))

    (add-hook 'find-file-hook #'completion-lsp-on-project-switch)
    (add-hook 'project-switch-project-hook #'completion-lsp-on-project-switch))

  (provide 'completion-lsp)
  ;;; completion/completion-lsp.el ends here
#+end_src

*** ui/
:PROPERTIES:
:CUSTOM_ID: ui-modules
:END:

**** Purpose
Define the **visual presentation and interaction layer** of this Emacs configuration.

This layer governs *how Emacs looks and feels*—colors, spacing, typography,
and visual feedback—while deliberately avoiding any influence on semantic
behavior, data flow, or feature logic.

UI modules exist to improve *clarity, comfort, and legibility*, never correctness.

**** What this layer does
UI modules are responsible for presentation-only concerns that shape the
day-to-day user experience without altering functional meaning.

Specifically, this layer:

- Configures visual appearance:
  - theme selection and palette coordination
  - face definitions and typography (fixed / variable pitch)
  - line spacing, margins, and window layout hints
- Adjusts interaction-related presentation:
  - cursor style and visibility
  - modeline content, density, and signaling
  - subtle visual feedback for focus, state, and context
- Applies UI customizations that are:
  - orthogonal to functionality
  - safe to toggle or replace independently
  - reversible at runtime

**** What this layer does *not* do
UI modules intentionally do **not**:

- Define global keybinding schemes or leader layouts
  (handled by =core/general.el=)
- Select, configure, or orchestrate functional backends
  (completion, LSP, VCS, Org logic, etc.)
- Introduce workflow logic or mode-specific behavior
- Encode user-, device-, or host-specific policy
  (handled by =personal/= overlays)

Those responsibilities belong to other layers.

**** Design constraints
- UI modules may depend on:
  - =core=

- UI modules must not depend on:
  - =completion=
  - =orgx=
  - =dev=
  - =vcs=
  - =utils=

- All UI changes must be:
  - presentation-only (no semantic side effects)
  - reversible at runtime
  - safe in GUI, terminal, daemon, and batch contexts
  - tolerant of partial availability (fonts, icons, faces)

**** Design principles
- Presentation and behavior are strictly separated
- UI modules must be safe to disable wholesale for debugging
- Defaults favor readability and low visual noise
- Visual signaling should be subtle, not distracting
- Optional UI bundles are treated as *overlays*, not foundations

**** Optional UI bundles
Theme and modeline bundles (e.g. Doom-style or Nano-style) are treated as
**optional overlays**.

Selection and activation are controlled exclusively via
=core/switches.el=, ensuring that:

- the base UI remains minimal and stable
- bundles can be compared or disabled safely
- UI experimentation never destabilizes core behavior

**** Benefits
This separation ensures that:

- visual experimentation does not affect correctness
- alternative UI styles can be swapped without refactoring logic
- debugging can be performed with a minimal, presentation-free setup
- long-running sessions remain visually predictable

**** Module map
| File | Responsibility |
|------+----------------|
| =ui/ui-font.el= | Font family, size, and typography defaults |
| =ui/ui-theme.el= | Theme selection and color palette coordination |
| =ui/ui-window.el= | Frame, window, margin, and spacing presentation rules |
| =ui/ui-utils.el= | Small visual helpers and UI-safe conveniences |
| =ui/ui-health-modeline.el= | Session health indicators and visual status signaling |
| =ui/ui-icons.el= | Icon and glyph integration (GUI-safe, optional) |
| =ui/ui-macos.el= | macOS-specific visual tuning (fonts, frames, rendering) |
| =ui/ui-doom-modeline.el= | Doom-style modeline bundle (optional overlay) |
| =ui/ui-nano-modeline.el= | Nano-style modeline bundle (optional overlay) |

**** Notes
- UI modules must never introduce functional dependencies.
- Any visual adjustment that affects behavior belongs elsewhere.
- If a UI change feels “clever” or “opinionated”, it is likely misplaced.

This layer should remain *boring, predictable, and comfortable*.

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
  ;;; ui/ui-font.el --- Font configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2025
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Font configuration for graphical UI frames.
  ;;
  ;; This module:
  ;; - configures default, variable-pitch, and emoji fonts
  ;; - applies OS-aware fallbacks and daemon-safe hooks
  ;; - optionally enables programming ligatures
  ;;
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
  ;;; ui/ui-nano-palette.el --- Nano-style color palette -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Single source of truth for Nano-style UI colors.
  ;;
  ;; This module:
  ;; - defines all palette entries as defcustom variables
  ;; - applies faces through a single normalization function
  ;; - avoids duplicated literal colors across the UI layer
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
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Theme orchestration and face normalization.
  ;;
  ;; This module:
  ;; - applies Nano-based themes and palettes
  ;; - re-normalizes faces after theme changes
  ;; - adjusts UI faces for dired-subtree and padding
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (require 'ui-nano-palette)

  (setq-default line-spacing 0.24)

  (defvar ui-theme--initialized nil
    "Non-nil once the Nano theme and faces have been initialized.")

  (defun ui-theme--apply ()
    "Apply Nano theme and related face customizations once."
    (unless ui-theme--initialized
      (setq ui-theme--initialized t)

      ;; Reapply faces after theme change
      (with-eval-after-load 'cus-theme
        (if (boundp 'enable-theme-functions)
            (add-hook 'enable-theme-functions #'my/nano--reapply-after-theme)
          (advice-add 'enable-theme :after #'my/nano--reapply-after-theme)))

      ;; Dired subtree faces
      (with-eval-after-load 'dired-subtree
        (set-face-attribute 'dired-subtree-depth-1-face nil :background "#e6f4ea")
        (set-face-attribute 'dired-subtree-depth-2-face nil :background "#dff0e3")
        (set-face-attribute 'dired-subtree-depth-3-face nil :background "#d8ecdc")
        (set-face-attribute 'dired-subtree-depth-4-face nil :background "#d1e8d5"))

      ;; Nano theme stack
      (require 'nano-layout)
      (require 'nano-faces)
      (nano-faces)

      (set-face-attribute 'nano-face-strong nil
                          :foreground (face-foreground 'nano-face-default)
                          :weight 'bold)

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
                                                :weight bold))))))

      ;; Padding
      (when (display-graphic-p)
        (spacious-padding-mode 1)
        (my/nano-apply-faces))))

  ;; Nano packages (definitions only; no eager work)
  (leaf nano-emacs
    :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs"))

  (leaf nano-theme
    :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))

  (leaf spacious-padding
    :straight t
    :if (display-graphic-p)
    :custom
    ((spacious-padding-widths . '((left . 15) (right . 15)))
     (spacious-padding-subtle-mode-line . t)))

  ;; Apply theme after first frame is ready
  (add-hook 'emacs-startup-hook #'ui-theme--apply)

  (provide 'ui-theme)
  ;;; ui/ui-theme.el ends here
#+end_src

***** ui/ui-doom-modeline.el
:PROPERTIES:
:CUSTOM_ID: ui-doom-modeline
:header-args:emacs-lisp: :tangle lisp/ui/ui-doom-modeline.el
:END:
#+begin_src emacs-lisp
  ;;; ui/ui-doom-modeline.el --- Doom modeline UI bundle -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Doom-modeline based mode-line UI bundle.
  ;;
  ;; This module:
  ;; - configures and enables doom-modeline
  ;; - integrates Nerd Icons for the mode-line
  ;; - exposes an explicit interactive enable command
  ;;
  ;;; Code:

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
  ;;; ui/ui-nano-modeline.el --- Nano modeline UI bundle -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Nano-modeline based lightweight mode-line bundle.
  ;;
  ;; This module:
  ;; - initializes nano-modeline lazily and safely
  ;; - activates mode-specific modeline hooks
  ;; - exposes an explicit interactive enable command
  ;;
  ;;; Code:

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
  ;;; ui/ui-health-modeline.el --- Session health indicators -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Mode-line indicators for Emacs session health.
  ;;
  ;; This module:
  ;; - displays buffer, process, and LSP counts
  ;; - updates indicators reactively via lightweight hooks
  ;; - introduces no control or policy decisions
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
  ;;; ui/ui-window.el --- Window management helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Window and layout management helpers.
  ;;
  ;; This module:
  ;; - enables zoom, winner, and desktop persistence
  ;; - provides interactive window layout save/restore
  ;; - avoids enforcing rigid window policies
  ;;
  ;;; Code:

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
  ;;; ui/ui-utils.el --- UI utilities and Treemacs configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Assorted UI-related utilities and panels.
  ;;
  ;; This module:
  ;; - configures mode-line helpers and system indicators
  ;; - integrates Treemacs and Dired icon helpers
  ;; - provides platform-specific clipboard support
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

***** ui/ui-imenu.el
:PROPERTIES:
:CUSTOM_ID: ui-imenu
:header-args:emacs-lisp: :tangle lisp/ui/ui-imenu.el
:END:
#+begin_src emacs-lisp
  ;;; ui/ui-imenu.el --- Imenu / outline UI integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; UI-level integration for Imenu-based navigation.
  ;;
  ;; This module:
  ;; - installs and autoloads imenu-list as an optional UI component
  ;; - does NOT define commands or workflows itself
  ;; - provides a stable dependency surface for utils-layer helpers
  ;;
  ;; Design notes:
  ;; - utils may opportunistically (require 'imenu-list nil 'noerror)
  ;; - this module owns the decision to install the package
  ;; - loading is deferred until explicitly used
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf))

  ;;;; Built-in -------------------------------------------------------------------

  (require 'imenu)

  ;;;; imenu-list -----------------------------------------------------------------
  ;; Optional sidebar-style Imenu UI.
  ;; Loaded only on demand via commands.

  (leaf imenu-list
    :straight t
    :commands
    (imenu-list-minor-mode
     imenu-list-display-dwim
     imenu-list-stop-timer)
    :custom
    ((imenu-list-position . 'left)
     (imenu-list-size . 36)
     (imenu-list-focus-after-activation . t)))

  (provide 'ui-imenu)
  ;;; ui/ui-imenu.el ends here
#+end_src

***** ui/ui-visual-aids.el
:PROPERTIES:
:CUSTOM_ID: ui-visual-aids
:header-args:emacs-lisp: :tangle lisp/ui/ui-visual-aids.el
:END:
#+begin_src emacs-lisp
  ;;; ui/ui-visual-aids.el --- Subtle visual helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Non-intrusive visual aids for editing and navigation.
  ;;
  ;; This module:
  ;; - highlights cursor movement and TODO keywords
  ;; - adds rainbow delimiters and indent guides
  ;; - avoids persistent or disruptive visual effects
  ;;
  ;;; Code:

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
  ;;; ui/ui-macos.el --- macOS-specific UI niceties -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: ui
  ;;
  ;; Commentary:
  ;; Small macOS-specific UI adjustments.
  ;;
  ;; This module:
  ;; - enables native title-bar integration on macOS
  ;; - avoids any effect on non-darwin systems
  ;;
  ;;; Code:

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
core semantics, file format, and data model.

This layer encapsulates all Org-specific customization, workflows,
and assumptions, ensuring that Org behavior remains isolated from
non-Org contexts and other subsystems.

Orgx exists to make Org usage *explicit, auditable, and modular*,
without turning Org into a bespoke or opaque system.

**** What this layer does
Orgx modules are responsible for **Org-mode–scoped behavior only**.

This layer:

- Extends Org-mode in a structured, non-invasive manner
- Defines Org-specific workflows, commands, and helper utilities
- Integrates Org with other subsystems *only at explicit boundaries*
- Centralizes agenda, capture, and navigation policy

Typical responsibilities include:

- Agenda file discovery, scoping rules, and caching
- Capture templates, inbox flows, and refiling strategy
- Org-specific navigation, folding, and structural helpers
- Visual and UX enhancements limited strictly to Org buffers
- Optional integration with roam-style or graph-based note systems

**** What this layer does *not* do
Orgx modules intentionally do **not**:

- Reimplement, fork, or override core Org internals
- Affect behavior in non-Org buffers
- Define global editing, completion, or UI policy
- Introduce development, VCS, or infrastructure concerns
- Encode user-, device-, or host-specific paths directly

Those responsibilities belong to other layers or to
=personal/= overlays.

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
  - discoverable through module boundaries
  - safe to disable as a group without data loss

**** Design principles
- Org remains **plain Org** on disk
  (no proprietary formats, sidecar metadata, or irreversible transforms)
- Extensions are additive, reversible, and optional
- Agenda and capture behavior is explicit and reviewable
- Org-specific UX must degrade gracefully when extensions are disabled
- Configuration favors transparency over clever automation

**** Benefits
This separation ensures that:

- Org files remain portable and future-proof
- Non-Org workflows are unaffected by Org customization
- Debugging Org behavior is localized and predictable
- Org extensions can evolve without destabilizing the rest of Emacs

**** Module map
| File | Responsibility |
|------+----------------|
| =orgx/org-core.el= | Core Org defaults and shared Org policy |
| =orgx/org-agenda.el= | Agenda discovery, scoping, and views |
| =orgx/org-capture.el= | Capture templates and inbox workflows |
| =orgx/org-navigation.el= | Navigation, folding, and structural helpers |
| =orgx/org-roam.el= | Roam-style linking and graph-based notes |
| =orgx/org-export.el= | Export configuration and backends |
| =orgx/org-ui.el= | Org-specific visual and UX enhancements |

**** Notes
- Orgx is the *only* layer allowed to assume Org semantics.
- Any logic that could apply outside Org likely belongs elsewhere.
- If an Org change affects global behavior, it is misplaced.

This layer exists to make Org powerful *without making it fragile*.

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
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Core Org Mode configuration and directory layout.
  ;;
  ;; This module:
  ;; - defines canonical Org directory structure
  ;; - configures agenda, capture, refile, and TODO workflows
  ;; - avoids UI presentation or export concerns
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
  ;;; orgx/orgx-visual.el --- Org visual enhancements -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Visual enhancements for Org buffers and agenda views.
  ;;
  ;; This module:
  ;; - enables org-modern for improved Org visuals
  ;; - customizes headings, lists, TODO states, and priorities
  ;; - adjusts Org Agenda visual presentation only
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (defvar orgx-visual--initialized nil
    "Non-nil once org-modern visual variables are initialized.")

  (defun orgx-visual--apply ()
    "Apply org-modern visual configuration."
    (unless orgx-visual--initialized
      (setq orgx-visual--initialized t)

      (setq
       org-startup-indented t
       org-hide-leading-stars t
       org-auto-align-tags nil
       org-tags-column 0
       org-catch-invisible-edits 'show-and-error
       org-special-ctrl-a/e t
       org-insert-heading-respect-content t
       org-hide-emphasis-markers t
       org-pretty-entities t

       org-modern-todo-faces
       '(("TODO"     :background "#673AB7" :foreground "#f8f8f2")
         ("SOMEDAY"  :background "#6b7280" :foreground "#f8f8f2")
         ("WAITING"  :background "#6272a4" :foreground "#f8f8f2")
         ("DONE"     :background "#373844" :foreground "#b0b8d1")
         ("CANCELED" :background "#4b5563" :foreground "#e5e7eb"))

       org-modern-list '((?+ . "◦") (?- . "–") (?* . "•"))
       org-modern-checkbox '((?X . "") (?- . "") (?\s . ""))
       org-modern-priority '((?A . "") (?B . "") (?C . ""))
       org-modern-replace-stars ""

       org-agenda-tags-column 0
       org-agenda-block-separator ?─
       org-agenda-time-grid
       '((daily today require-timed)
         (800 1000 1200 1400 1600 1800 2000)
         " ┄┄┄┄┄ " " ┄┄┄┄┄ ")
       org-agenda-current-time-string
       "⭠ now ─────────────────────────────────────────────────"))

    (require 'org-modern)
    (org-modern-mode 1))

  (leaf org-modern :straight t)

  (add-hook 'org-mode-hook #'orgx-visual--apply)

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
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Optional but production-safe Org extensions.
  ;;
  ;; This module:
  ;; - enables org-journal and org-roam with defensive guards
  ;; - delays org-roam autosync to avoid IO races
  ;; - integrates org-download, TOC, and link helpers
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
  ;;; orgx/orgx-fold.el --- Extra Org folding helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Convenience helpers for Org subtree folding.
  ;;
  ;; This module:
  ;; - wraps org-fold primitives with interactive helpers
  ;; - installs local keybindings in org-mode only
  ;; - avoids global keybinding pollution
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
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Export and publishing helpers for Org documents.
  ;;
  ;; This module:
  ;; - configures Hugo, Markdown, and preview exporters
  ;; - enables Org Babel languages for diagrams
  ;; - avoids altering core Org editing behavior
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

***** orgx/orgx-notes-markdown.el
:PROPERTIES:
:CUSTOM_ID: orgx-notes-markdown
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-notes-markdown.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-notes-markdown.el --- Markdown notes under Org -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Prose-oriented Markdown notes as a sibling domain under Org.
  ;;
  ;; This module:
  ;; - manages a Markdown notes directory outside agenda scope
  ;; - provides note creation, navigation, and search helpers
  ;; - configures Markdown UX optimized for prose
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  ;;; Customization ------------------------------------------------------------

  (defgroup my:notes nil
    "Prose-oriented Markdown notes under Org directory."
    :group 'convenience)

  (defcustom my:notes-directory-name
    "notes"
    "Directory name for Markdown notes under `my:d:org`."
    :type 'string)

  (defcustom my:notes-default-notebooks
    '("inbox" "work" "life" "ideas" "archive")
    "Default notebook subdirectories under the notes directory."
    :type '(repeat string))

  ;;; Derived paths ------------------------------------------------------------

  (defvar my:d:notes
    (expand-file-name my:notes-directory-name my:d:org)
    "Root directory for Markdown notes.

  This directory is a sibling of Org files, not part of agenda scope.")

  ;;; Internal helpers ---------------------------------------------------------

  (defun my/notes--ensure-root ()
    "Ensure `my:d:notes` and default notebooks exist."
    (unless (file-directory-p my:d:notes)
      (make-directory my:d:notes t))
    (dolist (name my:notes-default-notebooks)
      (let ((dir (expand-file-name name my:d:notes)))
        (unless (file-directory-p dir)
          (make-directory dir t)))))

  (defun my/notes--slugify (title)
    "Return a filesystem-safe slug derived from TITLE."
    (let* ((lower (downcase title))
           (repl  (replace-regexp-in-string "[^[:alnum:]]+" "-" lower)))
      (string-trim repl "-+" "-+")))

  ;;; Note creation & navigation ----------------------------------------------

  (defun my/notes-new-note (notebook title)
    "Create a new Markdown note in NOTEBOOK with TITLE.

  NOTEBOOK is a subdirectory under `my:d:notes`."
    (interactive
     (progn
       (my/notes--ensure-root)
       (let* ((choices (directory-files my:d:notes nil "^[^.]"))
              (notebook (completing-read "Notebook: " choices nil nil "inbox"))
              (title (read-string "Title: ")))
         (list notebook title))))
    (my/notes--ensure-root)
    (let* ((dir (expand-file-name notebook my:d:notes))
           (slug (my/notes--slugify title))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (filename (format "%s-%s.md" timestamp slug))
           (path (expand-file-name filename dir)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (find-file path)
      (when (= (buffer-size) 0)
        (insert
         (format
          "---\n\
  title: %s\n\
  notebook: %s\n\
  tags: []\n\
  created: %s\n\
  updated: %s\n\
  ---\n\n"
          title
          notebook
          (format-time-string "%Y-%m-%d")
          (format-time-string "%Y-%m-%d")))
        (save-buffer))))

  (defun my/notes-open-root ()
    "Open the Markdown notes root under Org in Dired."
    (interactive)
    (my/notes--ensure-root)
    (dired my:d:notes))

  ;;; Markdown UX --------------------------------------------------------------

  (leaf markdown-mode
    :straight t
    :mode (("\\.md\\'" . gfm-mode)
           ("README\\.md\\'" . gfm-mode))
    :hook
    ((markdown-mode . visual-line-mode)
     (markdown-mode . variable-pitch-mode)
     (markdown-mode . my/notes-markdown-visual-fill))
    :custom
    ((markdown-command . "pandoc")
     (markdown-fontify-code-blocks-natively . t))
    :config
    (defun my/markdown-toggle-live-preview ()
      "Toggle `markdown-live-preview-mode` in the current buffer."
      (interactive)
      (if (bound-and-true-p markdown-live-preview-mode)
          (markdown-live-preview-mode -1)
        (markdown-live-preview-mode 1)))
    (define-key markdown-mode-map (kbd "C-c C-p")
                #'my/markdown-toggle-live-preview))

  (leaf visual-fill-column
    :straight nil
    :commands (visual-fill-column-mode)
    :init
    (defun my/notes-markdown-visual-fill ()
      "Configure `visual-fill-column` for prose Markdown buffers."
      (setq-local visual-fill-column-width 100)
      (setq-local visual-fill-column-center-text t)
      (visual-fill-column-mode 1)))

  ;;; Search & navigation ------------------------------------------------------

  (leaf consult-notes
    :straight t
    :after consult
    :require t
    :init
    (defun my/notes-consult-ripgrep ()
      "Run `consult-ripgrep` scoped to Markdown notes under Org."
      (interactive)
      (my/notes--ensure-root)
      (let ((default-directory my:d:notes))
        (consult-ripgrep)))
    :config
    (setq consult-notes-file-dir-sources
          (list
           (list "Org Notes (MD)" ?m my:d:notes))))

  ;;; Image handling -----------------------------------------------------------

  (with-eval-after-load 'org-download
    (defun my/notes-markdown-org-download-setup ()
      "Enable `org-download` for Markdown notes under Org."
      (setq-local org-download-link-format "![](%s)")
      (setq-local org-download-image-dir "./images")
      (org-download-enable))
    (add-hook 'markdown-mode-hook
              #'my/notes-markdown-org-download-setup))

  (provide 'orgx-notes-markdown)
  ;;; orgx/orgx-notes-markdown.el ends here
#+end_src
