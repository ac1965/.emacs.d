README.org part 2/3
--------------------

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

***** core/editing.el
:PROPERTIES:
:CUSTOM_ID: core-editing
:header-args:emacs-lisp: :tangle lisp/core/editing.el
:END:
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

***** core/switches.el
:PROPERTIES:
:CUSTOM_ID: core-switches
:header-args:emacs-lisp: :tangle lisp/core/switches.el
:END:
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

***** core/custom.el
:PROPERTIES:
:CUSTOM_ID: custom-file
:header-args:emacs-lisp: :tangle lisp/core/custom.el
:END:
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

***** core/custom-ui-extras.el
:PROPERTIES:
:CUSTOM_ID: core-custom-ui-extras
:header-args:emacs-lisp: :tangle lisp/core/custom-ui-extras.el
:END:
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

*** ui/
:PROPERTIES:
:CUSTOM_ID: ui-modules
:END:

**** Purpose
Define the **visual presentation and interaction layer** of Emacs.

This category controls how Emacs looks and feels, without affecting semantics.

**** What it does
- Configures themes, faces, layout, and visual feedback
- Adjusts interaction-related defaults (cursor, modeline, spacing)
- Applies appearance customizations independent of functionality

**** Notes
- ui modules must not introduce functional dependencies
- Behavior must remain safe in terminal and batch contexts
- Visual changes must be reversible and localized

**** Implementation
- Loaded after core
- Does not define business logic or data structures
- Relies only on guarantees established by core

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

*** completion/
:PROPERTIES:
:CUSTOM_ID: completion-modules
:END:

**** Purpose
Provide a **coherent and explicit completion architecture**.

This category centralizes completion behavior to avoid fragmentation.

**** What it does
- Defines completion frameworks and orchestration
- Manages CAPF composition and precedence
- Establishes consistent completion UX across modes

**** Notes
- completion modules may depend on core and ui
- They must not assume the presence of orgx or dev
- Completion behavior should be deterministic and debuggable

**** Implementation
- Loaded after ui
- Completion sources are registered explicitly
- Implicit global side effects are avoided

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

*** orgx/
:PROPERTIES:
:CUSTOM_ID: orgx-modules
:END:

**** Purpose
Extend Org-mode beyond upstream defaults while preserving its core semantics.

This category isolates Org-specific customization and extensions.

**** What it does
- Adds Org-mode utilities, workflows, and integrations
- Customizes Org behavior without forking upstream logic
- Encapsulates Org-related assumptions

**** Notes
- orgx modules may depend on core, ui, and completion
- They must not leak Org assumptions into non-Org buffers
- Agenda and capture behavior must remain inspectable

**** Implementation
- Loaded after completion
- All Org extensions live exclusively under this category
- Feature boundaries follow Org concepts, not file size

***** orgx/org-core.el
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

***** orgx/org-visual.el
:PROPERTIES:
:CUSTOM_ID: orgx-visual
:header-args:emacs-lisp: :tangle lisp/orgx/org-visual.el
:END:
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

***** orgx/org-extensions.el
:PROPERTIES:
:CUSTOM_ID: orgx-extensions
:header-args:emacs-lisp: :tangle lisp/orgx/org-extensions.el
:END:

****** Purpose
Provide **optional but production-safe extensions** for Org workflows.

****** What it does
- Enables org-journal and org-roam
- Adds defensive guards for org-roam hashing
- Delays org-roam autosync to avoid IO races
- Adds helpers for downloads, TOC, and link capture

****** Notes
- org-roam autosync is intentionally delayed
- Guards avoid crashes on network/iCloud filesystems

****** Implementation

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

***** orgx/org-export.el
:PROPERTIES:
:CUSTOM_ID: orgx-export
:header-args:emacs-lisp: :tangle lisp/orgx/org-export.el
:END:

****** Purpose
Provide a **robust Org export pipeline** for Hugo, Markdown, and diagrams.

****** What it does
- Configures ox-hugo and Hugo capture templates
- Enables Markdown editing and preview
- Enables Mermaid and Graphviz in Org Babel
- Provides Hugo draft review helpers

****** Notes
- Hugo filenames are generated safely and deterministically
- Mermaid requires `mmdc` executable

****** Implementation

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

***** orgx/org-typography.el
:PROPERTIES:
:CUSTOM_ID: orgx-typography
:header-args:emacs-lisp: :tangle lisp/orgx/org-typography.el
:END:

****** Purpose
Provide **modern typography enhancements** for Org buffers without
overlapping with structural or decorative visuals.

This module focuses on *readability and flow* of prose-heavy Org documents,
complementing `orgx/org-visual.el` (org-modern).

****** What it does
- Aligns Org tables and inline images visually using `valign`
- Reveals emphasis markers, links, and sub/superscripts contextually
  using `org-appear`
- Improves prose editing experience while keeping code blocks untouched

****** Notes
- This module is intentionally **orthogonal** to `org-modern`
- No faces, icons, or agenda styling are defined here
- Variable-pitch support is documented but disabled by default
- Safe to enable in long-running sessions

****** Implementation

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

*** dev/
:PROPERTIES:
:CUSTOM_ID: dev-modules
:END:

**** Purpose
Support **development, diagnostics, and build-related workflows**.

This category exists for maintainers, not day-to-day editing.

**** What it does
- Provides tooling for development and inspection
- Adds helpers for build, compile, and debug tasks
- Supports configuration maintenance activities

**** Notes
- dev modules must never be required for normal editing
- Side effects must be opt-in and explicit
- Safe to disable entirely in production usage

**** Implementation
- Loaded after orgx
- Commands are interactive and discoverable
- No hard dependency from core, ui, completion, or orgx

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
****** Implementation

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

***** dev/dev-rest.el
:PROPERTIES:
:CUSTOM_ID: dev-rest
:header-args:emacs-lisp: :tangle lisp/dev/dev-rest.el
:END:
#+begin_src emacs-lisp
  ;;; dev-rest.el --- REST client helpers (Next.js / Ollama / Qdrant) -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; REST client integration for testing HTTP requests inside Emacs.
  ;; Supports JSON parsing with jq.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf restclient :straight t :mode ("\\.http\\'" . restclient-mode))
  (leaf restclient-jq :straight t :after restclient)

  (provide 'dev-rest)
  ;;; dev/dev-rest.el ends here
#+end_src

*** vcs/
:PROPERTIES:
:CUSTOM_ID: vcs-modules
:END:

**** Purpose
Provide a **consistent and inspectable version control interaction layer**
within Emacs.

This category encapsulates all integrations with external version control
systems.

**** What it does
- Integrates Emacs with version control backends (e.g. Git)
- Provides interactive commands for common VCS workflows
- Exposes status, diff, log, and history information inside Emacs buffers

**** Notes
- vcs modules must not be required for basic editing or startup
- They may depend on core, ui, and completion
- They must not impose workflow decisions on the user
- External tools and repositories are treated as authoritative
- Failure or absence of VCS backends must degrade gracefully

**** Implementation
- Loaded explicitly by =modules.el=
- Each file corresponds to one VCS-related concern
- Integration is achieved via well-defined external interfaces
- No project-specific assumptions are embedded

***** vcs/vcs-magit.el
:PROPERTIES:
:CUSTOM_ID: vcs-magit
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-magit.el
:END:
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

***** vcs/vcs-gutter.el
:PROPERTIES:
:CUSTOM_ID: vcs-gutter
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-gutter.el
:END:
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

***** vcs/vcs-forge.el
:PROPERTIES:
:CUSTOM_ID: vcs-forge
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-forge.el
:END:
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

*** utils/
:PROPERTIES:
:CUSTOM_ID: utils-modules
:END:

**** Purpose
Host **small, domain-specific utilities** that do not justify a standalone layer.

This category is intentionally heterogeneous.

**** What it does
- Provides narrowly scoped helper functions and tools
- Solves local problems without expanding global abstractions
- Complements higher-level modules without owning workflows

**** Notes
- utils modules must remain small and focused
- Promotion to another category should be considered if scope grows
- Dependencies should be minimal and explicit

**** Implementation
- Loaded last by =modules.el=
- No other category may depend on utils implicitly
- Utilities are optional and loosely coupled

***** utils/utils-functions.el
:PROPERTIES:
:CUSTOM_ID: utils-functions
:header-args:emacs-lisp: :tangle lisp/utils/utils-functions.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-functions.el --- General utility functions -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Small, safe utilities loaded across the configuration.
  ;; - Org helpers: indirect subtree view, quick sidebar (imenu-list)
  ;; - Editing helpers: smart buffer/window killing
  ;; Notes:
  ;; - Avoid compile warnings by declaring optional deps and vars.
  ;; - Prefer modern Org APIs when available.
  ;;
  ;;; Code:

  ;;;; Built-ins ------------------------------------------------------------------

  (require 'imenu) ;; built-in

  ;;;; Optional deps & vars (for byte-compiler) -----------------------------------

  (eval-when-compile
    (declare-function imenu-list-minor-mode  "imenu-list")
    (declare-function imenu-list-stop-timer  "imenu-list")
    (declare-function imenu-list-display-dwim "imenu-list")
    (declare-function nano-modeline-render   "nano-modeline"))

  (defvar imenu-list-after-jump-hook nil)
  (defvar imenu-list-position 'left)
  (defvar imenu-list-size 30)
  (defvar imenu-list-focus-after-activation t)
  (defvar imenu-list--displayed-buffer nil)

  ;;;; Editing helpers -------------------------------------------------------------

  (defun my/kill-buffer-smart ()
    "Kill buffer and window when there are multiple windows; otherwise kill buffer."
    (interactive)
    (if (one-window-p)
        (kill-buffer)
      (kill-buffer-and-window)))

  (defalias 'my/smart-kill-buffer #'my/kill-buffer-smart)

  ;;;; Nano/Modeline helper --------------------------------------------------------

  (defun my/nano-headerline (buf subtitle)
    "Return a header-line string; prefer nano-modeline when available."
    (let* ((name (if (and buf (buffer-live-p buf))
                     (buffer-name buf)
                   (buffer-name)))
           (extra ""))
      (if (fboundp 'nano-modeline-render)
          (nano-modeline-render nil name subtitle extra)
        (concat "  "
                (propertize name 'face 'mode-line-buffer-id)
                "  " subtitle))))

  ;;;; Org helpers ----------------------------------------------------------------

  (defun my/org-tree-to-indirect-buffer ()
    "Show current Org subtree in an indirect buffer and reveal its content."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (cond
       ((fboundp 'org-fold-show-all) (org-fold-show-all))
       ((fboundp 'org-show-all)      (org-show-all))))
    (org-tree-to-indirect-buffer))

  (defun my/org-sidebar ()
    "Open an imenu-list sidebar with safe fallbacks."
    (interactive)
    (unless (locate-library "imenu-list")
      (user-error "imenu-list is not installed"))
    (autoload 'imenu-list-minor-mode  "imenu-list" nil t)
    (autoload 'imenu-list-stop-timer  "imenu-list" nil t)
    (autoload 'imenu-list-display-dwim "imenu-list" nil t)

    (setq imenu-list-after-jump-hook #'recenter
          imenu-list-position 'left
          imenu-list-size 36
          imenu-list-focus-after-activation t)

    (when (buffer-base-buffer)
      (switch-to-buffer (buffer-base-buffer)))

    (imenu-list-minor-mode 1)
    (when (fboundp 'imenu-list-stop-timer)
      (imenu-list-stop-timer))

    (hl-line-mode 1)
    (when (facep 'nano-subtle)
      (face-remap-add-relative 'hl-line :inherit 'nano-subtle))

    (setq header-line-format
          `(:eval
            (my/nano-headerline
             ,(when (boundp 'imenu-list--displayed-buffer)
                'imenu-list--displayed-buffer)
             "(outline)")))
    (setq-local cursor-type nil)

    (when (fboundp 'imenu-list-display-dwim)
      (imenu-list-display-dwim)))

  (defun my/org-sidebar-toggle ()
    "Toggle the imenu-list sidebar."
    (interactive)
    (let ((win (get-buffer-window "*Ilist*")))
      (if win
          (progn
            (quit-window nil win)
            (when (buffer-base-buffer)
              (switch-to-buffer (buffer-base-buffer))))
        (my/org-sidebar))))

  (provide 'utils-functions)
  ;;; utils/utils-functions.el ends here
#+end_src

***** utils/utils-async.el
:PROPERTIES:
:CUSTOM_ID: utils-async
:header-args:emacs-lisp: :tangle lisp/utils/utils-async.el
:END:
#+begin_src emacs-lisp
  ;;; utils-async.el --- Safe async task helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Provide a minimal helper for running asynchronous tasks safely.
  ;; Errors are caught and reported without affecting Emacs stability.
  ;;
  ;;; Code:

  (defun my/safe-run-async (task)
    "Run TASK asynchronously, catching and reporting any errors."
    (run-at-time 0 nil
                 (lambda ()
                   (condition-case err
                       (funcall task)
                     (error (message "[async] error: %s" err))))))

  (provide 'utils-async)
  ;;; utils/utils-async.el ends here
#+end_src

***** utils/utils-gc.el
:PROPERTIES:
:CUSTOM_ID: utils-gc
:header-args:emacs-lisp: :tangle lisp/utils/utils-gc.el
:END:
#+begin_src emacs-lisp
  ;;; utils-gc.el --- Safe garbage collection helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Run garbage collection at safe moments during long-running sessions.
  ;; Triggered on focus-out and minibuffer exit only.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf nil
    :straight nil
    :init
    (defcustom utils-gc-enable-p t
      "Enable GC hooks for long-running sessions."
      :type 'boolean
      :group 'utils)

    (defun utils-gc--collect ()
      "Run garbage collection safely."
      (when utils-gc-enable-p
        (condition-case _err
            (garbage-collect)
          (error nil))))

    (add-hook 'focus-out-hook #'utils-gc--collect)
    (add-hook 'minibuffer-exit-hook #'utils-gc--collect))

  (provide 'utils-gc)
  ;;; utils/utils-gc.el ends here
#+end_src

***** utils/utils-backup.el
:PROPERTIES:
:CUSTOM_ID: utils-backup
:header-args:emacs-lisp: :tangle lisp/utils/utils-backup.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-backup.el --- Backup and auto-save helpers -*- lexical-binding: t; -*-
  ;;
  ;; Category: utils
  ;;
  ;;; Code:

  (defun my/delete-old-backups ()
    "Delete backup files older than 7 days."
    (interactive)
    (let ((backup-dir (concat no-littering-var-directory "backup/"))
          (threshold (- (float-time (current-time)) (* 7 24 60 60))))
      (when (file-directory-p backup-dir)
        (dolist (file (directory-files backup-dir t))
          (when (and (file-regular-p file)
                     (< (float-time
                         (file-attribute-modification-time
                          (file-attributes file)))
                        threshold))
            (delete-file file))))))

  (add-hook 'emacs-startup-hook #'my/delete-old-backups)

  (provide 'utils-backup)
  ;;; utils/utils-backup.el ends here
#+end_src

***** utils/utils-buffers.el
:PROPERTIES:
:CUSTOM_ID: utils-buffers
:header-args:emacs-lisp: :tangle lisp/utils/utils-buffers.el
:END:
#+begin_src emacs-lisp
  ;;; utils-buffers.el --- Automatic buffer housekeeping -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Automatically clean up temporary and dead-process buffers.
  ;; Visible or modified buffers are never touched.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf nil
    :straight nil
    :init
    (defcustom utils-buffers-enable-p t
      "Enable automatic buffer housekeeping."
      :type 'boolean
      :group 'utils)

    (defvar utils-buffers--temporary-regexp
      (rx string-start "*"
          (or "Help" "Warnings" "Compile-Log" "Backtrace"
              "Async-native-compile-log" "eglot-events")
          (* any) string-end))

    (defun utils-buffers--temporary-p (buffer)
      (with-current-buffer buffer
        (and (string-match-p utils-buffers--temporary-regexp (buffer-name))
             (not (buffer-file-name))
             (not (buffer-modified-p))
             (not (get-buffer-window buffer 'visible)))))

    (defun utils-buffers--dead-process-p (buffer)
      (let ((proc (get-buffer-process buffer)))
        (and proc
             (memq (process-status proc) '(exit signal)))))

    (defun utils-buffers-cleanup ()
      "Clean up temporary and dead-process buffers."
      (interactive)
      (when utils-buffers-enable-p
        (dolist (buf (buffer-list))
          (when (or (utils-buffers--temporary-p buf)
                    (utils-buffers--dead-process-p buf))
            (ignore-errors
  	    (kill-buffer buf))))))

    (run-with-timer 900 900 #'utils-buffers-cleanup))

  (provide 'utils-buffers)
  ;;; utils/utils-buffers.el ends here
#+end_src

***** utils/utils-org-agenda.el
:PROPERTIES:
:CUSTOM_ID: utils-org-agenda
:header-args:emacs-lisp: :tangle lisp/utils/utils-org-agenda.el
:END:
#+begin_src emacs-lisp
  ;;; utils-org-agenda.el --- Cached org-agenda-files builder -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Build org-agenda-files using a persistent on-disk cache.
  ;; Avoids expensive recursive scans on every startup.
  ;;
  ;;; Code:

  (require 'subr-x)
  (require 'seq)

  (defgroup utils-org-agenda nil
    "Cached org-agenda-files builder."
    :group 'org)

  (defcustom utils-org-agenda-cache-file
    (expand-file-name "org-agenda-files.cache" user-emacs-directory)
    "Cache file for org-agenda-files."
    :type 'file
    :group 'utils-org-agenda)

  (defcustom utils-org-agenda-exclude-regexp
    "archives"
    "Regexp used to exclude files from org-agenda."
    :type 'regexp
    :group 'utils-org-agenda)

  (defun utils-org-agenda--scan (org-directory)
    "Recursively scan ORG-DIRECTORY and return a list of agenda files."
    (seq-filter
     (lambda (file)
       (and (string-match-p "\\.org\\'" file)
            (not (file-symlink-p file))
            (not (string-match-p utils-org-agenda-exclude-regexp file))))
     (directory-files-recursively
      org-directory "\\.org\\'" nil nil)))

  (defun utils-org-agenda--load-cache ()
    "Load cached agenda files from disk."
    (when (file-readable-p utils-org-agenda-cache-file)
      (with-temp-buffer
        (insert-file-contents utils-org-agenda-cache-file)
        (read (current-buffer)))))

  (defun utils-org-agenda--save-cache (files)
    "Save FILES to the agenda cache."
    (with-temp-file utils-org-agenda-cache-file
      (prin1 files (current-buffer))))

  ;;;###autoload
  (defun utils-org-agenda-build (&optional force)
    "Return agenda files using the cache.
  If FORCE is non-nil, rebuild the cache."
    (let ((cached (and (not force)
                       (utils-org-agenda--load-cache))))
      (if (and cached (listp cached))
          cached
        (let ((files (utils-org-agenda--scan org-directory)))
          (utils-org-agenda--save-cache files)
          files))))

  ;;;###autoload
  (defun utils-org-agenda-rebuild ()
    "Force a rebuild of the org-agenda-files cache."
    (interactive)
    (setq org-agenda-files (utils-org-agenda-build t))
    (message "org-agenda-files cache rebuilt (%d files)"
             (length org-agenda-files)))

  (provide 'utils-org-agenda)
  ;;; utils-org-agenda.el ends here
#+end_src

***** utils/utils-search-nav.el
:PROPERTIES:
:CUSTOM_ID: search-nav
:header-args:emacs-lisp: :tangle lisp/utils/utils-search-nav.el
:END:
#+begin_src emacs-lisp
  ;;; utils-search-nav.el --- Search and navigation helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Project-wide search and navigation helpers.
  ;; Built on ripgrep, dumb-jump, and lightweight EWW integration.
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  (leaf dumb-jump
    :straight t
    :hook (xref-backend-functions . dumb-jump-xref-activate)
    :custom ((dumb-jump-force-searcher  . 'rg)
             (dumb-jump-prefer-searcher . 'rg)))

  (leaf multiple-cursors :straight t)

  (leaf eww
    :straight nil
    :custom ((eww-search-prefix . "https://duckduckgo.com/html/?kl=jp-jp&k1=-1&kc=1&kf=-1&q=")
             (eww-download-directory . "~/Downloads"))
    :init
    (defvar my:d:eww
      (expand-file-name "eww/"
                        (or (bound-and-true-p my:d:var)
                            user-emacs-directory)))
    (setopt eww-cache-directory (expand-file-name "cache/" my:d:eww))
    (my/ensure-directory-exists eww-cache-directory)
    (setq eww-history-limit 200)

    (defvar eww-hl-search-word nil)

    (defun my/eww-search (term)
      "Search TERM with EWW and start isearch."
      (interactive "sSearch terms: ")
      (setq eww-hl-search-word term)
      (eww-browse-url (concat eww-search-prefix term)))

    (add-hook 'eww-after-render-hook
              (lambda ()
                (when eww-hl-search-word
                  (isearch-mode t)
                  (isearch-yank-string eww-hl-search-word)
                  (setq eww-hl-search-word nil))))

    (defun my/eww-toggle-images ()
      (interactive)
      (setq shr-inhibit-images (not shr-inhibit-images))
      (eww-reload)))

  (provide 'utils-search-nav)
  ;;; utils-search-nav.el ends here
#+end_src
