README.org part 3/3
--------------------

***** utils/utils-notes-markdown.el
:PROPERTIES:
:CUSTOM_ID: utils-notes-markdown
:header-args:emacs-lisp: :tangle lisp/utils/utils-notes-markdown.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-notes-markdown.el --- Prose-oriented Markdown notes under Org -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;;
  ;; This module provides a prose-oriented Markdown notes system,
  ;; implemented as a sibling domain under `my:d:org`.
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

  (provide 'utils-notes-markdown)
  ;;; utils/utils-notes-markdown.el ends here
#+end_src

***** utils/utils-scratch.el
:PROPERTIES:
:CUSTOM_ID: utils-scratch
:header-args:emacs-lisp: :tangle lisp/utils/utils-scratch.el
:END:
#+begin_src emacs-lisp
  ;;; utils-scratch.el --- Persistent *scratch* buffer helper -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Ensure that *scratch* buffer always exists.
  ;; Automatically recreated when killed.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf my:scratch-auto-recreate
    :straight nil
    :init
    (defun my/create-scratch-buffer ()
      "Create or reset a `*scratch*` buffer."
      (let ((buf (get-buffer-create "*scratch*")))
        (with-current-buffer buf
          (lisp-interaction-mode)
          (erase-buffer)
          (insert ";; This is a new *scratch* buffer\n\n"))
        buf))

    (defun my/kill-scratch-buffer-advice (buf)
      "If BUF is *scratch*, recreate it shortly after kill."
      (when (string= (buffer-name buf) "*scratch*")
        (run-at-time 0 nil #'my/create-scratch-buffer)))

    (add-hook 'kill-buffer-hook
              (lambda ()
                (my/kill-scratch-buffer-advice (current-buffer)))))

  (provide 'utils-scratch)
  ;;; utils/utils-scratch.el ends here
#+end_src

***** utils/utils-lsp.el
:PROPERTIES:
:CUSTOM_ID: utils-lsp
:header-args:emacs-lisp: :tangle lisp/utils/utils-lsp.el
:END:
#+begin_src emacs-lisp
  ;;; utils-lsp.el --- LSP lifecycle cleanup helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Manage Eglot server lifecycle when switching projects.
  ;; Obsolete servers are shut down defensively.
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf nil
    :straight nil
    :init
    (defcustom utils-lsp-enable-p t
      "Enable LSP lifecycle cleanup."
      :type 'boolean
      :group 'utils)

    (defvar utils-lsp--current-project-root nil)

    (defun utils-lsp--project-root ()
      (when-let* ((project (project-current nil))
  		(roots (project-roots project)))
        (car roots)))

    (defun utils-lsp-on-project-switch ()
      "Shutdown obsolete eglot servers on project switch."
      (when utils-lsp-enable-p
        (let ((new-root (utils-lsp--project-root)))
          (when (and utils-lsp--current-project-root
                     new-root
                     (not (string-equal utils-lsp--current-project-root new-root))
                     (featurep 'eglot))
            (dolist (server eglot--servers)
              (when (string-prefix-p utils-lsp--current-project-root
                                     (eglot--project-root (cdr server)))
                (ignore-errors
                  (eglot-shutdown (cdr server))))))
          (setq utils-lsp--current-project-root new-root))))

    (add-hook 'find-file-hook #'utils-lsp-on-project-switch)
    (add-hook 'project-switch-project-hook #'utils-lsp-on-project-switch))

  (provide 'utils-lsp)
  ;;; utils/utils-lsp.el ends here
#+end_src

***** utils/utils-lint.el
:PROPERTIES:
:CUSTOM_ID: utils-lint
:header-args:emacs-lisp: :tangle lisp/utils/utils-lint.el
:END:
#+begin_src emacs-lisp
  ;;; utils-lint.el --- Static lint helpers for Emacs Lisp -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Provide static linting helpers for Emacs Lisp modules.
  ;;
  ;; This module integrates:
  ;; - checkdoc: documentation and header/comment conventions
  ;; - package-lint: package metadata and public API validation
  ;;
  ;; All commands are interactive and never modify files.
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  ;; ---------------------------------------------------------------------------
  ;; Optional dependencies (declared defensively)
  ;; ---------------------------------------------------------------------------

  (declare-function checkdoc-current-buffer "checkdoc")
  (declare-function checkdoc-file           "checkdoc")
  (declare-function package-lint-current-buffer "package-lint")
  (declare-function package-lint-buffer     "package-lint")

  ;; ---------------------------------------------------------------------------
  ;; Customization
  ;; ---------------------------------------------------------------------------

  (defgroup utils-lint nil
    "Static lint helpers for Emacs Lisp."
    :group 'lisp)

  (defcustom utils-lint-enable-package-lint-p t
    "Enable package-lint checks when available."
    :type 'boolean
    :group 'utils-lint)

  ;; ---------------------------------------------------------------------------
  ;; Helpers
  ;; ---------------------------------------------------------------------------

  (defun utils-lint--elisp-file-p (file)
    "Return non-nil if FILE looks like an Emacs Lisp file."
    (and (stringp file)
         (string-match-p "\\.el\\'" file)
         (file-regular-p file)))

  (defun utils-lint--ensure-elisp-buffer ()
    "Signal an error unless current buffer is visiting an Emacs Lisp file."
    (unless (and buffer-file-name
                 (utils-lint--elisp-file-p buffer-file-name))
      (user-error "Not visiting an Emacs Lisp file")))

  ;; ---------------------------------------------------------------------------
  ;; Public commands
  ;; ---------------------------------------------------------------------------

  ;;;###autoload
  (defun utils-lint-checkdoc-current-buffer ()
    "Run checkdoc on the current Emacs Lisp buffer."
    (interactive)
    (utils-lint--ensure-elisp-buffer)
    (require 'checkdoc)
    (checkdoc-current-buffer)
    (message "checkdoc completed"))

  ;;;###autoload
  (defun utils-lint-package-lint-current-buffer ()
    "Run package-lint on the current Emacs Lisp buffer, if available."
    (interactive)
    (utils-lint--ensure-elisp-buffer)
    (unless utils-lint-enable-package-lint-p
      (user-error "package-lint is disabled by customization"))
    (unless (require 'package-lint nil t)
      (user-error "package-lint is not installed"))
    (package-lint-current-buffer)
    (message "package-lint completed"))

  ;;;###autoload
  (defun utils-lint-run-all ()
    "Run checkdoc and package-lint on the current Emacs Lisp buffer."
    (interactive)
    (utils-lint-checkdoc-current-buffer)
    (when (and utils-lint-enable-package-lint-p
               (require 'package-lint nil t))
      (utils-lint-package-lint-current-buffer)))

  ;; ---------------------------------------------------------------------------
  ;; Batch / CI helper
  ;; ---------------------------------------------------------------------------

  ;;;###autoload
  (defun utils-lint-run-on-directory (directory)
    "Run checkdoc and package-lint on all .el files under DIRECTORY."
    (interactive "DDirectory: ")
    (let ((files (directory-files-recursively directory "\\.el\\'")))
      (unless files
        (message "No .el files found"))
      (dolist (file files)
        (when (utils-lint--elisp-file-p file)
          (with-current-buffer (find-file-noselect file)
            (condition-case err
                (progn
                  (checkdoc-current-buffer)
                  (when (and utils-lint-enable-package-lint-p
                             (require 'package-lint nil t))
                    (package-lint-current-buffer)))
              (error
               (message "[lint] %s: %s"
                        (file-name-nondirectory file)
                        (error-message-string err))))
            ;; IMPORTANT: must be inside with-current-buffer
            (kill-buffer (current-buffer))))))
    (message "Lint run completed"))

  (provide 'utils-lint)
  ;;; utils-lint.el ends here
#+end_src

** Personal Profile & Device Integrations — `user.el`
:PROPERTIES:
:CUSTOM_ID: personal-profile-and-device-integrations
:END:

*** Overview

**** Personal Layer Philosophy

The =personal/= layer provides *user- and device-specific overlays* on top of the
shared, version-controlled configuration.

It exists to express *identity, environment, and workflow glue* without
influencing global policy or shared behavior.

This layer MAY contain:
- Identity information (name, email)
- Feature flags and personal thresholds (UI/LSP selection, timing knobs)
- Device- and OS-specific glue (input methods, mouse/scroll tuning)
- Personal keybindings and workflow integrations

This layer MUST NOT contain:
- Core architectural or cross-user decisions
- Shared defaults or policy
- Modules that other layers depend on

Hooks and timers are permitted *only when they are strictly local to the user’s
device or workflow*, degrade safely to no-ops when unavailable, and do not
affect global behavior.

**** Purpose

Provide *personal overlays* that adapt the configuration to a specific user and
machine, without compromising modularity or reproducibility of shared layers.

Concretely, this layer covers:
- Identity and safe editor defaults.
- Preferred fonts and sizes, and *global switches* for UI (=nano=) and LSP (=eglot=).
- A portable Org directory layout rooted in the user’s cloud path.
- macOS conveniences:
  - Input-method auto-switching (English ⇄ Japanese) via =sis=,
  - Apple Music control via AppleScript and Hydra.
- Device-specific pointer and scroll tuning (Logitech MX Ergo profile).
- Small QoL glue (cursor color normalization after theme changes, load-path hygiene).

**** What this configuration does

- *Identity & safety*
  - Sets =user-full-name= and =user-mail-address=.
  - Disables font cache compaction on macOS and enables passphrase caching for
    =plstore=.

- *Look & feel switches*
  - Declares personal font preferences (=my:font-*=) and default size.
  - Selects UI and LSP stacks via =my:use-ui= and =my:use-lsp=.

- *Directories & Org wiring*
  - Defines a cloud root at =~/Documents= and derives =my:d:org= and =my:d:blog=.
  - Ensures required directories exist.
  - Sets =org-directory= and computes =org-agenda-files= by scanning non-archive
    =.org= files.
  - Removes sensitive paths from =load-path=.

- *macOS input method (=sis=)*
  - Configures ABC ⇄ Kotoeri (Romaji) via =macism= at startup.
  - Enables cursor-color, respect, and inline modes when available.

- *Cursor color keep-alive*
  - Re-applies the frame cursor color to the =cursor= face after theme reloads.

- *Device profile (MX Ergo S)*
  - Smooth scrolling, conservative movement, margin preservation, tilt scrolling.
  - Mouse bindings:
    - =mouse-2= → =yank=
    - =mouse-4/5= → previous/next buffer

- *Apple Music integration (macOS)*
  - Async and sync AppleScript helpers.
  - Interactive commands for play/pause, next/previous track, playlist playback,
    and current track info.
  - A Hydra bound to =C-c M=, with optional Meow leader integration.

**** Module map (where things live)

| Module file | Role |
|-------------+------|
| =personal/user.el= | Personal overlays: identity, fonts, UI/LSP switches, Org paths |
| =personal/device-darwin.el= | macOS-only device and IME glue |
| =personal/apple-music.el= | Apple Music integration (AppleScript + Hydra) |

**** How it works (flow)

1. *Personal bootstrap*:
   - Identity, fonts, and UI/LSP switches are set.
   - Cloud, Org, and blog paths are defined and ensured.

2. *Org wiring*:
   - =org-directory= is set.
   - =org-agenda-files= is computed by filtering non-archive Org files.

3. *Hygiene*:
   - Sensitive directories are removed from =load-path=.

4. *macOS-only glue*:
   - =sis= is configured defensively (only if functions exist).
   - An =after-load-theme= hook keeps the cursor face in sync.

5. *Device profile*:
   - Mouse and scroll tuning is applied for the MX Ergo S.

6. *Apple Music integration*:
   - AppleScript runners are defined.
   - Interactive commands and =my/hydra-apple-music= are exposed.

**** Key settings (reference)

- =my:font-default= :: ="JetBrains Mono NL"=
- =my:font-alt= :: ="Noto Sans JP"=
- =my:emoji-font= :: ="Apple Color Emoji"=
- =my:font-size= :: =18=
- =my:use-ui= :: ='nano=
- =my:use-lsp= :: ='eglot=
- =org-directory= :: =~/Documents/org=
- =org-agenda-files= :: All =*.org= under =org-directory=, excluding =archives=
- =MX Ergo scroll profile= ::
  =mouse-wheel-scroll-amount='(1 ((shift) . 5) ((control) . 10))=,
  =scroll-conservatively=10000=,
  =scroll-margin=2=,
  =scroll-preserve-screen-position=t=
- =Cursor color keep-alive= ::
  Re-apply =(set-face-background 'cursor (frame-parameter nil 'cursor-color))=
  on =after-load-theme=

**** Usage tips

- *Switch UI or LSP quickly*:
  - Set =my:use-ui= to ='nano=, ='doom=, or ='none=.
  - Set =my:use-lsp= to ='eglot= or ='lsp=.

- *Change fonts*:
  - Adjust =my:font-*= and =my:font-size= here.
  - The UI font module picks them up automatically.

- *Apple Music control*:
  - =C-c M= opens the Hydra.
  - Keys: =p= (play/pause), =n= (next), =b= (back),
    =l= (playlist), =i= (track info).

- *Agenda scope*:
  - Use an =archives/= directory or include “archives” in filenames to exclude
    files from =org-agenda-files=.

**** Troubleshooting

- *sis does not switch input methods on macOS* →
  Ensure the input source IDs match:
  ="com.apple.keylayout.ABC"= and
  ="com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"=.
  Missing functions are guarded with =fboundp=.

- *Cursor color looks wrong after a theme change* →
  Verify the theme sets the frame’s =cursor-color= parameter; the hook reapplies it.

- *Hydra key not found* →
  Confirm =hydra= is installed and loaded; the binding is added inside
  =with-eval-after-load=.

- *Meow leader binding missing* →
  Requires both =meow= and =hydra= to be loaded; the binding is added defensively.

**** Related source blocks

#+begin_src emacs-lisp :tangle no
  ;; See:
  ;; - personal/user.el
  ;; - personal/device-darwin.el
  ;; - personal/apple-music.el
#+end_src

*** user.el
:PROPERTIES:
:CUSTOM_ID: personal-user
:header-args:emacs-lisp: :tangle (eval (format "personal/%s.el" (user-login-name)))
:END:
#+begin_src emacs-lisp
  ;;; user.el --- Personal configuration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: personal
  ;;
  ;;; Commentary:
  ;; Personal configuration overrides.
  ;;
  ;; This file intentionally contains:
  ;; - Identity information
  ;; - Feature enable/disable flags
  ;; - Threshold and interval overrides
  ;;
  ;; This file intentionally does NOT contain:
  ;; - Timers
  ;; - Hooks
  ;; - Operational logic
  ;; - UI rendering code
  ;;
  ;; All behavior is implemented in core/, ui/, and utils/.
  ;;
  ;;; Code:

  ;; ---------------------------------------------------------------------------
  ;; Runtime requirements (do NOT rely on byte-compile)
  ;; ---------------------------------------------------------------------------
  (eval-when-compile
    (require 'leaf))

  ;; ---------------------------------------------------------------------------
  ;; Personal Information
  ;; ---------------------------------------------------------------------------
  (leaf *personals
    :straight nil
    :init
    ;; Identity & safety
    (setq user-full-name "YAMASHITA, Takao"
          user-mail-address "tjy1965@gmail.com"
          inhibit-compacting-font-caches t
          plstore-cache-passphrase-for-symmetric-encryption t)

    ;; Fonts / UI
  (setq ui-font-default "JetBrains Mono"
        ui-font-variable-pitch "Noto Sans JP"
        ui-font-size 18)

    (setq my:use-ui 'nano
          my:use-lsp 'eglot)

    ;; Cloud / Org / Blog directories
    (defvar my:d:cloud
      (expand-file-name "Documents" (getenv "HOME")))
    (defvar my:d:org
      (expand-file-name "org" my:d:cloud))
    (defvar my:d:blog
      (expand-file-name "devel/repos/mysite" my:d:cloud))
    (defvar my:f:capture-blog-file
      (expand-file-name "all-posts.org" my:d:blog))

    ;; Safety: excluded paths
    (defvar my:d:excluded-directories
      (list (expand-file-name "Library/Accounts" (getenv "HOME"))))

    ;; Ensure directories exist
    (mapc #'my/ensure-directory-exists
          (list my:d:cloud my:d:org my:d:blog))

    ;; core/core-treesit
    (setq core-treesit-enable-p t)
    (setq core-treesit-auto-install-p t)

    ;; Org wiring
    (setq org-directory my:d:org)
    (setq org-roam-db-node-include-function
        (lambda ()
          (let ((file (buffer-file-name)))
            (if (null file)
                t
              (not (string-match-p "/chatgpt/" file))))))

    (setq org-agenda-files
  	(when (fboundp 'utils-org-agenda-build)
            (utils-org-agenda-build)))


    ;; load-path hygiene
    (setq load-path
          (seq-remove
           (lambda (dir)
             (member dir my:d:excluded-directories))
           load-path)))

  ;; ---------------------------------------------------------------------------
  ;; Core session orchestration knobs
  ;; ---------------------------------------------------------------------------

  ;; Master switch
  (setq core-session-enable-p t)

  ;; Timing overrides
  (setq core-session-idle-delay
        (* 30 60))                     ;; 30 minutes idle

  (setq core-session-periodic-interval
        600)                           ;; 10 minutes

  ;; Risk thresholds (personal tolerance)
  (setq core-session-buffer-threshold
        350)

  (setq core-session-process-threshold
        8)

  ;; ---------------------------------------------------------------------------
  ;; UI: session health modeline knobs
  ;; ---------------------------------------------------------------------------

  (setq ui-health-show-buffers-p t)
  (setq ui-health-show-processes-p t)
  (setq ui-health-show-eglot-p t)

  ;; ---------------------------------------------------------------------------
  ;; Optional personal safety preferences
  ;; ---------------------------------------------------------------------------

  ;; Avoid font cache compaction on long-running sessions
  (setq inhibit-compacting-font-caches t)

  ;; Cache passphrase in memory for encrypted plstore
  (setq plstore-cache-passphrase-for-symmetric-encryption t)

  ;; ---------------------------------------------------------------------------
  ;; Load personal optional modules
  ;; ---------------------------------------------------------------------------

  (when (eq system-type 'darwin)
    (require 'device-darwin nil t)
    (require 'apple-music nil t))

  (provide 'user)
  ;;; personal/user.el ends here
#+end_src

*** device-darwin.el
:PROPERTIES:
:CUSTOM_ID: personal-device-darwin
:header-args:emacs-lisp: :tangle personal/device-darwin.el
:END:
#+begin_src emacs-lisp
  ;;; device-darwin.el --- macOS device profile -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: personal
  ;;
  ;;; Commentary:
  ;; macOS-specific device and input configuration.
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf))

  (when (eq system-type 'darwin)

    ;; -------------------------------------------------------------------------
    ;; IME integration (sis)
    ;; -------------------------------------------------------------------------

    (leaf sis
      :straight t
      :commands (sis-ism-lazyman-config
                 sis-global-cursor-color-mode
                 sis-global-respect-mode
                 sis-global-inline-mode)
      :hook
      (emacs-startup .
                     (lambda ()
                       (when (fboundp 'sis-ism-lazyman-config)
                         (sis-ism-lazyman-config
                          "com.apple.keylayout.ABC"
                          "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"
                          'macism))
                       (when (fboundp 'sis-global-cursor-color-mode)
                         (sis-global-cursor-color-mode t))
                       (when (fboundp 'sis-global-respect-mode)
                         (sis-global-respect-mode t))
                       (when (fboundp 'sis-global-inline-mode)
                         (sis-global-inline-mode t)))))

    ;; -------------------------------------------------------------------------
    ;; Cursor color keep-alive after theme load
    ;; -------------------------------------------------------------------------

    (add-hook 'after-load-theme-hook
              (lambda ()
                (when (facep 'cursor)
                  (let ((c (frame-parameter nil 'cursor-color)))
                    (when (stringp c)
                      (set-face-background 'cursor c))))))

    ;; -------------------------------------------------------------------------
    ;; Mouse / scroll profile (MX Ergo S)
    ;; -------------------------------------------------------------------------

    (leaf device-mx-ergo-s
      :straight nil
      :init
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 10))
            mouse-wheel-progressive-speed nil
            scroll-conservatively 10000
            scroll-margin 2
            scroll-preserve-screen-position t
            mac-mouse-wheel-smooth-scroll t
            mouse-wheel-tilt-scroll t
            mouse-wheel-flip-direction nil)

      (global-set-key [mouse-2] #'yank)
      (global-set-key [mouse-4] #'previous-buffer)
      (global-set-key [mouse-5] #'next-buffer)))

  (provide 'device-darwin)
  ;;; personal/device-darwin.el ends here
#+end_src

*** apple-music.el
:PROPERTIES:
:CUSTOM_ID: personal-apple-music
:header-args:emacs-lisp: :tangle personal/apple-music.el
:END:

Purpose:
Control Apple Music from Emacs on macOS.

What it does:
- Provides async/sync AppleScript helpers
- Defines interactive playback commands
- Exposes a hydra and optional meow leader binding

Notes:
- This module is strictly optional and macOS-only.
- No core/session or utils logic is used here.

#+begin_src emacs-lisp
  ;;; apple-music.el --- Apple Music integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: personal
  ;;
  ;;; Commentary:
  ;; Control Apple Music using AppleScript.
  ;;
  ;;; Code:

  (when (eq system-type 'darwin)

    ;; -------------------------------------------------------------------------
    ;; AppleScript helpers
    ;; -------------------------------------------------------------------------

    (defun my/apple-music--osascript-async (script &optional callback)
      "Run AppleScript SCRIPT asynchronously.
  If CALLBACK is non-nil, call it with the trimmed output."
      (let* ((proc-name "apple-music-async")
             (buffer-name "*Apple Music Async*")
             (proc (apply #'start-process
                          proc-name buffer-name
                          (list "osascript" "-e" script))))
        (when callback
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (funcall callback
                          (string-trim (buffer-string))))
               (kill-buffer (process-buffer process))))))))

    (defun my/apple-music--osascript-sync (script)
      "Run AppleScript SCRIPT synchronously and return trimmed output."
      (string-trim
       (shell-command-to-string
        (format "osascript -e '%s'" script))))

    ;; -------------------------------------------------------------------------
    ;; Interactive commands
    ;; -------------------------------------------------------------------------

    ;;;###autoload
    (defun my/apple-music-play-pause ()
      "Toggle play/pause in Apple Music."
      (interactive)
      (my/apple-music--osascript-async
       "tell application \"Music\" to playpause"))

    ;;;###autoload
    (defun my/apple-music-next-track ()
      "Skip to the next track in Apple Music."
      (interactive)
      (my/apple-music--osascript-async
       "tell application \"Music\" to next track"))

    ;;;###autoload
    (defun my/apple-music-previous-track ()
      "Return to the previous track in Apple Music."
      (interactive)
      (my/apple-music--osascript-async
       "tell application \"Music\" to previous track"))

    ;;;###autoload
    (defun my/apple-music-current-track-info ()
      "Display current track information."
      (interactive)
      (message "%s"
               (my/apple-music--osascript-sync
                "tell application \"Music\" \
  to (get name of current track) & \" — \" & (get artist of current track) \
  & \" [\" & (get album of current track) & \"]\"")))

    (defun my/apple-music-get-playlists ()
      "Return a list of playlist names."
      (split-string
       (my/apple-music--osascript-sync
        "tell application \"Music\" to get name of playlists")
       ", "))

    ;;;###autoload
    (defun my/apple-music-play-playlist (playlist)
      "Prompt for PLAYLIST and play it."
      (interactive
       (list (completing-read
              "Playlist: "
              (my/apple-music-get-playlists))))
      (my/apple-music--osascript-async
       (format "tell application \"Music\" to play playlist \"%s\""
  	     (replace-regexp-in-string "\"" "\\\\\"" playlist))))

    ;; -------------------------------------------------------------------------
    ;; Hydra / meow integration
    ;; -------------------------------------------------------------------------

    (with-eval-after-load 'hydra
      (defhydra my/hydra-apple-music (:hint nil)
        "
  Apple Music
  -----------
  _p_: Play/Pause   _n_: Next   _b_: Back
  _l_: Playlist     _i_: Info   _q_: Quit
  "
        ("p" my/apple-music-play-pause)
        ("n" my/apple-music-next-track)
        ("b" my/apple-music-previous-track)
        ("l" my/apple-music-play-playlist)
        ("i" my/apple-music-current-track-info)
        ("q" nil "quit"))
      (global-set-key (kbd "C-c M") #'my/hydra-apple-music/body))

    (with-eval-after-load 'meow
      (with-eval-after-load 'hydra
        (when (fboundp 'meow-leader-define-key)
          (meow-leader-define-key
           '("M" . my/hydra-apple-music/body))))))

  (provide 'apple-music)
  ;;; personal/apple-music.el ends here
#+end_src

(provide 'README)

;;; README.org ends here
