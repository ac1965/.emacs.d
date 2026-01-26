README.org part 3/3
--------------------

*** dev/dev-rest.el
:PROPERTIES:
:CUSTOM_ID: dev-rest
:header-args:emacs-lisp: :tangle lisp/dev/dev-rest.el
:END:

**** Purpose
Provide a simple **HTTP/REST client workflow** inside Emacs for quick
API testing and iteration.

**** What it does
- Enables `restclient-mode` for `.http` files
- Adds `restclient-jq` integration for JSON parsing/formatting

**** Notes
- `jq` should be available in PATH for best results
- This module keeps configuration minimal (just mode and integration)

**** Implementation

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

(provide 'dev/dev-rest)
;;; dev/dev-rest.el ends here
#+end_src

*** utils/utils-functions.el
:PROPERTIES:
:CUSTOM_ID: utils-functions
:header-args:emacs-lisp: :tangle lisp/utils/utils-functions.el
:END:

**** Purpose
Provide **small, safe utility functions** shared across the configuration,
covering editing helpers, Org helpers, and lightweight UI glue.

**** What it does
- Smart buffer/window killing
- Org subtree → indirect buffer helper
- Org outline sidebar using `imenu-list`
- Nano-modeline–aware header-line rendering (with safe fallback)

**** Notes
- Optional dependencies are declared defensively to avoid byte-compile warnings
- Prefers modern Org APIs (`org-fold-show-all`) when available
- Designed to degrade gracefully if optional packages are missing

**** Implementation

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

  (provide 'utils/utils-functions)
  ;;; utils/utils-functions.el ends here
#+end_src

*** utils/utils-scratch.el
:PROPERTIES:
:CUSTOM_ID: utils-scratch
:header-args:emacs-lisp: :tangle lisp/utils/utils-scratch.el
:END:

**** Purpose
Ensure that `*scratch*` is **always available** by automatically recreating it
when killed.

**** What it does
- Recreates `*scratch*` after it is killed
- Uses `lisp-interaction-mode`
- Inserts a small header

**** Notes
- Recreation is asynchronous to avoid interfering with kill flow

**** Implementation

#+begin_src emacs-lisp
  ;;; utils/utils-scratch.el --- Scratch buffer helpers -*- lexical-binding: t; -*-
  ;;
  ;; Category: utils
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

  (provide 'utils/utils-scratch)
  ;;; utils/utils-scratch.el ends here
#+end_src

*** utils/utils-backup.el
:PROPERTIES:
:CUSTOM_ID: utils-backup
:header-args:emacs-lisp: :tangle lisp/utils/utils-backup.el
:END:

**** Purpose
Automatically **clean up old backup files** to keep the var directory tidy.

**** What it does
- Deletes backup files older than 7 days
- Runs once at Emacs startup

**** Notes
- Assumes `no-littering-var-directory` is in use

**** Implementation

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

  (provide 'utils/utils-backup)
  ;;; utils/utils-backup.el ends here
#+end_src

*** utils/utils-async.el
:PROPERTIES:
:CUSTOM_ID: utils-async
:header-args:emacs-lisp: :tangle lisp/utils/utils-async.el
:END:

**** Purpose
Provide a **safe helper** for running asynchronous tasks without crashing Emacs.

**** What it does
- Executes a function asynchronously
- Catches and logs any errors

**** Notes
- Intended for lightweight background tasks only

**** Implementation

#+begin_src emacs-lisp
  ;;; utils/utils-async.el --- Async helpers -*- lexical-binding: t; -*-
  ;;
  ;; Category: utils
  ;;
  ;;; Code:

  (defun my/safe-run-async (task)
    "Run TASK asynchronously, catching and reporting any errors."
    (run-at-time 0 nil
                 (lambda ()
                   (condition-case err
                       (funcall task)
                     (error (message "[async] error: %s" err))))))

  (provide 'utils/utils-async)
  ;;; utils/utils-async.el ends here
#+end_src

*** utils/utils-org-agenda.el
:PROPERTIES:
:CUSTOM_ID: utils-org-agenda
:header-args:emacs-lisp: :tangle lisp/utils/utils-org-agenda.el
:END:

**** Purpose
Build `org-agenda-files` using a **persistent cache** to avoid expensive
recursive scans on every startup.

**** What it does
- Recursively scans `org-directory` for `*.org`
- Excludes paths matching a configurable regexp (default: `archives`)
- Caches results on disk under `user-emacs-directory`
- Returns agenda files in *O(1)* time when cached

**** Notes
- Does **not** define `org-directory`
- Does **not** automatically set `org-agenda-files`
- Full rescans happen only when explicitly requested

**** Implementation

#+begin_src emacs-lisp
  ;;; utils-org-agenda.el --- Cached org-agenda-files builder -*- lexical-binding: t; -*-
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
            (not (string-match-p utils-org-agenda-exclude-regexp file))))
     (directory-files-recursively org-directory "\\.org\\'")))

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

  (provide 'utils/utils-org-agenda)
  ;;; utils-org-agenda.el ends here
#+end_src

*** utils/utils-search-nav.el
:PROPERTIES:
:CUSTOM_ID: search-nav
:header-args:emacs-lisp: :tangle lisp/utils/utils-search-nav.el
:END:

**** Purpose
Provide **modern search and navigation** helpers built on ripgrep and Consult,
with safe fallbacks.

**** What it does
- Jump-to-definition via `dumb-jump`
- Multiple cursors support
- Lightweight web search and browsing via `eww`

**** Notes
- Search backend prefers ripgrep (`rg`)
- EWW helpers avoid heavy browser dependencies

**** Implementation

#+begin_src emacs-lisp
  ;;; utils/utils-search-nav.el --- Project search & navigation -*- lexical-binding: t; -*-
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
    (defvar my:d:eww (expand-file-name "eww/" my:d:var))
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

  (provide 'utils/utils-search-nav)
  ;;; utils-search-nav.el ends here
#+end_src

*** utils/utils-notes-markdown.el
:PROPERTIES:
:CUSTOM_ID: utils-notes-markdown
:header-args:emacs-lisp: :tangle lisp/utils/utils-notes-markdown.el
:END:

**** Purpose
Provide a **prose-oriented Markdown notes system** that lives *alongside* Org
files under the same logical knowledge base, without interfering with Org’s
agenda, capture, or task semantics.

This module defines Markdown notes as a *sibling domain* of Org, optimized for
long-form writing, reflection, and reference material rather than task
management.

**** What it does
- Defines a dedicated Markdown notes root under =my:d:org/notes=
- Ensures a stable notebook-style directory structure
- Provides helpers to:
  - Create timestamped Markdown notes with front matter
  - Open the notes root quickly
- Configures Markdown UX for prose:
  - Visual line wrapping and variable-pitch text
  - Centered text via =visual-fill-column=
  - Native code block fontification
- Integrates search and navigation:
  - Scoped ripgrep search via =consult-ripgrep=
  - Source registration for =consult-notes=
- Enables image pasting in Markdown via =org-download= with Markdown-friendly
  link formatting

**** Notes
- Assumes =my:d:org= is already defined and points to the Org root directory
- Markdown notes are **explicitly excluded** from:
  - Org agenda
  - Org capture
  - Zettelkasten / backlink semantics
- This module defines *no policy* about when notes are created or searched
- Search and navigation rely on consult + ripgrep
- Safe to load early; all filesystem operations are defensive and idempotent

Directory layout (conceptual):

#+begin_example
my:d:org/
  ├─ agenda/        (Org tasks, schedules)
  ├─ projects/      (Org project files)
  └─ notes/         (Markdown prose notes)  ← this module
#+end_example

**** Implementation

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

  (provide 'utils/utils-notes-markdown)
  ;;; utils/utils-notes-markdown.el ends here
#+end_src

*** utils/utils-gc.el
:PROPERTIES:
:CUSTOM_ID: utils-gc
:header-args:emacs-lisp: :tangle lisp/utils/utils-gc.el
:END:

**** Purpose
Run **garbage collection at safe moments** during long-running sessions.

**** What it does
- Triggers GC on focus-out
- Triggers GC on minibuffer exit

**** Notes
- Never signals errors
- No logging or visualization

**** Implementation

#+begin_src emacs-lisp
  ;;; utils/utils-gc.el --- GC helpers for long-running sessions -*- lexical-binding: t; -*-
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

  (provide 'utils/utils-gc)
  ;;; utils/utils-gc.el ends here
#+end_src

*** utils/utils-buffers.el
:PROPERTIES:
:CUSTOM_ID: utils-buffers
:header-args:emacs-lisp: :tangle lisp/utils/utils-buffers.el
:END:

**** Purpose
Provide **automatic housekeeping** for temporary and dead-process buffers.

**** What it does
- Detects temporary buffers by name/pattern
- Cleans buffers with exited processes
- Runs periodically in the background

**** Notes
- Skips modified or visible buffers
- Safe defaults only

**** Implementation

#+begin_src emacs-lisp
  ;;; utils/utils-buffers.el --- Buffer housekeeping utilities -*- lexical-binding: t; -*-
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
            (kill-buffer buf)))))

    (run-with-timer 900 900 #'utils-buffers-cleanup))

  (provide 'utils/utils-buffers)
  ;;; utils/utils-buffers.el ends here
#+end_src

*** utils/utils-lsp.el
:PROPERTIES:
:CUSTOM_ID: utils-lsp
:header-args:emacs-lisp: :tangle lisp/utils/utils-lsp.el
:END:

**** Purpose
Manage **Eglot lifecycle** when switching projects.

**** What it does
- Detects project root changes
- Shuts down obsolete Eglot servers

**** Notes
- Defensive: never errors if Eglot internals change
- Complements `dev/dev-lsp-eglot.el`

**** Implementation

#+begin_src emacs-lisp
  ;;; utils/utils-lsp.el --- LSP lifecycle helpers -*- lexical-binding: t; -*-
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
      (when-let* ((project (project-current nil)))
        (car (project-roots project))))

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

  (provide 'utils/utils-lsp)
  ;;; utils/utils-lsp.el ends here
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

Purpose:
Personal-only knobs and identity information.

What it does:
- Defines user identity
- Overrides defcustom values from core/session and ui/health-modeline
- Injects personal paths and preferences

Notes:
- This file contains NO hooks, timers, or operational logic.
- Device- and workflow-specific code lives in separate personal modules.

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

    ;; Fonts / UI / LSP switches
    (setq my:font-default "JetBrains Mono"
          my:font-alt     "Noto Sans JP"
          my:emoji-font   "Apple Color Emoji"
          my:font-size    18)

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

Purpose:
macOS-specific device and input method configuration.

What it does:
- Configure IME behavior via sis
- Apply macOS-specific mouse and scroll tuning
- Restore cursor color after theme reload

Notes:
- This file is loaded only on darwin systems.
- No core/session or UI orchestration logic lives here.

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
       (format "tell application \"Music\" to play playlist \"%s\"" playlist)))

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
