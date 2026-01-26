README.org part 3/3
--------------------

*** utils/utils-lsp.el
:PROPERTIES:
:CUSTOM_ID: utils-lsp
:header-args:emacs-lisp: :tangle lisp/utils/utils-lsp.el
:END:

#+begin_src emacs-lisp
  ;;; utils/utils-lsp.el --- LSP lifecycle helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;;; Commentary:
  ;; Utilities for managing eglot lifecycle on project switches.
  ;;
  ;; This module provides:
  ;; - Detection of project root changes
  ;; - Shutdown of obsolete eglot servers
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

    (defvar utils-lsp--current-project-root nil
      "Current project root for LSP lifecycle management.")

    (defun utils-lsp--project-root ()
      "Return the current project root."
      (when-let* ((project (project-current nil)))
        (car (project-roots project))))

    (defun utils-lsp-on-project-switch ()
      "Shutdown obsolete eglot servers when switching projects."
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
