README.org part 3/3
--------------------

***** orgx/orgx-auto-tangle.el
:PROPERTIES:
:CUSTOM_ID: orgx-auto-tangle
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-auto-tangle.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-auto-tangle.el --- Automatic tangling helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Automatic tangling of Org source blocks in README.org.
  ;;
  ;; This module:
  ;; - tangles only when saving README.org
  ;; - suppresses confirmation prompts *locally*
  ;; - scopes hooks strictly to Org buffers
  ;;
  ;;; Code:

  (require 'org)
  (require 'ob-tangle)

  (defconst my/orgx-auto-tangle-target-file "README.org"
    "File name eligible for automatic tangling.")

  (defun my/orgx-auto-tangle--eligible-p ()
    "Return non-nil if current buffer should trigger auto-tangle."
    (and (derived-mode-p 'org-mode)
         buffer-file-name
         (string=
          (file-name-nondirectory buffer-file-name)
          my/orgx-auto-tangle-target-file)))

  (defun my/orgx-auto-tangle--maybe ()
    "Automatically tangle Org source blocks when saving README.org.

  This function is intentionally conservative:
  - No global variables are mutated.
  - Confirmation suppression is buffer-local and ephemeral.
  - Future diff-based tangling can be introduced here."
    (when (my/orgx-auto-tangle--eligible-p)
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        #'my/orgx-auto-tangle--maybe
                        nil
                        'local)))

  (provide 'orgx-auto-tangle)
  ;;; orgx/orgx-auto-tangle.el ends here
#+end_src

***** orgx/orgx-typography.el
:PROPERTIES:
:CUSTOM_ID: orgx-typography
:header-args:emacs-lisp: :tangle lisp/orgx/orgx-typography.el
:END:
#+begin_src emacs-lisp
  ;;; orgx/orgx-typography.el --- Org typography enhancements -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: org
  ;;
  ;; Commentary:
  ;; Typography and prose readability helpers for Org buffers.
  ;;
  ;; This module:
  ;; - complements orgx-visual without overlapping responsibilities
  ;; - improves table alignment and emphasis visibility
  ;; - focuses on long-form prose readability
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
Support **development, diagnostics, and build-oriented workflows**
for this Emacs configuration.

This layer exists primarily for maintainers and advanced users who
develop, inspect, or evolve the configuration itself, rather than
for routine day-to-day editing.

Dev modules are strictly *auxiliary*: useful when needed, invisible
when not.

**** What this layer does
Dev modules provide tooling that helps *understand, debug, and extend*
the running Emacs environment and its configuration.

This layer is responsible for:

- Tooling for configuration development and internal inspection
- Support for build, compile, and debug workflows
- Diagnostics and introspection of the live Emacs instance
- Maintenance-oriented helpers for evolving the configuration safely

Typical responsibilities include:

- Language Server Protocol (LSP) client wiring and toggles
- Debugging, profiling, tracing, and performance analysis helpers
- Compilation, test execution, and task orchestration
- Developer-facing inspection commands and reports

**** What this layer does *not* do
Dev modules intentionally do **not**:

- Define global editing behavior or keybinding policy
- Establish startup-critical behavior or core invariants
- Introduce visual presentation or theming concerns
- Encode user-, device-, or host-specific preferences
- Affect Org workflows or semantic editing behavior

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
  - opt-in via customization variables or switches
  - safe to disable as a group

**** Design principles
- Dev tooling is **additive**, never required for normal operation
- Failure in this layer must degrade gracefully
- Disabling this layer must not affect:
  - startup success
  - baseline editing behavior
  - Org workflows or persistence
- Debugging aids should favor observability over automation

**** Benefits
This separation ensures that:

- Development aids never compromise configuration stability
- Power-user tooling can evolve rapidly without risk
- Production usage remains lightweight and predictable
- Debugging can be enabled surgically when needed

**** Module map
| File | Responsibility |
|------+----------------|
| =dev/dev-lsp-eglot.el= | Eglot-based LSP configuration and integration |
| =dev/dev-lsp-mode.el= | lsp-mode–based LSP configuration (alternative backend) |
| =dev/dev-dap.el= | Debug Adapter Protocol integration |
| =dev/dev-compile.el= | Compile, build, and task helpers |
| =dev/dev-profile.el= | Profiling and performance diagnostics |
| =dev/dev-inspect.el= | Runtime inspection and developer-facing debug utilities |

**** Notes
- Dev is the *only* layer allowed to assume a maintainer mindset.
- Any tool required for basic usage does not belong here.
- If disabling this layer breaks normal editing, it is misplaced.

This layer exists to make the configuration *easier to evolve*,
not harder to use.

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
  ;;; dev/dev-lsp-eglot.el --- Eglot LSP setup -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Baseline Eglot configuration with safe auto-enable.
  ;;
  ;; This module:
  ;; - detects whether an LSP server is applicable
  ;; - enables Eglot lazily in programming buffers
  ;; - exposes an explicit interactive enable command
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
  ;;; dev/dev-lsp-mode.el --- lsp-mode setup -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; lsp-mode baseline with optional lsp-ui integration.
  ;;
  ;; This module:
  ;; - enables lsp-mode with deferred startup
  ;; - delegates completion to Corfu
  ;; - provides an explicit interactive enable command
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
  ;;; dev/dev-ai.el --- AI-assisted development helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; AI-assisted coding via Aider / Aidermacs.
  ;;
  ;; This module:
  ;; - centralizes Aider runtime files and history
  ;; - configures environment variables consistently
  ;; - supports OpenRouter and OpenAI backends
  ;; - prevents duplication in `aidermacs-extra-args`
  ;;
  ;;; Code:

  (eval-when-compile
    (require 'leaf)
    (require 'subr-x))

  ;; ---------------------------------------------------------------------------
  ;; Base directory
  ;; ---------------------------------------------------------------------------

  (defvar my:d:aider
    (expand-file-name "aideremacs/"
                      (or (bound-and-true-p my:d:var)
                          (expand-file-name "~/.var/")))
    "Base directory to store aider runtime files.")

  ;; Ensure directory exists
  (my/ensure-directory-exists my:d:aider)

  ;; ---------------------------------------------------------------------------
  ;; Environment variables (officially supported by aider)
  ;; ---------------------------------------------------------------------------

  (setenv "AIDER_INPUT_HISTORY_FILE"
          (expand-file-name "input.history" my:d:aider))
  (setenv "AIDER_CHAT_HISTORY_FILE"
          (expand-file-name "chat.history.md" my:d:aider))
  (setenv "AIDER_LLM_HISTORY_FILE"
          (expand-file-name "llm.history" my:d:aider))
  (setenv "AIDER_ANALYTICS_LOG"
          (expand-file-name "analytics.log" my:d:aider))

  ;; ---------------------------------------------------------------------------
  ;; Helper: deduplicated arg append
  ;; ---------------------------------------------------------------------------

  (defun my/aidermacs--append-unique-args (args)
    "Append ARGS to `aidermacs-extra-args`, avoiding duplicates.

  ARGS must be a list of strings.  Order is preserved:
  existing entries are kept, and only missing elements are appended."
    (setq aidermacs-extra-args
          (append aidermacs-extra-args
                  (seq-remove
                   (lambda (arg)
                     (member arg aidermacs-extra-args))
                   args))))

  ;; ---------------------------------------------------------------------------
  ;; Aidermacs integration
  ;; ---------------------------------------------------------------------------

  (with-eval-after-load 'aidermacs
    ;; Defensive declaration for older versions
    (defvar aidermacs-extra-args nil
      "Extra CLI arguments passed to the `aider` command.")

    ;; Optional per-user .env support
    (let ((env-file (expand-file-name ".env" my:d:aider)))
      (when (file-exists-p env-file)
        (my/aidermacs--append-unique-args
         (list "--env-file" env-file))))

    ;; Force history files via CLI flags as well (redundant but safe)
    (my/aidermacs--append-unique-args
     (list "--input-history-file" (getenv "AIDER_INPUT_HISTORY_FILE")
           "--chat-history-file"  (getenv "AIDER_CHAT_HISTORY_FILE")
           "--llm-history-file"   (getenv "AIDER_LLM_HISTORY_FILE"))))

  ;; ---------------------------------------------------------------------------
  ;; Aidermacs package configuration
  ;; ---------------------------------------------------------------------------

  (leaf aidermacs
    :straight t
    :init
    (cond
     ((getenv "OPENROUTER_API_KEY")
      (setenv "OPENAI_API_BASE" "https://openrouter.ai/api/v1")
      (setenv "OPENAI_API_KEY"  (getenv "OPENROUTER_API_KEY"))
      (setopt aidermacs-default-model
              "openrouter/anthropic/claude-3.5-sonnet"))
     ((getenv "OPENAI_API_KEY")
      (setenv "OPENAI_API_BASE" "https://api.openai.com/v1")
      (setopt aidermacs-default-model
              "gpt-4o-mini"))
     (t
      (display-warning
       'aidermacs
       "No API keys set. Set OPENROUTER_API_KEY or OPENAI_API_KEY.")))
    :custom
    ((aidermacs-retry-attempts . 3)
     (aidermacs-retry-delay   . 2.0)
     (aidermacs-backend       . 'vterm)))

  ;; ---------------------------------------------------------------------------
  ;; Footer
  ;; ---------------------------------------------------------------------------

  (provide 'dev-ai)
  ;;; dev/dev-ai.el ends here
#+end_src

***** dev/dev-term.el
:PROPERTIES:
:CUSTOM_ID: dev-term
:header-args:emacs-lisp: :tangle lisp/dev/dev-term.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-term.el --- Terminal integration helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Terminal and shell workflow integration.
  ;;
  ;; This module:
  ;; - configures vterm and vterm-toggle
  ;; - applies UI palette consistently to terminal faces
  ;; - defines window display rules for terminal buffers
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
  ;;; dev/dev-web-core.el --- Project core helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Core project configuration and editor hygiene.
  ;;
  ;; This module:
  ;; - configures project.el and editor defaults
  ;; - enforces trailing whitespace and newline policy
  ;; - explicitly excludes Tree-sitter policy ownership
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf project :straight nil)

  (leaf files :straight nil
    :custom ((require-final-newline . t)
             (delete-trailing-lines . t))
    :hook ((before-save-hook . delete-trailing-whitespace)))

  (leaf editorconfig
    :straight t
    :global-minor-mode t)

  (provide 'dev-web-core)
  ;;; dev/dev-web-core.el ends here
#+end_src

***** dev/dev-build.el
:PROPERTIES:
:CUSTOM_ID: dev-build
:header-args:emacs-lisp: :tangle lisp/dev/dev-build.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-build.el --- Build and Makefile tooling -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Build system helpers focused on Makefile workflows.
  ;;
  ;; This module:
  ;; - configures Makefile editing modes
  ;; - sets sane defaults for compilation buffers
  ;; - preserves minimal global impact
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf make-mode :straight nil
    :mode (("\\`Makefile\\'"    . makefile-gmake-mode)
           ("\\`GNUmakefile\\'" . makefile-gmake-mode)
           ("\\`makefile\\'"    . makefile-gmake-mode))
    :hook
    ((makefile-mode . my/set-make-compile-command)
     (makefile-mode . (lambda ()
                        (setq-local indent-tabs-mode t
                                    tab-width 8
                                    show-trailing-whitespace t))))
    :config

    (leaf compile :straight nil
      :custom ((compilation-scroll-output . t)
               (compilation-skip-threshold . 2)))

    (leaf ansi-color :straight nil
      :hook
      (compilation-filter . (lambda ()
                              (let ((inhibit-read-only t))
                                (ansi-color-apply-on-region
                                 compilation-filter-start
                                 (point-max)))))))

  (defun my/set-make-compile-command ()
    "Set `compile-command' to `make -k' in Makefile buffers."
    (setq-local compile-command "make -k"))

  (provide 'dev-build)
  ;;; dev/dev-build.el ends here
#+end_src

***** dev/dev-format.el
:PROPERTIES:
:CUSTOM_ID: dev-format
:header-args:emacs-lisp: :tangle lisp/dev/dev-format.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-format.el --- Code formatting via Apheleia -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Automatic code formatting for web-centric development.
  ;;
  ;; This module:
  ;; - enables Apheleia globally
  ;; - configures Prettier / prettierd formatters
  ;; - applies formatting to common web and markup modes
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
  ;;; dev/dev-infra-modes.el --- Infrastructure file modes -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Syntax support for infrastructure-related files.
  ;;
  ;; This module:
  ;; - enables modes for Docker Compose, .env, TOML, and Makefiles
  ;; - focuses purely on syntax and editing support
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
  ;;; dev/dev-docker.el --- Docker integration helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Docker and container-oriented development helpers.
  ;;
  ;; This module:
  ;; - provides major modes for Dockerfile and Compose files
  ;; - integrates Docker CLI management commands
  ;; - configures TRAMP container access
  ;; - supplies Dockerfile templates via Tempel
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
  ;;; dev/dev-sql.el --- SQL and PostgreSQL helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; SQL editing and formatting helpers.
  ;;
  ;; This module:
  ;; - configures SQL modes for PostgreSQL
  ;; - enables indentation and formatting helpers
  ;;
  ;;; Code:

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
  ;;; dev/dev-rest.el --- REST client helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; REST client integration for testing HTTP APIs inside Emacs.
  ;;
  ;; This module:
  ;; - enables restclient for .http files
  ;; - integrates jq-based JSON inspection
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (leaf restclient :straight t :mode ("\\.http\\'" . restclient-mode))
  (leaf restclient-jq :straight t :after restclient)

  (provide 'dev-rest)
  ;;; dev/dev-rest.el ends here
#+end_src

***** dev/dev-navigation.el
:PROPERTIES:
:CUSTOM_ID: dev-navigation
:header-args:emacs-lisp: :tangle lisp/dev/dev-navigation.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-navigation.el --- Search and navigation helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: dev
  ;;
  ;; Commentary:
  ;; Project-wide search and navigation helpers.
  ;;
  ;; This module:
  ;; - integrates ripgrep-based jumping via dumb-jump
  ;; - provides lightweight EWW search helpers
  ;; - avoids intrusive global navigation policy
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
    (unless (file-directory-p eww-cache-directory)
      (make-directory eww-cache-directory t))
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

  (provide 'dev-navigation)
  ;;; dev/dev-navigation.el ends here
#+end_src

***** dev/dev-tools.el
:PROPERTIES:
:CUSTOM_ID: dev-tools
:header-args:emacs-lisp: :tangle lisp/dev/dev-tools.el
:END:
#+begin_src emacs-lisp
  ;;; dev/dev-tools.el --- Development helper tools -*- lexical-binding: t; -*-
  ;;
  ;; Category: dev
  ;;
  ;;; Code:

  (defun my/open-by-vscode ()
    "Open current file at point in VS Code."
    (interactive)
    (when (buffer-file-name)
      (async-shell-command
       (format "code -r -g %s:%d:%d"
               (buffer-file-name)
               (line-number-at-pos)
               (current-column)))))

  (defun my/show-env-variable (var)
    "Show environment variable VAR."
    (interactive "sEnvironment variable: ")
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

  (provide 'dev-tools)
  ;;; dev/dev-tools.el ends here
#+end_src

*** vcs/
:PROPERTIES:
:CUSTOM_ID: vcs-modules
:END:

**** Purpose
Provide a **consistent, explicit, and inspectable version control interaction
layer** within Emacs.

This layer encapsulates all interaction with external version control systems
— primarily Git — while treating those systems as the authoritative source
of truth.

Emacs is used as a *control surface*, not as a replacement for VCS tooling.

**** What this layer does
VCS modules integrate Emacs with external version control systems in a way that
is observable, reversible, and failure-tolerant.

Specifically, this layer is responsible for:

- Bridging Emacs with VCS backends (primarily Git)
- Exposing repository state, history, and diffs inside Emacs buffers
- Providing interactive commands for common VCS workflows
- Surfacing VCS information in buffers without mutating editing semantics

Typical responsibilities include:

- Repository status inspection and change overview
- Commit, diff, hunk, and blame navigation
- File- and project-scoped VCS operations
- Lightweight review, history browsing, and annotation

**** What this layer does *not* do
VCS modules intentionally do **not**:

- Define global editing behavior or keybinding policy
- Enforce a specific branching, commit, or review workflow
- Replace, fork, or reimplement external VCS tools
- Assume the presence of a repository or remote

Those responsibilities belong to user workflows or external tooling.

**** Design constraints
- VCS modules may depend on:
  - =core=
  - =ui=
  - =completion=

- VCS modules must not depend on:
  - =orgx=
  - =dev=
  - =utils=

- Interaction with external tools must be:
  - explicit
  - inspectable
  - resilient to tool absence or failure

**** Design principles
- External VCS tools are the **source of truth**
- Emacs acts as an interface layer, not an abstraction leak
- All commands must degrade gracefully when:
  - no repository is present
  - required binaries are missing
  - network access is unavailable
- User workflow choices remain:
  - opt-in
  - reversible
  - discoverable

**** Observed implementation characteristics
Based on the current configuration:

- Git integration is centered on *Magit* for interactive workflows
- Change visualization relies on lightweight gutter-style indicators
- No assumptions are made about:
  - hosting provider
  - branching model
  - collaboration workflow
- VCS features can be enabled or disabled independently

**** Module map
| File | Responsibility |
|------+----------------|
| =vcs/vcs-magit.el= | Magit-based interactive Git workflows |
| =vcs/vcs-gutter.el= | In-buffer change indicators (diff-hl) |
| =vcs/vcs-forge.el= | Optional GitHub / GitLab integration via Forge |

**** Notes
- This layer is intentionally thin and compositional.
- Any logic that *depends* on VCS state belongs elsewhere.
- If disabling this layer breaks core editing, the dependency is misplaced.

This layer exists to make version control *visible and accessible*,
not opinionated or mandatory.

**** Implementation

- Loaded after =dev=
- Each file:
  - provides exactly one VCS-related feature
  - documents backend assumptions and failure modes
- Disabling this layer must leave Emacs fully usable for
  non-version-controlled files

***** vcs/vcs-magit.el
:PROPERTIES:
:CUSTOM_ID: vcs-magit
:header-args:emacs-lisp: :tangle lisp/vcs/vcs-magit.el
:END:
#+begin_src emacs-lisp
  ;;; vcs/vcs-magit.el --- Magit Git integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: vcs
  ;;
  ;; Commentary:
  ;; Core Magit configuration for Git operations.
  ;;
  ;; This module:
  ;; - enables Magit commands and status buffers
  ;; - adjusts refresh and revert behavior conservatively
  ;; - keeps Git interaction explicit and user-driven
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
  ;;; vcs/vcs-gutter.el --- Git change indicators -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: vcs
  ;;
  ;; Commentary:
  ;; Visual indicators for Git working tree changes.
  ;;
  ;; This module:
  ;; - shows added, modified, and deleted lines in the fringe
  ;; - integrates diff-hl with dired and magit buffers
  ;; - avoids altering Git workflows or commands
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
  ;;; vcs/vcs-forge.el --- Forge integration for Git forges -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: vcs
  ;;
  ;; Commentary:
  ;; GitHub and GitLab integration via Forge.
  ;;
  ;; This module:
  ;; - enables issue and pull/merge request workflows
  ;; - configures Forge database location explicitly
  ;; - avoids implicit database or network side effects
  ;;
  ;;; Code:

  (eval-when-compile (require 'leaf))

  (require 'utils-path nil t) ;; my/ensure-directory-exists

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
Host **small, domain-specific utilities** that do not justify
a dedicated architectural layer.

This layer exists as a *pragmatic utility shelf*: narrowly scoped helpers
live here to avoid contaminating core abstractions, while remaining easy
to audit, extract, or delete.

**** What this layer does
Utils modules provide *localized assistance* to other layers without
owning workflows, policy, or long-lived behavior.

They are responsible for:

- Small, focused helper functions and interactive commands
- Solving localized or cross-cutting problems that span layers
- Supplying glue code that would be inappropriate in higher layers

Typical responsibilities include:

- Buffer, window, and process housekeeping helpers
- Lightweight automation and defensive cleanup logic
- One-off integrations with external tools or Emacs internals
- Quality-of-life helpers that do not define behavior on their own

**** What this layer does *not* do
Utils modules intentionally do **not**:

- Define global policy, defaults, or invariants
- Perform orchestration or lifecycle management
- Introduce new architectural concepts or abstractions
- Replace, override, or shadow behavior from higher layers
- Encode user-, device-, or environment-specific configuration

Those responsibilities are explicitly owned by other layers.

**** Design constraints
- Utils modules may depend on:
  - =core=
  - =ui=
  - =completion=
  - =orgx=
  - =dev=
  - =vcs=

- Utils modules must **not** be depended on by:
  - =core=
  - =ui=
  - =completion=
  - =orgx=
  - =dev=
  - =vcs=

- All dependencies must be:
  - minimal
  - explicit
  - non-circular

This makes utils a *leaf layer* in the dependency graph.

**** Design principles
- Keep modules **small, single-purpose, and disposable**
- Avoid accumulation of loosely related helpers in one file
- Prefer promotion to a dedicated layer if scope or importance grows
- Deletion must always remain a valid and low-risk refactoring option
- Utilities should be easy to reason about in isolation

**** Observed implementation characteristics
Based on the current configuration:

- Utilities are defensive and side-effect conscious
- Many helpers are interactive diagnostics or maintenance tools
- No utils module is required for startup correctness
- Disabling the entire utils layer should degrade convenience,
  not correctness

**** Module map
| File | Responsibility |
|------+----------------|
| =utils/utils-path.el= | Safe directory and path helpers |
| =utils/utils-async.el= | Minimal asynchronous task helpers |
| =utils/utils-buffers.el= | Buffer lifecycle and cleanup helpers |
| =utils/utils-edit.el= | Editing and save-time helper utilities |
| =utils/utils-dired.el= | Dired convenience helpers |
| =utils/utils-org-agenda.el= | Cached org-agenda file discovery |
| =utils/utils-lint.el= | Static lint helpers for Emacs Lisp |
| =utils/utils-diagnostics.el= | Configuration and runtime diagnostics |
| =utils/utils-functions.el= | Small general-purpose helper functions |

**** Notes
- This layer is intentionally heterogeneous and low-ceremony.
- Utilities should *never* become implicit dependencies.
- If a utils module feels “important”, it likely belongs elsewhere.

This layer exists to make the rest of the system *simpler*, not smarter.

**** Implementation

- Loaded last by =modules.el=
- Each file:
  - provides exactly one utility feature
  - remains safe to load in isolation
- Removing this layer must not break
  any higher-level architectural guarantees

***** utils/utils-functions.el
:PROPERTIES:
:CUSTOM_ID: utils-functions
:header-args:emacs-lisp: :tangle lisp/utils/utils-functions.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-functions.el --- General-purpose small utilities -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Small, safe utility functions shared across the configuration.
  ;;
  ;; This module:
  ;; - provides lightweight editing and navigation helpers
  ;; - includes optional Org and UI convenience functions
  ;; - avoids owning long-lived workflows or global policy
  ;;
  ;;; Code:

  ;;;; Built-ins ------------------------------------------------------------------

  (require 'cl-lib) ;; built-in
  (require 'imenu) ;; built-in2

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

    (when (require 'imenu-list nil 'noerror)
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
        (imenu-list-display-dwim))))

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

***** utils/utils-path.el
:PROPERTIES:
:CUSTOM_ID: utils-path
:header-args:emacs-lisp: :tangle lisp/utils/utils-path.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-path.el --- Minimal filesystem helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Small helpers for safe filesystem operations.
  ;;
  ;; This module:
  ;; - provides defensive directory creation utilities
  ;; - avoids introducing global filesystem policy
  ;; - acts as a fallback when higher-level helpers are absent
  ;;
  ;;; Code:

  (unless (fboundp 'my/ensure-directory-exists)
    (defun my/ensure-directory-exists (dir)
      "Create DIR if it does not exist."
      (unless (file-directory-p dir)
        (make-directory dir t))))

  (provide 'utils-path)
  ;;; utils/utils-path.el ends here
#+end_src

***** utils/utils-async.el
:PROPERTIES:
:CUSTOM_ID: utils-async
:header-args:emacs-lisp: :tangle lisp/utils/utils-async.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-async.el --- Safe asynchronous helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Minimal helpers for running asynchronous tasks safely.
  ;;
  ;; This module:
  ;; - executes tasks asynchronously with error isolation
  ;; - reports failures without interrupting Emacs
  ;; - avoids background scheduling policy
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

***** utils/utils-dired.el
:PROPERTIES:
:CUSTOM_ID: utils-dired
:header-args:emacs-lisp: :tangle lisp/utils/utils-dired.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-dired.el --- Small Dired navigation helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Minor helper commands extending Dired navigation.
  ;;
  ;; This module:
  ;; - provides convenience commands for file viewing
  ;; - avoids redefining or overriding core Dired behavior
  ;; - remains local to interactive use
  ;;
  ;;; Code:

  (declare-function dired-get-file-for-visit "dired")
  (declare-function dired-goto-subdir "dired")

  (defun my/dired-view-file-other-window ()
    "Open Dired file or directory in another window."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (or (and (cdr dired-subdir-alist)
                   (dired-goto-subdir file))
              (dired file))
        (view-file-other-window file))))

  (provide 'utils-dired)
  ;;; utils/utils-dired.el ends here
#+end_src

***** utils/utils-buffers.el
:PROPERTIES:
:CUSTOM_ID: utils-buffers
:header-args:emacs-lisp: :tangle lisp/utils/utils-buffers.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-buffers.el --- Temporary buffer housekeeping -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Automatic cleanup of temporary and dead-process buffers.
  ;;
  ;; This module:
  ;; - periodically removes unused helper buffers
  ;; - never touches visible, modified, or file-backed buffers
  ;; - runs conservatively with explicit opt-in customization
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

***** utils/utils-edit.el
:PROPERTIES:
:CUSTOM_ID: utils-edit
:header-args:emacs-lisp: :tangle lisp/utils/utils-edit.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-edit.el --- Lightweight editing helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Small editing-related helpers for day-to-day workflows.
  ;;
  ;; This module:
  ;; - provides quick buffer revert utilities
  ;; - updates lightweight timestamps on save
  ;; - avoids altering semantic file formats (Org, binaries, etc.)
  ;;
  ;;; Code:

  (defun my/revert-buffer-quick ()
    "Revert current buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defun my/save-buffer-wrapper ()
    "Insert or update a `$Lastupdate` timestamp at the top of the buffer.

  This runs only for:
  - file-backed buffers
  - non-Org text buffers"
    (when (and buffer-file-name
               (derived-mode-p 'text-mode)
               (not (derived-mode-p 'org-mode)))
      (let ((timestamp
             (concat "$Lastupdate: "
                     (format-time-string "%Y/%m/%d %H:%M:%S")
                     " $")))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  "\\$Lastupdate: [0-9/: ]*\\$" nil t)
            (replace-match timestamp t nil))))))

  (add-hook 'before-save-hook #'my/save-buffer-wrapper)

  (provide 'utils-edit)
  ;;; utils/utils-edit.el ends here
#+end_src

***** utils/utils-org-agenda.el
:PROPERTIES:
:CUSTOM_ID: utils-org-agenda
:header-args:emacs-lisp: :tangle lisp/utils/utils-org-agenda.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-org-agenda.el --- Cached org-agenda-files builder -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Build org-agenda-files using a persistent on-disk cache.
  ;;
  ;; This module:
  ;; - scans org-directory defensively
  ;; - stores agenda file lists in a reusable cache
  ;; - avoids runtime penalties during startup and agenda refresh
  ;;
  ;;; Code:

  (require 'subr-x)
  (require 'seq)

  (defgroup utils-org-agenda nil
    "Cached org-agenda-files builder."
    :group 'org)

  (defcustom utils-org-agenda-cache-file
    (expand-file-name "org-agenda-files.cache"
                      (or (bound-and-true-p no-littering-var-directory)
                          user-emacs-directory))
    "Cache file for org-agenda-files."
    :type 'file
    :group 'utils-org-agenda)

  (defcustom utils-org-agenda-exclude-regexp
    "archives"
    "Regexp used to exclude files from org-agenda."
    :type 'regexp
    :group 'utils-org-agenda)

  (defun utils-org-agenda--valid-org-directory-p ()
    "Return non-nil if org-directory is defined and readable."
    (and (boundp 'org-directory)
         (stringp org-directory)
         (file-directory-p org-directory)))

  (defun utils-org-agenda--scan (org-directory)
    "Recursively scan ORG-DIRECTORY and return a list of agenda files."
    (when (file-directory-p org-directory)
      (seq-filter
       (lambda (file)
         (and (string-match-p "\\.org\\'" file)
              (not (file-symlink-p file))
              (not (string-match-p utils-org-agenda-exclude-regexp file))))
       (directory-files-recursively
        org-directory "\\.org\\'" nil nil))))

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
    (if (not (utils-org-agenda--valid-org-directory-p))
        nil
      (let ((cached (and (not force)
                         (utils-org-agenda--load-cache))))
        (if (and cached (listp cached))
            cached
          (let ((files (utils-org-agenda--scan org-directory)))
            (utils-org-agenda--save-cache files)
            files)))))

  ;;;###autoload
  (defun utils-org-agenda-rebuild ()
    "Force a rebuild of the org-agenda-files cache."
    (interactive)
    (setq org-agenda-files (utils-org-agenda-build t))
    (message "org-agenda-files cache rebuilt (%d files)"
             (length org-agenda-files)))

  (provide 'utils-org-agenda)
  ;;; utils/utils-org-agenda.el ends here
#+end_src

***** utils/utils-lint.el
:PROPERTIES:
:CUSTOM_ID: utils-lint
:header-args:emacs-lisp: :tangle lisp/utils/utils-lint.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-lint.el --- Static lint helpers for Emacs Lisp -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Interactive static lint helpers for Emacs Lisp buffers.
  ;;
  ;; This module:
  ;; - integrates checkdoc and package-lint when available
  ;; - provides buffer- and directory-scoped commands
  ;; - never modifies files or editor state
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

***** utils/utils-diagnostics.el
:PROPERTIES:
:CUSTOM_ID: utils-diagnostics
:header-args:emacs-lisp: :tangle lisp/utils/utils-diagnostics.el
:END:
#+begin_src emacs-lisp
  ;;; utils/utils-diagnostics.el --- Diagnostic helpers -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: utils
  ;;
  ;; Commentary:
  ;; Diagnostic helpers for inspecting configuration consistency.
  ;;
  ;; This module:
  ;; - defines interactive diagnostic commands only
  ;; - introduces no global hooks
  ;; - never alters runtime behavior
  ;;
  ;;; Code:

  (defun my/find-keybinding-conflicts ()
    "Find conflicting keybindings in active keymaps."
    (interactive)
    (let ((conflicts (make-hash-table :test 'equal)))
      (dolist (map (current-active-maps t))
        (map-keymap
         (lambda (key cmd)
           (when (commandp cmd)
             (let* ((desc (key-description (vector key)))
                    (lst (gethash desc conflicts)))
               (puthash desc (delete-dups (cons cmd lst)) conflicts))))
         map))
      (with-current-buffer (get-buffer-create "*Keybinding Conflicts*")
        (erase-buffer)
        (maphash
         (lambda (k v)
           (when (> (length v) 1)
             (insert (format "%s → %s\n"
                             k (mapconcat #'symbol-name v ", ")))))
         conflicts)
        (view-mode 1)
        (pop-to-buffer (current-buffer)))))

  (defun my/check-provide-matches-file (&optional buffer)
    "Check whether BUFFER provides a feature matching its file name.

  If BUFFER is nil, use the current buffer.
  This command is purely diagnostic and has no side effects."
    (interactive)
    (let* ((buf (or buffer (current-buffer)))
           (file (buffer-file-name buf)))
      (unless file
        (user-error "Current buffer is not visiting a file"))
      (unless (string-match-p "\\.el\\'" file)
        (user-error "Not an Emacs Lisp file"))
      (let* ((base (file-name-base file))
             (feat (intern base)))
        (if (featurep feat)
            (message "[provide] OK: `%s` is provided" feat)
          (message "[provide] WARNING: `%s` is not provided" feat)))))

  (defun my/check-provide-in-directory (directory)
    "Check provide/filename consistency for all .el files under DIRECTORY."
    (interactive "DDirectory: ")
    (let ((files (directory-files-recursively directory "\\.el\\'")))
      (dolist (file files)
        (with-current-buffer (find-file-noselect file)
          (condition-case err
              (my/check-provide-matches-file (current-buffer))
            (error
             (message "[provide] %s: %s"
                      (file-name-nondirectory file)
                      (error-message-string err))))
          (kill-buffer (current-buffer))))
      (message "Provide check completed")))

  (provide 'utils-diagnostics)
  ;;; utils/utils-diagnostics.el ends here
#+end_src

** Personal Profile & Device Integrati
:PROPERTIES:
:CUSTOM_ID: personal-profile-and-device-integrations
:END:

*** Overview

**** Personal Layer Philosophy
The =personal/= layer provides **user- and device-specific overlays**
on top of the shared, version-controlled configuration.

Its role is to express *identity, environment, and personal workflow glue*
without influencing global policy, architectural decisions, or cross-user
behavior.

This layer exists explicitly to keep the shared system:

- reproducible
- auditable
- portable across machines and users

while still allowing the configuration to feel *native* on a specific device.

**** Scope definition

This layer MAY contain:

- Identity information (user name, email address)
- Personal feature flags and thresholds
  (UI bundle selection, LSP preference, timing knobs)
- Device- and OS-specific glue
  (IME behavior, mouse/scroll tuning, platform quirks)
- Personal keybindings and workflow integrations
- Optional integrations with external, user-owned services

This layer MUST NOT contain:

- Core architectural or cross-user decisions
- Shared defaults or global policy
- Modules that other layers depend on
- Assumptions that affect startup correctness

Hooks and timers are permitted **only** when they are:

- strictly local to the user’s device or workflow
- defensive (safe to no-op when unavailable)
- non-invasive to global behavior

**** Purpose
Provide *personal overlays* that adapt the configuration to a specific
user and machine, without compromising the modularity, determinism,
or reproducibility of shared layers.

Concretely, this layer exists to:

- encode identity and safety-related editor defaults
- select between alternative shared stacks (UI / LSP)
- bind shared infrastructure to local filesystem layout
- integrate OS- and device-specific affordances
- host convenience glue that would be inappropriate elsewhere

**** What this configuration does

***** Identity & safety
- Sets =user-full-name= and =user-mail-address=
- Applies safety-related editor preferences:
  - disables font cache compaction on macOS
  - enables in-memory passphrase caching for =plstore=

***** Look & feel switches
- Declares personal font preferences and default size
- Selects UI and LSP stacks explicitly:
  - =my:use-ui= (e.g. =nano=)
  - =my:use-lsp= (e.g. =eglot=)
- Delegates actual behavior to shared UI and dev layers

***** Directories & Org wiring
- Defines a cloud-rooted workspace (default: =~/Documents=)
- Derives:
  - =my:d:org=
  - =my:d:blog=
- Ensures required directories exist
- Sets =org-directory=
- Computes =org-agenda-files= dynamically by scanning
  non-archive =.org= files
- Removes sensitive or excluded paths from =load-path=

***** macOS input method integration (=sis=)
- Configures ABC ⇄ Kotoeri (Romaji) switching via =macism=
- Enables cursor-color, respect, and inline modes when available
- All functionality is guarded with =fboundp= checks

***** Cursor color keep-alive
- Reapplies the frame’s cursor color to the =cursor= face
  after theme reloads
- Prevents cursor desynchronization across theme switches

***** Device profile (MX Ergo S)
- Applies conservative, smooth scrolling defaults
- Preserves screen position and margins
- Enables tilt scrolling
- Mouse bindings:
  - =mouse-2= → yank
  - =mouse-4/5= → previous / next buffer

***** Apple Music integration (macOS)
- Defines asynchronous and synchronous AppleScript helpers
- Exposes interactive commands:
  - play / pause
  - next / previous track
  - playlist selection
  - current track info
- Provides a Hydra bound to =C-c M=
- Optionally integrates with Meow leader keys when available

**** Module map
| Module file | Role |
|-------------+------|
| =personal/user.el= | Identity, fonts, UI/LSP switches, Org paths |
| =personal/device-darwin.el= | macOS-specific device and IME glue |
| =personal/apple-music.el= | Apple Music control (AppleScript + Hydra) |

**** Execution flow

1. *Personal bootstrap*
   - Identity, fonts, and UI/LSP switches are established
   - Cloud, Org, and blog directories are defined and ensured

2. *Org wiring*
   - =org-directory= is set
   - =org-agenda-files= is derived by filtering non-archive Org files

3. *Hygiene*
   - Sensitive or excluded directories are removed from =load-path=

4. *macOS-only glue*
   - =sis= is configured defensively
   - Cursor color synchronization hook is installed

5. *Device profile*
   - Mouse and scroll tuning for MX Ergo S is applied

6. *Apple Music integration*
   - AppleScript runners are defined
   - Interactive commands and Hydra are exposed

**** Key settings (reference)

- =my:font-default= :: "JetBrains Mono"
- =my:font-variable-pitch= :: "Noto Sans JP"
- =my:font-size= :: 18
- =my:use-ui= :: 'nano
- =my:use-lsp= :: 'eglot
- =org-directory= :: =~/Documents/org=
- =org-agenda-files= ::
  all =*.org= under =org-directory=, excluding =archives=
- MX Ergo scroll profile ::
  - =mouse-wheel-scroll-amount='(1 ((shift) . 5) ((control) . 10))=
  - =scroll-conservatively=10000=
  - =scroll-margin=2=
  - =scroll-preserve-screen-position=t=
- Cursor color keep-alive ::
  reapply =(set-face-background 'cursor (frame-parameter nil 'cursor-color))=
  on =after-load-theme=

**** Usage tips

- *Switch UI or LSP stacks*
  - Change =my:use-ui= to ='nano=, ='doom=, or ='none=
  - Change =my:use-lsp= to ='eglot= or ='lsp=

- *Change fonts*
  - Adjust font variables here
  - Shared UI modules consume them automatically

- *Apple Music control*
  - =C-c M= opens the Hydra
  - Keys:
    - =p= play/pause
    - =n= next
    - =b= back
    - =l= playlist
    - =i= track info

- *Agenda scope control*
  - Place files under an =archives/= directory
  - Or include “archives” in filenames to exclude them

**** Troubleshooting

- *sis does not switch input methods*
  - Verify input source IDs:
    - "com.apple.keylayout.ABC"
    - "com.apple.inputmethod.Kotoeri.RomajiTyping.Japanese"
  - All calls are guarded; missing functions will no-op

- *Cursor color incorrect after theme change*
  - Ensure the theme sets the frame’s =cursor-color= parameter

- *Hydra key missing*
  - Confirm =hydra= is installed and loaded
  - Binding is added via =with-eval-after-load=

- *Meow leader binding missing*
  - Requires both =meow= and =hydra=
  - Binding is added defensively

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
  ;; Commentary:
  ;; Personal configuration overrides.
  ;;
  ;; This module:
  ;; - defines user identity and safety-related variables
  ;; - provides personal feature toggles and threshold overrides
  ;; - supplies directory paths and exclusions for runtime hygiene
  ;;
  ;;; Code:

  ;; ---------------------------------------------------------------------------
  ;; Runtime requirements (do NOT rely on byte-compile)
  ;; ---------------------------------------------------------------------------
  (require 'leaf)
  (require 'seq)

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
  ;; Personal editing preferences
  ;; ---------------------------------------------------------------------------

  (add-hook 'before-save-hook #'my/save-buffer-wrapper)

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
  ;;; personal/device-darwin.el --- macOS device profile -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: personal
  ;;
  ;; Commentary:
  ;; macOS-specific device and input configuration.
  ;;
  ;; This module:
  ;; - configures macOS IME integration and cursor behavior
  ;; - defines mouse and scroll characteristics for a specific device profile
  ;; - applies device-local settings without affecting global policy
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
  ;;; personal/apple-music.el --- Apple Music integration -*- lexical-binding: t; -*-
  ;;
  ;; Copyright (c) 2021-2026
  ;; Author: YAMASHITA, Takao
  ;; License: GNU GPL v3 or later
  ;;
  ;; Category: personal
  ;;
  ;; Commentary:
  ;; Control Apple Music using AppleScript.
  ;;
  ;; This module:
  ;; - provides asynchronous and synchronous AppleScript helpers
  ;; - defines interactive commands to control Apple Music playback
  ;; - integrates playback control with hydra and modal key systems
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
    (with-temp-buffer
      (let ((exit (process-file "osascript" nil t nil "-e" script)))
        (if (eq exit 0)
            (string-trim (buffer-string))
          (error "osascript failed: %s" (string-trim (buffer-string)))))))

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
