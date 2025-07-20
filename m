diff --git a/README.org b/README.org
index e482e2e..ef459e1 100644
--- a/README.org
+++ b/README.org
@@ -800,65 +800,70 @@ This section performs early performance optimizations during Emacs startup,
 such as adjusting garbage collection thresholds and compatibility checks.
 
 #+begin_src emacs-lisp :tangle early-init.el
-  ;;; --- My early-init script -*- mode: emacs-lisp; lexical-binding:t; -*-
-
+  ;;; early-init.el --- Early initialization for Emacs -*- lexical-binding: t; -*-
+  ;;
   ;; Copyright (c) 2021-2025 YAMASHITA, Takao <tjy1965@gmail.com>
   ;; Licensed under the GNU General Public License version 3 or later.
-
+  ;;
   ;;; Commentary:
-
-  ;; This is the `early-init.el` file, introduced in Emacs 27+, and tailored here for Emacs 30+.
   ;;
-  ;; It performs essential early-stage configuration to optimize startup and resource usage:
+  ;; This is an `early-init.el` file designed for Emacs 30+.
+  ;; It performs early-stage optimizations and environment setup:
   ;;
-  ;; - Verifies Emacs version (must be 30 or newer)
-  ;; - Sets up key directories (`my:d`, `.cache/`, etc.)
-  ;; - Increases `gc-cons-threshold` and `read-process-output-max` for faster startup
-  ;; - Schedules post-startup cleanup via `emacs-startup-hook`
+  ;; - Ensures that Emacs 30 or newer is running.
+  ;; - Defines key directories (e.g., `my:d`, `.cache/`).
+  ;; - Optimizes garbage collection (GC) and process I/O thresholds for faster startup.
+  ;; - Configures native compilation cache and performance parameters.
+  ;; - Sets macOS-specific paths and Dired configuration (when applicable).
+  ;; - Adjusts frame behavior (pixelwise resizing and maximized startup).
   ;;
-  ;; Notes:
-  ;; - This file is loaded before the UI is initialized, so avoid UI-specific settings.
-  ;; - Avoid loading packages or setting themes here.
+  ;; Note:
+  ;; - `early-init.el` is loaded *before* the UI is initialized.
+  ;;   Avoid package loading, theme settings, or UI-dependent configuration here.
+  ;; - A modular `init.el` should handle further initialization after startup.
   ;;
-  ;; This early-init is designed to cooperate with a modular `init.el` loaded after startup.
-
   ;;; Code:
-
   ;; ---------------------------------------------------------------------------
-  ;;; Compatibility Check (Emacs 30+)
+  ;;; Compatibility Check (Require Emacs 30+)
   (when (version< emacs-version "30")
     (error "This configuration requires Emacs 30 or higher."))
 
   ;; ---------------------------------------------------------------------------
   ;;; Directories
-  (defvar my:d (if load-file-name
-                   (file-name-directory (file-chase-links load-file-name))
-                 user-emacs-directory)
-    "Base directory for user-specific configuration.")
+  (defvar my:d
+    (if load-file-name
+        (file-name-directory (file-chase-links load-file-name))
+      user-emacs-directory)
+    "Base directory for user-specific configuration files.")
 
-  (defvar my:d:cache (expand-file-name ".cache/" my:d)
-    "Cache directory for temporary files.")
-  (make-directory my:d:cache t) ;; Ensure cache directory exists
+  (defvar my:d:cache
+    (expand-file-name ".cache/" my:d)
+    "Directory for temporary and cache files.")
+  (make-directory my:d:cache t)  ;; Ensure cache directory exists
 
   ;; ---------------------------------------------------------------------------
   ;;; Performance Optimization
-  (setq gc-cons-threshold (* 128 1024 1024)
-        read-process-output-max (* 8 1024 1024))
+  ;; Increase GC threshold and process I/O buffer size during startup.
+  (setq gc-cons-threshold (* 128 1024 1024)    ;; 128 MB
+        read-process-output-max (* 8 1024 1024)) ;; 8 MB
 
   (add-hook 'emacs-startup-hook
             (lambda ()
-              (setq gc-cons-threshold (* 64 1024 1024))
+              ;; Reduce GC threshold after startup.
+              (setq gc-cons-threshold (* 64 1024 1024)) ;; 64 MB
               (message "Emacs loaded in %.2f seconds with %d garbage collections."
                        (float-time (time-subtract after-init-time before-init-time))
                        gcs-done)))
 
+  ;; Disable automatic package initialization at startup.
   (setq package-enable-at-startup nil)
 
   ;; ---------------------------------------------------------------------------
   ;;; Native Compilation Optimization
-  (setq native-comp-async-report-warnings-errors 'error)
+  (setq native-comp-async-report-warnings-errors 'error) ;; Report only errors
   (setq native-comp-async-jobs-number (or (getenv "EMACS_NATIVE_COMP_JOBS") 4))
   (setq native-comp-speed 2)
+
   (when (boundp 'native-comp-eln-load-path)
     (startup-redirect-eln-cache
      (expand-file-name "eln-cache/" my:d:cache)))
@@ -866,23 +871,25 @@ such as adjusting garbage collection thresholds and compatibility checks.
   ;; ---------------------------------------------------------------------------
   ;;; macOS Specific Settings
   (when (eq system-type 'darwin)
-    ;; Homebrew and GCC Paths
+    ;; Add Homebrew paths to exec-path and environment PATH.
     (dolist (path '("/opt/homebrew/bin" "/usr/local/bin"))
       (when (file-directory-p path)
         (add-to-list 'exec-path path)
         (setenv "PATH" (concat path ":" (getenv "PATH")))))
 
-    ;; GNU ls (gls) for Dired
+    ;; Use GNU ls (gls) for Dired, if available.
     (when (executable-find "gls")
       (setq insert-directory-program "gls"
             dired-use-ls-dired t
             dired-listing-switches "-aBhl --group-directories-first")))
 
   ;; ---------------------------------------------------------------------------
-  ;; Use pixelwise resizing and fullscreen by default
+  ;;; Frame Settings
+  ;; Use pixel-precise frame resizing and start maximized.
   (setq frame-resize-pixelwise t)
   (add-to-list 'default-frame-alist '(fullscreen . maximized))
 
+  ;; ---------------------------------------------------------------------------
   (provide 'early-init)
   ;;; early-init.el ends here
 #+end_src
@@ -890,47 +897,50 @@ such as adjusting garbage collection thresholds and compatibility checks.
 ** Initialization
 
 #+begin_src emacs-lisp :tangle init.el
-  ;;; --- Main configuration file -*- mode: emacs-lisp; lexical-binding:t; -*-
-
+  ;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-
+  ;;
   ;; Copyright (c) 2021-2025 YAMASHITA, Takao <tjy1965@gmail.com>
   ;; Licensed under the GNU General Public License version 3 or later.
   ;; Keywords: initialization, modular
-
+  ;;
   ;;; Commentary:
-
-  ;; This is the main configuration file for Emacs.
   ;;
-  ;; It performs the following tasks:
+  ;; This is the primary Emacs configuration file. It is designed to be minimal,
+  ;; delegating most of the setup to modular files and a literate configuration
+  ;; (`README.org`) via Org Babel tangling.
   ;;
-  ;; - Initializes user-specific directory structure (`my:d:cache`, etc.)
-  ;; - Sets up Emacs behavior through modular files
-  ;; - Automatically tangles and loads settings from `README.org` using Org Babel
-  ;; - Loads personal customizations from `custom.el`
+  ;; Features:
+  ;; - Initializes user-specific directory structure (`.cache/`, `.etc/`, `.var/`).
+  ;; - Auto-tangles and loads configuration blocks from `README.org`.
+  ;; - Separates user customizations into `custom.el`.
   ;;
   ;; Dependencies:
-  ;; - Org mode for literate configuration
-  ;; - Emacs 27 or higher is recommended for full compatibility
+  ;; - Org mode (for literate configuration).
+  ;; - Emacs 27+ is recommended for compatibility.
   ;;
   ;; File structure:
-  ;; - `README.org`: Primary literate configuration source
-  ;; - `.cache/`, `.etc/`, `.var/`: Directories for runtime data
+  ;; - `README.org`   : Primary literate configuration source.
+  ;; - `.cache/`      : Runtime cache and package directory.
+  ;; - `.etc/`        : Stores configuration files (e.g., `custom.el`).
+  ;; - `.var/`        : Stores variable runtime data.
   ;;
   ;; Usage:
-  ;; This file is loaded by Emacs on startup. It defers most setup
-  ;; to Org-mode tangling and modular files to keep this file concise.
+  ;; This file is loaded at startup. It remains concise by deferring most
+  ;; setup to `README.org` and modular components.
+  ;;
   ;;; Code:
-
   ;; ---------------------------------------------------------------------------
   ;;; Utility Functions
+
   (defun my:ensure-directory-exists (dir)
-    "Ensure that the directory DIR exists, creating it if necessary."
+    "Ensure that directory DIR exists, creating it if necessary."
     (unless (file-directory-p dir)
       (condition-case err
           (make-directory dir t)
         (error (warn "Failed to create directory: %s - %s" dir err)))))
 
   (defun my:auto-tangle-updated-src-blocks ()
-    "Automatically tangle only updated src blocks when saving README.org."
+    "Automatically tangle updated Org source blocks when saving `README.org`."
     (when (and buffer-file-name
                (string= (file-name-nondirectory buffer-file-name) "README.org"))
       (let ((org-confirm-babel-evaluate nil))
@@ -942,41 +952,39 @@ such as adjusting garbage collection thresholds and compatibility checks.
                         nil 'make-it-local)))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Directories
-  ;; Define essential directories for configuration, cache, and variable data.
+  ;;; Directory Structure
+  ;; Ensure `my:d` (defined in early-init.el) is available.
   (unless (boundp 'my:d)
-    (error "`my:d` is not defined. Make sure early-init.el was loaded."))
+    (error "`my:d` is not defined. Ensure early-init.el was loaded first."))
 
   (defvar my:d:cache (expand-file-name ".cache/" my:d)
     "Cache directory for temporary files.")
   (defvar my:d:etc (expand-file-name ".etc/" my:d)
-    "Directory for storing configuration files.")
+    "Directory for configuration files.")
   (defvar my:d:var (expand-file-name ".var/" my:d)
-    "Directory for storing variable data.")
+    "Directory for variable runtime data.")
   (defvar my:f:custom (expand-file-name "custom.el" my:d:etc)
-    "File for storing user customizations (custom-file).")
+    "File for storing user customizations (`custom-file`).")
 
-  ;; Ensure necessary directories exist
+  ;; Create required directories
   (mapc #'my:ensure-directory-exists (list my:d:cache my:d:etc my:d:var))
 
   ;; ---------------------------------------------------------------------------
   ;;; Custom File Setup
-  ;; Separate custom settings to a dedicated file
+  ;; Store `customize` variables in a separate file.
   (setq custom-file my:f:custom)
   (when (and custom-file (file-exists-p custom-file))
     (ignore-errors (load custom-file)))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Package Settings
-  ;; Configure directories for cleanup.
+  ;;; Package Configuration
+  ;; Place installed packages in `.cache/elpa/`.
   (setq package-user-dir (expand-file-name "elpa/" my:d:cache))
-
-  ;; Ensure package directory exists
   (my:ensure-directory-exists package-user-dir)
 
   ;; ---------------------------------------------------------------------------
   ;;; Load Configuration from README.org
-  ;; Use org-babel to load additional configuration details.
+  ;; Use `org-babel-load-file` to load tangled configuration.
   (setq init-org-file (expand-file-name "README.org" my:d))
 
   (when (file-exists-p init-org-file)
@@ -985,44 +993,44 @@ such as adjusting garbage collection thresholds and compatibility checks.
           (setq org-confirm-babel-evaluate nil)
           (org-babel-load-file init-org-file))
       (error
-       (display-warning 'init (format "Failed to load %s: %s" init-org-file (error-message-string err))
-                        :error))))
-
+       (display-warning
+        'init
+        (format "Failed to load %s: %s"
+                init-org-file (error-message-string err))
+        :error))))
 
   (provide 'init)
   ;;; init.el ends here
 #+end_src
 
-**
-
 ** User Configuqration
 
 #+begin_src emacs-lisp :tangle user.el
-  ;;; --- Personal Configuration -*- mode: emacs-lisp; lexical-binding:t; -*-
-
+  ;;; user.el --- Personal Configuration -*- lexical-binding: t; -*-
+  ;;
   ;; Copyright (c) 2021-2025 YAMASHITA, Takao <tjy1965@gmail.com>
   ;; Licensed under the GNU General Public License version 3 or later.
-
+  ;;
   ;; Keywords: personal, device configuration
-
+  ;;
   ;;; Commentary:
-
+  ;;
   ;; This file contains personal and device-specific configuration settings.
+  ;; It is designed to be loaded after the core configuration.
   ;;
-  ;; Included settings:
-  ;; - Personal identity (full name, email address)
-  ;; - Font customization using `my:font-default` and `my:font-size`
-  ;; - Directory constants for cloud documents and blog development
-  ;; - Miscellaneous performance and compatibility settings
+  ;; Features:
+  ;; - Personal identity (full name, email address).
+  ;; - Font customization using `my:font-default`, `my:font-alt`, and `my:font-size`.
+  ;; - Directory constants for cloud documents and blog development.
+  ;; - Miscellaneous performance and compatibility settings.
+  ;; - Device-specific configuration for Logitech MX Ergo S (macOS).
   ;;
-  ;; This file is intended to be loaded after the core configuration,
-  ;; and defines personal preferences and paths that are unlikely to be shared.
-
   ;;; Code:
-
+  ;; ---------------------------------------------------------------------------
+  ;;; Personal Settings
   (leaf *personal
     :config
-    ;; User identity
+    ;; User identity and preferences
     (setq user-full-name "YAMASHITA, Takao"
           user-mail-address "tjy1965@gmail.com"
           my:font-default "JetBrains Mono"
@@ -1041,13 +1049,13 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
     (defconst my:f:capture-blog-file
       (expand-file-name "all-posts.org" my:d:blog)
-      "File path for blog post capture.")
+      "Path to the file used for blog post capture.")
 
     (defvar my:excluded-directories
       '("/Users/ac1965/Library/Accounts")
-      "List of directories to exclude from certain operations.")
+      "List of directories to exclude from specific operations.")
 
-    ;; Ensure directory exists
+    ;; Directory validation function
     (defun ensure-directory (dir)
       "Ensure that DIR exists and is a directory.
   Skips directories listed in `my:excluded-directories`."
@@ -1059,7 +1067,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
          ((not (file-directory-p dir))
           (warn "Path exists but is not a directory: %s" dir)))))
 
-    ;; Ensure key directories are valid
+    ;; Validate key directories
     (let ((essential-dirs (list my:d:cloud my:d:blog)))
       (mapc #'ensure-directory essential-dirs))
 
@@ -1070,27 +1078,25 @@ such as adjusting garbage collection thresholds and compatibility checks.
                       load-path)))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Logitech MX Ergo S Configuration (macOS)
+  ;;; Logitech MX Ergo S (macOS)
   (leaf *device/MX_ErgoS
     :config
-    ;; Basic mouse settings
-    (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 10)))
-    (setq mouse-wheel-progressive-speed nil)  ; disable acceleration
-
-    ;; Smooth scrolling
-    (setq scroll-conservatively 10000)
-    (setq scroll-margin 2)
-    (setq scroll-preserve-screen-position t)
-
-    ;; macOS specific settings
+    ;; Mouse and scroll settings
+    (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 10))
+          mouse-wheel-progressive-speed nil  ;; Disable acceleration
+          scroll-conservatively 10000        ;; Smooth scrolling
+          scroll-margin 2
+          scroll-preserve-screen-position t)
+
+    ;; macOS-specific mouse settings
     (setq mac-mouse-wheel-smooth-scroll t
           mouse-wheel-tilt-scroll t
           mouse-wheel-flip-direction nil)
 
     ;; Trackball button configuration
-    (global-set-key [mouse-2] 'yank)             ; middle click to paste
-    (global-set-key [mouse-4] 'previous-buffer)  ; extra button 1
-    (global-set-key [mouse-5] 'next-buffer))
+    (global-set-key [mouse-2] 'yank)             ;; Middle click -> paste
+    (global-set-key [mouse-4] 'previous-buffer)  ;; Extra button 1
+    (global-set-key [mouse-5] 'next-buffer))     ;; Extra button 2
 
   (provide 'user)
   ;;; user.el ends here
@@ -1105,7 +1111,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
   ;; Copyright (c) 2021-2025 YAMASHITA, Takao <tjy1965@gmail.com>
   ;; Licensed under the GNU General Public License version 3 or later.
 
-  ;; $Lastupdate: 2025/07/19 23:35:02 $
+  ;; $Lastupdate: 2025/07/20 09:55:10 $
 
   ;;; Commentary:
   ;; It includes package management, user-specific settings, and modular design.
@@ -1219,23 +1225,26 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Insert Timestamp on Save
-  ;; - Automatically updates a `$Lastupdate` timestamp in the file before saving.
+  ;; This automatically updates a `$Lastupdate` timestamp in the file
+  ;; before saving. It searches for `$Lastupdate ...$` and replaces the
+  ;; content with the current date and time.
 
   (defun my:save-buffer-wrapper ()
-    "Insert a timestamp at the top of the buffer before saving."
+    "Insert or update a `$Lastupdate` timestamp at the top of the buffer."
     (interactive)
-    (let ((tostr (concat "$Lastupdate: " (format-time-string "%Y/%m/%d %H:%M:%S") " $")))
+    (let ((timestamp (concat "$Lastupdate: " (format-time-string "%Y/%m/%d %H:%M:%S") " $")))
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "\\$Lastupdate\\([0-9/: ]*\\)?\\$" nil t)
-          (replace-match tostr t nil)))))
+          (replace-match timestamp t nil)))))
 
   (add-hook 'before-save-hook #'my:save-buffer-wrapper)
 
   ;; -----------------------------------------------------------------------------
   ;;; TRAMP Setup
-  ;; - Configures TRAMP (remote file editing) to use `scp` as the default method.
-  ;; - Stores TRAMP-related temporary files in `no-littering` directories.
+  ;; Configure TRAMP for remote file editing:
+  ;; - Use `scp` as the default remote copy method.
+  ;; - Store TRAMP-related files in `no-littering` directories.
 
   (leaf tramp
     :pre-setq
@@ -1243,12 +1252,12 @@ such as adjusting garbage collection thresholds and compatibility checks.
       (tramp-auto-save-directory . ,(concat no-littering-var-directory "tramp-autosave")))
     :custom
     `((tramp-default-method . "scp")
-      (tramp-verbose . 10)))
+      (tramp-verbose . 10))) ;; Set verbosity to 10 for detailed TRAMP logs.
 
   ;; -----------------------------------------------------------------------------
   ;;; Auto-Save and Backup Configuration
-  ;; - Redirects backup and auto-save files to `no-littering` directories.
-  ;; - Automatically saves visited files every 2 seconds.
+  ;; Redirects backup and auto-save files to `no-littering` directories.
+  ;; Automatically saves visited files every 2 seconds.
 
   (leaf files
     :custom
@@ -1265,7 +1274,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Saveplace (Remember Cursor Positions)
-  ;; - Restores the last cursor position when reopening files.
+  ;; Restores the last cursor position when reopening files.
 
   (leaf saveplace
     :init
@@ -1274,7 +1283,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Recentf (Recently Opened Files)
-  ;; - Keeps a list of recently opened files, saved to `no-littering` var dir.
+  ;; Maintains a list of recently opened files, stored under `no-littering`.
 
   (leaf recentf
     :init
@@ -1284,18 +1293,18 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Savehist (Minibuffer History Persistence)
-  ;; - Saves command and search history for better UX.
+  ;; Saves minibuffer history (e.g., commands, searches) across sessions.
 
   (leaf savehist
     :custom
     `((savehist-file . ,(concat no-littering-var-directory "savehist"))
       (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
-      (savehist-autosave-interval . 300))  ;; Save every 5 minutes
+      (savehist-autosave-interval . 300))  ;; Save every 5 min
     :global-minor-mode t)
 
   ;; -----------------------------------------------------------------------------
   ;;; Paredit (Structured Editing)
-  ;; - Enables Paredit in Emacs Lisp mode for safer parentheses handling.
+  ;; Enables Paredit in Emacs Lisp mode for strict parenthesis management.
 
   (leaf paredit
     :ensure t
@@ -1303,7 +1312,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Paren Highlighting
-  ;; - Highlights matching parentheses instantly.
+  ;; Highlights matching parentheses instantly with full expression style.
 
   (leaf paren
     :custom
@@ -1313,9 +1322,9 @@ such as adjusting garbage collection thresholds and compatibility checks.
     :global-minor-mode show-paren-mode)
 
   ;; -----------------------------------------------------------------------------
-  ;;; Puni (Smart Pairing)
-  ;; - Provides smarter handling of paired delimiters (braces, brackets, etc.).
-  ;; - Globally enabled, but disabled in minibuffer to avoid conflicts.
+  ;;; Puni (Smart Pair Handling)
+  ;; Provides intelligent paired delimiter operations.
+  ;; Globally enabled but disabled in minibuffer to avoid conflicts.
 
   (leaf puni
     :ensure t
@@ -1324,8 +1333,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Tree-Sitter (Modern Syntax Highlighting)
-  ;; - Enables Tree-Sitter for advanced syntax highlighting and parsing.
-  ;; - Sets font-lock level for rich syntax features.
+  ;; Enables Tree-Sitter for advanced syntax parsing and rich font-lock.
 
   (leaf tree-sitter
     :ensure t
@@ -1336,22 +1344,21 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Tree-Sitter-Langs (Language Grammars)
-  ;; - Installs and manages Tree-Sitter grammars for various languages.
-  ;; - Automatically installs grammars if missing.
+  ;; Installs and manages Tree-Sitter grammars for supported languages.
 
   (leaf tree-sitter-langs
     :ensure t
     :config
     (when (require 'tree-sitter-langs nil t)
-      (unless (ignore-errors (directory-files (concat tree-sitter-langs--bin-dir "grammars/")))
+      (unless (ignore-errors
+                (directory-files (concat tree-sitter-langs--bin-dir "grammars/")))
         (condition-case err
             (tree-sitter-langs-install-grammars)
-          (error (message "Failed to install Tree-sitter grammars: %s" err))))))
+          (error (message "Failed to install Tree-Sitter grammars: %s" err))))))
 
   ;; -----------------------------------------------------------------------------
   ;;; Auto-Revert
-  ;; - Automatically reloads files when they change on disk.
-  ;; - Updates every 2 seconds without displaying messages.
+  ;; Automatically reloads files when changed on disk (silent refresh every 2s).
 
   (leaf autorevert
     :custom
@@ -1361,7 +1368,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Which-Key (Key Binding Hints)
-  ;; - Displays available keybindings in a popup when typing a prefix.
+  ;; Shows available keybindings in a popup for the current prefix.
 
   (leaf which-key
     :ensure t
@@ -1370,7 +1377,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Undo-Fu (Advanced Undo/Redo)
-  ;; - Provides a better undo/redo mechanism with linear history.
+  ;; Provides linear undo/redo history with better region handling.
 
   (leaf undo-fu
     :ensure t
@@ -1378,7 +1385,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Hydra (Keybinding Framework)
-  ;; - Defines transient keymaps for repeated commands (e.g., text scaling).
+  ;; Defines transient keymaps for repeating related commands.
 
   (leaf hydra
     :ensure t)
@@ -1389,7 +1396,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Hydra for Text Scaling
-  ;; - Provides a quick interface to increase/decrease/reset text size.
+  ;; Provides quick keybindings to increase, decrease, or reset text size.
 
   (defhydra hydra-text-scale (:hint nil :color red)
     "
@@ -1404,81 +1411,87 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Common Key Bindings
-  ;; - Sets up frequently used keybindings for navigation, file operations,
-  ;;   search, text manipulation, and Org mode.
+  ;; Defines frequently used keybindings for:
+  ;; - Navigation (buffers, windows)
+  ;; - File operations
+  ;; - Text editing (scaling, commenting, alignment)
+  ;; - Search (consult, ripgrep)
+  ;; - Org mode (agenda, capture, roam)
+  ;; - Git (magit)
+  ;; - Miscellaneous (restart, execute commands)
 
   (leaf-keys
    ;; Function keys and help
-   (("<f1>"          . help)
-    ("<f8>"          . treemacs)
-    ("C-h"           . backward-delete-char)
+   (("<f1>"    . help)
+    ("<f8>"    . treemacs)
+    ("C-h"     . backward-delete-char)
 
     ;; Undo/redo
-    ("C-/"           . undo-fu-only-undo)
-    ("C-?"           . undo-fu-only-redo)
+    ("C-/"     . undo-fu-only-undo)
+    ("C-?"     . undo-fu-only-redo)
 
     ;; Text scaling
-    ("C-+"           . text-scale-increase)
-    ("C--"           . text-scale-decrease)
-    ("C-c z"         . hydra-text-scale/body)
+    ("C-+"     . text-scale-increase)
+    ("C--"     . text-scale-decrease)
+    ("C-c z"   . hydra-text-scale/body)
 
     ;; Buffer navigation
-    ("s-n"           . next-buffer)
-    ("s-p"           . previous-buffer)
-    ("s-<up>"        . beginning-of-buffer)
-    ("s-<down>"      . end-of-buffer)
-    ("C-c b"         . consult-buffer)
+    ("s-n"     . next-buffer)
+    ("s-p"     . previous-buffer)
+    ("s-<up>"  . beginning-of-buffer)
+    ("s-<down>". end-of-buffer)
+    ("C-c b"   . consult-buffer)
 
     ;; Window management
-    ("C-."           . other-window)
-    ("C-c 2"         . my:toggle-window-split)
-    ("s-."           . ace-swap-window)
-    ("s-d"           . delete-frame)
-    ("s-m"           . (lambda () (interactive)
-                         (let ((frame (make-frame)))
-                           (with-selected-frame frame
-                             (switch-to-buffer (generate-new-buffer "untitled"))))))
+    ("C-."     . other-window)
+    ("C-c 2"   . my:toggle-window-split)
+    ("s-."     . ace-swap-window)
+    ("s-d"     . delete-frame)
+    ("s-m"     . (lambda () (interactive)
+                   (let ((frame (make-frame)))
+                     (with-selected-frame frame
+                       (switch-to-buffer (generate-new-buffer "untitled"))))))
 
     ;; File operations
-    ("s-j"           . find-file-other-window)
-    ("s-o"           . find-file-other-frame)
-    ("C-c o"         . find-file)
-    ("C-c v"         . find-file-read-only)
-    ("C-c V"         . view-file-other-window)
-    ("C-c k"         . kill-buffer-and-window)
+    ("s-j"     . find-file-other-window)
+    ("s-o"     . find-file-other-frame)
+    ("C-c o"   . find-file)
+    ("C-c v"   . find-file-read-only)
+    ("C-c V"   . view-file-other-window)
+    ("C-c k"   . kill-buffer-and-window)
 
     ;; Search
-    ("C-s"           . consult-line)
-    ("C-c r"         . consult-ripgrep)
+    ("C-s"     . consult-line)
+    ("C-c r"   . consult-ripgrep)
 
     ;; Text manipulation
-    ("C-="           . er/expand-region)
-    ("C-c M-a"       . align-regexp)
-    ("C-c ;"         . comment-or-uncomment-region)
-    ("C-c l"         . display-line-numbers-mode)
+    ("C-="     . er/expand-region)
+    ("C-c M-a" . align-regexp)
+    ("C-c ;"   . comment-or-uncomment-region)
+    ("C-c l"   . display-line-numbers-mode)
 
-    ;; Org mode and Roam
-    ("C-c d a"       . org-agenda)
-    ("C-c d c"       . org-capture)
-    ("C-c d i"       . org-roam-node-insert)
-    ("C-c d f"       . org-roam-node-find)
+    ;; Org mode & Roam
+    ("C-c d a" . org-agenda)
+    ("C-c d c" . org-capture)
+    ("C-c d i" . org-roam-node-insert)
+    ("C-c d f" . org-roam-node-find)
 
     ;; Misc
-    ("M-x"           . execute-extended-command)
-    ("C-x g"         . magit-status)
-    ("s-r"           . restart-emacs)))
+    ("M-x"     . execute-extended-command)
+    ("C-x g"   . magit-status)
+    ("s-r"     . restart-emacs)))
 
-  ;; Enable directional window navigation with Shift + arrow keys
+  ;; Enable directional window navigation with Shift + arrow keys.
   (windmove-default-keybindings)
 
   ;; -----------------------------------------------------------------------------
   ;;; Dired Enhancements
-  ;; - Adds a custom `z` key in Dired to open files in another window.
+  ;; Adds a custom `z` key in Dired to open files in another window.
 
   (add-hook 'dired-mode-hook
             (lambda ()
               (define-key dired-mode-map "z"
-                          'my:dired-view-file-other-window)))
+  			'my:dired-view-file-other-window)))
 #+end_src
 
 **** System Utilities
@@ -1486,7 +1499,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; GCMH (Garbage Collection Magic Hack)
-  ;; - Optimizes Emacs garbage collection to reduce UI pauses.
+  ;; Dynamically optimizes Emacs garbage collection to reduce UI stuttering
+  ;; by adjusting `gc-cons-threshold` based on user activity.
 
   (leaf gcmh
     :ensure t
@@ -1494,7 +1508,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Shell Environment Variables
-  ;; - Defines a list of environment variables to import on macOS.
+  ;; List of environment variables to import from the user's shell
+  ;; (mainly for macOS and GUI Emacs).
 
   (defvar my:shell-env-vars
     '("PATH" "MANPATH" "PASSWORD_STORE_DIR" "GPG_KEY_ID" "OPENROUTER_API_KEY")
@@ -1502,7 +1517,9 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Exec-Path-from-Shell
-  ;; - Ensures Emacs inherits environment variables on macOS.
+  ;; Ensures Emacs inherits essential environment variables on macOS.
+  ;; This is particularly important when Emacs is launched as a GUI app,
+  ;; since GUI apps do not automatically inherit the user's shell environment.
 
   (leaf exec-path-from-shell
     :ensure t
@@ -1628,22 +1645,26 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; LSP Configuration (Eglot or LSP-Mode)
-  ;; - Provides language server protocol (LSP) support for intelligent code features.
-  ;; - `my:use-lsp` determines which backend is used: `eglot` (default) or `lsp-mode`.
+  ;; Provides Language Server Protocol (LSP) support for intelligent code features.
+  ;; `my:use-lsp` determines which backend to use:
+  ;; - `eglot` (default, lightweight)
+  ;; - `lsp`   (LSP-Mode, feature-rich)
 
-  (defvar my:use-lsp 'eglot) ;; Change to 'lsp to use lsp-mode instead of eglot.
+  (defvar my:use-lsp 'eglot
+    "LSP backend selection. Use `eglot` (default) or `lsp`.")
 
   ;; -----------------------------------------------------------------------------
   ;;; Eglot (Lightweight LSP Client)
-  ;; - Automatically starts language servers in `prog-mode`.
-  ;; - Configures helpful keybindings for code actions, rename, etc.
+  ;; - Starts language servers automatically in `prog-mode`.
+  ;; - Provides essential LSP features (rename, code actions, diagnostics).
+  ;; - Uses Flymake for on-the-fly diagnostics.
 
   (when (eq my:use-lsp 'eglot)
     (leaf eglot
       :hook (prog-mode . eglot-ensure)
       :custom
-      `((eglot-autoshutdown . t)
-        (eglot-sync-connect . nil)
+      `((eglot-autoshutdown . t)      ;; Stop servers when not in use
+        (eglot-sync-connect . nil)    ;; Connect asynchronously
         (eglot-events-buffer-size . 200))
       :bind (:eglot-mode-map
              ("C-c h" . eglot-help-at-point)
@@ -1652,31 +1673,35 @@ such as adjusting garbage collection thresholds and compatibility checks.
              ("C-c d" . flymake-show-buffer-diagnostics))))
 
   ;; -----------------------------------------------------------------------------
-  ;;; LSP-Mode (Optional Full-Featured LSP)
-  ;; - If `my:use-lsp` is set to `lsp`, this will be activated.
-  ;; - Provides more advanced features like breadcrumbs and integrated diagnostics.
+  ;;; LSP-Mode (Full-Featured LSP Client)
+  ;; - Activated when `my:use-lsp` is set to `lsp`.
+  ;; - Includes advanced features such as:
+  ;;   - Breadcrumb navigation
+  ;;   - Extensive diagnostics
+  ;;   - Enhanced completion
 
   (when (eq my:use-lsp 'lsp)
     (leaf lsp-mode
       :ensure t
-      :hook ((python-mode . lsp)
-             (rust-mode . lsp)
-             (go-mode . lsp)
-             (js-mode . lsp)
-             (typescript-mode . lsp)
-             (c-mode . lsp)
-             (c++-mode . lsp))
+      :hook ((python-mode      . lsp)
+             (rust-mode        . lsp)
+             (go-mode          . lsp)
+             (js-mode          . lsp)
+             (typescript-mode  . lsp)
+             (c-mode           . lsp)
+             (c++-mode         . lsp))
       :custom
-      `((lsp-enable-snippet . t)
-        (lsp-idle-delay . 0.5)
+      `((lsp-enable-snippet . t)            ;; Enable snippet completion
+        (lsp-idle-delay . 0.5)              ;; Delay before LSP actions
         (lsp-headerline-breadcrumb-enable . t)
-        (lsp-prefer-flymake . nil))
+        (lsp-prefer-flymake . nil))         ;; Use Flycheck instead of Flymake
       :config
       (setq lsp-completion-provider :capf)))
 
   ;; -----------------------------------------------------------------------------
   ;;; LSP UI Enhancements
-  ;; - Adds inline documentation, code actions, and diagnostics UI for LSP-Mode.
+  ;; - Adds inline documentation, diagnostics, and code action hints.
+  ;; - Works only when using LSP-Mode.
 
   (leaf lsp-ui
     :ensure t
@@ -1695,27 +1720,25 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Org Mode Configuration
-  ;; - Configures Org Mode for notes, tasks, agendas, and GTD-style workflows.
-  ;; - Defines org-directory, agenda files, capture templates, and more.
+  ;; Provides a GTD-style workflow with notes, tasks, agendas, and capture templates.
 
   (leaf org
     :leaf-defer t
     :preface
-    ;; Define Org Cloud Directory
+    ;; Org directory setup
     (defvar warning-suppress-types nil)
     (unless (boundp 'my:d:cloud)
       (setq my:d:cloud (concat no-littering-var-directory "./")))
 
-    ;; Return list of opened Org mode buffer files
+    ;; Utility: List all open Org files
     (defun org-buffer-files ()
-      "Return a list of opened Org mode buffer files."
+      "Return a list of currently open Org files."
       (delq nil
-            (mapcar (lambda (buf) (buffer-file-name buf))
-                    (org-buffer-list 'files))))
+            (mapcar #'buffer-file-name (org-buffer-list 'files))))
 
-    ;; Show Org buffer file in current window
+    ;; Utility: Show a specific Org file in current buffer
     (defun show-org-buffer (file)
-      "Show an org FILE in the current buffer."
+      "Display an Org FILE from `org-directory`."
       (interactive (list (read-file-name "Org file: " org-directory nil t)))
       (let ((filepath (expand-file-name file org-directory)))
         (if (get-file-buffer filepath)
@@ -1724,28 +1747,29 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
     :custom ((org-support-shift-select . t))
     :init
-    ;; Set Org Directory
+    ;; Org directory
     (setq org-directory (expand-file-name "org/" my:d:cloud))
     (my:ensure-directory-exists org-directory)
 
-    ;; Link and Cache Settings
+    ;; Link & cache settings
     (setq org-return-follows-link t
           org-mouse-1-follows-link t
           warning-suppress-types (append warning-suppress-types '((org-element-cache)))
           org-element-use-cache nil)
 
-    ;; LaTex to PDF Export
+    ;; PDF export (LaTeX)
     (setq org-latex-pdf-process
-  	'("pdflatex -interaction nonstopmode -output-directory %o %f"
+          '("pdflatex -interaction nonstopmode -output-directory %o %f"
             "pdflatex -interaction nonstopmode -output-directory %o %f"))
 
+    ;; Key bindings for quick access to major Org files
     :bind
-    (("C-M--" . #'(lambda () (interactive) (show-org-buffer "gtd.org")))
-     ("C-M-^" . #'(lambda () (interactive) (show-org-buffer "notes.org")))
-     ("C-M-~" . #'(lambda () (interactive) (show-org-buffer "kb.org"))))
+    (("C-M--" . (lambda () (interactive) (show-org-buffer "gtd.org")))
+     ("C-M-^" . (lambda () (interactive) (show-org-buffer "notes.org")))
+     ("C-M-~" . (lambda () (interactive) (show-org-buffer "kb.org"))))
 
     :config
-    ;; General Org Settings
+    ;; General Org settings
     (setq org-agenda-files (list org-directory)
           org-cycle-emulate-tab 'white-space
           org-default-notes-file "notes.org"
@@ -1754,28 +1778,26 @@ such as adjusting garbage collection thresholds and compatibility checks.
           org-log-done 'time
           org-startup-folded 'content
           org-startup-truncated nil
-          org-use-speed-commands t)
-
-    ;; File Link Settings
-    (setq org-link-frame-setup '((file . find-file)))
+          org-use-speed-commands t
+          org-link-frame-setup '((file . find-file)))
 
-    ;; Agenda File Configuration (excluding archives)
+    ;; Agenda files (exclude archives)
     (setq org-agenda-files
           (seq-filter (lambda (file)
                         (not (string-match-p "archives" file)))
                       (directory-files-recursively org-directory "\\.org$")))
 
-    ;; TODO Keywords
+    ;; TODO keywords
     (setq org-todo-keywords
           '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
 
-    ;; Refile Targets
+    ;; Refile targets
     (setq org-refile-targets
           '((nil :maxlevel . 3)
             (org-buffer-files :maxlevel . 1)
             (org-agenda-files :maxlevel . 3)))
 
-    ;; Capture Templates for tasks, notes, journals, etc.
+    ;; Capture templates
     (setq org-capture-templates
           `(("t" "Todo" entry (file+headline ,(expand-file-name "gtd.org" org-directory) "Inbox")
              "* TODO %?\n %i\n %a")
@@ -1788,8 +1810,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Org Modern Styling
-  ;; - Makes Org Mode visually appealing by hiding markup, improving indentation,
-  ;;   and customizing agenda and ellipsis display.
+  ;; Improves Org visual style with cleaner headings, ellipsis, and agenda tweaks.
 
   (leaf org-modern
     :config
@@ -1820,14 +1841,19 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Org Superstar (Pretty Headings)
-  ;; - Replaces default headline bullets with custom Unicode symbols.
+  ;; Enhances the visual appearance of Org headlines by replacing the default
+  ;; asterisks with a set of Unicode symbols.
 
   (leaf org-superstar
     :after org
     :custom
+    ;; Custom bullet symbols for different heading levels
     (org-superstar-headline-bullets-list . '("◉" "★" "○" "▷"))
+    ;; Keep leading stars (set to `t` to remove them completely)
     (org-superstar-remove-leading-stars . nil)
-    :hook (org-mode . org-superstar-mode))
+    :hook
+    ;; Enable `org-superstar-mode` automatically for Org buffers
+    (org-mode . org-superstar-mode))
 #+end_src
 
 ***** Additional Org-related packages
@@ -1835,15 +1861,16 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Org LaTeX Export Configuration
-  ;; - Adds common LaTeX packages for better PDF exports.
-  ;; - Configures `pdflatex` with multiple passes and BibTeX integration.
+  ;; Adds common LaTeX packages and defines a multi-pass `pdflatex` build pipeline
+  ;; with BibTeX integration for high-quality PDF exports.
 
   (leaf org-latex
     :after org
     :custom
-    (org-latex-packages-alist '(("" "graphicx" t)
-                                ("" "longtable" nil)
-                                ("" "wrapfig" nil)))
+    (org-latex-packages-alist
+     '(("" "graphicx" t)
+       ("" "longtable" nil)
+       ("" "wrapfig" nil)))
     (setq org-latex-pdf-process
           '("pdflatex -interaction nonstopmode -output-directory %o %f"
             "bibtex %b"
@@ -1852,7 +1879,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Org Journal
-  ;; - Daily journal entries with agenda integration.
+  ;; Daily journaling with agenda integration.
 
   (leaf org-journal
     :ensure t
@@ -1866,7 +1893,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Org Babel (Code Execution in Org)
-  ;; - Enables execution of code blocks in supported languages.
+  ;; Enables execution of code blocks in multiple languages.
 
   (leaf ob
     :after org
@@ -1882,9 +1909,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
        (plantuml . t))))
 
   ;; -----------------------------------------------------------------------------
-  ;;; Org-Roam (Networked Note-Taking)
-  ;; - Provides a personal knowledge base with backlinks and graph views.
-  ;; - Automatically synchronizes the database with changes.
+  ;;; Org Roam (Networked Note-Taking)
+  ;; A personal knowledge base with backlinks and a graph-based view.
 
   (leaf org-roam
     :ensure t
@@ -1896,9 +1922,9 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (org-roam-db-autosync-mode))
 
   ;; -----------------------------------------------------------------------------
-  ;;; Org-Download (Image Management)
-  ;; - Enables drag-and-drop or clipboard image insertion in Org files.
-  ;; - Stores images in a "pictures" subdirectory.
+  ;;; Org Download (Image Management)
+  ;; Enables drag-and-drop or clipboard-based image insertion into Org files.
+  ;; Images are stored in an "pictures" directory under `org-directory`.
 
   (leaf org-download
     :ensure t
@@ -1910,7 +1936,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; TOC-Org (Table of Contents)
-  ;; - Automatically generates and updates tables of contents in Org or Markdown.
+  ;; Automatically generates and updates tables of contents for Org and Markdown.
 
   (leaf toc-org
     :ensure t
@@ -1920,8 +1946,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (add-hook 'markdown-mode-hook 'toc-org-mode))
 
   ;; -----------------------------------------------------------------------------
-  ;;; Org-Cliplink (Insert Clickable Links)
-  ;; - Fetches and inserts a link with the title of a webpage.
+  ;;; Org Cliplink (Insert Clickable Links)
+  ;; Fetches the title of a webpage and inserts a properly formatted Org link.
 
   (leaf org-cliplink
     :ensure t
@@ -1930,7 +1956,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Org Export to Hugo (Static Site Generation)
-  ;; - Integrates Org Mode with the Hugo static site generator.
+  ;; Exports Org content to the Hugo static site generator format.
 
   (leaf ox-hugo
     :ensure t
@@ -1940,7 +1966,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Hugo Blog Capture Template
-  ;; - Provides a template for creating blog posts directly from Org-Capture.
+  ;; Adds an Org-Capture template for quickly creating new Hugo blog posts.
 
   (leaf *ox-hugo--capture
     :require org-capture
@@ -1957,7 +1983,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Markdown Mode
-  ;; - Enables `markdown-mode` for `.md` files.
+  ;; Enables `markdown-mode` for `.md` files.
 
   (leaf markdown-mode
     :ensure t
@@ -1969,14 +1995,14 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Visual Line Mode
-  ;; - Enables soft line wrapping for text files.
+  ;; Enables soft line wrapping for text-based buffers.
 
   (leaf visual-line-mode
     :hook (text-mode . visual-line-mode))
 
   ;; -----------------------------------------------------------------------------
   ;;; macOS Clipboard Integration
-  ;; - Enables `pbcopy` to share clipboard with macOS.
+  ;; Ensures Emacs uses the macOS clipboard via `pbcopy`.
 
   (leaf pbcopy
     :if (memq window-system '(mac ns))
@@ -1986,18 +2012,19 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Dired Enhancements
-  ;; - Adds filtering and subtree expansion to `dired`.
+  ;; Adds filtering and subtree expansion to Dired.
 
   (leaf dired-filter :ensure t)
-  (leaf dired-subtree :ensure t
+  (leaf dired-subtree
+    :ensure t
     :after dired
     :bind (:dired-mode-map
-           ("i" . dired-subtree-insert)
+           ("i"   . dired-subtree-insert)
            ("TAB" . dired-subtree-toggle)))
 
   ;; -----------------------------------------------------------------------------
   ;;; Editing Tools
-  ;; - Expands regions by semantic units and enforces auto-indentation.
+  ;; Region expansion, aggressive indentation, and selection overwrite.
 
   (leaf expand-region :ensure t)
   (leaf aggressive-indent
@@ -2008,37 +2035,37 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Search Tools
-  ;; - Configures `rg` (ripgrep) as the default search tool.
+  ;; Configures `rg` (ripgrep) as the default search backend.
 
   (setq grep-program "rg")
   (leaf rg :ensure t)
 
   ;; -----------------------------------------------------------------------------
   ;;; Code Navigation
-  ;; - Uses Dumb-Jump for quick symbol navigation with `rg`.
+  ;; Uses Dumb-Jump with `rg` for fast symbol navigation.
 
   (leaf dumb-jump
     :ensure t
     :hook (xref-backend-functions . dumb-jump-xref-activate)
     :custom
-    `((dumb-jump-force-searcher . 'rg)
+    `((dumb-jump-force-searcher  . 'rg)
       (dumb-jump-prefer-searcher . 'rg)))
 
   ;; -----------------------------------------------------------------------------
   ;;; Multiple Cursors
-  ;; - Enables editing with multiple cursors.
+  ;; Enables simultaneous editing with multiple cursors.
 
   (leaf multiple-cursors :ensure t)
 
   ;; -----------------------------------------------------------------------------
   ;;; Magit (Git Integration)
-  ;; - Provides a powerful Git UI.
+  ;; A powerful and user-friendly Git interface.
 
   (leaf magit :ensure t)
 
   ;; -----------------------------------------------------------------------------
-  ;;; Syntax and Spell Checking
-  ;; - Configures Flycheck for syntax checks and Flyspell for spell-checking.
+  ;;; Syntax & Spell Checking
+  ;; Configures Flycheck (syntax) and Flyspell (spelling).
 
   (leaf flycheck
     :ensure t
@@ -2051,15 +2078,15 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Project Management
-  ;; - Uses Projectile for fast project navigation.
+  ;; Projectile for project navigation and search.
 
   (leaf projectile
     :ensure t
     :global-minor-mode t)
 
   ;; -----------------------------------------------------------------------------
-  ;;; Snippet Management with YASnippet
-  ;; - Loads user-defined snippets from `my:yas-snippet-dir`.
+  ;;; Snippet Management (YASnippet)
+  ;; Loads user-defined snippets from `my:yas-snippet-dir`.
 
   (leaf yasnippet
     :ensure t
@@ -2067,11 +2094,9 @@ such as adjusting garbage collection thresholds and compatibility checks.
     :init
     (defvar my:yas-snippet-dir (concat my:d "snippets")
       "Default directory for YASnippet user snippets.")
-
-    ;; Automatically create snippet directory if it does not exist
+    ;; Create snippet directory if it doesn't exist
     (unless (file-directory-p my:yas-snippet-dir)
       (make-directory my:yas-snippet-dir t))
-
     :config
     (setq yas-snippet-dirs (list my:yas-snippet-dir))
     (yas-reload-all))
@@ -2082,15 +2107,15 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; AUCTeX (LaTeX Editing)
-  ;; - Configures AUCTeX for PDF-based workflows using `latexmk`.
+  ;; Configures AUCTeX for PDF-based workflows with `latexmk`.
 
   (leaf auctex
     :ensure t
     :init
-    (setq TeX-auto-save t)
-    (setq TeX-parse-self t)
-    (setq TeX-save-query nil)
-    (setq TeX-PDF-mode t)
+    (setq TeX-auto-save t
+          TeX-parse-self t
+          TeX-save-query nil
+          TeX-PDF-mode t)
     (setq-default TeX-master nil)
     :config
     (setq TeX-command-default "LatexMk")
@@ -2103,7 +2128,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; -----------------------------------------------------------------------------
   ;;; Authentication Management
-  ;; - Manages credentials securely with `auth-source`, `pass`, and GPG.
+  ;; Secure credential management using `auth-source`, `pass`, and GPG.
 
   (leaf *authentication
     :init
@@ -2133,7 +2158,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
     ;; Password-store and auth-source-pass
     (leaf password-store :ensure t)
-    (leaf auth-source-pass :ensure t
+    (leaf auth-source-pass
+      :ensure t
       :config
       (when (executable-find "pass")
         (auth-source-pass-enable)))
@@ -2150,35 +2176,36 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Ellama Configuration (AI Integration)
-  ;; - Configures the Ellama package for language tasks (e.g., translation).
-  ;; - Integrates with local or remote LLM (Ollama, etc.).
+  ;; Integrates the Ellama package with LLM backends (e.g., Ollama).
+  ;; Provides translation and language tasks with multiple provider options.
 
   (leaf ellama
     :ensure t
     :after llm-ollama
-    :config
-    ;; Default language for Ellama tasks
+    :custom
+    ;; Default language for Ellama tasks (used for translations, etc.)
     (ellama-language . "Japanese")
 
-    ;; Session directory
+    ;; Directory for storing Ellama session logs and history
     (ellama-sessions-dir . (concat no-littering-var-directory "ellama-sessions"))
+
     :init
     ;; Use LLM-based naming scheme for sessions
     (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
 
-    ;; Default LLM provider
+    ;; Default provider (main LLM)
     (setopt ellama-provider
             (make-llm-ollama
              :chat-model "codestral:22b-v0.1-q4_K_S"
              :embedding-model "codestral:22b-v0.1-q4_K_S"))
 
-    ;; Translation provider
+    ;; Translation provider (optimized for language translation tasks)
     (setopt ellama-translation-provider
             (make-llm-ollama
              :chat-model "llama3:8b-instruct-q8_0"
              :embedding-model "llama3:8b-instruct-q8_0"))
 
-    ;; Multiple available providers
+    ;; Define multiple available providers
     (setopt ellama-providers
             '(("codestral" . (make-llm-ollama
                               :chat-model "codestral:22b-v0.1-q4_K_S"
@@ -2190,9 +2217,9 @@ such as adjusting garbage collection thresholds and compatibility checks.
                                     :chat-model "llama3:8b-instruct-q8_0"
                                     :embedding-model "llama3:8b-instruct-q8_0"))))
 
-    ;; Command to switch providers
+    ;; Command to switch between available providers interactively
     (defun ellama-set-provider (provider-name)
-      "Set Ellama provider to PROVIDER-NAME."
+      "Set Ellama provider to PROVIDER-NAME (from `ellama-providers`)."
       (interactive
        (list (completing-read "Select provider: " (mapcar #'car ellama-providers))))
       (if-let* ((provider (cdr (assoc provider-name ellama-providers))))
@@ -2203,7 +2230,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
           (message "Provider '%s' not found. Defaulting to 'codestral'." provider-name)
           (setopt ellama-provider (cdr (assoc "codestral" ellama-providers))))))
 
-    ;; Sanity check
+    ;; Sanity check for provider configuration
     (unless (and ellama-provider ellama-translation-provider)
       (message "Ellama configuration is incomplete. Verify providers.")))
 #+end_src
@@ -2213,21 +2240,20 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; -----------------------------------------------------------------------------
   ;;; Scratch Buffer Management
-  ;; - Ensures *scratch* buffer always exists and can be recreated.
+  ;; Ensures that the `*scratch*` buffer always exists, and allows recreation.
 
   (defun my:create-scratch-buffer ()
     "Ensure that a `*scratch*` buffer exists."
-    (let ((scratch-buffer (get-buffer "*scratch*")))
-      (unless scratch-buffer
-        (with-current-buffer (get-buffer-create "*scratch*")
-          (funcall initial-major-mode)
-          (when (and initial-scratch-message
-                     (not (string-empty-p initial-scratch-message)))
-            (insert initial-scratch-message))
-          (current-buffer)))))
+    (unless (get-buffer "*scratch*")
+      (with-current-buffer (get-buffer-create "*scratch*")
+        (funcall initial-major-mode)
+        (when (and initial-scratch-message
+                   (not (string-empty-p initial-scratch-message)))
+          (insert initial-scratch-message))
+        (current-buffer))))
 
   (defun my:recreate-scratch-buffer ()
-    "Kill the current `*scratch*` buffer and create a new one."
+    "Kill and recreate the `*scratch*` buffer."
     (interactive)
     (when (get-buffer "*scratch*")
       (kill-buffer "*scratch*"))
@@ -2235,17 +2261,17 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (switch-to-buffer "*scratch*"))
 
   (defun my:after-kill-buffer-advice (&rest _)
-    "Ensure `*scratch*` buffer exists after killing it."
+    "Ensure `*scratch*` buffer exists after any buffer is killed."
     (run-at-time 0.1 nil #'my:create-scratch-buffer))
 
   (advice-add 'kill-buffer :after #'my:after-kill-buffer-advice)
 
   ;; -----------------------------------------------------------------------------
   ;;; Automatic Lexical Binding
-  ;; - Inserts `lexical-binding: t` in new `.el` files under `no-littering-var-directory`.
+  ;; Inserts a `lexical-binding: t` header into `.el` files in `no-littering-var-directory`.
 
   (defun my:auto-insert-lexical-binding ()
-    "Automatically insert lexical-binding cookie in Emacs Lisp files under `no-littering-var-directory`."
+    "Automatically insert `lexical-binding: t` in Emacs Lisp files under `no-littering-var-directory`."
     (when (and (stringp buffer-file-name)
                (boundp 'no-littering-var-directory)
                (string-prefix-p (expand-file-name no-littering-var-directory)
@@ -2258,84 +2284,75 @@ such as adjusting garbage collection thresholds and compatibility checks.
         (goto-char (point-min))
         (insert ";; -*- lexical-binding: t; -*- \n"))))
 
-  ;; ---------------------------------------------------------------------------
-  ;;; Asynchronous Task Execution
+  ;; -----------------------------------------------------------------------------
+  ;;; Asynchronous Task Execution Helper
 
   (defun my:safe-run-async (task)
-    "Run TASK asynchronously and handle any errors gracefully."
+    "Run TASK asynchronously, catching and reporting any errors."
     (run-at-time 0 nil
                  (lambda ()
                    (condition-case err
                        (funcall task)
-                     (error (message "An error occurred during asynchronous execution: %s" err))))))
+                     (error (message "Async error: %s" err))))))
 
   ;; -----------------------------------------------------------------------------
   ;;; Backup File Cleanup
-  ;; - Deletes old backups (older than 7 days) asynchronously.
+  ;; Deletes old backup files (older than 7 days) asynchronously.
 
   (defun my:delete-old-backups ()
-    "Delete backup files older than 7 days asynchronously."
+    "Delete backup files older than 7 days."
     (interactive)
     (my:safe-run-async
      (lambda ()
        (let ((backup-dir (concat no-littering-var-directory "backup/"))
              (threshold (- (float-time (current-time)) (* 7 24 60 60))))
          (when (file-directory-p backup-dir)
-           (dolist (file (directory-files backup-dir t nil t))
+           (dolist (file (directory-files backup-dir t))
              (when (and (file-regular-p file)
-                        (> (float-time (file-attribute-modification-time (file-attributes file))) threshold))
+                        (< (float-time (file-attribute-modification-time
+                                        (file-attributes file)))
+                           threshold))
                (delete-file file))))))))
 
-  ;; ---------------------------------------------------------------------------
-  ;;; Backup File Cleanup
-  ;; View mode handling
+  ;; -----------------------------------------------------------------------------
+  ;;; Read-Only Buffer Handling
+  ;; Automatically enables `view-mode` for read-only buffers.
 
   (defun my:enable-view-mode-on-read-only ()
-    "Enable `view-mode` if the buffer is read-only, disable otherwise."
+    "Enable `view-mode` when buffer is read-only."
     (if buffer-read-only
         (view-mode 1)
       (view-mode -1)))
   (add-hook 'read-only-mode-hook #'my:enable-view-mode-on-read-only)
 
-  ;; Toggle line number display
+  ;; -----------------------------------------------------------------------------
+  ;;; UI & Navigation Helpers
+
   (defun my:toggle-linum-lines ()
-    "Toggle line number display using `display-line-numbers-mode`."
+    "Toggle line numbers using `display-line-numbers-mode`."
     (interactive)
     (display-line-numbers-mode 'toggle))
 
-  ;; Toggle window split orientation
   (defun my:toggle-window-split ()
-    "Toggle between horizontal and vertical split with two windows."
+    "Toggle between horizontal and vertical split for two windows."
     (interactive)
-    ;; Only proceed if there are exactly two windows
     (when (= (count-windows) 2)
-      ;; Get the buffers and edge positions of both windows
-      (let* ((this-win-buffer (window-buffer))
-             (next-win-buffer (window-buffer (next-window)))
-             (this-win-edges (window-edges (selected-window)))
-             (next-win-edges (window-edges (next-window)))
-             ;; Determine if current layout is vertical (split top/bottom)
-             (split-vertically
-              (= (car this-win-edges)
-                 (car next-win-edges)))
-             ;; Choose the opposite split function
-             (new-split
-              (if split-vertically
-                  #'split-window-horizontally
-                #'split-window-vertically)))
-        ;; Remove all other windows
+      (let* ((this-buf (window-buffer))
+             (next-buf (window-buffer (next-window)))
+             (this-edges (window-edges))
+             (next-edges (window-edges (next-window)))
+             (split-vert (= (car this-edges) (car next-edges)))
+             (split-fn (if split-vert
+                           #'split-window-horizontally
+                         #'split-window-vertically)))
         (delete-other-windows)
-        ;; Create new split
-        (funcall new-split)
-        ;; Restore original buffers
-        (set-window-buffer (selected-window) this-win-buffer)
-        (set-window-buffer (next-window) next-win-buffer)
-        ;; Reselect the original window
+        (funcall split-fn)
+        (set-window-buffer (selected-window) this-buf)
+        (set-window-buffer (next-window) next-buf)
         (select-window (selected-window)))))
 
-  ;; Find keybinding conflicts
   (defun my:find-keybinding-conflicts ()
-    "Detect and display keybinding conflicts across active keymaps."
+    "Find and display conflicting keybindings across active keymaps."
     (interactive)
     (let ((conflicts (make-hash-table :test 'equal))
           (buffer-name "*Keybinding Conflicts*"))
@@ -2344,9 +2361,10 @@ such as adjusting garbage collection thresholds and compatibility checks.
                     (map-keymap
                      (lambda (key cmd)
                        (when (commandp cmd)
-                         (let* ((key-desc (key-description (vector key)))
-                                (existing (gethash key-desc conflicts)))
-                           (puthash key-desc (delete-dups (cons cmd existing)) conflicts))))
+                         (let ((desc (key-description (vector key)))
+                               (existing (gethash desc conflicts)))
+                           (puthash desc (delete-dups (cons cmd existing))
+                                    conflicts))))
                      (symbol-value sym)))))
       (with-current-buffer (get-buffer-create buffer-name)
         (read-only-mode -1)
@@ -2361,9 +2379,11 @@ such as adjusting garbage collection thresholds and compatibility checks.
         (read-only-mode 1))
       (switch-to-buffer buffer-name)))
 
-  ;; Dired file and directory viewing
+  ;; -----------------------------------------------------------------------------
+  ;;; Dired Helper
+
   (defun my:dired-view-file-other-window ()
-    "Open the selected file or directory in another window."
+    "Open selected Dired file or directory in another window."
     (interactive)
     (let ((file (dired-get-file-for-visit)))
       (if (file-directory-p file)
@@ -2372,9 +2392,11 @@ such as adjusting garbage collection thresholds and compatibility checks.
               (dired file))
         (view-file-other-window file))))
 
-  ;; External editor integration
+  ;; -----------------------------------------------------------------------------
+  ;;; External Integration
+
   (defun my:open-by-vscode ()
-    "Open the current file in Visual Studio Code at the current line and column."
+    "Open current file in Visual Studio Code at line/column."
     (interactive)
     (when (buffer-file-name)
       (async-shell-command
@@ -2383,21 +2405,17 @@ such as adjusting garbage collection thresholds and compatibility checks.
                (line-number-at-pos)
                (current-column)))))
 
-  ;; Displau the value of environment
   (defun my:show-env-variable (var)
-    "Display the value of environment variable VAR in the minibuffer."
+    "Display the value of environment variable VAR."
     (interactive "sEnvironment variable: ")
-    (let ((value (getenv var)))
-      (if value
-          (message "%s = %s" var value)
-        (message "Environment variable %s is not set." var))))
+    (let ((val (getenv var)))
+      (message "%s = %s" var (or val "Not set"))))
 
-  ;; Emacs build information
   (defun my:print-build-info ()
-    "Display detailed information about the current Emacs build."
+    "Show Emacs build details (commit, branch, system, features)."
     (interactive)
     (switch-to-buffer (get-buffer-create "*Build info*"))
-    (let ((buffer-read-only nil))
+    (let ((inhibit-read-only t))
       (erase-buffer)
       (insert
        (format "- GNU Emacs %s\n\n|Commit|%s|\n|Branch|%s|\n|System|%s|\n|Date|%s|\n"
@@ -2409,12 +2427,30 @@ such as adjusting garbage collection thresholds and compatibility checks.
                (format-time-string "%Y-%m-%d %T (%Z)" emacs-build-time)))
       (insert (format "|Patch|%s ns-inline.patch|\n"
                       (if (boundp 'mac-ime--cursor-type) "with" "without")))
-      (insert
-       (format "|Features|%s|\n" system-configuration-features))
-      (insert
-       (format "|Options|%s|\n" system-configuration-options)))
+      (insert (format "|Features|%s|\n" system-configuration-features))
+      (insert (format "|Options|%s|\n" system-configuration-options)))
     (view-mode))
 
+  ;; -----------------------------------------------------------------------------
+  ;;; Org Mode Folding Shortcuts
+  (with-eval-after-load 'org
+    (require 'org-fold)
+    (defun my-org-fold-subtree ()   (interactive) (org-fold-subtree t))
+    (defun my-org-unfold-subtree () (interactive) (org-show-subtree))
+    (defun my-org-toggle-fold ()
+      "Toggle fold for current Org subtree."
+      (interactive)
+      (save-excursion
+        (org-back-to-heading t)
+        (if (org-fold-folded-p (point))
+            (org-show-subtree)
+          (org-fold-subtree t))))
+    (define-key org-mode-map (kbd "C-c C-f") #'my-org-fold-subtree)
+    (define-key org-mode-map (kbd "C-c C-e") #'my-org-unfold-subtree)
+    (define-key org-mode-map (kbd "C-c C-t") #'my-org-toggle-fold))
+
+  ;; -----------------------------------------------------------------------------
+  ;;; Hooks
   (add-hook 'emacs-startup-hook #'my:delete-old-backups)
   (add-hook 'find-file-hook #'my:auto-insert-lexical-binding)
   (add-hook 'before-save-hook 'delete-trailing-whitespace)
@@ -2428,30 +2464,30 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;;; Font Setup ---------------------------------------------------------------
 
-  ;; Utility function to check if a font exists on the system.
+  ;; -----------------------------------------------------------------------------
+  ;; Utility function to check if a font is available on the system.
   (defun font-exists-p (font-name)
-    "Check if FONT-NAME is available in the system."
+    "Return non-nil if FONT-NAME is available on the system."
     (find-font (font-spec :family font-name)))
 
-  ;; Default font family
+  ;; -----------------------------------------------------------------------------
+  ;; Default font configuration
   (defvar my:font-default
     (or (getenv "EMACS_FONT_FAMILY")
         (cond
          ((eq system-type 'windows-nt) "Consolas")
          ((eq system-type 'darwin) "SF Mono")
          (t "Monospace")))
-    "Default font for Emacs.")
+    "Primary default font for Emacs.")
 
-  ;; Alternate font for variable-pitch, comments, etc.
   (defvar my:font-alt
     (or (getenv "EMACS_FONT_ALT")
         (cond
          ((eq system-type 'windows-nt) "Consolas")
          ((eq system-type 'darwin) "SF Mono")
          (t "Monospace")))
-    "Alternate font for Emacs (e.g., for comments).")
+    "Alternate font, e.g., for comments or variable-pitch text.")
 
-  ;; Default font size
   (defvar my:font-size
     (let ((env (getenv "EMACS_FONT_SIZE")))
       (if env
@@ -2461,74 +2497,63 @@ such as adjusting garbage collection thresholds and compatibility checks.
                  (> (display-pixel-width) 1920))
             24
           20)))
-    "Default font size for Emacs.")
+    "Default font size (in pt).")
 
-  ;; Emoji font
   (defvar my:emoji-font "Noto Color Emoji"
-    "Default emoji font for Emacs.")
+    "Default font for displaying emojis.")
 
+  ;; -----------------------------------------------------------------------------
   (defun font-setup (&optional frame)
-    "Apply font settings to FRAME or the currently selected frame.
-  This includes default, variable-pitch, and emoji fonts if available."
+    "Apply font settings to FRAME or current frame.
+  Includes default font, variable-pitch font, and emoji font."
     (let ((target-frame (or frame (selected-frame))))
-      ;; Only proceed if in graphical mode
       (when (display-graphic-p target-frame)
-        ;; Ensure font-exists-p is defined
-        (unless (fboundp 'font-exists-p)
-          (defun font-exists-p (font)
-            "Return non-nil if FONT is available on the system."
-            (if (find-font (font-spec :name font)) t nil)))
-
-        ;; Apply default font
-        (when (and (boundp 'my:font-default)
-                   (font-exists-p my:font-default))
+        ;; Default font
+        (when (font-exists-p my:font-default)
           (set-face-attribute 'default target-frame
                               :family my:font-default
                               :height (* my:font-size 10))
           (message "✅ Default font set: %s (%dpt)" my:font-default my:font-size))
 
-        ;; Apply variable-pitch font
-        (when (and (boundp 'my:font-alt)
-                   (font-exists-p my:font-alt))
+        ;; Variable-pitch font
+        (when (font-exists-p my:font-alt)
           (set-face-attribute 'variable-pitch target-frame
                               :family my:font-alt)
           (message "✅ Variable-pitch font set: %s" my:font-alt))
 
-        ;; Apply emoji font if defined
-        (when (and (boundp 'my:emoji-font)
-                   (font-exists-p my:emoji-font))
+        ;; Emoji font
+        (when (font-exists-p my:emoji-font)
           (set-fontset-font t 'unicode
                             (font-spec :family my:emoji-font) nil 'prepend)
           (message "✅ Emoji font set: %s" my:emoji-font)))))
 
-  ;; Hook to apply font settings for daemon-created frames
   (defun my:font-setup-on-frame (frame)
-    "Apply `font-setup` to newly created FRAME in daemon mode."
+    "Apply `font-setup` to newly created FRAME in daemon sessions."
     (when (display-graphic-p frame)
       (with-selected-frame frame
         (font-setup))))
 
-  ;; Apply font settings based on session type
   (if (daemonp)
       (add-hook 'after-make-frame-functions #'my:font-setup-on-frame)
     (when (display-graphic-p)
       (font-setup)))
 
-  ;; Theme-dependent face configuration
+  ;; -----------------------------------------------------------------------------
+  ;; Adjust font-lock faces after loading a theme
   (add-hook 'after-load-theme-hook
             (lambda ()
-              (when (and my:font-alt (font-exists-p my:font-alt))
+              (when (font-exists-p my:font-alt)
                 (set-face-attribute 'font-lock-comment-face nil
-                                    :family my:font-alt
-                                    :slant 'italic)
+                                    :family my:font-alt :slant 'italic)
                 (set-face-attribute 'font-lock-doc-face nil
-                                    :family my:font-alt
-                                    :slant 'italic)
+                                    :family my:font-alt :slant 'italic)
                 (message "Comment/doc font set to: %s" my:font-alt))))
 
-  ;; ---------------------------------------------------------------------------
+  ;; -----------------------------------------------------------------------------
   ;;; Nerd Icons Setup
-  (defvar my:nerd-icons-font "Symbols Nerd Font Mono" "Font for Nerd Icons.")
+  (defvar my:nerd-icons-font "Symbols Nerd Font Mono"
+    "Font used for Nerd Icons.")
+
   (leaf nerd-icons
     :ensure t
     :if (display-graphic-p)
@@ -2540,10 +2565,10 @@ such as adjusting garbage collection thresholds and compatibility checks.
     :if (display-graphic-p)
     :hook (dired-mode . nerd-icons-dired-mode))
 
-  ;;; Ligature Setup ------------------------------------------------------------
-
+  ;; -----------------------------------------------------------------------------
+  ;;; Ligature Setup
   (defvar my:ligature-font "Fira Code"
-    "Default font for programming ligatures.")
+    "Font used for programming ligatures.")
 
   (leaf ligature
     :ensure t
@@ -2551,7 +2576,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (when (and (font-exists-p my:font-default)
                (font-exists-p my:ligature-font))
       (ligature-set-ligatures 'prog-mode
-                              '("->" "=>" "::" "===" "!=" "&&" "||" "|||"
+                              '("->" "=>" "::" "===" "!=" "&&" "||"
                                 ":::" "!!" "??" "-->" "<--" "->>" "<<-"))
       (global-ligature-mode 1)))
 #+end_src
@@ -2561,6 +2586,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 #+begin_src emacs-lisp :tangle README.el
   ;; ---------------------------------------------------------------------------
   ;;; Fullscreen Mode Configuration
+  ;; Ensures Emacs starts in fullscreen mode.
   (leaf fullscreen
     :init
     (if (daemonp)
@@ -2571,7 +2597,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
       (set-frame-parameter nil 'fullscreen 'fullboth)))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Dynamic Window Resizing with Golden-Ratio
+  ;;; Dynamic Window Resizing (Golden Ratio)
+  ;; Automatically resizes windows, focusing the current one.
   (leaf golden-ratio
     :ensure t
     :hook (after-init-hook . golden-ratio-mode)
@@ -2581,7 +2608,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
              (golden-ratio-exclude-buffer-names . '("*Messages*" "*Help*"))))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Theme Configuration: ef-themes
+  ;;; Theme Configuration (ef-themes)
+  ;; Loads `ef-frost` in GUI or `deeper-blue` in terminal.
   (leaf ef-themes
     :ensure t
     :custom ((ef-themes-to-toggle . '(ef-frost ef-spring)))
@@ -2589,7 +2617,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (load-theme (if (display-graphic-p) 'ef-frost 'deeper-blue) t))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Spacious Padding Configuration
+  ;;; Spacious Padding
+  ;; Adds extra padding around UI elements for a clean look.
   (leaf spacious-padding
     :ensure t
     :if (display-graphic-p)
@@ -2606,7 +2635,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (spacious-padding-mode 1))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Minions: Mode Line Icon Management
+  ;;; Minions (Mode Line Management)
+  ;; Consolidates minor modes into a single menu.
   (leaf minions
     :ensure t
     :custom ((minions-mode-line-lighter . "⚙"))
@@ -2615,6 +2645,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; ---------------------------------------------------------------------------
   ;;; Time and Battery in Mode-Line
+  ;; Displays time and battery status in the mode line.
   (leaf time-battery
     :init
     (setq display-time-interval 30
@@ -2626,7 +2657,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (display-battery-mode 1))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Tab Bar Configuration
+  ;;; Tab Bar & Tab Line
+  ;; Enables tab-bar and tab-line with custom format.
   (leaf tab-bar
     :custom ((tab-bar-show . 1)
              (tab-bar-new-tab-choice . "*scratch*")
@@ -2636,7 +2668,8 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (global-tab-line-mode 1))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Treemacs Configuration
+  ;;; Treemacs (Project Drawer)
+  ;; Provides a sidebar file explorer.
   (leaf treemacs
     :ensure t
     :if (display-graphic-p)
@@ -2650,10 +2683,11 @@ such as adjusting garbage collection thresholds and compatibility checks.
 
   ;; ---------------------------------------------------------------------------
   ;;; Desktop Session Management
+  ;; Saves and restores window layouts and open files.
   (leaf desktop
     :custom `((desktop-dirname . ,(concat no-littering-var-directory "desktop"))
               (desktop-save . 'if-exists)
-  	    (desktop-load-locked-desktop . t)
+              (desktop-load-locked-desktop . t)
               (desktop-auto-save-timeout . 180)
               (desktop-restore-eager . 10))
     :hook ((kill-emacs-hook . desktop-save-in-desktop-dir)
@@ -2664,16 +2698,16 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (desktop-save-mode 1))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Winner Mode Configuration
+  ;;; Winner Mode
+  ;; Allows undo/redo of window configurations.
   (leaf winner
-    :doc "Window configuration undo/redo"
     :bind (("M-[" . winner-undo)
            ("M-]" . winner-redo))
     :config
     (winner-mode 1))
 
   ;; ---------------------------------------------------------------------------
-  ;;; Window Layout Utilities
+  ;;; Custom Window Layout Utilities
   (defvar my:saved-window-config nil
     "Stores the current window configuration for later restoration.")
 
@@ -2684,7 +2718,7 @@ such as adjusting garbage collection thresholds and compatibility checks.
     (message "Window configuration saved."))
 
   (defun my:restore-window-layout ()
-    "Restore the saved window configuration."
+    "Restore the previously saved window configuration."
     (interactive)
     (if my:saved-window-config
         (progn
