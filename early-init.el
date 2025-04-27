;; early-init.el ---  Early initialization for macOS -*- lexical-binding: t; -*-
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; License: GNU General Public License version 3 or later
;; Keywords: initialization, performance, macos
;; Version: 1.1

;;; Commentary:
;; This configuration file optimizes Emacs startup performance with focus on macOS compatibility.
;; Designed for Emacs 30 and above, it provides necessary settings while removing redundancies.

;;; Code:

;; ---------------------------------------------------------------------------
;;; Compatibility Check
;; Ensure that Emacs version is 30 or higher.
(when (version< emacs-version "30")
  (error "This configuration requires Emacs 30 or higher"))

;; ---------------------------------------------------------------------------
;;; Directories
;; Define essential directories for configuration and cache.
(defvar my:d (if load-file-name
                 (file-name-directory (file-chase-links load-file-name))
               user-emacs-directory)
  "Base directory for user-specific configuration.")
(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")

;; ---------------------------------------------------------------------------
;;; Performance Optimization
;; Adjust garbage collection and file handling during startup for faster initialization.
(setq gc-cons-threshold (* 64 1024 1024) ; 64MB during startup is sufficient
      read-process-output-max (* 8 1024 1024) ; 8MB for process output
      file-name-handler-alist nil) ; Disable file handlers temporarily

;; Restore default garbage collection settings after startup
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 32 1024 1024) ; 32MB is a good balance
		  file-name-handler-alist (default-value 'file-name-handler-alist))))

;; Disable package.el in favor of straight.el or other managers
(setq package-enable-at-startup nil)

;; ---------------------------------------------------------------------------
;;; Enable native compilation optimizations
(setq native-comp-async-report-warnings-errors 'silent)

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" my:d:cache)))

(with-eval-after-load 'comp
  ;; Adjust jobs based on your CPU core count
  (setq native-comp-async-jobs-number (max 4 (min 8 (/ (num-processors) 2)))
        ;; Speed 2 is a good balance between compilation speed and runtime performance
        native-comp-speed 2))

;; ---------------------------------------------------------------------------
;;; macOS-Specific Settings
;; Configure macOS-specific environment variables and paths.
(when (eq system-type 'darwin)
  ;; Set LIBRARY_PATH for GCC
  (let ((gcc-paths (seq-filter #'file-directory-p
                               '("/opt/homebrew/opt/gcc/lib/gcc/current"
                                 "/usr/local/opt/gcc/lib/gcc/current"))))
    (when gcc-paths
      (setenv "LIBRARY_PATH" (string-join gcc-paths ":"))))

  ;; Set PATH for Homebrew
  (let ((brew-paths '("/opt/homebrew/bin" "/usr/local/bin")))
    (dolist (path brew-paths)
      (when (file-directory-p path)
        (setenv "PATH" (string-join (cons path (split-string (getenv "PATH") ":")) ":"))
        (add-to-list 'exec-path path))))

  ;; Use GNU ls (gls) for dired if available
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first"))

  ;; Prevent issues with macOS clipboard
  (setq save-interprogram-paste-before-kill t))

;; ---------------------------------------------------------------------------
;;; Frame and UI Customization

;; Prefer using pixels for resizing
(setq frame-resize-pixelwise t)

;; Configure frame appearance and behavior
(setq default-frame-alist '((fullscreen . maximized) ; Start maximized
                            (resize-pixelwise . t))  ; Pixel-perfect resizing
      frame-title-format "%b")                       ; Display buffer name in frame title

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable smooth scrolling
(when (featurep 'pixel-scroll)
  (pixel-scroll-precision-mode 1))

;; Configure scrolling behavior
(setq scroll-margin 3                  ; Keep 3 lines of context (8 was too much)
      scroll-conservatively 101        ; Avoid recentering
      scroll-preserve-screen-position t) ; Maintain cursor position

;; ---------------------------------------------------------------------------
;;; Miscellaneous Optimizations
;; General improvements for smoother operation.
(setq inhibit-startup-screen t              ; Disable startup screen
      initial-scratch-message nil           ; Empty scratch buffer
      initial-major-mode 'text-mode         ; Use text-mode in the scratch buffer
      use-short-answers t                   ; Enable y/n answers
      create-lockfiles nil                  ; Disable lockfiles
      display-line-numbers-type 'relative   ; Use relative line numbers
      epg-gpg-program "gpg")

;; File backup settings - use a dedicated directory instead of disabling
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" my:d:cache)))
      backup-by-copying t     ; Don't delink hardlinks
      version-control t       ; Use version numbers on backups
      delete-old-versions t   ; Automatically delete excess backups
      kept-new-versions 6     ; Number of newest versions to keep
      kept-old-versions 2)    ; Number of old versions to keep

;; Auto-save settings - use a dedicated directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my:d:cache) t)))

;; Enable useful global modes
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-prettify-symbols-mode 1)

;; ---------------------------------------------------------------------------
;;; Startup Metrics
;; Log startup time and garbage collection statistics.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'early-init)
;;; early-init.el ends here
