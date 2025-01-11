;;; early-init.el --- Early initialization for Emacs 30+ -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; This configuration optimizes Emacs startup performance and includes platform-specific settings.
;; Designed for Emacs 30 and above, it leverages features like native compilation and precise scrolling.

;;; Code:

;; Debug enabled
(setq debug-on-error t)

;; Ensure compatibility with Emacs 30 or later
(when (version< emacs-version "30")
  (error "This configuration requires Emacs 30 or higher"))

;; Optimize garbage collection and file handler processing during startup
(setq gc-cons-threshold (* 64 1024 1024) ; Set to 64MB to minimize GC overhead during startup
      read-process-output-max (* 8 1024 1024) ; Improve LSP and subprocess performance
      file-name-handler-alist nil)            ; Temporarily disable file handlers for faster startup

;; Restore default GC and file handler settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024) ; Reduce GC threshold to 32MB
                  file-name-handler-alist (default-value 'file-name-handler-alist))))

;; Native compilation settings
;; Optimize for speed and suppress warnings during async compilation
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-speed 3)

;; macOS-specific settings
(when (eq system-type 'darwin)
  ;; Configure LIBRARY_PATH for GCC and set PATH for Homebrew
  (let ((gcc-paths (seq-filter #'file-directory-p
                               '("/opt/homebrew/opt/gcc/lib/gcc/current"
                                 "/usr/local/opt/gcc/lib/gcc/current"))))
    (when gcc-paths
      (setenv "LIBRARY_PATH" (string-join gcc-paths ":"))))

  (let ((brew-paths '("/opt/homebrew/bin" "/usr/local/bin")))
    (dolist (path brew-paths)
      (when (file-directory-p path)
        (setenv "PATH" (string-join (cons path (split-string (getenv "PATH") ":")) ":"))
        (add-to-list 'exec-path path))))

  ;; Use `gpg` for encryption, if available
  (setq epg-gpg-program (or (executable-find "gpg") epg-gpg-program))

  ;; Use GNU ls (gls) for dired if available
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first")))

;; Frame settings
(setq default-frame-alist '((fullscreen . maximized) ; Start with maximized frame
                            (resize-pixelwise . t)) ; Allow pixel-perfect resizing
      frame-title-format "%b") ; Display the buffer name in the frame title

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable smooth scrolling using `pixel-scroll-precision-mode`
;; This feature is available from Emacs 29 onwards
(when (featurep 'pixel-scroll)
  (pixel-scroll-precision-mode 1))

;; Configure scrolling behavior
(setq scroll-margin 8 ; Keep 8 lines of context when scrolling
      scroll-conservatively 101 ; Avoid recentering unless necessary
      scroll-preserve-screen-position t) ; Maintain the cursor position during scrolling

;; Disable startup screen and configure initial buffer
(setq inhibit-startup-screen t ; Skip the startup screen
      initial-scratch-message nil ; Start with an empty *scratch* buffer
      initial-major-mode 'text-mode) ; Use text-mode for the initial buffer

;; Miscellaneous optimizations
(setq use-short-answers t   ; Use y/n prompts instead of yes/no
      create-lockfiles nil  ; Disable lockfiles
      make-backup-files nil ; Disable backup files
      auto-save-default nil ; Disable auto-save
      auto-save-list-file-prefix nil ; Prevent auto-save list files
      display-line-numbers-type 'relative) ; Set relative numbering

;; Enable setting
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-prettify-symbols-mode 1)
(modifier-bar-mode 1)

;; Display startup performance metrics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'early-init)
;;; early-init.el ends here
