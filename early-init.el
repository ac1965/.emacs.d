;;; early-init.el --- Early initialization for Emacs 30+ -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; Optimizes Emacs startup performance and includes platform-specific settings.
;; Takes advantage of Emacs 30+ features for better configuration and maintenance.

;;; Code:

;; Ensure compatibility with Emacs 30 or later
(when (version< emacs-version "30")
  (error "This configuration requires Emacs 30 or higher"))

;; Optimize garbage collection during startup
(setq gc-cons-threshold (* 128 1024 1024) ; Larger threshold reduces GC during init
      read-process-output-max (* 2 1024 1024) ; Increase process output buffer size
      file-name-handler-alist nil)        ; Temporarily disable file handlers

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent ; Suppress warnings
        native-comp-speed 3                              ; Maximize speed optimizations
        native-comp-deferred-compilation t))             ; Use deferred compilation

;; macOS-specific settings
(when (eq system-type 'darwin)
  ;; Set environment variables for GCC and Homebrew
  (defun set-env-paths (env-var paths)
    "Set ENV-VAR to a colon-separated list of PATHS."
    (setenv env-var (string-join paths ":")))

  (let ((gcc-paths (seq-filter #'file-directory-p
                               '("/opt/homebrew/opt/gcc/lib/gcc/current"
                                 "/usr/local/opt/gcc/lib/gcc/current"))))
    (when gcc-paths
      (set-env-paths "LIBRARY_PATH" gcc-paths)))

  (let ((brew-paths '("/opt/homebrew/bin" "/usr/local/bin")))
    (dolist (path brew-paths)
      (when (file-directory-p path)
        (set-env-paths "PATH" (cons path (split-string (getenv "PATH") ":")))
        (add-to-list 'exec-path path))))

  ;; Configure GPG
  (setq epg-gpg-program "gpg")

  ;; Use GNU ls for dired
  (setq dired-use-ls-dired t
        insert-directory-program "gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Frame settings
(setq default-frame-alist '((fullscreen . maximized)       ; Start maximized
                            (resize-pixelwise . t))        ; Pixel-perfect resizing
      frame-title-format "%b")                            ; Buffer name in title

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable smooth scrolling
(setq scroll-margin 8
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Disable startup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode)

;; Miscellaneous optimizations
(setq use-short-answers t
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil)

;; Restore GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

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
