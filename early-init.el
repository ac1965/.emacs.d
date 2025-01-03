;;; early-init.el --- Early initialization for Emacs 30+ -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; Optimizes Emacs startup performance and includes platform-specific settings.
;; This configuration leverages new features available in Emacs 30+.

;;; Code:

;; Ensure compatibility with Emacs 30 or later
(when (version< emacs-version "30")
  (error "This configuration requires Emacs 30 or higher"))

;; Optimize garbage collection and file handler processing during startup
(setq gc-cons-threshold most-positive-fixnum  ; Prevent GC during startup
      read-process-output-max (* 4 1024 1024) ; Increase for better LSP performance
      file-name-handler-alist nil)            ; Temporarily disable file handlers

;; Restore file handler and GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024) ; Lower threshold post-startup
                  file-name-handler-alist (default-value 'file-name-handler-alist))))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-speed 3
        native-comp-jit-compilation t)) ; JIT compilation enabled by default in Emacs 30

;; macOS-specific settings
(when (eq system-type 'darwin)
  ;; Set environment variables for GCC and Homebrew paths
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

  ;; Configure GPG for macOS
  (setq epg-gpg-program "gpg")

  ;; Use GNU ls for dired if available
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first")))

;; Frame settings: maximize frame and enable pixel-perfect resizing
(setq default-frame-alist '((fullscreen . maximized)
                            (resize-pixelwise . t))
      frame-title-format "%b") ; Display buffer name in frame title

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable smooth scrolling using `pixel-scroll-precision-mode`
(pixel-scroll-precision-mode 1) ; New in Emacs 29+, better smooth scrolling

(setq scroll-margin 8
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Disable startup screen and configure initial buffer
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode)

;; Miscellaneous optimizations
(setq use-short-answers t   ; Use y/n instead of yes/no
      create-lockfiles nil  ; Disable lockfiles
      make-backup-files nil ; Disable backup files
      auto-save-default nil ; Disable auto-save
      auto-save-list-file-prefix nil)
(global-prettify-symbols-mode 1)

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
