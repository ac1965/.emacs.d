;;; early-init.el --- Early initialization for Emacs 29+ -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; This file optimizes Emacs startup performance and configures platform-specific
;; settings (e.g., macOS). It also initializes native compilation settings.

;;; Code:

;; Optimize garbage collection during startup
(setq gc-cons-threshold (* 100 1024 1024) ; Increase threshold for fewer GC cycles
      read-process-output-max (* 1024 1024) ; Increase subprocess output buffer
      file-name-handler-alist nil)        ; Temporarily disable file handlers

;; Enable native compilation
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent ; Suppress warnings
        native-comp-speed 2                              ; Optimize for speed
        native-comp-jit-compilation t))                  ; Enable JIT compilation

;; macOS-specific configuration
(when (eq system-type 'darwin)
  ;; Utility function to set environment variables
  (defun my-set-env-paths (env-var paths)
    "Set ENV-VAR to a colon-separated list of PATHS."
    (setenv env-var (string-join paths ":")))

  ;; Configure GCC-related paths
  (let ((gcc-base-paths (list "/opt/homebrew/opt/gcc/lib/gcc/current"
                              "/opt/homebrew/opt/libgccjit/lib/gcc/current"
                              "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin/14"
                              "/usr/local/opt/gcc/lib/gcc/current"
                              "/usr/local/opt/libgccjit/lib/gcc/current"
                              "/usr/local/opt/gcc/lib/gcc/current/gcc/x86_64-apple-darwin/14"))
        (gcc-paths nil))
    ;; Filter existing directories from the base paths
    (setq gcc-paths (seq-filter #'file-directory-p gcc-base-paths))
    (when gcc-paths
      (my-set-env-paths "LIBRARY_PATH" gcc-paths)))

  ;; Automatically detect and set Homebrew paths
  (let ((brew-paths '("/opt/homebrew/bin" "/usr/local/bin")))
    (dolist (path brew-paths)
      (when (file-directory-p path)
        ;; Prepend the Homebrew path to PATH
        (my-set-env-paths "PATH" (cons path (split-string (getenv "PATH") ":")))
        ;; Add the Homebrew path to exec-path
        (add-to-list 'exec-path path))))

  ;; Configure dired to use GNU Core Utilities
  (setq dired-use-ls-dired t
        insert-directory-program "gls" ; Use GNU ls
        dired-listing-switches "-aBhl --group-directories-first")); Human-readable format


(add-hook 'focus-out-hook #'garbage-collect)

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; Set default frame options
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Start maximized
(setq frame-title-format "%b")                              ; Show buffer name in title
(setq frame-resize-pixelwise t)                             ; Enable pixel-perfect resizing

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Smooth scrolling
(setq scroll-margin 8                     ; Add margin when scrolling
      scroll-conservatively 101           ; Avoid large jumps
      scroll-preserve-screen-position t)  ; Preserve cursor position during scrolling

;; Configure startup screen
(setq inhibit-startup-screen t            ; Disable splash screen
      initial-scratch-message nil         ; Remove scratch buffer message
      initial-major-mode 'text-mode)      ; Set default scratch buffer mode

;; Miscellaneous optimizations
(setq create-lockfiles nil                ; Disable lockfiles
      make-backup-files nil               ; Disable backup files
      auto-save-default nil)              ; Disable autosave

;; Show Emacs Startup Performance
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Restore GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)))) ; Lower threshold for runtime

(provide 'early-init)
;;; early-init.el ends here
