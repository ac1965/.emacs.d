;;; --- My early-init script -*- mode: emacs-lisp; lexical-binding:t; -*-

;; Copyright (c) 2021-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;; $Lastupdate: 2025/06/22 17:45:23 $

;;; Commentary:
;; It is designed for Emacs 30 and above, providing essential settings while eliminating redundancy.

;;; Code:

;; ---------------------------------------------------------------------------
;;; Compatibility Check (Emacs 30+)
(when (version< emacs-version "30")
  (error "This configuration requires Emacs 30 or higher."))

;; ---------------------------------------------------------------------------
;;; Directories
(defvar my:d (if load-file-name
                 (file-name-directory (file-chase-links load-file-name))
               user-emacs-directory)
  "Base directory for user-specific configuration.")

(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")
(make-directory my:d:cache t) ;; Ensure cache directory exists

;; ---------------------------------------------------------------------------
;;; Performance Optimization
(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 8 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(setq package-enable-at-startup nil)

;; ---------------------------------------------------------------------------
;;; Native Compilation Optimization
(setq native-comp-async-report-warnings-errors 'error)
(setq native-comp-async-jobs-number (or (getenv "EMACS_NATIVE_COMP_JOBS") 4))
(setq native-comp-speed 2)
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" my:d:cache)))

;; ---------------------------------------------------------------------------
;;; macOS Specific Settings
(when (eq system-type 'darwin)
  ;; Homebrew and GCC Paths
  (dolist (path '("/opt/homebrew/bin" "/usr/local/bin"))
    (when (file-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ":" (getenv "PATH")))))

  ;; GNU ls (gls) for Dired
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first")))

;; ---------------------------------------------------------------------------
;;; UI Customization
(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)

;; ---------------------------------------------------------------------------
;;; Miscellaneous Optimizations
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      use-short-answers t
      create-lockfiles nil
      display-line-numbers-type 'relative)
