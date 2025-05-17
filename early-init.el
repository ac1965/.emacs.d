;; -*- lexical-binding: t; -*-

;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; License: GNU General Public License version 3 or later
;; Keywords: initialization, performance, macos
;; Version: 1.2

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
      read-process-output-max (* 8 1024 1024)
      file-name-handler-alist nil)

(let ((default-handlers file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 64 1024 1024))
              (setq file-name-handler-alist default-handlers))))

(setq package-enable-at-startup nil)

;; ---------------------------------------------------------------------------
;;; Native Compilation Optimization
(setq native-comp-async-report-warnings-errors 'silent)

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" my:d:cache)))

(setq native-comp-async-jobs-number (or (getenv "EMACS_NATIVE_COMP_JOBS") 4)
      native-comp-speed 2)

;; ---------------------------------------------------------------------------
;;; macOS Specific Settings
(when (eq system-type 'darwin)
  ;; Set LIBRARY_PATH for GCC
  (let ((gcc-paths '("/opt/homebrew/opt/gcc/lib/gcc/current"
                     "/usr/local/opt/gcc/lib/gcc/current")))
    (setenv "LIBRARY_PATH" (string-join (seq-filter #'file-directory-p gcc-paths) ":")))

  ;; Set PATH for Homebrew
  (dolist (path '("/opt/homebrew/bin" "/usr/local/bin"))
    (when (file-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ":" (getenv "PATH")))))

  ;; Use GNU ls (gls) for dired if available
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first")))

;; ---------------------------------------------------------------------------
;;; UI Customization
(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(resize-pixelwise . t))
(setq frame-title-format "%b")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)

;;; Backup and Auto-Save Settings
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" my:d:cache)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" my:d:cache) t))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;;; Miscellaneous Optimizations
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      use-short-answers t
      create-lockfiles nil
      display-line-numbers-type 'relative
      epg-gpg-program "gpg")

;;; Enable Useful Global Modes
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-prettify-symbols-mode 1)

;; ---------------------------------------------------------------------------
;;; Startup Metrics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'early-init)
;;; early-init.el ends here
