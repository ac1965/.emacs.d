;; early-init.el ---  Early initialization for macOS -*- lexical-binding: t; -*-
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; License: GNU General Public License version 3 or later
;; Keywords: initialization, performance, macos
;; Version: 1.2

;;; Commentary:
;; This configuration file optimizes Emacs startup performance with a focus on macOS.
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

;; ---------------------------------------------------------------------------
;;; Performance Optimization
(setq gc-cons-threshold (* 128 1024 1024)  ;; 128MB during startup
      read-process-output-max (* 8 1024 1024) ;; 8MB for process output
      file-name-handler-alist nil) ;; Disable file handlers during startup

;; Restore default settings after startup
(let ((default-handlers file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 64 1024 1024)) ;; 64MB after startup
              (setq file-name-handler-alist default-handlers))))

;; Disable package.el in favor of other package managers
(setq package-enable-at-startup nil)

;; ---------------------------------------------------------------------------
;;; Native Compilation Optimization
(setq native-comp-async-report-warnings-errors 'silent)

(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" my:d:cache)))

(setq native-comp-async-jobs-number (or (getenv "EMACS_NATIVE_COMP_JOBS")
                                        (max 4 (min 8 (/ (num-processors) 2))))
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
;;; Frame and UI Customization
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (resize-pixelwise . t))
      frame-title-format "%b")

;; Disable unnecessary UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable smooth scrolling
(when (featurep 'pixel-scroll)
  (pixel-scroll-precision-mode 1))

(setq scroll-margin 3
      scroll-conservatively 10000
      scroll-step 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; ---------------------------------------------------------------------------
;;; Miscellaneous Optimizations
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      use-short-answers t
      create-lockfiles nil
      display-line-numbers-type 'relative
      epg-gpg-program "gpg")

;; Backup and Auto-Save Settings
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" my:d:cache)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my:d:cache) t)))

;; ---------------------------------------------------------------------------
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
