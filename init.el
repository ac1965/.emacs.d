;;; init.el --- Main configuration file -*- lexical-binding: t; -*-
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; License: GNU General Public License version 3 or later
;; Keywords: initialization, modular

;;; Commentary:
;; This is the main configuration file for Emacs. It initializes directories,
;; sets up packages, and loads modular configurations from `README.org`.

;;; Code:

;; ---------------------------------------------------------------------------
;;; Utility Functions
(defun my:ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; ---------------------------------------------------------------------------
;;; Directories
;; Define essential directories for configuration, cache, and variable data.
(defvar my:d (if load-file-name
                 (file-name-directory (file-chase-links load-file-name))
               user-emacs-directory)
  "Base directory for user-specific configuration.")

(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")
(defvar my:d:etc (expand-file-name ".etc/" my:d)
  "Directory for storing configuration files.")
(defvar my:d:var (expand-file-name ".var/" my:d)
  "Directory for storing variable data.")
(defvar my:d:custom (expand-file-name "custom.el" my:d:etc)
  "File for storing user customizations (custom-file).")

;; Ensure necessary directories exist
(mapc #'my:ensure-directory-exists (list my:d:cache my:d:etc my:d:var))

;; ---------------------------------------------------------------------------
;;; Custom File Setup
;; Separate custom settings to a dedicated file
(setq custom-file my:d:custom)
(my:ensure-directory-exists (file-name-directory custom-file))

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; ---------------------------------------------------------------------------
;;; Performance Optimization
(setq gc-cons-threshold (* 128 1024 1024)) ;; 128MB during startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024)))) ;; 32MB after startup

;; ---------------------------------------------------------------------------
;;; Native Compilation
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (let ((eln-cache-dir (expand-file-name "eln-cache/" my:d:cache)))
    (setq native-comp-eln-load-path (list eln-cache-dir))
    (my:ensure-directory-exists eln-cache-dir)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache eln-cache-dir))))

;; ---------------------------------------------------------------------------
;;; Package Settings
;; Configure directories for cleanup.
(setq package-user-dir (expand-file-name "elpa/" my:d:cache))

;; Ensure package directory exists
(my:ensure-directory-exists package-user-dir)

;; Bootstrap straight.el for declarative package management.
(setq straight-base-dir (expand-file-name "straight/" my:d:cache))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package to use straight.el by default.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq load-prefer-newer t)
(straight-use-package 'no-littering)
(setq no-littering-etc-directory my:d:etc
      no-littering-var-directory my:d:var)
(straight-use-package 'org)

;; ---------------------------------------------------------------------------
;;; Load Configuration from README.org
;; Use org-babel to load additional configuration details.
(setq init-org-file (expand-file-name "README.org" my:d))

(when (file-exists-p init-org-file)
  (condition-case err
      (progn
        (setq org-confirm-babel-evaluate nil)
        (org-babel-load-file init-org-file))
    (error
     (display-warning 'init (format "Failed to load %s: %s" init-org-file (error-message-string err))
                      :error))))

;; ---------------------------------------------------------------------------
;;; Load Configuration from user-specific-config
;; Loading user-specific settings.
(setq user-specific-config (concat my:d user-login-name ".el"))
(if (file-exists-p user-specific-config) (load user-specific-config))

;; ---------------------------------------------------------------------------
;;; Package Initialization
;; (package-initialize) is not necessary in Emacs 29+

(provide 'init)
;;; init.el ends here
