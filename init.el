;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; This file initializes Emacs, sets up directories, configures
;; essential packages, and ensures a modular and clean setup.

;;; Code:

;;; Utility Functions
(defun my:ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;; Directories
;; Define base and essential directories
(defvar my:d (file-name-directory (file-chase-links load-file-name))
  "Base directory for user-specific configuration.")
(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")
(defvar my:d:etc (expand-file-name ".etc/" my:d)
  "Directory for storing config files.")
(defvar my:d:var (expand-file-name ".var/" my:d)
  "Directory for storing variable data.")

;; Ensure necessary directories exist
(mapc #'my:ensure-directory-exists (list my:d:cache my:d:etc my:d:var))

;;; Native Compilation Settings
;; Redirect ELN cache to a user-specific directory for better organization
(when (native-comp-available-p)
  (let ((eln-cache-dir (expand-file-name "eln-cache/" my:d:cache)))
    (setq native-comp-eln-load-path (list eln-cache-dir))
    (my:ensure-directory-exists eln-cache-dir)
    (startup-redirect-eln-cache eln-cache-dir)))

;;; Package and no-littering configuration
(setq package-user-dir (expand-file-name "elpa/" my:d:cache) ; Customize package installation directory
      no-littering-etc-directory (expand-file-name ".etc/" my:d) ; Store config files
      no-littering-var-directory (expand-file-name ".var/" my:d)) ; Store variable data

;;; Load settings from README.org using org-babel
(require 'org)
(setq init-org-file (expand-file-name "README.org" my:d))

;; Check if README.org exists, and load it
(if (file-exists-p init-org-file)
    (condition-case err
        (org-babel-load-file init-org-file)
      (error
       (message "Error loading org file: %s" (error-message-string err))))
  (message "README.org not found. Please ensure it exists in %s" my:d))

;; ;;; Asynchronous Package Loading (Emacs 30+)
;; ;; Optimize package activation for Emacs 30 and later
;; (when (fboundp 'package-activate-all)
;;   (package-activate-all :async t))

;;; Provide the init feature
(provide 'init)
;;; init.el ends here
