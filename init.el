;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; This file initializes Emacs, loads core settings, and configures
;; essential packages. Most of the detailed configuration is in `README.org`.

;;; Code:

;;; Directories
(defvar my:d (file-name-directory (file-chase-links load-file-name))
  "Base directory for user-specific configuration.")
(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")

;; Ensure cache directories exist
(dolist (dir (list my:d:cache))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;; Package and no-littering configuration
(setq package-user-dir (expand-file-name "elpa/" my:d:cache) ; Customize package installation directory
      no-littering-etc-directory (expand-file-name ".etc/" my:d) ; Store config files
      no-littering-var-directory (expand-file-name ".var/" my:d)) ; Store variable data

;;; Native Compilation Settings
(when (native-comp-available-p)
  ;; Redirect ELN (Emacs Lisp Native) cache to user-specific directory
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" my:d:cache)))
  (make-directory (car native-comp-eln-load-path) t) ; Ensure directory exists
  (startup-redirect-eln-cache (car native-comp-eln-load-path)))

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

;;; Provide the init feature
(provide 'init)
;;; init.el ends here
