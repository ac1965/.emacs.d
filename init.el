;;; init.el --- Main configuration file -*- coding: utf-8 ; lexical-binding: t; -*-

;; Copyright (c) 2021-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.
;; Keywords: initialization, modular

;; $Lastupdate: 2025/05/25 10:33:25 $

;;; Commentary:
;; This is the main configuration file for Emacs. It initializes directories,
;; sets up packages, and loads modular configurations from `README.org`.

;;; Code:

;; ---------------------------------------------------------------------------
;;; Utility Functions
(defun my:ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (condition-case err
        (make-directory dir t)
      (error (warn "Failed to create directory: %s - %s" dir err)))))

(defun my/org-babel-tangle-on-save ()
  "Tangle org file automatically when saving."
  (when (string= (file-name-extension buffer-file-name) "org")
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/org-babel-tangle-on-save)

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
(when (and custom-file (file-exists-p custom-file))
  (ignore-errors (load custom-file)))

;; ---------------------------------------------------------------------------
;;; Package Settings
;; Configure directories for cleanup.
(setq package-user-dir (expand-file-name "elpa/" my:d:cache))

;; Ensure package directory exists
(my:ensure-directory-exists package-user-dir)

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


(provide 'init)
;;; init.el ends here
