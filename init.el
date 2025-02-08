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
(defvar my:d (file-name-directory (file-chase-links load-file-name))
  "Base directory for user-specific configuration.")
(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")
(defvar my:d:etc (expand-file-name ".etc/" my:d)
  "Directory for storing configuration files.")
(defvar my:d:var (expand-file-name ".var/" my:d)
  "Directory for storing variable data.")

;; Ensure necessary directories exist
(mapc #'my:ensure-directory-exists (list my:d:cache my:d:etc my:d:var))

;; ---------------------------------------------------------------------------
;;; Native Compilation
;; Redirect ELN cache to a user-specific directory for better organization.
(when (native-comp-available-p)
  (let ((eln-cache-dir (expand-file-name "eln-cache/" my:d:cache)))
    (setq native-comp-eln-load-path (list eln-cache-dir))
    (my:ensure-directory-exists eln-cache-dir)
    (startup-redirect-eln-cache eln-cache-dir)))

;; ---------------------------------------------------------------------------
;;; Package Settings
;; Configure directories for package installation and cleanup.
(setq package-user-dir (expand-file-name "elpa/" my:d:cache)
      no-littering-etc-directory my:d:etc
      no-littering-var-directory my:d:var)

;; ---------------------------------------------------------------------------
;;; Load Configuration from README.org
;; Use org-babel to load additional configuration details.
(require 'org)
(setq init-org-file (expand-file-name "README.org" my:d))

(condition-case err
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-load-file init-org-file))
  (error
   (message "Error loading org file: %s" (error-message-string err))
   (warn "Failed to load %s" init-org-file)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
