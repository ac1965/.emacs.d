;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; This file initializes Emacs, loads core settings, and configures
;; essential packages. Most of the detailed configuration is in `README.org`.

;;; Code:

;;; Initialization
(defvar my:d (file-name-directory (file-chase-links load-file-name))
  "Base directory for user-specific configuration.")
(defvar my:d:cache (expand-file-name ".cache/" my:d)
  "Cache directory for temporary files.")

;; Package and no-littering configuration
(setq package-user-dir (expand-file-name "elpa/" my:d:cache)
      no-littering-etc-directory (expand-file-name ".etc/" my:d)
      no-littering-var-directory (expand-file-name ".var/" my:d))

;; Optimize startup performance
(setq gc-cons-threshold (* 100 1024 1024)  ; Increase for faster startup
      read-process-output-max (* 4 1024 1024)  ; Enhance subprocess I/O performance
      inhibit-default-init t)

;; Use Emacs 30's native startup improvements
(when (boundp 'early-init-file)
  (setq native-comp-jit-compilation t)) ; Enable JIT compilation

;; Native Compilation Settings
(when (native-comp-available-p)
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" my:d:cache)))
  (make-directory (car native-comp-eln-load-path) t)
  (startup-redirect-eln-cache (car native-comp-eln-load-path)))

;; Load settings from README.org using org-babel
(require 'org)
(setq init-org-file (expand-file-name "README.org" my:d))
(when (file-exists-p init-org-file)
  (condition-case err
      (org-babel-load-file init-org-file)
    (error (message "Error loading org file: %s" (error-message-string err)))))

(provide 'init)
;;; init.el ends here
