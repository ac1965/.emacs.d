;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 YAMASHITA Takao <ac1965@ty07.net>
;; Licensed under the GNU General Public License version 3 or later.

;;; Commentary:
;; This file initializes Emacs, loads core settings, and configures
;; essential packages. Most of the detailed configuration is in `README.org`.

;;; Code:

;;; Initialization
(defvar my:d (file-name-directory (file-chase-links load-file-name)) "The giant turtle on which the world rests.")
(defvar my:d:cache (concat my:d ".cache/") "Cache directory.")

(setq package-user-dir (concat my:d:cache "elpa")
      no-littering-etc-directory (concat my:d ".etc/")
      no-littering-var-directory (concat my:d ".var/"))

;; Startup performance optimization.
(setq gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (* 1024 1024)
      inhibit-default-init t)

;; Native Comp
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" my:d:cache)))
  (make-directory (car native-comp-eln-load-path) t)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "eln-cache/" my:d:cache))))

;;; Window Management
(defvar my/saved-window-config nil "Store window layout.")

;; Load settings from README.org using org-babel
(require 'org)
(setq init-org-file (expand-file-name "README.org" my:d))
(when (file-exists-p init-org-file)
  (condition-case err
      (org-babel-load-file init-org-file)
    (error (message "Error loading org file: %s" err))))

(provide 'init)
;;; init.el ends here
