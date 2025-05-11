;;; tjy.el --- Personal Configuration -*- lexical-binding: t; -*-
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; License: GNU General Public License version 3 or later
;; Keywords: personal, configuration, authentication, email

;;; Commentary:
;; This file contains personal configurations, including:
;; - User information and directory setup
;; - Authentication management
;; - Email client settings (Mew)

;;; Code:

;; ---------------------------------------------------------------------------
;;; User Information and General Configuration
(use-package emacs
  :config
  ;; User information
  (setq user-full-name "YAMASHITA Takao"
        user-mail-address "ac1965@ty07.net"
        my:font-family "Iosevka Nerd Font"
        my:font-size 18
        inhibit-compacting-font-caches t
        plstore-cache-passphrase-for-symmetric-encryption t)

  ;; Font settings
  (set-face-attribute 'default nil
                      :family my:font-family
                      :height (* my:font-size 10))

  ;; Define essential directories
  (defconst my:cloud-directory "~/Documents/"
    "Directory for cloud-synced documents.")
  (defconst my:blog-directory (concat my:cloud-directory "devel/repos/mysite/")
    "Directory for blog development.")
  (defconst my:capture-blog-file
    (expand-file-name "all-posts.org" my:blog-directory)
    "Path to the blog capture file.")
  (defvar my:excluded-directories '("/Users/ac1965/Library/Accounts"))

  ;; Function to ensure directories exist
  (defun ensure-directory (dir)
    "Ensure that DIR exists."
    (unless (file-directory-p dir)
      (warn "Directory does not exist: %s" dir)))

  ;; Ensure essential directories exist
  (mapc #'ensure-directory
        (list my:cloud-directory
              my:blog-directory))

  ;; Add custom elisp directories to `load-path`
  (defconst my:elisp-directory "~/.elisp"
    "Base directory for custom elisp files.")
  (dolist (dir (list (expand-file-name my:elisp-directory)))
    (when (and (stringp dir) (file-directory-p dir))
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path))))

;; ---------------------------------------------------------------------------
;;; Logitech MX Ergo S Configuration (macOS)
(use-package emacs
  :config
  ;; Basic mouse settings
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 10)))
  (setq mouse-wheel-progressive-speed nil)  ; disable acceleration

  ;; Smooth scrolling
  (setq scroll-conservatively 10000)
  (setq scroll-margin 2)
  (setq scroll-preserve-screen-position t)

  ;; macOS specific settings
  (setq mac-mouse-wheel-smooth-scroll t
        mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction nil)

  ;; Trackball button configuration
  (global-set-key [mouse-2] 'yank)             ; middle click to paste
  (global-set-key [mouse-4] 'previous-buffer)  ; extra button 1
  (global-set-key [mouse-5] 'next-buffer))     ; extra button 2

(provide 'ac1965)
;;; ac1965.el ends here
