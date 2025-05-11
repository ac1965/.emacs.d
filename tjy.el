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
(leaf *personal-configuration
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
  (defconst my:d:cloud "~/Documents/"
    "Directory for cloud-synced documents.")
  (defconst my:d:blog (concat my:d:cloud "devel/repos/mysite/")
    "Directory for blog development.")
  (defconst my:f:capture-blog-file
    (expand-file-name "all-posts.org" my:d:blog)
    "Path to the blog capture file.")

  ;; Function to ensure directories exist
  (defun ensure-directory (dir)
    "Ensure that DIR exists."
    (unless (file-directory-p dir)
      (warn "Directory does not exist: %s" dir)))

  ;; Ensure essential directories exist
  (mapc #'ensure-directory
        (list my:d:cloud
              my:d:blog))

  ;; Add custom elisp directories to `load-path`
  (defconst my:elisp-directory "~/.elisp"
    "Base directory for custom elisp files.")
  (dolist (dir (let ((dir (expand-file-name my:elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path)))))

;; ---------------------------------------------------------------------------
;; Emacs configuration for Logitech MX Ergo S on macOS
;; Basic mouse settings
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . 10)))
(setq mouse-wheel-progressive-speed nil)  ; disable acceleration (precision is prioritized for trackballs)

;; Smooth scrolling
(setq scroll-conservatively 10000)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position t)

;; macOS specific settings
(setq mac-mouse-wheel-smooth-scroll t)   ; enable smooth scrolling
(setq mouse-wheel-tilt-scroll t)         ; horizontal scroll support (when possible)
(setq mouse-wheel-flip-direction nil)    ; don't invert direction

;; Trackball button configuration
;; Button numbers on macOS may differ from Linux
(global-set-key [mouse-2] 'yank)             ; middle click to paste
(global-set-key [mouse-4] 'previous-buffer)  ; extra button 1
(global-set-key [mouse-5] 'next-buffer)      ; extra button 2

;; ---------------------------------------------------------------------------
;;; Email Client Configuration (Mew)
(leaf mew
  :require nil t
  :config
  ;; Autoload Mew functions
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)

  ;; Set Mew as the default mail reader
  (setq read-mail-command 'mew)

  ;; Configure Mew as the user agent for sending emails
  (autoload 'mew-user-agent-compose "mew" nil t)
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'mew-user-agent
        'mew-user-agent-compose
        'mew-draft-send-message
        'mew-draft-kill
        'mew-send-hook))

  ;; Mew server settings (example)
  (setq mew-mail-domain "ty07.net"
        mew-smtp-server "smtp.ty07.net"
        mew-imap-server "imap.ty07.net"
        mew-imap-user "ac1965@ty07.net"))

(provide 'tjy)
;;; tjy.el ends here
