;;; tjy.el --- Personal Configuration -*- lexical-binding: t; -*-
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; License: GNU General Public License version 3 or later
;; Keywords: personal, configuration, authentication, email

;;; Commentary:
;; This file contains personal configurations including:
;; - User information and directory setup
;; - Authentication management
;; - Email client settings

;;; Code:

;; ---------------------------------------------------------------------------
;;; Personal Information and General Configuration
(leaf *personal-configuration
  :config
  ;; User information
  (setq user-full-name "YAMASHITA Takao"
        user-mail-address "ac1965@ty07.net"
        conf:font-family "Source Code Pro" ; Preferred font
        conf:font-size 16 ; Font size
        inhibit-compacting-font-caches t ; Improve font performance
        plstore-cache-passphrase-for-symmetric-encryption t)

  ;; Define essential directories
  (defconst my:d:cloud "~/Documents/"
    "Directory for cloud-synced documents.")
  (defconst my:d:blog (concat my:d:cloud "devel/repos/mysite/")
    "Directory for blog development.")
  (defconst my-capture-blog-file
    (expand-file-name "all-posts.org" my:d:blog)
    "Path to the blog capture file.")
  (defconst my:d:password-store
    (or (getenv "PASSWORD_STORE_DIR")
        (concat my:d:cloud "password-store"))
    "Path to the password store.")

  ;; Add custom elisp directories to `load-path`
  (defconst my-elisp-directory "~/.elisp"
    "Base directory for custom elisp files.")
  (dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path)))))

;; ---------------------------------------------------------------------------
;;; Authentication Management
(leaf *authentication
  :if (and (getenv "GPG_KEY_ID")
           (file-directory-p my:d:password-store))
  :init
  ;; Check for necessary environment variables and directories
  (unless (getenv "GPG_KEY_ID")
    (warn "GPG_KEY_ID is not set. Authentication features may not work properly."))
  (unless (file-directory-p my:d:password-store)
    (warn "Password store directory does not exist: %s" my:d:password-store))

  ;; Default settings for `plstore`
  (setq leaf-default-plstore
        (plstore-open (expand-file-name "plstore.plist" my:d:password-store)))

  ;; Exclude password store from version control
  (add-to-list 'vc-directory-exclusion-list
               (expand-file-name my:d:password-store))

  ;; Configure authentication sources
  (leaf auth-source
    :custom
    `((auth-source-gpg-encrypt-to . (getenv "GPG_KEY_ID"))))

  ;; Use password-store and auth-source-pass for password management
  (leaf password-store :ensure t)
  (leaf auth-source-pass :ensure t)

  ;; Configure `plstore` for secure storage
  (leaf plstore
    :custom
    `((plstore-secret-keys . 'silent)
      (plstore-encrypt-to . ,(getenv "GPG_KEY_ID")))))

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
        'mew-send-hook)))

(provide 'tjy)
;;; tjy.el ends here
