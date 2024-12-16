;;; tjy.el --- Emacs.d -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

;; Personal Information and General Configuratio
(leaf *personal-configuration
  :config
  (setq user-full-name "YAMASHITA Takao"
        user-mail-address "ac1965@ty07.net"
        conf:font-family "Source Code Pro" ; "Roboto Mono" ; "FiraCode Nerd Font" ; "HackGen35
        conf:font-size 16
        inhibit-compacting-font-caches t
        plstore-cache-passphrase-for-symmetric-encryption t)

  ;; Define directories
  (defconst my:d:cloud "~/Documents/")
  (defconst my:d:blog (concat my:d:cloud "devel/repos/mysite/"))
  (defconst my-capture-blog-file (expand-file-name "all-posts.org" my:d:blog))
  (defconst my:d:password-store
    (if (getenv "PASSWORD_STORE_DIR")
        (concat my:d:cloud (getenv "PASSWORD_STORE_DIR"))))

  ;; Add custom elisp directories to load-path
  (defconst my-elisp-directory "~/.elisp")
  (dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path)))))

;; Personal Authentication
(leaf *authentication
  :if (and (getenv "GPG_KEY_ID")
           (file-directory-p my:d:password-store))
  :init
  (setq leaf-default-plstore
        (plstore-open
         (expand-file-name "plstore.plist" my:d:password-store)))
  (add-to-list 'vc-directory-exclusion-list
               (expand-file-name my:d:password-store))
  (leaf auth-source
    :custom
    `((auth-source-gpg-encrypt-to . '(getenv "GPG_KEY_ID"))
      ;; (auth-sources
      ;;  . ,(expand-file-name "authinfo.gpg" my:d:password-store))
      )
    )
  (leaf password-store :ensure t)
  (leaf auth-source-pass :ensure t)
  (leaf plstore
    :custom
    `((plstore-secret-keys . 'silent)
      (plstore-encrypt-to  . ,(getenv "GPG_KEY_ID")))
    ))

;; Configure MEW Email Client
(leaf mew
  :require nil t
  :config
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)
  (setq read-mail-command 'mew)
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
