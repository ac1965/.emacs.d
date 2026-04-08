;;; orgx/orgx-roam-ui.el --- org-roam-ui graph visualiser -*- lexical-binding: t; -*-

;;; Code:

(condition-case err
    (progn
      (straight-use-package 'simple-httpd)
      (require 'web-server)
      (provide 'simple-httpd))
  (error (message "[orgx-roam-ui] web-server load failed: %s"
                  (error-message-string err))))

(condition-case err
    (progn
      (straight-use-package 'websocket)
      (require 'websocket))
  (error (message "[orgx-roam-ui] websocket load failed: %s"
                  (error-message-string err))))

(condition-case err
    (progn
      (straight-use-package
       '(org-roam-ui :type git :host github
                     :repo "org-roam/org-roam-ui"
                     :branch "main" :files ("*.el" "out")))
      (require 'org-roam-ui))
  (error (message "[orgx-roam-ui] org-roam-ui load failed: %s"
                  (error-message-string err))))

(provide 'orgx-roam-ui)
;;; orgx/orgx-roam-ui.el ends here
;;; orgx/orgx-fold.el --- Extra Org folding helpers -*- lexical-binding: t; -*-
;;
;;
;; Copyright (c) 2021-2026
;; Author: YAMASHITA, Takao
;; License: GNU GPL v3 or later
;;
;; Category: org
;;
;;; Commentary:
;;
;; Extra Org folding commands with Emacs version compatibility.
;;
;; This file:
;; - provides `my/org-fold-subtree`, `my/org-unfold-subtree`, and
;;   `my/org-toggle-fold` as interactive commands
;; - binds them to C-c f, C-c e, C-c t in `org-mode-map`
;; - guards `org-fold-folded-p` with `fboundp`; falls back to
;;   `outline-invisible-p` on Emacs < 29
;;; Code:

(with-eval-after-load 'org
  (require 'org-fold)

  (defun my/org-fold-subtree ()
    "Fold the subtree at point."
    (interactive) (org-fold-subtree t))

  (defun my/org-unfold-subtree ()
    "Unfold (show) the subtree at point."
    (interactive) (org-show-subtree))

  (defun my/org-toggle-fold ()
    "Toggle fold state of the subtree at point."
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (if (if (fboundp 'org-fold-folded-p)
              (org-fold-folded-p (point))
            ;; Emacs < 29 fallback: check outline visibility
            (outline-invisible-p (line-end-position)))
          (org-show-subtree)
        (org-fold-subtree t))))

  (define-key org-mode-map (kbd "C-c f") #'my/org-fold-subtree)
  (define-key org-mode-map (kbd "C-c e") #'my/org-unfold-subtree)
  (define-key org-mode-map (kbd "C-c t") #'my/org-toggle-fold))

(provide 'orgx-fold)
;;; org/orgx-fold.el ends here
