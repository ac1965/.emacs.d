;;; tjy.el --- Emacs.d -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA Takao <tjy1965@gmail.com>
;; $Lastupdate: 2023/11/04 21:45:23 $
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

(leaf *personal-configuration
  :config
  (setq user-full-name "YAMASHITA Takao"
        user-mail-address "tjy1965@gmail.com")

  (setq conf:font-name "FiraCode Nerd Font Mono" ; FiraCode Nerd Font Mono
        conf:font-size 15
        inhibit-compacting-font-caches t)

  (defconst my-elisp-directory "~/.elisp")
  (dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'font-setup-frame))

  (when window-system
    (progn
      (font-setup))))

;; $Lastupdate: yyyy/mm/dd hh:mm:ss $
(leaf *lastupdate
  :preface (defun my:save-buffer-wrapper ()
             (interactive)
             (let ((tostr (concat "$Lastupdate: " (format-time-string "%Y/%m/%d %k:%M:%S") " $")))
               (save-excursion
                 (goto-char (point-min))
                 (while (re-search-forward
                         "\\$Lastupdate\\([0-9/: ]*\\)?\\$" nil t)
                   (replace-match tostr nil t)))))
  :config
  (if (not (memq 'my:save-buffer-wrapper before-save-hook))
      (setq before-save-hook
            (cons 'my:save-buffer-wrapper before-save-hook))))

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


(defvar my-capture-blog-file "~/devel/repos/hugo-blog/all-posts.org")
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

;; ox-hugo
(leaf ox-hugo
  :straight t
  :after ox
  :custom ((org-hugo-front-matter-format . "yaml"))
  :config
  (defun c/ox-hugo-add-lastmod nil
    "Add `lastmod' property with the current time."
    (interactive)
    (org-set-property "EXPORT_HUGO_LASTMOD"
                      (format-time-string "[%Y-%m-%d %a %H:%M]"))))

;; ox-hugo-capture
(leaf *ox-hugo--capture
  :require org-capture
  :defvar (org-capture-templates)
  :config
  (add-to-list 'org-capture-templates
               '("b" "Create new blog post" entry
                 (file+headline my-capture-blog-file "blog")
                 "** TODO %?
:PROPERTIES:
:EXPORT_FILE_NAME: %(apply #'format \"%s-%s-%s\"
(format-time-string \"%Y\")
(let ((sha1 (sha1 (shell-command-to-string \"head -c 1024 /dev/urandom\"))))
(cl-loop for (a b c d) on (cdr (split-string sha1 \"\")) by #'cddddr repeat 2 collect (concat a b c d))))
:EXPORT_DATE:
:EXPORT_HUGO_TAGS:
:EXPORT_HUGO_CATEGORIES:
:EXPORT_HUGO_LASTMOD:
:END:
"
                 )))

;;
;; (leaf *chatgpt
;;   :config
;;   (leaf openai :straight (openai :type git :host github :repo "emacs-openai"))
;;   (leaf chatgpi :straight (chatgpi :type git :host github :repo "emacs-openai/chatgpt"))
;;   (leaf codegpt :straight (codegpi :type git :host github :repo "emacs-openai/codegpt"))
;;   (leaf dall-e :straight (dall-e :type git :host github :repo "emacs-openai/dall-e"))
;;   :init
;;   (if (file-exists-p (expand-file-name ".env.el" emacs-d))
;;       (load (expand-file-name ".env.el" emacs-d))))

;;
(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;; Https://takaxp.github.io/utility.html
(defun my-print-build-info ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Build info*"))
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert
     (format "GNU Emacs %s\nCommit:\t\t%s\nBranch:\t\t%s\nSystem:\t\t%s\nDate:\t\t\t%s\n"
             emacs-version
             (emacs-repository-get-version)
             (when (version< "27.0" emacs-version)
               (emacs-repository-get-branch))
             system-configuration
             (format-time-string "%Y-%m-%d %T (%Z)" emacs-build-time)))
    (insert (format "Patch:\t\t%s ns-inline.patch\n"
                    (if (boundp 'mac-ime--cursor-type) "with" "without")))
    (insert
     (format "Features:\t%s\n" system-configuration-features))
    ;; (insert
    ;;  (format "Options:\t%s\n"  system-configuration-options))
    )
  (view-mode))

;;
(defun add-org-task-to-reminder ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (setq reminder-list-name "リマインダー")
    (setq element (org-element-at-point))              ;; カーソル位置のタスク(エレメント)を取得する
    (setq title (org-element-property :title element)) ;; そのタスク(エレメント)から名前を取得する
    (setq command (format "reminders add %s %s" reminder-list-name title))
    (shell-command-to-string command)))


(provide 'tjy)

;;; tjy.el ends here
