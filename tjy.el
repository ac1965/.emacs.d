;;; tjy.el --- Emacs.d -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA Takao <tjy1965@gmail.com>
;; $Lastupdate: 2021/05/23  8:29:17 $
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

(leaf *personal-configuration
  :config
  (setq user-full-name "YAMASHITA Takao"
        user-mail-address "tjy1965@gmail.com")

  (setq conf:font-name "HackGen35"
        conf:font-size 16
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

;; https://uwabami.github.io/cc-env/Emacs.html
(leaf *delete-file-if-no-contents
  :preface (defun my:delete-file-if-no-contents ()
             (when (and (buffer-file-name (current-buffer))
                        (= (point-min) (point-max)))
               (delete-file
                (buffer-file-name (current-buffer)))))
  :config
  (if (not (memq 'my:delete-file-if-no-contents after-save-hook))
      (setq after-save-hook
            (cons 'my:delete-file-if-no-contents after-save-hook))))

(leaf *keepscratchbuffer
  :preface
  (defun my:make-scratch (&optional arg)
    (interactive)
    (progn
      ;; "*scratch*" を作成して buffer-list に放り込む
      (set-buffer (get-buffer-create "*scratch*"))
      (funcall initial-major-mode)
      (erase-buffer)
      (when (and initial-scratch-message (not inhibit-startup-message))
        (insert initial-scratch-message))
      (or arg
          (progn
            (setq arg 0)
            (switch-to-buffer "*scratch*")))
      (cond ((= arg 0) (message "*scratch* is cleared up."))
            ((= arg 1) (message "another *scratch* is created")))))
  ;;
  (defun my:buffer-name-list ()
    (mapcar (function buffer-name) (buffer-list))
    :hook  ((kill-buffer-query-functions
             . (lambda ()
                 (if (string= "*scratch*" (buffer-name))
                     (progn (my:make-scratch 0) nil)
                   t)))
            (after-save-hook
             . (lambda ()
                 (unless (member "*scratch*" (my:buffer-name-list))
                   (my:make-scratch 1)))))))


(defvar my-capture-blog-file "~/devel/repos/hugo-blog/all-posts.org")
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(defun tmp ()
  (interactive)
  (mapcar #'(lambda (mode)
              (insert (s-concat "\n*** " mode))
              (mapcar #'(lambda (file)
                          (insert
                           (s-concat
                            "\n**** " file
                            "\n#+BEGIN_SRC snippet\n"
                            (f-read-text (f-join "~/.emacs.d/snippets" mode file))
                            "\n#+END_SRC")))
                      (directory-files (f-join "~/.emacs.d/snippets" mode) nil "[^.]")))
          (directory-files "~/.emacs.d/snippets" nil "[^.]")))

(leaf org-generate
  :straight t
  :custom
  (org-generate-file . `,(locate-user-emacs-file "yasnippets.org")))

(leaf ox-hugo
  :straight t
  :require t
  :after ox
  :custom ((org-hugo-front-matter-format . "yaml"))
  :config
  (defun c/ox-hugo-add-lastmod nil
    "Add `lastmod' property with the current time."
    (interactive)
    (org-set-property "EXPORT_HUGO_LASTMOD"
                      (format-time-string "[%Y-%m-%d %a %H:%M]"))))

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
"))
  (add-to-list 'org-capture-templates
               '("p" "Create new package post" entry
                 (file+headline my-capture-blog-file "emacs")
                 "** TODO %?
:PROPERTIES:
:EXPORT_FILE_NAME:
:EXPORT_DATE:
:EXPORT_HUGO_TAGS: emacs
:EXPORT_HUGO_CATEGORIES: emacs
:EXPORT_HUGO_LASTMOD:
:END:
")))

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
