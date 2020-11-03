;;; tjy.el --- Emacs.d -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA Takao <tjy1965@gmail.com>
;; $Lastupdate: 2020/11/03 13:05:46 $
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

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'font-setup-frame))

  (when window-system
    (progn
      (font-setup)
      (set-frame-parameter nil 'fullscreen 'fullboth)))

  )

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
            (cons 'my:save-buffer-wrapper before-save-hook)))
  )

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
            (cons 'my:delete-file-if-no-contents after-save-hook)))
  )

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
                 (my:make-scratch 1))))))
  )


(defvar my-capture-blog-file "~/devel/src/github.com/ac1965/hugo-blog/all-posts.org")

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
:EXPORT_HUGO_TAGS+:
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
:EXPORT_HUGO_TAGS+: emacs
:EXPORT_HUGO_CATEGORIES: emacs
:EXPORT_HUGO_LASTMOD:
:END:
")))


(provide 'tjy)

;;; tjy.el ends here
