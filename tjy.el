;;; tjy.el --- Emacs.d -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA Takao <tjy1965@gmail.com>
;; $Lastupdate: 2023/11/19 20:44:29 $
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

  (defconst my-cloud-directory "~/Documents")

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


;; org-mode
(leaf Org-mode
  :config
  (leaf org
    :leaf-defer t
    :init
    (setq org-directory (expand-file-name "Org/" my-cloud-directory))
    (unless (file-exists-p org-directory)
      (make-directory org-directory))
    (defun org-buffer-files ()
      "Return list of opened Org mode buffer files."
      (mapcar (function buffer-file-name)
	          (org-buffer-list 'files)))
    (defun show-org-buffer (file)
      "Show an org-file FILE on the current buffer."
      (interactive)
      (if (get-buffer file)
	      (let ((buffer (get-buffer file)))
	        (switch-to-buffer buffer)
	        (message "%s" file))
	    (find-file (concat org-directory "/" file))))
    :bind
    (("\C-ca" . org-agenda)
     ("\C-cc" . org-capture)
     ("\C-ch" . org-store-link)
     ("C-M--" . #'(lambda () (interactive)
		            (show-org-buffer "gtd.org")))
     ("C-M-^" . #'(lambda () (interactive)
		            (show-org-buffer "notes.org")))
     ("C-M-~" . #'(lambda () (interactive)
    		        (show-org-buffer "kb.org")))
     )
    :config
    (setq  org-agenda-files (list org-directory)
	       org-default-notes-file "notes.org"
	       org-log-done 'time
	       org-startup-truncated nil
	       org-startup-folded 'content
	       org-use-speed-commands t
	       org-enforce-todo-dependencies t)
    (remove (concat org-directory "/archives") org-agenda-files)
    (setq org-todo-keywords
	      '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c@)")))
    (setq org-refile-targets
	      (quote ((nil :maxlevel . 3)
		          (org-buffer-files :maxlevel . 1)
		          (org-agenda-files :maxlevel . 3))))
    (setq org-capture-templates
	      '(("t" "Todo" entry (file+headline "gtd.org" "Inbox")
	         "* TODO %?\n %i\n %a")
            ("n" "Note" entry (file+headline "notes.org" "Notes")
	         "* %?\nEntered on %U\n %i\n %a")
            ("j" "Journal" entry (function org-journal-find-location)
	         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
	        ("h" "Hugo post" entry (file+olp "jamhattaorg.org" "Blog Ideas")
             (function org-hugo-new-subtree-post-capture-template))
	        ))
    )

  ;; org-babel
  (leaf ob
    :after org
    :defun org-babel-do-load-languages
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (python . t)
       (R . t)
       (ditaa . t)
       (plantuml . t)
       ))
    )

  ;; org-superstar
  (leaf org-superstar
    :after org
    :straight t
    :custom
    (org-superstar-headline-bullets-list . '("◉" "★" "○" "▷" "" ""))
    :hook
    (org-mode-hook (lambda () (org-superstar-mode 1)))
    )

  ;; org-journal
  (leaf org-journal
    :after org
    :straight t
    :config
    (setq org-journal-dir (concat org-directory "/journal")
	      org-journal-enable-agenda-integration t)
    (defun org-journal-find-location ()
      ;; Open today's journal, but specify a non-nil prefix argument in order to
      ;; inhibit inserting the heading; org-capture will insert the heading.
      (org-journal-new-entry t)
      ;; Position point on the journal's top-level heading so that org-capture
      ;; will add the new entry as a child entry.
      (goto-char (point-min))
      )
    )

  ;; org-cliplink
  (leaf org-cliplink
    :after org
    :straight t
    :bind
    ("C-x p i" . org-cliplink)
    )

  ;; org-download
  (leaf org-download
    :after org
    :straight t
    :config
    (setq-default org-download-image-dir (concat org-directory "/pictures"))
    )

  ;; org-web-tools
  (leaf org-web-tools
    :after org
    :straight t
    )

  ;; toc-org
  (leaf toc-org
    :after org markdown-mode
    :straight t
    ;;:commands toc-org-enable
    :config
    (add-hook 'org-mode-hook 'toc-org-enable)
    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode)
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
    )

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

  ;; ox-qmd
  (leaf ox-qmd
    :after ox
    :straight t
    :require t
    )

  ;; org2blog
  (leaf org2blog
    :after org
    ;; the latest version doesn't work
    :straight (org2blog :type git :host github :repo "sachac/org2blog")
    :leaf-autoload org2blog-autoloads
    :commands org2blog-user-login
    :config
    (setq org2blog/wp-use-sourcecode-shortcode t)
    (setq org2blog/wp-blog-alist
          `(("wp"
	         :url "https://www.example.org/xmlrpc.php" ;; CHANGEME
             :username ,(car (auth-source-user-and-password "wordpress")) ;; CHANGEME
             :password ,(cadr (auth-source-user-and-password "wordpress")) ;; CHANGEME
	         )
	        ))
    (setq org2blog/wp-buffer-template
	      "#+TITLE:
#+CATEGORY:
#+TAGS:
#+OPTIONS:
#+PERMALINK: \n")
    )

  ;; org-roam
  (leaf org-roam
    :straight t
    :after org
    :bind
    ("C-c n l" . org-roam-buffer-toggle)
    ("C-c n f" . org-roam-node-find)
    ("C-c n g" . org-roam-graph)
    ("C-c n i" . org-roam-node-insert)
    ("C-c n c" . org-roam-capture)
    ;; Dailies
    ("C-c n j" . org-roam-dailies-capture-today)
    :config
    (setq org-roam-directory (concat org-directory "/org-roam"))
    (unless (file-exists-p org-directory)
      (make-directory org-roam-directory))
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    (require 'org-roam-protocol)
    )

  ;; org-roam-ui
  (leaf org-roam-ui
    :straight (org-roam-ui :host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    )
  )

(defvar my-capture-blog-file "~/devel/repos/hugo-blog/all-posts.org")
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))


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
