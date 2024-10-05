;;; tjy.el --- Emacs.d -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; $Lastupdate: 2024/09/30  9:06:03 $
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

(leaf *personal-configuration
  :config
  (setq user-full-name "YAMASHITA Takao"
        user-mail-address "tjy1965@gmail.com")
  (setq conf:font-name "HackGen35" ; "Source Code Pro" ; "HackGen35" ; FiraCode Nerd Font Mono
        conf:font-size 16
        inhibit-compacting-font-caches t)
  (defconst my-cloud-directory "~/Documents/")
  (defconst my-blog-directory (concat my-cloud-directory "devel/repos/mysite/"))
  (defconst my-capture-blog-file (expand-file-name "all-posts.org" my-blog-directory))
  (defconst my-elisp-directory "~/.elisp")
  (dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
                 (list dir (format "%s%d" dir emacs-major-version))))
    (when (and (stringp dir) (file-directory-p dir))
      (let ((default-directory dir))
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path))))

  ;; Coding system configuration
  (set-coding-system-priority 'utf-8)
  (when (eq system-type 'darwin)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix)
    (setq-default default-process-coding-system '(utf-8 . utf-8)
                  buffer-file-coding-system 'utf-8-auto-unix
                  x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

  ;; Enable disabled commands
  (dolist (cmd '(narrow-to-defun
                 narrow-to-page
                 narrow-to-region
                 upcase-region
                 set-goal-column))
    (put cmd 'disabled nil))

  (if (fboundp 'normal-erase-is-backspace-mode)
      (normal-erase-is-backspace-mode 0))
  (load custom-file)

  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'font-setup-frame))
  (when window-system
    (progn
      (font-setup))))

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

;; request.el
(use-package request
  :ensure t)

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
    (("C-c a" . org-agenda)
     ("C-c h" . org-store-link)
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
            ;; ("j" "Journal" entry (function org-journal-find-location)
	        ;;  "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
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
    :ensure t
    :custom
    (org-superstar-headline-bullets-list . '("◉" "★" "○" "▷" "" ""))
    :hook
    (org-mode-hook (lambda () (org-superstar-mode 1)))
    )

  ;; org-journal
  (leaf org-journal
    :after org
    :ensure t
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
    :ensure t
    :bind
    ("C-x p i" . org-cliplink)
    )

  ;; org-download
  (leaf org-download
    :after org
    :ensure t
    :config
    (setq-default org-download-image-dir (concat org-directory "/pictures"))
    )

  ;; org-web-tools
  (leaf org-web-tools
    :after org
    :ensure t
    )

  ;; toc-org
  (leaf toc-org
    :after org markdown-mode
    :ensure t
    ;;:commands toc-org-enable
    :config
    (add-hook 'org-mode-hook 'toc-org-enable)
    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode)
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
    )

  ;; tomelr
  (leaf tomelr
    :ensure t)

  ;; ox-hugo
  (leaf ox-hugo
    :ensure t
    :require t
    :after ox
    :custom ((org-hugo-front-matter-format . "toml")))

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
:EXPORT_HUGO_CUSTOM_FRONT_MATTER: :pin false
:END:
\n
"))))

;; (defvar yt-iframe-format
;;   ;; You may want to change your width and height.
;;   (concat "<iframe width=\"440\""
;;           " height=\"335\""
;;           " src=\"https://www.youtube.com/embed/%s\""
;;           " frameborder=\"0\""
;;           " allowfullscreen>%s</iframe>"))

;;
;; (leaf *chatgpt
;;   :config
;;   (when (file-exists-p (expand-file-name ".env.el" my:d))
;;     (dolist (package '((openai . "https://github.com/emacs-openai/openai")
;;                        (chatgpt . "https://github.com/emacs-openai/chatgpt")
;;                        (codegpt . "https://github.com/emacs-openai/codegpt")
;;                        (dall-e . "https://github.com/emacs-openai/dall-e")
;;                        (org-ai . "https://github.com/rksm/org-ai")))
;;       (leaf (car package)
;;         :vc (:url (cdr package))
;;         :require (when (eq (car package) 'org-ai) t)
;;         :config
;;         (when (eq (car package) 'org-ai)
;;           ;; (setq org-ai-openai-api-key openai-key)
;;           (setq org-ai-openai-api-token openai-key)
;;           (add-hook 'org-mode-hook #'org-ai-mode))))))

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

;;
(defun my/open-my-file ()
  "Open file command"
  (interactive)
  (find-file my-capture-blog-file))
(define-key global-map (kbd "C-c b") 'my/open-my-file)

;;
(defun my/open-by-vscode ()
  (interactive)
  (shell-command
   (format "code -r -g %s:%d:%d"
           (buffer-file-name)
           (line-number-at-pos)
           (current-column))))
(define-key global-map (kbd "C-c C-v") 'my/open-by-vscode)

;; https://takaxp.github.io/utility.html
(defun my/print-build-info ()
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

;; Generate a cheat sheet of key bindings for the current major and minor modes.
(defun my/describe-bindings-cheatsheet ()
  "Generate a cheat sheet of key bindings for the current major and minor modes."
  (interactive)
  (let ((buffer (get-buffer-create "*Keybindings Cheat Sheet*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Keybindings Cheat Sheet for %s\n\n" major-mode))

      ;; Global keybindings
      (insert "Global keybindings:\n")
      (insert (substitute-command-keys "\\{global-map}\n\n"))

      ;; Major mode keybindings
      (insert (format "Keybindings for %s:\n" major-mode))
      (let ((major-mode-map (current-local-map)))
        (if major-mode-map
            (insert (substitute-command-keys (format "\\{%s}\n" (symbol-name major-mode))))
          (insert "No local keybindings for this mode.\n\n")))

      ;; Minor mode keybindings
      (insert "Minor mode keybindings:\n")
      (dolist (mode minor-mode-list)
        (when (and (boundp mode) (symbol-value mode))
          (let ((mode-name (symbol-name mode)))
            (insert (format "Keybindings for minor mode: %s:\n" mode-name))
            (let ((mode-map (or (cdr (assq mode minor-mode-map-alist))
                                (when (boundp mode) (symbol-value mode)))))
              (if mode-map
                  (insert (substitute-command-keys (format "\\{%s}\n" mode-map)))
                (insert "No keybindings for this mode.\n"))))))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))


;; Ignore specific windows during switching.
(defun my/ignore-window ()
  "Ignore specific windows during switching."
  (let ((excluded-buffers '("*Help*" "*Calendar*")))
    (when (member (buffer-name) excluded-buffers)
      (setq this-command 'ignore))))

(add-hook 'window-configuration-change-hook 'my/ignore-window)

;; Save window configuration
(global-set-key (kbd "C-c s")
  (lambda ()
    (interactive)
    (message "Save window configuration")
    (setq my-window-config (current-window-configuration))))

;; Restore window configuration
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (message "Restore window configuration")
                  (set-window-configuration my-window-config)))


(provide 'tjy)
;;; tjy.el ends here
