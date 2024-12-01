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
        (normal-top-level-add-subdirs-to-load-path))))

  (when window-system
    (font-setup)))

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

;; Org-mode Setup
(leaf Org-mode
  :config
  (leaf org
    :leaf-defer t
    :init
    (setq org-directory (expand-file-name "Org/" my:d:cloud))
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
     ("C-c c" . org-capture)
     ("C-c h" . org-store-link)
     ("C-M--" . #'(lambda () (interactive)
		            (show-org-buffer "gtd.org")))
     ("C-M-^" . #'(lambda () (interactive)
		            (show-org-buffer "notes.org")))
     ("C-M-~" . #'(lambda () (interactive)
    		        (show-org-buffer "kb.org"))))
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
	        )))

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
       )))

  ;; org-superstar
  (leaf org-superstar
    :after org
    :ensure t
    :custom
    (org-superstar-headline-bullets-list . '("◉" "★" "○" "▷" "" ""))
    :hook
    (org-mode-hook (lambda () (org-superstar-mode 1))))

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
      (goto-char (point-min))))

  ;; org-cliplink
  (leaf org-cliplink
    :after org
    :ensure t
    :bind
    ("C-x p i" . org-cliplink))

  ;; org-download
  (leaf org-download
    :after org
    :ensure t
    :config
    (setq-default org-download-image-dir (concat org-directory "/pictures")))

  ;; org-web-tools
  (leaf org-web-tools
    :after org
    :ensure t)

  ;; toc-org
  (leaf toc-org
    :after org markdown-mode
    :ensure t
    ;;:commands toc-org-enable
    :config
    (add-hook 'org-mode-hook 'toc-org-enable)
    ;; enable in markdown, too
    (add-hook 'markdown-mode-hook 'toc-org-mode)
    (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point))

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
")))

  ;; org-roam
  (leaf org-roam
    :ensure t
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
    (require 'org-roam-protocol))
  )

;; Setup ChatGPT, CodeGPT, and DALL-E
(leaf *chatgpt
  :config
  (leaf openai :vc (:url "https://github.com/emacs-openai/openai"))
  (leaf chatgpi :vc (:url "https://github.com/emacs-openai/chatgpt"))
  (leaf codegpt :vc (:url "https://github.com/emacs-openai/codegpt"))
  (leaf dall-e :vc (:url "https://github.com/emacs-openai/dall-e"))
  :init
  (if (file-exists-p (expand-file-name ".env.el" my:d))
      (load (expand-file-name ".env.el" my:d))))

;; Miscellaneous Functions
(defvar my/yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format my/yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

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

;; Generate a table of keybindings sorted by key sequence and command name.
(defun my/generate-keybinding-table ()
  "Generate a table of keybindings sorted by key sequence and command name."
  (interactive)
  (let ((bindings '()))
    ;; Iterate through all keymaps and collect keybindings
    (mapatoms
     (lambda (sym)
       (when (commandp sym)
         (let ((keys (where-is-internal sym)))
           (dolist (key keys)
             (push (list (key-description key) (symbol-name sym)) bindings))))))
    ;; Sort by key sequence and then by command name
    (setq bindings
          (sort bindings
                (lambda (a b)
                  (or (string< (car a) (car b))
                      (and (string= (car a) (car b))
                           (string< (cadr a) (cadr b)))))))
    ;; Create the table in tabulated-list-mode
    (with-current-buffer (get-buffer-create "*Keybindings Table*")
      (tabulated-list-mode)
      (setq tabulated-list-format [("Key" 20 t) ("Command" 40 t)])
      (setq tabulated-list-entries
            (mapcar (lambda (x)
                      (list (car x) (vector (car x) (cadr x))))
                    bindings))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(define-key global-map (kbd "C-c C-k") 'my/generate-keybinding-table)

;; Convert KEYMAP to an Org-mode table.
(defun my/keymap-to-org-table (keymap)
  "Convert KEYMAP to an Org-mode table."
  (let ((bindings '())
        (output ""))
    ;; Collect key bindings and commands
    (map-keymap
     (lambda (key binding)
       (when (and (vectorp key) (commandp binding))
         (let ((key-desc (key-description key))
               (command (symbol-name binding)))
           (push (list key-desc command) bindings))))
     keymap)
    ;; Sort by key order
    (setq bindings (sort bindings (lambda (a b) (string< (car a) (car b)))))
    ;; Create Org-mode table header
    (setq output "| Key | Command |\n|-\n")
    ;; Populate table rows
    (dolist (binding bindings)
      (setq output (concat output "| " (car binding) " | " (cadr binding) " |\n")))
    ;; Return the Org table
    output))

;; Print the Org table for the given KEYMAP.
(defun my/print-keymap-org-table (keymap)
  "Print the Org table for the given KEYMAP."
  (interactive "aKeymap: ")
  (let ((org-table (my/keymap-to-org-table keymap)))
    (with-current-buffer (get-buffer-create "*Keymap Org Table*")
      (erase-buffer)
      (insert org-table)
      (org-mode)
      (pop-to-buffer (current-buffer)))))

;; Output the global keymap in Org-mode table format
;; (my/print-keymap-org-table (current-global-map))

(defun my/show-mode-keybindings ()
  "Display a list of keybindings for the major and minor modes of the current buffer in a new *Help* buffer if one already exists."
  (interactive)
  (let ((help-buffer (get-buffer-create "*Help*"))) ;; Create or retrieve the *Help* buffer
    (when (get-buffer-window help-buffer) ;; Check if *Help* buffer is already visible
      (setq help-buffer (generate-new-buffer "*Help*"))) ;; Create a new buffer if visible
    (with-current-buffer help-buffer
      (describe-mode)) ;; Display mode keybindings in the buffer
    (display-buffer help-buffer))) ;; Show the buffer in the current window

(define-key global-map (kbd "C-c C-s") 'my/show-mode-keybindings)

(defun my/keybindings-to-org-table (prefix)
  "Show keybindings under a given PREFIX as an Org-mode table."
  (interactive "sEnter key prefix (e.g., 'C-c'): ")
  (let ((keymap (current-global-map))
        (output '()))
    ;; Get all key bindings starting with the given prefix
    (map-keymap
     (lambda (event binding)
       (let ((key (vector event)))
         (when (and (keymapp binding)
                    (key-binding (vconcat (list (kbd prefix)) key)))
           (map-keymap
            (lambda (ev bind)
              (let ((full-key (vconcat (list (kbd prefix) ev))))
                (push (list (key-description full-key)
                            (format "%s" bind))
                      output)))
            binding))))
     keymap)
    ;; Sort by keybinding
    (setq output (sort output (lambda (a b) (string< (car a) (car b)))))
    ;; Output as Org-mode table
    (insert "| Keybinding | Command |\n")
    (insert "|------------+---------|\n")
    (dolist (entry output)
      (insert (format "| %s | %s |\n" (car entry) (cadr entry))))
    (org-table-align)))

(define-key global-map (kbd "C-c C-;") 'my/keybindings-to-org-table)

;;
(defun my/add-org-task-to-reminder ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (setq reminder-list-name "リマインダー")
    (setq element (org-element-at-point))
    (setq title (org-element-property :title element))
    (setq command (format "reminders add %s %s" reminder-list-name title))
    (shell-command-to-string command)))


(provide 'tjy)
;;; tjy.el ends here
