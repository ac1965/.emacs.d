;;; Basic Configuration

;; Update last modified timestamp before saving the file
(leaf *lastupdate
  :preface
  (defun my/save-buffer-wrapper ()
    (interactive)
    (let ((tostr (concat "$Lastupdate: " (format-time-string "%Y/%m/%d %k:%M:%S") " $")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\$Lastupdate\\([0-9/: ]*\\)?\\$" nil t)
          (replace-match tostr nil t)))))
  :hook (before-save-hook . my/save-buffer-wrapper))

;; no-littering: Keep Emacs clean by organizing config and cache files
(leaf no-littering
  :ensure t
  :custom ((no-littering-etc-directory . (expand-file-name ".etc/" my:d))
           (no-littering-var-directory . (expand-file-name ".var/" my:d))))

;; macOS specific settings for shell integration
(leaf exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :custom ((exec-path-from-shell-variables . '("PATH" "MANPATH" "SHELL")))
  :config
  (exec-path-from-shell-initialize))

;; Customize basic Emacs behaviors
(leaf cus-edit
  :custom ((custom-file . ,(concat no-littering-etc-directory "custom.el"))))

(leaf cus-start
  :custom '((create-lockfiles . nil)
            (tab-width . 4)
            (debug-on-error . nil)
            (init-file-debug . nil)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (truncate-lines . t)
            (inhibit-startup-screen . t)
            (menu-bar-mode . t)
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)
            (tab-bar-mode . t))
  :bind (("M-ESC ESC" . redraw-frame)))

;; Automatically revert buffers if file changes on disk
(leaf autorevert
  :global-minor-mode global-auto-revert-mode)

;; Paren matching highlighting
(leaf paren
  :custom ((show-paren-delay . 0)
           (show-paren-style . 'expression))
  :global-minor-mode show-paren-mode
  :config
  (leaf puni
    :ensure t
    :global-minor-mode puni-global-mode))

;; Auto save and backup settings
(leaf files
  :custom `((auto-save-file-name-transforms . '((".*" ,(concat no-littering-var-directory "backup/") t)))
            (backup-directory-alist . '((".*" . ,(concat no-littering-var-directory "backup"))))
            (version-control . t)
            (delete-old-versions . t)
            (auto-save-visited-interval . 1))
  :global-minor-mode auto-save-visited-mode)

;; Tramp: Remote file editing
(leaf tramp
  :custom `((tramp-persistency-file-name . ,(concat no-littering-var-directory "tramp"))
            (tramp-auto-save-directory . ,(concat no-littering-var-directory "tramp-autosave"))
            (tramp-default-method . "scp")
            (tramp-use-ssh-controlmaster-options . nil)
            (tramp-password-prompt-regexp . (concat "^.*"
                                                    (regexp-opt '("passphrase" "password" "Verification code") t)
                                                    ".*:\0? *"))))

;; Miscellaneous useful settings
(leaf startup
  :custom `((auto-save-list-file-prefix . ,(concat no-littering-var-directory "backup/.saves-"))))

(leaf savehist
  :global-minor-mode t
  :custom `((savehist-file . ,(concat no-littering-var-directory "savehist"))))

;; Display keybindings in a popup
(leaf which-key
  :ensure t
  :global-minor-mode t)

;; Automatic parenthesis pairing
(leaf elec-pair
  :global-minor-mode electric-pair-mode)

;; Tree-sitter support for improved syntax highlighting and parsing
(leaf tree-sitter
  :ensure t
  :hook (go-mode-hook . tree-sitter-mode))

(leaf tree-sitter-langs
  :ensure t)

;; Enable global visual-line-mode for better word wrapping
(leaf visual-line-mode
  :global-minor-mode t)

;; pbcopy integration for macOS clipboard support
(leaf pbcopy
  :ensure t
  :if (memq window-system '(mac ns)))

;; Useful utilities for dired, expand-region, and aggressive-indent
(leaf dired-filter
  :ensure t)

(leaf expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(leaf aggressive-indent
  :ensure t
  :global-minor-mode global-aggressive-indent-mode)

;; Search and jump utilities
(leaf rg
  :ensure t)

(leaf dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom ((dumb-jump-force-searcher . 'rg)))

;; Multi-cursor editing
(leaf multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Version control using Magit
(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Flymake and Flycheck for on-the-fly syntax checking
(leaf flymake
  :ensure t
  :global-minor-mode t)

(leaf flycheck
  :ensure t
  :global-minor-mode t)

;; Flyspell for spell checking
(leaf flyspell
  :ensure t
  :hook (text-mode . flyspell-mode)
  :custom ((ispell-program-name . "aspell")))

;; Projectile for project management
(leaf projectile
  :ensure t
  :global-minor-mode t)

;; Yasnippet for snippet support
(leaf yasnippet
  :ensure t
  :global-minor-mode yas-global-mode)

;; Miscellaneous helper functions
(defun toggle-linum-lines ()
  "Toggle display line number."
  (interactive)
  (display-line-numbers-mode (if display-line-numbers-mode -1 1)))

(defun toggle-window-split ()
  "Toggle window split between horizontal and vertical."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd
              (not (and (<= (car this-win-edges)
                            (car next-win-edges))
                        (<= (cadr this-win-edges)
                            (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Remove duplicate entries from kill-ring
(defun my/no-kill-new-duplicate (yank)
  (setq kill-ring (delete yank kill-ring)))
(advice-add 'kill-new :before #'my/no-kill-new-duplicate)

;; Clean up whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

