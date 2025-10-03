;;; core/builtin-guard.el --- Guard :straight on built-ins -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025
;; License: GNU GPL v3 or later

;;; Code:
(require 'core/policies)

(defun my/guard--file-violations (file)
  "Return a list of strings describing violations in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t)
          (lst '()))
      (goto-char (point-min))
      ;; naive scan: (leaf <pkg> ... :straight t)
      (while (re-search-forward "(leaf\\s-+\\([^\\s)]+\\)\\([\\s\\S]*?\\))" nil t)
        (let* ((pkg (match-string 1))
               (body (match-string 2)))
          (when (and (member (downcase pkg) (mapcar #'downcase my/builtin-or-bundled-packages))
                     (string-match-p ":straight\\s-+t\\b" body))
            (push (format "Built-in '%s' declared with :straight t" pkg) lst))))
      (nreverse lst))))

(defun my/guard-scan-lisp-directory ()
  "Scan `lisp/' dir for :straight t on built-ins and warn."
  (interactive)
  (when my/policy-warn-on-builtin-straight
    (let* ((root (or user-emacs-directory default-directory))
           (lisp (expand-file-name "lisp" root)))
      (when (file-directory-p lisp)
        (dolist (file (directory-files-recursively lisp "\\.el\\'"))
          (dolist (msg (my/guard--file-violations file))
            (display-warning 'builtin-guard (format "%s: %s" file msg))))))))

(add-hook 'emacs-startup-hook #'my/guard-scan-lisp-directory)

(provide 'core/builtin-guard)
;;; core/builtin-guard.el ends here
