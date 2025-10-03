;;; core/utils-lint.el --- Org lint helpers -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025
;; License: GNU GPL v3 or later

;;; Code:
(require 'org)
(require 'subr-x)
(require 'core/policies)

(defun my/org--collect-src-blocks ()
  "Return a list of (BEG LINE LANG HEADER TANGLE)."
  (save-excursion
    (goto-char (point-min))
    (let (acc)
      (while (re-search-forward "^#\\+begin_src\\s-+\\([^ \n]+\\)\\(.*\\)$" nil t)
        (let* ((lang (string-trim (match-string 1)))
               (header (string-trim (match-string 2)))
               (beg (match-beginning 0))
               (line (line-number-at-pos beg))
               (tangle (when (string-match ":tangle\\s-+\\(?:\"\\([^\"]+\\)\"\\|\\([^\"[:space:]]+\\)\\)" header)
                         (or (match-string 1 header) (match-string 2 header)))))
          (push (list beg line lang header tangle) acc)))
      (nreverse acc))))

(defun my/org--topdir (path)
  "Return top-level directory of PATH or nil."
  (when (and path (string-match "\\`\\([^/]+\\)/" path))
    (match-string 1 path)))

(defun my/org-lint-current-buffer ()
  "Lint current Org buffer and show an Org table report."
  (interactive)
  (let* ((rows (my/org--collect-src-blocks))
         (table (list (list "line" "lang" "tangle" "issue")))
         (warn-dirs my/policy-warn-on-nonstandard-dirs))
    (dolist (r rows)
      (pcase-let ((`(,beg ,line ,lang ,header ,tangle) r))
        (cond
         ((null tangle)
          (push (list (number-to-string line) lang "" "missing :tangle or use :tangle no") table))
         (t
          (let ((top (if (string-match "/" tangle)
                         (my/org--topdir tangle)
                       "TOP")))
            (cond
             ;; Top-level file but not allowed
             ((and (string= top "TOP")
                   (not (member tangle my/allowed-top-level-files)))
              (push (list (number-to-string line) lang tangle "top-level file not allowed") table))
             ;; Unknown top-level dir
             ((and warn-dirs (not (string= top "TOP"))
                   (not (member top my/allowed-module-dirs)))
              (push (list (number-to-string line) lang tangle (format "unknown top-level dir '%s'" top)) table))))))))
    (with-current-buffer (get-buffer-create "*Org Inconsistencies*")
      (erase-buffer)
      (org-mode)
      (insert "* Org Lint Report\n\n")
      (insert (orgtbl-to-orgtbl table nil))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun my/org-fix-top-level-here (&optional target)
  "If point is at a `#+begin_src` without allowed top-level :tangle,
rewrite :tangle to TARGET directory interactively."
  (interactive)
  (save-excursion
    (org-beginning-of-line)
    (when (looking-at "^#\\+begin_src\\s-+\\([^ \n]+\\)\\(.*\\)$")
      (let* ((header (string-trim (match-string 2)))
             (tangle (when (string-match ":tangle\\s-+\\(?:\"\\([^\"]+\\)\"\\|\\([^\"[:space:]]+\\)\\)" header)
                       (or (match-string 1 header) (match-string 2 header)))))
        (when (and tangle (not (string-match "/" tangle))
                   (not (member tangle my/allowed-top-level-files)))
          (let* ((dir (or target (completing-read "Move to dir: " my/allowed-module-dirs nil t)))
                 (new (concat dir "/" tangle)))
            (when (y-or-n-p (format "Rewrite :tangle to %s ? " new))
              (let* ((beg (line-beginning-position))
                     (end (line-end-position))
                     (newline (replace-regexp-in-string
                               (regexp-quote tangle) new header t t)))
                (delete-region beg end)
                (insert (format "#+begin_src %s %s" (match-string 1) newline))
                (message "Rewrote to %s" new)))))))))

(provide 'core/utils-lint)
;;; core/utils-lint.el ends here
