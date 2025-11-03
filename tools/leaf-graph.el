;;; leaf-graph.el --- Generate dependency graph from leaf/require (safe/org lowercase, cycles+labels default on) -*- lexical-binding: t; -*-
;;
;; Author: YAMASHITA, Takao
;; Version: 0.9
;; License: GPLv3
;; Category: tools
;;
;;; Commentary:
;; This script scans .el files recursively under a directory (default: lisp/)
;; and extracts dependency relationships from both (leaf ...) and (require '...)
;; forms. It outputs an Org-mode document containing a lowercase mermaid graph.
;;
;; Features:
;;   - Supports :after relationships in (leaf ...)
;;   - Handles (require ...) and (eval-when-compile (require ...))
;;   - Skips dotted pairs safely (no listp errors)
;;   - Org syntax is lowercase (# +begin_src / # +end_src)
;;   - Detects circular dependencies and labels nodes with file names by default.
;;
;; Usage (batch example):
;;   emacs -Q --batch \
;;     --load path/to/leaf-graph.el \
;;     --eval '(leaf-graph-generate "lisp")' \
;;     --eval '(with-current-buffer "*leaf-graph*" (write-file "LEAF-GRAPH.org"))'
;;
;; You can disable features via:
;;   --eval '(setq leaf-graph-detect-cycles nil leaf-graph-show-files nil)'
;;
;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------
;; Default switches (can be overridden via --eval)
;; ---------------------------------------------------------------------

(defvar leaf-graph-detect-cycles t
  "If non-nil, detect and print circular dependencies.
Enabled by default. Disable with --eval '(setq leaf-graph-detect-cycles nil)'.")

(defvar leaf-graph-show-files t
  "If non-nil, label each node with its source file name.
Enabled by default. Disable with --eval '(setq leaf-graph-show-files nil)'.")

;; ---------------------------------------------------------------------
;; Utility functions
;; ---------------------------------------------------------------------

(defun leaf-graph--safe-read-all (buffer)
  "Safely read all top-level forms from BUFFER."
  (goto-char (point-min))
  (let ((forms '()))
    (condition-case _err
        (while t
          (push (read buffer) forms))
      (end-of-file nil)
      (error (message "[leaf-graph] read error ignored")))
    (nreverse forms)))

(defun leaf-graph--directory-el-files (dir)
  "Return list of all .el files under DIR recursively."
  (when (file-directory-p dir)
    (directory-files-recursively dir "\\.el$")))

(defun leaf-graph--topdir (file root)
  "Return the top-level subdirectory name of FILE under ROOT."
  (let ((rel (file-relative-name file root)))
    (car (split-string rel "/" t))))

(defun leaf-graph--sanitize (sym)
  "Return a mermaid-safe identifier for SYM."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (symbol-name sym)))

(defun leaf-graph--node-label (sym files)
  "Return formatted label for SYM, with file info if enabled."
  (let ((file (and leaf-graph-show-files (gethash sym files))))
    (if file
        (format "%s[%s\\n(%s)]"
                (leaf-graph--sanitize sym)
                (symbol-name sym)
                (file-name-nondirectory file))
      (format "%s[%s]" (leaf-graph--sanitize sym) (symbol-name sym)))))

;; ---------------------------------------------------------------------
;; Core parsers
;; ---------------------------------------------------------------------

(defun leaf-graph--parse-require-forms (forms)
  "Safely extract all (require 'pkg) symbols from FORMS."
  (cond
   ((atom forms) nil)
   ((and (eq (car-safe forms) 'require)
         (symbolp (cadr forms)))
    (list (cadr forms)))
   ((and (eq (car-safe forms) 'eval-when-compile)
         (listp (cdr forms)))
    (leaf-graph--parse-require-forms (cdr forms)))
   ((and (listp forms) (listp (cdr forms)))
    (apply #'append (mapcar #'leaf-graph--parse-require-forms forms)))
   ((consp forms) nil)
   (t nil)))

(defun leaf-graph--parse-leaf-form (form)
  "If FORM is (leaf pkg ...), return (pkg . deps) from :after."
  (when (and (listp form)
             (eq (car-safe form) 'leaf)
             (symbolp (cadr form)))
    (let ((name (cadr form))
          (deps '()))
      (dolist (sub (cddr form))
        (when (and (listp sub) (eq (car-safe sub) ':after))
          (setq deps (append deps (cdr sub)))))
      (when deps (cons name deps)))))

;; ---------------------------------------------------------------------
;; Scanning
;; ---------------------------------------------------------------------

(defun leaf-graph--scan-file (file)
  "Scan FILE for leaf and require dependencies."
  (let ((temp (generate-new-buffer " *leaf-graph-temp*" t)))
    (save-current-buffer
      (set-buffer temp)
      (unwind-protect
          (progn
            (insert-file-contents file)
            (let* ((forms (leaf-graph--safe-read-all (current-buffer)))
                   leaf-edges req-edges)
              (dolist (f forms)
                (let ((lr (leaf-graph--parse-leaf-form f)))
                  (when lr (push lr leaf-edges))))
              (let* ((feat (intern (file-name-base file)))
                     (deps (cl-remove-duplicates
                            (cl-mapcan #'leaf-graph--parse-require-forms forms)
                            :test #'eq)))
                (when deps (push (cons feat deps) req-edges)))
              (let ((table (make-hash-table :test #'eq)))
                (dolist (pair (append leaf-edges req-edges))
                  (let* ((k (car pair))
                         (v (cdr pair))
                         (old (gethash k table)))
                    (puthash k (cl-remove-duplicates (append old v) :test #'eq)
                             table)))
                (let (out)
                  (maphash (lambda (k v) (push (cons k v) out)) table)
                  out))))
        (when (buffer-name temp) (kill-buffer temp))))))

(defun leaf-graph--scan-directory (dir)
  "Scan all .el files under DIR; return plist (:alist :groups :files)."
  (let* ((files (leaf-graph--directory-el-files dir))
         (alist nil)
         (groups (make-hash-table :test #'eq))
         (files-ht (make-hash-table :test #'eq)))
    (dolist (file files)
      (let ((pairs (leaf-graph--scan-file file))
            (feat (intern (file-name-base file))))
        (puthash feat (intern (leaf-graph--topdir file dir)) groups)
        (puthash feat file files-ht)
        (setq alist (append pairs alist))))
    (let ((tbl (make-hash-table :test #'eq)))
      (dolist (cell alist)
        (let* ((k (car cell))
               (v (cdr cell))
               (old (gethash k tbl)))
          (puthash k (cl-remove-duplicates (append old v) :test #'eq) tbl)))
      (let (out)
        (maphash (lambda (k v) (push (cons k v) out)) tbl)
        (list :alist out :groups groups :files files-ht)))))

;; ---------------------------------------------------------------------
;; Cycle detection
;; ---------------------------------------------------------------------

(defun leaf-graph--detect-cycles (alist)
  "Detect circular dependencies in ALIST (alist of (pkg . deps)).
Return list of cycles as list of symbol lists."
  (let ((visited (make-hash-table :test #'eq))
        (cycles '()))
    (cl-labels
        ((dfs (node path)
              (cond
               ((member node path)
                (push (reverse
                       (cons node
                             (cl-subseq path 0 (cl-position node path))))
                      cycles))
               ((not (gethash node visited))
                (puthash node t visited)
                (dolist (dep (cdr (assoc node alist)))
                  (dfs dep (cons node path)))))))
      (dolist (cell alist)
        (dfs (car cell) '())))
    (delete-dups cycles)))

(defun leaf-graph--cycles-to-org (cycles)
  "Return Org-mode comment lines for dependency CYCLES."
  (mapconcat
   (lambda (cyc)
     (format "  %% Cycle detected: %s"
             (mapconcat #'symbol-name cyc " → ")))
   cycles "\n"))

;; ---------------------------------------------------------------------
;; Org output
;; ---------------------------------------------------------------------

(defun leaf-graph--to-org (alist groups files &optional group-by)
  "Convert ALIST to Org-mode Mermaid block.
If GROUP-BY is non-nil, group by top-level directory."
  (let ((lines '("#+begin_src mermaid" "graph TD")))
    (if group-by
        (let ((group-map (make-hash-table :test #'eq)))
          (dolist (pair alist)
            (let* ((src (car pair))
                   (deps (cdr pair))
                   (grp (gethash src groups)))
              (puthash grp (cons (cons src deps)
                                 (gethash grp group-map))
                       group-map)))
          (maphash
           (lambda (grp pairs)
             (push (format "  subgraph %s" grp) lines)
             (dolist (pair pairs)
               (let ((src (car pair)))
                 (dolist (d (cdr pair))
                   (push (format "    %s --> %s"
                                 (leaf-graph--node-label src files)
                                 (leaf-graph--node-label d files))
                         lines))))
             (push "  end" lines))
           group-map))
      (dolist (pair alist)
        (let ((src (car pair)))
          (dolist (d (cdr pair))
            (push (format "  %s --> %s"
                          (leaf-graph--node-label src files)
                          (leaf-graph--node-label d files))
                  lines))))))
    (when leaf-graph-detect-cycles
      (let ((cycles (leaf-graph--detect-cycles alist)))
        (when cycles
          (push (leaf-graph--cycles-to-org cycles) lines))))
    (push "#+end_src" lines)
    (string-join (nreverse lines) "\n"))

;; ---------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------

;;;###autoload
(defun leaf-graph-generate (&optional dir)
  "Generate an Org-format Mermaid dependency graph for DIR (default: lisp/).
With prefix argument, group nodes by directory.
By default, cycle detection and file labeling are enabled."
  (interactive "DDirectory (default: lisp/): ")
  (let* ((root (or dir (expand-file-name "lisp" default-directory)))
         (scan (leaf-graph--scan-directory root))
         (alist (plist-get scan :alist))
         (groups (plist-get scan :groups))
         (files (plist-get scan :files))
         (group-by current-prefix-arg)
         (org-block (leaf-graph--to-org alist groups files group-by)))
    (with-current-buffer (get-buffer-create "*leaf-graph*")
      (erase-buffer)
      (insert (format "#+title: leaf graph\n#+date: %s\n\n;; directory: %s\n\n%s"
                      (format-time-string "%Y-%m-%d %H:%M:%S") root org-block))
      (org-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'leaf-graph)
;;; leaf-graph.el ends here
