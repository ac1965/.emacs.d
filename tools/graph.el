;;; tools/graph.el -*- lexical-binding: t; -*-

(defvar graph/require-edges (make-hash-table :test 'equal))
(defvar graph/require-seen (make-hash-table :test 'equal))

(defun graph--record (from to)
  (when (and (symbolp from) (symbolp to))
    (let* ((k (format "%s->%s" from to)))
      (puthash k t graph/require-edges))))

;; Advice require to record edges
(defun graph/require-advice (orig feature &optional filename noerror)
  (let* ((from (or load-file-name (buffer-file-name)))
         (from-sym (if from (intern (file-name-sans-extension (file-name-nondirectory from))) 'init))
         (to-sym (if (symbolp feature) feature (intern (format "%s" feature)))))
    (graph--record from-sym to-sym)
    (funcall orig feature filename noerror)))

(defun graph/edge-list ()
  (sort (hash-table-keys graph/require-edges) #'string<))

(defun graph/export-dot ()
  "Render recorded require edges as Graphviz DOT in *Require Graph (DOT)*."
  (interactive)
  (let ((buf (get-buffer-create "*Require Graph (DOT)*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "digraph G {\n  rankdir=LR;\n  node [shape=box, fontsize=10];\n")
      (dolist (e (graph/edge-list))
        (string-match "^\\([^->]+\\)->\\(.+\\)$" e)
        (insert (format "  \"%s\" -> \"%s\";\n" (match-string 1 e) (match-string 2 e))))
      (insert "}\n"))
    (pop-to-buffer buf)))

(defun graph/export-mermaid ()
  "Render recorded require edges as Mermaid in *Require Graph (Mermaid)*."
  (interactive)
  (let ((buf (get-buffer-create "*Require Graph (Mermaid)*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "```mermaid\nflowchart LR\n")
      (dolist (e (graph/edge-list))
        (string-match "^\\([^->]+\\)->\\(.+\\)$" e)
        (insert (format "  %s --> %s\n" (replace-regexp-in-string "[./-]" "_" (match-string 1 e))
                        (replace-regexp-in-string "[./-]" "_" (match-string 2 e)))))
      (insert "```\n"))
    (pop-to-buffer buf)))

;;; Enable/disable capture
(defun graph/enable-require-capture ()
  (interactive)
  (advice-add 'require :around #'graph/require-advice)
  (clrhash graph/require-edges)
  (message "[graph] require capture enabled"))

(defun graph/disable-require-capture ()
  (interactive)
  (advice-remove 'require #'graph/require-advice)
  (message "[graph] require capture disabled"))

(provide 'tools/graph)
;;; tools/graph.el ends here
