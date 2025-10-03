;;; completion/completion-fixes.el --- Enable key minor modes -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025
;; License: GNU GPL v3 or later

;;; Code:
(eval-when-compile (require 'leaf))

(leaf prescient
  :straight t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

(leaf saveplace
  :straight nil
  :global-minor-mode saveplace-mode)

(provide 'completion/completion-fixes)
;;; completion/completion-fixes.el ends here
