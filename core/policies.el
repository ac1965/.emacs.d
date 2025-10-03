;;; core/policies.el --- Policy constants -*- lexical-binding: t; -*-

;; Copyright (c) 2021-2025
;; License: GNU GPL v3 or later

;;; Code:

(defgroup my/policy nil
  "Project-wide policies."
  :group 'convenience)

(defcustom my/policy-warn-on-nonstandard-dirs t
  "Warn when :tangle path uses nonstandard top-level dirs."
  :type 'boolean)

(defcustom my/policy-warn-on-builtin-straight t
  "Warn when built-in/bundled packages are declared with :straight t."
  :type 'boolean)

(defconst my/allowed-top-level-files
  '("early-init.el" "init.el")
  "Only these are allowed at the repo top-level.")

(defconst my/allowed-module-dirs
  '("core" "ui" "completion" "org" "dev" "vcs" "utils")
  "Allowed top-level module directories for :tangle paths.")

;; Emacs 30 core/bundled (curated; extend as needed)
(defconst my/builtin-or-bundled-packages
  (let ((lst '(
    abbrev ange-ftp ansi-color ansi-osc auth-source battery calc calendar char-fold cl-lib
    comint comp compile cus-edit dabbrev delsel dired dired-aux dired-x ediff eldoc
    elisp-mode elec-pair emacs epg epg-config erc eshell epa epa-file face-remap
    ffap files finder flymake frame gdb-mi help hl-line ibuffer ido image image-dired
    imenu info isearch jit-lock json kmacro lisp-mode macroexp map minibuffer mule
    newcomment nxml-mode org package pixel-scroll-precision prog-mode project recentf
    replace ring savehist saveplace scroll-bar seq server simple so-long subr-x
    tab-bar tab-line term text-mode time timer tooltip tramp treesit type-break url
    user unix vc vc-git whitespace winner woman xref
  )))
    (mapcar #'symbol-name lst))
  "Built-in or bundled packages regarded as \"do not :straight t\" by policy.")

(provide 'core/policies)
;;; core/policies.el ends here
