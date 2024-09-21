;;; -- Emacs.d -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2024 YAMASHITA Takao

;; Author: YAMASHITA Takao <ac1965@ty07.net>
;; Keywords: emacs.d
;; $Lastupdate: 2024/09/21 18:55:12 $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; This is the initialisation file for GNU/Emacs.  At the end of this
;; file, it will call the proper configuration file written in
;; `org-mode'.  Visit that document to see the proper / full
;; documentation, code-snippets, and the idea behind it.
;;
;; The sole purpose of this file is to initialise loading the proper
;; configuration file.  Everything else is out-of-scope.
;;
;; Although you will find some code that SHOULD belong in the
;; `README.org', but I have put it here since it is important to be set
;; at the beginning.

;;; License: GPLv3

;;; Code:

;; (require 'profiler)
;; (profiler-start 'cpu)

(setq debug-on-error nil)
(setq gc-cons-threshold most-positive-fixnum)

(setq byte-compile-warnings '(not cl-functions obsolete))
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(eval-and-compile
  (setq nsm-settings-file (concat my:d:cache "network-security.data"))
  (setq network-security-level 'high)

  (setq straight-repository-branch "develop"
	    straight-base-dir my:d:cache
	    straight-check-for-modifications '(check-on-save-find-when-checking))

  (defvar bootstrap-version)
  (let ((bootstrap-file
	     (expand-file-name "straight/repos/straight.el/bootstrap.el" my:d:cache))
	    (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	    (goto-char (point-max))
	    (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (require 'straight-x)
  ;; Bootstrap ends here
  (setq vc-follow-symlinks t)

  (eval-and-compile
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (straight-use-package 'hydra)
    (straight-use-package 'org)
    (leaf-keywords-init))

  (leaf leaf
    :require t
    :init
    (leaf leaf-convert :straight t)
    (leaf leaf-tree :straight t)
    (leaf transient-dwim
      :straight t
      :bind (("M-=" . transient-dwim-dispatch))))

  (leaf dash
    :straight t
    :leaf-defer t)

  (leaf diminish
    :straight t
    :require t)

  (leaf async
    :straight t
    :leaf-defer nil
    :setq (async-bytecomp-package-mode . t))

  ;; macOS
  (leaf exec-path-from-shell
    :straight t
    :if (memq window-system '(mac ns))
    :commands (exec-path-from-shell-getenvs
               exec-path-from-shell-setenv)
    :init
    (exec-path-from-shell-initialize)
    (when (member system-type '(darwin))
      (let ((gls "/usr/local/bin/gls")) ;; brew install coreutils
        (if (file-exists-p gls) (setq insert-directory-program gls)))))

  ;; no-littering
  (setq no-littering-etc-directory
        (expand-file-name ".config/" my:d))
  (setq no-littering-var-directory
        (expand-file-name ".data/" my:d))
  (leaf no-littering
    :straight t
    :require t)

  (setq custom-file (expand-file-name "custom.el" my:d))
  (load custom-file 'noerror)

  (setq startup-file (expand-file-name "README.org" my:d))
  (when (file-exists-p startup-file)
    (org-babel-load-file startup-file)))

(provide 'init)
;;; init.el ends here
