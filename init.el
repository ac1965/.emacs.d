;;; init.el --- Emacs.d -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  YAMASHITA Takao

;; Author: YAMASHITA Takao <tjy1965@gmail.com>
;; Version: 1.5
;; Keywords: emacs.d

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

;;; License: GPLv3

;;; Code:

(setq debug-on-error nil)

(let ((minver 26)
      (recver 27))
  (if (< emacs-major-version minver)
      (error "Your Emacs is too old -- this config requires v%s or higher"
             minver)
    (when (< emacs-major-version recver)
      (format-message
       (concat "You are probably fine with Emacs v%s for this init-file, but "
               "I cannot guarantee it. Recommended version of Emacs is v%s")
       minver recver))))

(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")
(defconst my:d:cache (concat emacs-d ".cache/"))

(defvar cfg--file-name-handler-alist file-name-handler-alist)

(eval-when-compile (require 'cl-lib nil t))
(setq byte-compile-warnings '(not cl-functions obsolete))

(setq gc-cons-threshold most-positive-fixnum)

(unless (file-exists-p my:d:cache)
  (make-directory my:d:cache))

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; straight installation
(setq nsm-settings-file (concat my:d:cache "network-security.data"))
(setq network-security-level 'high)

(setq straight-repository-branch "develop"
      straight-base-dir my:d:cache
      straight-check-for-modifications '(check-on-save-find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" my:d:cache))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight-x)

(straight-use-package 'async)
(require 'async)
(setq async-bytecomp-package-mode t)

(straight-use-package 'gcmh)
(require 'gcmh)

(setq gcmh-low-cons-threshold 300000000
      read-process-output-max (* 1024 1024))
(gcmh-mode 1)

(straight-use-package 'literate-elisp)
(require 'literate-elisp)

(when (file-exists-p (expand-file-name "README.org" emacs-d))
  (literate-elisp-load (expand-file-name "README.org" emacs-d)))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist cfg--file-name-handler-alist)))


(provide 'init)
;;; init.el ends here
