;;; -- Emacs.d -*- lexical-binding: t; -*-

;; Copyright (C) 2024 YAMASHITA Takao

;; Author: YAMASHITA Takao <ac1965@ty07.net>
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

;;; Initialization
(defvar my:d (file-name-directory (file-chase-links load-file-name)) "The giant turtle on which the world rests.")
(defvar my:d:cache (concat my:d ".cache/") "Cache directory.")

(setq package-user-dir (concat my:d:cache "elpa")
      no-littering-etc-directory (concat my:d ".etc/")
      no-littering-var-directory (concat my:d ".var/"))

;;; Window Management
(defvar my/saved-window-config nil "Store window layout.")

;; Native Comp
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" my:d:cache)))
  (make-directory (car native-comp-eln-load-path) t)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "eln-cache/" my:d:cache))))

;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-variables
 '(byte-compile-warnings '(not cl-functions obsolete))
 '(create-lockfiles nil)
 '(cursor-in-non-selected-windows nil)
 '(debug-on-error nil)
 '(enable-recursive-minibuffers t)
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(file-name-handler-alist nil t)
 '(font-lock-maximum-decoration nil)
 '(font-lock-maximum-size nil)
 '(frame-inhibit-implied-resize t)
 '(frame-resize-pixelwise t)
 '(frame-title-format '("%b") t)
 '(gc-cons-percentage 0.2)
 '(gc-cons-threshold (* 512 1024 1024))
 '(read-process-output-max (* 1024 1024))
 '(large-file-warning-threshold 100000000)
 '(global-prettify-symbols-mode t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(init-file-debug nil t)
 '(initial-buffer-choice t)
 '(initial-major-mode 'emacs-lisp-mode)
 '(initial-scratch-message nil)
 '(menu-bar-mode t)
 '(package-enable-at-startup nil)
 '(package-selected-packages nil)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position t)
 '(scroll-margin 8)
 '(tab-bar-mode t)
 '(tab-width 4)
 '(text-quoting-style 'straight)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(use-short-answers t)
 '(window-divider-default-places 'right-only)
 '(window-divider-default-right-width 16)
 '(x-underline-at-descent-line t))

;; Ensure package management and load org-babel
(require 'package)
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

;; Load settings from README.org using org-babel
(require 'org)
(setq init-org-file (expand-file-name "README.org" my:d))
(when (file-exists-p init-org-file)
  (condition-case err
      (org-babel-load-file init-org-file)
    (error (message "Error loading org file: %s" err))))

(provide 'init)
;;; init.el ends here
