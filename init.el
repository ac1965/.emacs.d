;;; init.el --- Emacs.d -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2020  YAMASHITA Takao

;; Author: YAMASHITA Takao <tjy1965@gmail.com>
;; Version: 1.4
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


(eval-when-compile (require 'cl-lib nil t))
(setq debug-on-error nil)

(defvar emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(defvar my:d:cache (concat emacs-d "cache/"))
(defvar my:d:config (concat emacs-d "config/"))

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

(setq gc-cons-threshold (* 8 1024 1024)
      gc-cons-percentage 0.6
      garbage-collection-messages nil
      file-name-handler-alist nil)

(unless (file-exists-p my:d:cache)
  (make-directory my:d:cache))

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(when (version<= "9.2" (org-version))
    (require 'org-tempo))
(require 'org-install)
(when (file-exists-p (expand-file-name "init.org" my:d:config))
   (org-babel-load-file (expand-file-name "init.org" my:d:config)))


;;; init.el ends here
