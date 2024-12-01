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

;; (require 'profiler)
;; (profiler-start 'cpu)

(setq init-org-file (expand-file-name "README.org" my:d))
(when (file-exists-p init-org-file)
  (org-babel-load-file init-org-file))

(provide 'init)
;;; init.el ends here
