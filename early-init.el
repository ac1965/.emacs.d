;; early-init.el --- Early Init File for >= Emacs 27.

;; Copyright (c) 2021 YAMASHITA Takao <tjy1965@gmail.com>
;;
;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Define GC
(setq gc-cons-threshold 100000000)

;; Do not initialise the package manager.  This is done in `init.el'.
(setq package-enable-at-startup nil)

;; Do not set FNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Do not set SRF
(setq site-run-file nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; GUI elements
(menu-bar-mode 1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 1) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; early-init.el ends here
