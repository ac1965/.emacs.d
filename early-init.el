;; early-init.el --- Early Init File for >= Emacs 27.

;; Copyright (c) 2021-2024 YAMASHITA Takao <tjy1965@gmail.com>
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
;; (setq gc-cons-threshold 20000000)
(setq gc-cons-threshold most-positive-fixnum)

;; Do not initialise the package manager.  This is done in `init.el'.
(setq package-enable-at-startup nil)

;; Do not set SRF
(setq site-run-file nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Found on `https://github.com/miklos1/dotemacs/blob/master/early-init.el'.
;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; GUI elements
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)
(menu-bar-mode 1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 1) default-frame-alist)
  (push '(fullscreen . maximized) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; This code configures the native compiler and is in preparation for 28.1.
;; - Move eln files to a cache dir
;; - Don't bombard the user with warnings
;; - Compile packages on install, not at runtime
(unless (version-list-<
         (version-to-list emacs-version)
         '(28 0 1 0))
  (when (featurep 'native-compile)
    (when (boundp 'native-comp-eln-load-path)
      (startup-redirect-eln-cache (expand-file-name "~/.cache/emacs/")))
    (add-to-list 'native-comp-eln-load-path (expand-file-name "~/.cache/emacs/"))
    (setq native-comp-async-report-warnings-errors 'silent
          native-comp-deferred-compilation t)
    (with-eval-after-load 'comp
      (setopt native-comp-async-jobs-number 8
              native-comp-speed 1
              native-comp-always-compile t))))

;; early-init.el ends here
