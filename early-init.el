;; early-init.el --- Early Init File for >= Emacs 27.

;; Copyright (c) 2021-2024 YAMASHITA Takao <ac1965@ty07.net>
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

(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/14:/usr/local/opt/libgccjit/lib/gcc/14:/usr/local/opt/gcc/lib/gcc/14/gcc/x86_64-apple-darwin23/14")

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(defvar default-file-name-handler-alist file-name-handler-alist)

(defconst my:d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")

(defconst my:d:cache (concat my:d ".cache/"))
(unless (file-exists-p my:d:cache)
  (make-directory my:d:cache))

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "eln-cache/" my:d:cache))))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(debug-on-error nil)
 '(package-enable-at-startup nil)
 '(byte-compile-warnings '(not cl-functions obsolete))
 '(file-name-handler-alist nil)
 '(init-file-debug nil)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)
 '(initial-scratch-message nil)
 '(initial-frame-alist
   '((left-fringe . 0)
     (right-fringe   . 0)
     (internal-border-width . 8)
     (tool-bar-lines . 0)
     (fullscreen . maximized)))
 '(menu-bar-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(initial-buffer-choice t)
 '(initial-major-mode 'fundamental-mode)
 '(frame-inhibit-implied-resize t)
 '(frame-title-format '("%b"))
 '(frame-resize-pixelwise t)
 '(cursor-in-non-selected-windows nil)
 '(font-lock-maximum-decoration nil)
 '(font-lock-maximum-size nil)
 '(x-underline-at-descent-line t)
 '(window-divider-default-right-width 16)
 '(window-divider-default-places 'right-only)
 '(enable-recursive-minibuffers t)
 '(ring-bell-function 'ignore)
 '(global-prettify-symbols-mode t)
 '(text-quoting-style 'straight)
 '(use-short-answers t)
 '(create-lockfiles nil)
 '(tab-width 4)
 '(history-length 1000)
 '(history-delete-duplicates t)
 '(scroll-preserve-screen-position t)
 '(scroll-conservatively 100)
 '(indent-tabs-mode nil)
 '(truncate-lines t)
 '(tab-bar-mode t)
 '(gc-cons-threshold most-positive-fixnum)
 '(gc-cons-percentage 1))

(provide 'early-init)
;;; early-init.el ends here
