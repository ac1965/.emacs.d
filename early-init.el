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

(setenv "LIBRARY_PATH" (string-join
                        '("/usr/local/opt/gcc/lib/gcc/14"
                          "/usr/local/opt/libgccjit/lib/gcc/14"
                          "/usr/local/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/13")
                        ":"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-hook 'focus-out-hook #'garbage-collect)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

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
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-speed 2
        comp-deferred-compilation t
        native-comp-deferred-compilation-deny-list '("/intero/")))

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" my:d:cache)))
  (make-directory (car native-comp-eln-load-path) t)
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
 '(initial-frame-alist
   '((left-fringe . 0) (right-fringe . 0) (tool-bar-lines . 0)
     (fullscreen . maximized)))
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


(provide 'early-init)
;;; early-init.el ends here
