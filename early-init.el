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
                        '("/usr/local/opt/gcc/lib/gcc/current"
                          "/usr/local/opt/libgccjit/lib/gcc/current"
                          "/usr/local/opt/gcc/lib/gcc/current/gcc/x86_64-apple-darwin24/14")
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

(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-speed 2
        comp-deferred-compilation t
        native-comp-deferred-compilation-deny-list '("/intero/")))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

(provide 'early-init)
;;; early-init.el ends here
