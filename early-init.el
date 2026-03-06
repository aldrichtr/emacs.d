;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; My libraries of Elisp used throughout init and operation of GNU Emacs.
(add-to-list 'load-path (file-truename (locate-user-emacs-file "user-lisp")))

(require 'os)             ; Operating System/Environment utilities
(require 'data-size)      ; Data size calculations, such as mb, gb, etc
(require 'config-options) ; A collection of the most commonly changed options

(require 'package)

;; Add path to termux executables when on android
(when (os-android-p)
  (let ((termux-bin (file-name-concat
                     "/data/data/com.termux/files"
                     "usr/bin")))
    (when (file-exists-p termux-bin)
      (setenv "PATH" (concat (getenv "PATH") ":" termux-bin))
      (setq exec-path (append exec-path (list termux-bin))))))

;;; Performance

(setopt inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message "Timothy R. Aldrich"
        frame-inhibit-implied-resize t)

;;; Garbage Collection

;; Temporarily bump the garbage collection value way up
;; https://emacsredux.com/blog/2025/03/28/speed-up-emacs-startup-by-tweaking-the-gc-settings/
(setopt gc-cons-threshold most-positive-fixnum)

;; then set sane values after startup
(defun set-emacs-performance ()
  "Set some performance settings."
  (setopt gc-cons-threshold       (mb 256)
          read-process-output-max (mb 64)))

(add-hook 'emacs-startup-hook #'set-emacs-performance)

;;; Native Compilation

(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (file-name-concat config:emacs-local-dir "eln-cache"))
  (setopt package-native-compile t
          native-comp-speed 1
          compilation-safety 1
          native-comp-debug 1 ; emit debug symbols
          native-comp-verbose 0
          native-comp-async-jobs-number 4
          native-comp-async-report-warnings-errors nil
          native-comp-jit-compilation t))

;;; Package manager

;; Setting the path to the program is a bit fragile, but the alternative is to
;; trust that the path doesn't have the msys version before this one
(when (os-windows-p)
  (let ((gnupg-prog (file-truename
                     "c:/program files (x86)/GnuPG/bin/gpg.exe")))
    (when (file-exists-p gnupg-prog)
      (setopt epg-gpg-program gnupg-prog))))

;; as a fail-safe if the package manager still has an issue with the signatures,
;; you can uncomment this to get things working, but don't let it be permanant!!
;; (setopt package-check-signature nil)

;;;; Paths
(setopt
 package-user-dir config:emacs-package-dir
 package-gnupghome-dir (expand-file-name
                        (file-name-concat config:emacs-package-dir "gnupg")))

;;;; Archives

(setopt
 package-archives '(("melpa"  . "https://melpa.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                    ("gnu"    . "https://elpa.gnu.org/packages/"))

 package-archive-priorities
 '(("melpa" . 60) ("nongnu" . 40) ("gnu" . 20)))
;; Archives:1 ends here

;;;; Menu

(setopt
 package-menu-async t
 package-menu-hide-low-priority 'archive
 package-menu-use-current-if-no-marks t)

;;;; Package list format

(setopt
 package-name-column-width 30
 package-version-column-width 14
 package-status-column-width 12
 package-archive-column-width 8
 package-hidden-regexps '("available"))

;;;; Installation

(setopt
 package-install-upgrade-built-in nil)
;; Installation:1 ends here

;;;; Startup
(setopt
 package-quickstart nil
 package-enable-at-startup t)

(unless package-quickstart
  (package-initialize))

;;; Use-Package

;;;; Install if not already

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setopt package-install-upgrade-built-in t)
;;;; Load

(eval-when-compile
  (progn
    ;; `use-package' "extras"
    (require 'use-package-diminish)
    (require 'use-package-delight)
    (require 'use-package-ensure)
    (require 'use-package-ensure-system-package)

    ;; Show `use-package' progress when `--debug-init' is specified
    (let ((verbose (or nil init-file-debug)))
      (setq use-package-verbose verbose
            use-package-expand-minimally (not verbose)
            use-package-compute-statistics verbose
            debug-on-error verbose
            debug-on-message "buffer-local while locally let-bound"
            debug-on-quit verbose))

    (setopt use-package-compute-statistics t)
    ;; we almost always want to install the package if it is missing
    ;; use :ensure nil to avoid it
    (setopt use-package-always-ensure t)))

;;; early-init.el ends here
