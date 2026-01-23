;;; os.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;;;; Operating System Predicates

(defun os-windows-p ()
  "Return non-nil if OS is MS Windows."
  (eq system-type 'windows-nt))

(defun os-android-p ()
  "Return non-nil if OS is Android."
  (eq system-type 'android))

(defun os-linux-p ()
  "Return non-nil if OS is linux."
  (eq system-type 'gnu/linux))

(defun os-mac-p ()
  "Return non-nil if OS is mac."
  (eq system-type 'darwin))


;; TODO: Add functions that return architecture, etc. from `system-configuration'
;; TODO: Add advice to the xdg commands for windows

(provide 'os)


;;; os.el ends here