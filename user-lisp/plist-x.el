;;; plist-x.el --- Missing plist functions -*- lexical-binding: t -*-

;;; Commentary:
;; Additional functionality for Emacs Property-Lists

;;; Code:

(defun plist-remove (keys plist)
  "Remove list of KEYS from PLIST."
  (let (result)
    (while plist
      (unless (memq (car plist) keys)
        (push (car plist) result)
        (push (cadr plist) result))
      (setq plist (cddr plist)))
    (nreverse result)))

(provide 'plist-x)

;;; plist-x.el ends here