;;; config-keybindings.el --- Set keybindings in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

  (require 'general)
  (require 'leader-key-system)

(declare-function make-leader-menu "leader-key-system" ())
;; TODO: I want to add the ability to provide the initial body of the menu here.
;;       That involves passing a `general-definer' body which is &rest

(defcustom config:emacs-leader-key-menu-list '()
  "A list of keys to be used in the top-level leader-key menu.
The value is a list of key definitions of the form:
\\=(KEY NAME DISPLAY\\=) where DISPLAY is optional.

- KEY is the key that is to be added to the leader-menu.
- NAME is used to create the `general-definer' and `keymap'
- DISPLAY `which-key' description of the prefix.  If omitted, NAME is
transformed by capitalizing the first letter and adding an \\='s\\=' to the
end.  i.e. `error' becomes `Errors'"
  :type 'symbol
  :group 'config)

;;;###autoload
(defun leader-key-menu-initialize ()
  "Create the top-level leader-key menu."
  (dolist (lmenu config:emacs-leader-key-menu-list)
    (let ((key     (nth 0 lmenu))
          (name    (nth 1 lmenu))
          (display (nth 2 lmenu)))
      (eval `(make-leader-menu ,name ,key :display-name ,display)))))

(provide 'config-keybindings)

;;; config-keybindings.el ends here
