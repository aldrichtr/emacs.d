;;; config-keybindings.el --- Set keybindings in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

  (require 'general)
  (require 'leader-key-system)

(declare-function make-leader-menu "leader-key-system" ())
;; TODO: I want to add the ability to provide the initial body of the menu here.
;;       That involves passing a `general-definer' body which is &rest

(defcustom config:emacs-leader-key-menu-list
  '(;; symbols
    (">" "shell")
    ("!" "error")
    ("/" "search"  "Search")
    ;; lowercase
    ("a" "app"     "Applications")
    ("b" "buffer")
    ("c" "compile" "Compilation")
    ;;("d" )
    ;;("e" )
    ("g" "git"     "Version Control")
    ("h" "help"    "Help")
    ("p" "project")
    ("t" "toggle")
    ("v" "view")
    ("w" "window")
    ("x" "text"    "Text")
    ;; uppercase
    ("F" "frame")
    ("P" "package")
    ("Z" "quit"    "Quit"))
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
;;; Symbols
;;;; >
  (make-leader-menu "shell" ">")
;;;; !
  (make-leader-menu "error" "!")
;;;; /
  (make-leader-menu "search" "/" :display-name "Search")
;;; Uppercase
;;;; F
  (make-leader-menu "frame"   "F")
;;;; P
  (make-leader-menu "package" "P")
;;;; Z
  (make-leader-menu "quit"    "Z" :display-name "Quit")
;;; Lowercase
;;;; a
  (make-leader-menu "app"     "a" :display-name "Applications")
;;;; b
  (make-leader-menu "buffer"  "b")
;;;; c
  (make-leader-menu "compile" "c" :display-name "Compilation")
;;;; d
  (make-leader-menu "debug"   "d" :display-name "Debugging")
;;;; e
  ;; (make-leader-menu "" "e")
;;;; f
  (make-leader-menu "file" "f"
    "."   '("Find this file" . ffap)
    "<"   '("Recent files"   . recentf-open)
    "d"   '("Dired"          . dirvish)
    "D"   '("Delete file"    . delete-file-and-buffer)
    "f"   '("Find file"      . find-file)
    "M-r" '("Rename file"    . rename-visited-file)
    "s"   '("save"           . evil-save)
    "S"   '("Save all"       . evil-write-all)
    "w"   '("Write"          . evil-write))

  (add-leader-keys "Open" "o"
    :parent leader-file-menu
    "o" '("Other window" . find-file-other-window)
    "w" '("Other window" . find-file-other-window)
    "f" '("Other frame"  . find-file-other-frame)
    "t" '("Other tab"    . find-file-other-tab))

  (add-leader-keys "Recover" "R"
    :parent leader-file-menu
    "f" '("Current file" . recover-this-file)
    "o" '("Other file"   . recover-file)
    "s" '("Session"      . recover-session))

  (add-leader-keys "Emacs" "e"
    :parent leader-file-menu
    "i" '("Visit init file"   . visit-emacs-init)
    "e" '("Visit early init"  . visit-emacs-early-init)
    "c" '("Visit custom file" . visit-emacs-custom-file)
    "d" '("Config dir"        . (lambda () (consult-find config:emacs-config-dir)))
    "s" '("scratch buffer"    . scratch-buffer))
;;;; g
  (make-leader-menu "git" "g" :display-name "Version Control")
;;;; h
  (make-leader-menu "help" "h" :display-name "Help")
;;;; p
  (make-leader-menu "project" "p")
;;;; t
  (make-leader-menu "toggle" "t"
    "l" '("highlight line"  . hl-line-mode)
    "m" '("toggle menu-bar" . menu-bar-mode)
    "t" '("Toggle tool-bar" . tool-bar-mode))

  (add-leader-keys "Line numbers" "n"
    :parent leader-toggle-menu
    "r" '("relative" . menu-bar--display-line-numbers-mode-relative)
    "a" '("absolute" . menu-bar--display-line-numbers-mode-absolute)
    "v" '("visual"   . menu-bar--display-line-numbers-mode-visual)
    "q" '("off"      . menu-bar--display-line-numbers-mode-none))
;;;; v
  (make-leader-menu "view" "v"))


(provide 'config-keybindings)

;;; config-keybindings.el ends here
