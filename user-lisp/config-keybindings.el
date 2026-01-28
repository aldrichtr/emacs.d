;;; config-keybindings.el --- Set keybindings in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(dolist
    ;; Leader menu keybindings
    ;; Each item in the list is an item in the leader menu
    ;; in the form of (KEY NAME DISPLAY-NAME)
    ;; if DISPLAY-NAME is ommitted
    (lmenu
     '(("a" "app"     "Applications")
       ("b" "buffer"  "Buffers")
       ("c" "compile" "Compilation")
       ;;("d" )
       ;;("e" )
       ("f" "file")
       ("F" "frame")
       ("g" "git"     "Version Control")
       ("p" "project")
       ("P" "package")
       ("v" "View")
       ("w" "window")
       ("x" "Text")
       ;; symbols
       (">" "Shell")
       ))
  (let* ((key     (nth 0 lmenu))
         (name    (nth 1 lmenu))
         ;; If the display name is omitted, capitalize and pluralize
         ;; i.e.  `buffer' => `Buffers'
         (display (or (nth 2 lmenu) (concat (capitalize name) "s"))
    (eval `(make-leader-menu ,name ,key :display-name ,display))))


(provide 'config-keybindings)

;;; config-keybindings.el ends here
