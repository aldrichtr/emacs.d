;;; config-keybindings.el --- Set keybindings in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'general)
(eval-when-compile
  (require 'leader-key-system)
)


; (dolist (lmenu
;          '(;; symbols
;            (">" "shell")
;            ("!" "error")
;            ("/" "search"  "Search")
;            ;; lowercase
;            ("a" "app"     "Applications")
;            ("b" "buffer")
;            ("c" "compile" "Compilation")
;            ;;("d" )
;            ;;("e" )
;            ("f" "file")
;            ("g" "git"     "Version Control")
;            ("h" "help"    "Help")
;            ("p" "project")
;            ("t" "toggle")
;            ("v" "view")
;            ("w" "window")
;            ("x" "text"    "Text")
;            ;; uppercase
;            ("F" "frame")
;            ("P" "package")
;            ("Z" "quit"    "Quit")))
;
;   (let* ((key     (nth 0 lmenu))
;          (name    (nth 1 lmenu))
;          ;; If the display name is omitted, capitalize and pluralize
;          ;; i.e.  `buffer' => `Buffers'
;          (display (or (nth 2 lmenu) (concat (capitalize name) "s"))))
;     (eval `(make-leader-menu ,name ,key :display-name ,display))))

;;; Symbols

(make-leader-menu "shell" ">")
(make-leader-menu "error" "!")
(make-leader-menu "search" "/" :display-name "Search")

;;; lowercase

(make-leader-menu "app" "a" :display-name "Applications")

(make-leader-menu "buffer" "b")

(make-leader-menu "compile" "c" :display-name "Compilation")

(make-leader-menu "file" "f")

(leader-file-menu
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

(add-leader-keys "git" "g" :display-name "Version Control")
(add-leader-keys "help" "h" :display-name "Help")
(add-leader-keys "project" "p")

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


(add-leader-keys "view" "v" :display-name "View")
(add-leader-keys "window" "w")

;;; uppercase

(add-leader-keys "frame" "F")
(add-leader-keys "package" "P")
(add-leader-keys "quit" "Z" :display-name "Quit")

;;; Footer

(provide 'config-keybindings)

;;; config-keybindings.el ends here
