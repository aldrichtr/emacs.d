;;; init.el --- My emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;; Preliminaries

(setopt
 user-full-name "Timothy R. Aldrich"
 user-mail-address "timothy.r.aldrich@gmail.com")

;;;; Config options

(use-package config-options
  ;; Defines the config:* customization group
  :ensure nil
  :demand t
  :custom
  (when (os-android-p)
    (config:emacs-default-font-name "FiraCode Nerd Font Mono Regular")))



;;;; Dependencies

(dolist (pkg '(f s ts dash buttercup))
  (eval `(use-package ,pkg
           :ensure t
           :defer nil)))

(use-package package-macros
  ;; Defines convenience `use-package' macros
  :ensure nil
  :demand t)
(use-builtin package-metadata
  ;; Additional metadata
  :demand t)

;;; Colors & Themes

(dolist (theme (list "jetbrains-darcula"
                     "badger"
                     "colonoscopy"
                     "darktooth"
                     "zenburn"
                     "ample-zen"))
  (let ((name (concat theme "-theme")))
    (eval
     `(use-package ,(intern name)))))

(use-package sublime-themes)

(use-package ef-themes
  :custom
  (ef-themes-post-load-hook nil)
  (ef-themes-disable-other-themes t)
  (ef-themes-to-toggle nil)
  (ef-themes-to-rotate
   '(ef-owl
     ef-bio
     ef-dream
     ef-night
     ef-autumn
     ef-duo-dark
     ef-elea-dark
     ef-symbiosis
     ef-trio-dark
     ef-melissa-dark))

  (ef-themes-headings nil)
  (ef-themes-mixed-fonts nil)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-common-palette-overrides nil))

(use-package doom-themes
  :custom
  (doom-themes-padded-modeline nil)
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;;;; icons
(use-package nerd-icons
  :defines
  (config:emacs-icon-font-name)
  :custom
  (nerd-icons-font-family config:emacs-icon-font-name))

(use-feature ; theme
  :defines
  (custom-theme-directory
   config:emacs-custom-themes-dir
   config:emacs-default-theme)
  :init
  (setopt custom-theme-directory config:emacs-custom-themes-dir)
  (load-theme config:emacs-default-theme t))


;;; Keybinding

;;;; Hydra

(use-package hydra
  :demand t)

(use-package pretty-hydra
  :demand t)

(use-package major-mode-hydra
  :demand t)

;;;; General

(use-package general
  :after evil
  :functions (leader-menu)
  :demand t
  :custom
  (general-describe-keymap-sort-function #'general-sort-by-car)
  (general-describe-state-sort-function #'general-sort-by-car)

  ;; Evaluating a file with keybindings will update changed definitions
  ;; if nil, only unbound keybindings will be updated
  (general-describe-update-previous-definition 'on-change)
  :config
  ;; `general-evil-setup' provides `general-nmap', et. al.
  ;; with a non-nil argument will create evil-mode maps
  ;; without the `general-' prefix, `nmap' et. al.
  (general-evil-setup t)
  ;; Avoid "Key sequence starts with non-prefix key" errors
  (general-auto-unbind-keys))

;;;; Evil

(use-package evil
  :demand t
  :functions (evil-set-leader)
  :init
  ;; these need to be set prior to loading the package
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :custom
  (evil-default-state 'normal)
  (evil-want-minibuffer t)
  (evil-respect-visual-line-mode t)
  ;; Whitespace
  (evil-indent-convert-tabs t)
  (evil-backspace-join-lines t)
  (evil-shift-width 2)
  (evil-shift-round t)
  (evil-auto-indent t)
  ;; Fix window split direction
  (evil-split-window-below t)
  (evil-split-window-right t)
  ;; Scrolling in normal mode
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  ;; Undo
  (evil-want-fine-undo "yes")
  (evil-undo-system 'undo-fu)
  ;; Cursors
  (evil-normal-state-cursor '(hollow     "PeachPuff"))
  (evil-visual-state-cursor '(box        "PapayaWhip"))
  (evil-insert-state-cursor '((hbar . 4) "PapayaWhip"))
  :config
  ;; TODO: Not sure this is needed since we set it via general
  (evil-set-leader 'normal (kbd "<SPC>") (kbd "M-<SPC>"))
  ;; Start the minibuffer in insert mode
  (add-to-list 'evil-insert-state-modes 'minibuffer-mode)
  (evil-mode 1))

(use-package evil-collection
  :after (evil)
  :custom
  ;; Allow evil modes in the minibuffer (also in vertico)
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  ;; Adds gc/gC commands for commenting code
  :config
  (evil-commentary-mode 1))

(use-package evil-numbers
  :after (general)
  :general
  (nmap global
    "C-+" 'evil-numbers/inc-at-pt
    "C--" 'evil-numbers/dec-at-pt
    "C-<kp-add>" 'evil-numbers/inc-at-pt
    "C-<kp-subtract>" 'evil-numbers/dec-at-pt)
  (vmap global
    "C-+" 'evil-numbers/inc-at-pt-incremental
    "C--" 'evil-numbers/dec-at-pt-incremental
    "C-<kp-add>" 'evil-numbers/inc-at-pt-incremental
    "C-<kp-subtract>" 'evil-numbers/dec-at-pt-incremental))

(use-package evil-multiedit
  :requires (evil-mc)
  :functions (evil-multiedit-default-keybinds)
  :config
  (evil-multiedit-default-keybinds)
  :pretty-hydra
  (hydra-mc
   (:hint nil :title " Multiple cursors" :quit-key "q")
   ("Down"
    (("n"   mc/mark-next-like-this "Mark next line")
     ("M-n" mc/unmark-next-like-this "Unmark line going down")
     ("N"   mc/skip-to-next-like-this "Skip next line"))
    "Up"
    (("p"   mc/mark-previous-like-this "Mark previous line")
     ("P"   mc/skip-to-previous-like-this "Skip previous line")
     ("M-p" mc/unmark-previous-like-this "Unmark line going up"))
    "Mark many"
    (("l" mc/edit-lines "Convert region")
     ("a" mc/mark-all-like-this-dwim :exit t "Mark all like selection")
     ("g" mc/mark-all-in-region-regexp :exit t "Mark regexp in region")
     ("r" mc/mark-sgml-tag-pair :exit t "Mark tag pair")
     ("x" mc/mark-more-like-this-extended "Extended marking"))
    "Special"
    (("1" mc/insert-numbers "Insert numbers")
     ("^" mc/sort-regions   "Sort regions")
     ("|" mc/vertical-align "Vertially align")
     ("A" mc/insert-numbers "Insert letters"))))
  :general-config
  (nmap
    "M-d" '("Match next" . evil-multiedit-match-symbol-and-next)
    "M-D" '("Match prev" . evil-multiedit-match-symbol-and-prev))
  (vmap
    "R"   '("Match all"  . evil-multiedit-match-all)
    "M-d" '("Match next" . evil-multiedit-match-and-next)
    "M-D" '("Match prev" . evil-multiedit-match-and-prev))
  (general-def
    '(normal visual)
    'global
    "C-M-d" '("Restore cursors" . evil-multiedit-restore))

  (general-def
    'multiedit
    'global
    "M-d"  '("Match next" . evil-multiedit-match-and-next)
    "M-S-d" 'evil-multiedit-match-and-prev
    "RET"   'evil-multiedit-toggle-or-restrict-region)
  (general-def '(multiedit multiedit-insert)
    "C-n" 'evil-multiedit-next
    "C-p" 'evil-multiedit-prev)

  ;; evil-mc
  (nvmap 'global
    :prefix "gz"
    "" '(:ignore t :which-key "multi cursor")
    "m" 'evil-mc-make-all-cursors
    "u" 'evil-mc-undo-all-cursors
    "z" '+evil/mc-toggle-cursors
    "c" '+evil/mc-make-cursor-here
    "n" 'evil-mc-make-and-goto-next-cursor
    "p" 'evil-mc-make-and-goto-prev-cursor
    "N" 'evil-mc-make-and-goto-last-cursor
    "P" 'evil-mc-make-and-goto-first-cursor)

  (general-def '(normal visual)
    evil-mc-key-map
    "C-n" 'evil-mc-make-and-goto-next-cursor
    "C-N" 'evil-mc-make-and-goto-last-cursor
    "C-p" 'evil-mc-make-and-goto-prev-cursor
    "C-P" 'evil-mc-make-and-goto-first-cursor))

(use-package evil-lion
  ;; align things via gl/gL
  :config
  (evil-lion-mode 1))

(use-package evil-iedit-state)

;;;; Leader-Menu system
 (use-builtin leader-key-system
  ;;  Defines the macros used to create \"leader-key menus\"
  :demand t)

(use-builtin config-keybindings
  :requires (leader-key-system)
  :demand t
  :custom
  (config:emacs-leader-key-menu-list
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
    ("f" "file")
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
    ("Z" "quit"    "Quit")))
  :config
  (leader-key-menu-initialize))


;;; Which-key

(use-builtin which-key
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'right)
  (which-key-side-window-max-height 0.33)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-key-order)
  (which-key-idle-delay 1.0)
  (which-key-add-column-padding 1)
  (which-key-separator "  ")
  (which-key-unicode-correction 3)
  (which-key-prefix-prefix "")
  (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (which-key-show-prefix 'top)
  (which-key-show-operator-state-maps t)
  :config
  (dolist (replacement
           '((("TAB" . nil) . ("↹" . nil))
             (("RET" . nil) . ("⏎" . nil))
             (("DEL" . nil) . ("⇤" . nil))
             (("SPC" . nil) . ("␣" . nil))))
    (add-to-list 'which-key-replacement-alist replacement))
  (which-key-mode 1))

(use-package which-key-posframe
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-right-corner)
  (which-key-posframe-border-width 2)
  :general
  (leader-toggle-menu
   "k" '("wk posframe" . (lamda () which-key-posframe-mode)))
  :config
  (which-key-posframe-mode -1))



;;; Disaster Recovery

(use-feature ; auto-save
  :defer nil
  :custom
  (auto-save-interval 150)       ; characters typed
  (auto-save-timeout 20)         ; seconds of idle time
  (auto-save-visited-interval 5) ; seconds of idle if auto-save-visited-mode
  (auto-save-default t)          ; auto-save by default
  (auto-save-no-message t)       ; auto-save quietly
  (auto-save-file-name-transforms
   '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      (file-name-concat config:emacs-auto-save-dir "\\2") t)))
  (auto-save-list-file-prefix
   (file-name-concat config:emacs-auto-save-dir "sessions" ".saves-")))

;;;; History

(use-builtin savehist
  :custom
  (minibuffer-follows-selected-frame t)
  (minibuffer-auto-raise t)
  (savehist-autosave-interval 200)
  (savehist-save-minibuffer-history t)
  (savehist-file config:emacs-history-file)
  :config
  (savehist-mode 1))


(use-builtin recentf
  :defer 0.1
  :custom
  (recentf-save-file config:emacs-recent-files-file)
  :config
  (recentf-mode))

(use-builtin files
  :demand t
  :custom
  (make-backup-files t)
  ;; only make backups if no version control
  (vc-make-backup-files nil)
  (backup-by-copying t)
  (kept-new-versions 5)
  (kept-old-versions 2)
  (delete-old-versions t)
  (version-control t)
  (version-separator ".")
  (backup-directory-alist
   `((".*" . ,config:emacs-backup-dir)))
  :general
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
    "s" '("scratch buffer"    . scratch-buffer)))

;;; Package management

(use-builtin package
  :preface
  (defun package-list-mode-setup()
    "Setup the package list buffer"
    (hl-line-mode 1))
  :hook (package-menu-mode . package-list-mode-setup))

;;; Emacs Application

(use-builtin server
  :functions (server-running-p)
  :defines (config:emacs-server-run)
  :config
  (unless (and (server-running-p)
               config:emacs-server-run)
    (add-hook 'after-init-hook 'server-start t)))

(use-package restart-emacs
  :defer nil
  :requires (filesystem-x)
  :preface
  (defun restart-emacs-resume-layouts (&optional args)
    "Restart emacs and resume layouts."
    (interactive)
    (restart-emacs (cons "--resume-layouts" args)))

  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args)))

  (defun restart-emacs-timed-requires (&optional args)
    "Restart emacs and time loads / requires."
    (interactive)
    (restart-emacs (cons "--timed-requires" args)))

  (defun restart-emacs-adv-timers (&optional args)
    "Restart emacs and time loads / requires and spacemacs configuration."
    (interactive)
    (restart-emacs (cons "--adv-timers" args)))

  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'emacs-lisp-mode)

  :general-config
  (leader-quit-keys
    "q" '("Save & quit" . save-buffers-kill-emacs)
    "d" '("Restart with --debug-init" . restart-emacs-debug-init)
    "r" '("Restart" . restart-emacs)
    "R" '("Restart and resume" . restart-emacs-resume-layouts)
    "t" '("Restart with timer" . restart-emacs-timed-requires)
    "T" '("Restart with adv. timer" . restart-emacs-adv-timers)))


;;; UI Elements

(use-feature ; toggles
  :general
  (leader-toggle-menu
    "l" '("highlight line"  . hl-line-mode)
    "m" '("toggle menu-bar" . menu-bar-mode)
    "t" '("Toggle tool-bar" . tool-bar-mode))

  (add-leader-keys "Line numbers" "n"
    :parent leader-toggle-menu
    "r" '("relative" . menu-bar--display-line-numbers-mode-relative)
    "a" '("absolute" . menu-bar--display-line-numbers-mode-absolute)
    "v" '("visual"   . menu-bar--display-line-numbers-mode-visual)
    "q" '("off"      . menu-bar--display-line-numbers-mode-none))
  )

;;;; Mouse & cursors

(use-feature ; mouse
  :custom
  (mouse-autoselect-window nil)
  :config
  (when (display-graphic-p)
    (context-menu-mode 1)))

(use-package beacon
  :delight (beacon-mode "󰨀")
  :hook (prog-mode text-mode)
  :custom
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-blink-when-point-moves-horizontally 5)
  (beacon-blink-when-buffer-changes t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-duration 1.5)
  (beacon-blink-delay 0.2)
  (beacon-size 80)
  (beacon-color "#ffdead"))

;;;; Frame

(use-feature ; Frame properties - Common
  :defines (config:emacs-default-font)
  :custom
  ;; We want the window manager to control the window (frame) size
  (frame-inhibit-implied-resize t)
  :config
  (setq frame-title-format
        '(""
          invocation-name
          " " emacs-version
          " - "
          user-login-name " - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))))))
  (set-frame-font config:emacs-default-font nil t)
  (add-to-list 'default-frame-alist
               `(font . ,config:emacs-default-font))
  :general
  (leader-frame-menu
    "f" '("New" . make-frame-command)))

(use-feature ; Android
  :functions (os-android-p)
  :when (os-android-p)
  :custom
  (overriding-text-conversion-style nil)
  :config
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (tooltip-mode 1)
  (modifier-bar-mode t)
  (setq tool-bar-position 'bottom)
  (easy-menu-add-item
   nil '("Options" "Show/Hide")
   ["Keyboard" (if touch-screen-display-keyboard
                   (setopt touch-screen-display-keyboard nil)
                 (setopt touch-screen-display-keyboard t))
    :visible (eq system-type 'android)
    :style toggle
    :selected touch-screen-display-keyboard]))

(use-feature
  :functions (os-android-p)
  :unless (os-android-p)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;;;; Tabs

(use-package centaur-tabs
  :functions
  (centaur-tabs-enable-buffer-reordering
   centaur-tabs-choose-char
   centaur-tabs-headline-match
   centaur-tabs-get-group-name
   centaur-tabs-change-fonts)
  :hook (after-init)
  :custom
  (centaur-tabs-height 25)
  (centaur-tabs-style "wave")
  (centaur-tabs-set-bar 'under)
  ;; icons
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-icon-scale-factor 1.5)
  ;; markers
  (centaur-tabs-show-count t)
  (centaur-tabs-set-modified-marker t)
  ;; Have to use a nerd font if I want unicode symbols here
  (centaur-tabs-modified-marker (centaur-tabs-choose-char
                                 (make-string 1 #x2022) "*"))
  (centaur-tabs-close-button (centaur-tabs-choose-char
                              (make-string 1 #x00D7) "x"))
  ;; left :: open tab to left of current
  ;; right :: open tab to right of current
  ;; t :: open tab based on last visited tab
  (centaur-tabs-adjust-buffer-order 'right)
  ;; when nil, use completing-read which is what lets consult
  ;; do the completions
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-enable-key-bindings nil)
  :config
  ;;(centaur-tabs-change-fonts "Aldrich" 100)
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*Async-native-compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  :general
  (nmap
    :prefix "gT"
    ""   '(:ignore t :which-key "Tabs")
    "b" '("First" . centaur-tabs-select-beg-tab)
    "l" '("Last" . centaur-tabs-select-beg-tab)
    "n" '("Next" . centaur-tabs-forward)
    "p" '("Prev" . centaur-tabs-backward)
    "<" '("Move left"  . centaur-tabs-move-current-tab-to-left)
    ">" '("Move right" . centaur-tabs-move-current-tab-to-right)
    "q" '("Quit"       . centaur-tabs-kill-all-buffers-in-current-group)
    ))

;;;; Modeline

(use-package doom-modeline
  :defines
  (evil-normal-state-tag
   evil-emacs-state-tag
   evil-insert-state-tag
   evil-motion-state-tag
   evil-visual-state-tag
   evil-replace-state-tag
   evil-operator-state-tag
   evil-iedit-state-tag)

  :functions
  (doom-modeline-vcs-name)
  :hook (after-init . doom-modeline-mode)
  :custom
  (line-number-mode t)
  (column-number-mode t)
  ;; setup the modal state ----------------------------------------------------
  (doom-modeline-modal-icon t)
  (doom-modeline-modal-modern-icon t)
  (evil-normal-state-tag (propertize "[Normal]"))
  (evil-insert-state-tag (propertize "[Insert]"))
  (evil-emacs-state-tag (propertize "[Emacs]"))
  (evil-motion-state-tag (propertize "[Motion]"))
  (evil-replace-state-tag (propertize "[Replace]"))
  (evil-visual-state-tag (propertize "[Visual]"))
  (evil-operator-state-tag (propertize "[Operator]"))
  (evil-iedit-state-tag (propertize "[IEdit]"))
  (doom-modeline-height 1)
  (doom-modeline-bar-width 4)
  ;; icons --------------------------------------------------------------------
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  ;; buffer info --------------------------------------------------------------
  (doom-modeline-buffer-name t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-buffer-file-name-style 'auto)
  ;; lsp ----------------------------------------------------------------------
  (doom-modeline-lsp-icon t)
  (doom-modeline-lsp t)
  ;; time ---------------------------------------------------------------------
  (doom-modeline-time t)
  (doom-modeline-time-icon t)
  (doom-modeline-time-live-icon t)
  (doom-modeline-unicode-fallback t)
  ;; position info ------------------------------------------------------------
  (doom-modeline-column-zero-based t)
  (doom-modeline-percent-position '(-3 "%p"))
  (doom-modeline-position-line-format '("L%l"))
  (doom-modeline-position-column-format '("C%c"))
  (doom-modeline-position-column-line-format '("%l:%c"))
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)

  (doom-modeline-minor-modes nil)
  (doom-modeline-indent-info t)
  ;; version-control ----------------------------------------------------------
  (doom-modeline-vcs-icon t)
  (doom-modeline-vcs-max-length 16)
  (doom-modeline-vcs-display-function #'doom-modeline-vcs-name)

  (doom-modeline-check-icon t)

  (doom-modeline-always-show-macro-register t)

  (doom-modeline-env-version t)
  ;; projects and workspaces --------------------------------------------------
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-project-name t)
  (doom-modeline-workspace-name t)
  (doom-modeline-persp-name t)
  (doom-modeline-display-default-persp-name t)
  (doom-modeline-persp-icon t)

  :custom-face
  (mode-line
   ((t (:family "FiraCode Nerd Font Propo" :height 100))))
  (mode-line-active
   ((t (:family "FiraCode Nerd Font Propo" :height 100))))
  (mode-line-inactive
   ((t (:family "FiraCode Nerd Font Propo" :height 100))))
  ;; evil states
  (doom-modeline-evil-normal-state
   ((t (:background "grey45" :foreground "white"))))
  (doom-modeline-evil-emacs-state
   ((t (:background "thistle" :foreground "black"))))
  (doom-modeline-evil-insert-state
   ((t (:background "wheat4" :foreground "white"))))
  (doom-modeline-evil-motion-state
   ((t (:background "cornflowerblue" :foreground "white"))))
  (doom-modeline-evil-replace-state
   ((t (:background "cyan" :foreground "black"))))
  (doom-modeline-evil-visual-state
   ((t (:background "gray80" :foreground "black"))))
  (doom-modeline-evil-iedit-state
   ((t (:background "darkgreen" :foreground "white"))))
  (doom-modeline-evil-operator-state
   ((t (:background "bisque4" :foreground "black"))))
  :config
  (doom-modeline-mode 1))

;;;; Windows

(use-package winum
  :hook (emacs-startup . winum-mode)
  :custom
  (window-numbering-scope 'global) ; numbers unique across frames
  (winum-reverse-frame-list nil)
  (winum-auto-assign-0-to-minibuffer t)
  (winum-auto-setup-mode-line t)
  :config
  :general
  ;; Use either Alt-[num] or SPC-[num] for switching windows
  (global-def
    "M-0" '("Minibuffer" . winum-select-window-0)
    "M-1" 'winum-select-window-1
    "M-2" 'winum-select-window-2
    "M-3" 'winum-select-window-3
    "M-4" 'winum-select-window-4
    "M-5" 'winum-select-window-5
    "M-6" 'winum-select-window-6
    "M-7" 'winum-select-window-7
    "M-8" 'winum-select-window-8
    "M-9" 'winum-select-window-9)
  (leader-menu
    "1" '("Focus win 1" . winum-select-window-1)
    "2" '("Focus win 2" . winum-select-window-2)
    "3" '("Focus win 3" . winum-select-window-3)
    "4" '("Focus win 4" . winum-select-window-4)
    "5" '("Focus win 5" . winum-select-window-5)
    "6" '("Focus win 6" . winum-select-window-6)
    "7" '("Focus win 7" . winum-select-window-7)
    "8" '("Focus win 8" . winum-select-window-8)))

(use-package ace-window
  :demand t
  :custom
  (aw-ignore-on t)
  (aw-ignored-buffers '(treemacs-mode which-key-mode))
  :init
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?e aw-execute-command-other-window "Execute Command Other Window")
      (?F aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?T aw-transpose-frame "Transpose Frame")
      ;; ?i ?r ?t are used by hyperbole.el
      (?? aw-show-dispatch-help)))
  :pretty-hydra
  (win-size-hydra
   (:color red
           :quit-key "q"
           :title "Window Size")
   ("window size"
    (("+"  evil-window-increase-height "+ height")
     ("-"  evil-window-decrease-height "- height")
     ("="  balance-windows "balanced")
     ("<"  evil-window-decrease-width "< width")
     (">"  evil-window-increase-width "> width")
     ("_"  evil-window-set-height "set height"))))
  :general
  ;; ace-window has its own keymap that is built from aw-dispatch-alist
  (leader-window-menu
   "RET" '("Jump to"     . ace-window)
   ;; window size
   "s"  '("menu"       . win-size-hydra/body)
   "+"  '("+ height"   . evil-window-increase-height)
   "-"  '("- height"   . evil-window-decrease-height)
   "="  '("balanced"   . balance-windows)
   "<"  '("< width"    . evil-window-decrease-width)
   ">"  '("> width"    . evil-window-increase-width)
   "_"  '("set height" . evil-window-set-height)
   "|"  '("set width"  . evil-window-set-width)

   ;; window navigation
   "<up>"    '("up"   . evil-window-up)
   "k"       '("up"   . evil-window-up)
   "<down>"  '("down" . evil-window-down)
   "j"       '("down" . evil-window-down)
   "<left>"  '("left" . evil-window-left)
   "l"       '("left" . evil-window-right)
   "<right>" '("right". evil-window-right)
   "h"       '("right". evil-window-left)
   "w"       '("next" . evil-window-next)
   "W"       '("prev" . evil-window-prev)
   "p"       '("MRU"  . evil-window-mru)
   "b"       '("bottom" . evil-window-bottom-right)
   "t"       '("top"    . evil-window-top-left)
   "x"       '("exchange . "evil-window-exchange)

   ;; window movement
   "S-<left>" '("move far left"  . evil-window-move-far-left)
   "H"        '("move far left"  . evil-window-move-far-left)
   "S-<down>" '("move bottom"    . evil-window-move-very-bottom)
   "J"        '("move bottom"    . evil-window-move-very-bottom)
   "S-<up>"   '("move top"       . evil-window-move-very-top)
   "K"        '("move top"       . evil-window-move-very-top)
   "S-<down>" '("move far right" . evil-window-move-far-right)
   "L"        '("move far right" . evil-window-move-far-right)
   "R"   '("rotate up"   . evil-window-rotate-upwards)
   "r"   '("Rotate down" . evil-window-rotate-downwards)
   ;; window creation
   "s"   '("Split"     . evil-window-split)
   "v"   '("VSplit"    . evil-window-vsplit)
   "f"   '("Find File" . ffap-other-window)
   "n"   '("New"       . evil-window-new)
   ;; window destruction
   "o"   '("Only"   . delete-other-windows)
   "d"   '("Delete" . evil-window-delete)
   "q"   '("Quit"   . evil-quit)

   ;; Tabs
   ;; T          tab-window-detach
   ;; g T        tab-bar-switch-to-prev-tab
   ;; g t        evil-tab-next
   )
  :config
  (ace-window-display-mode 1))

(use-builtin ace-window-posframe
  :functions
  (ace-window-posframe-enable)
  :config
  (ace-window-posframe-enable))

;;;; tabline

(use-package breadcrumb
  :hook ((prog-mode text-mode conf-mode) . breadcrumb-mode)
  :custom
  (breadcrumb-project-crumb-separator "❱")
  (breadcrumb-imenu-crumb-separator "❱"))

;;;; Buffers

(use-feature ; buffer
  :preface
  (defun buffer-indent ()
    "Indent the entire buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max) nil)))

  (declare-function org-indent-region "org.el")
  (declare-function org-fill-element "org.el")

  (defun indent-and-fill-region ()
    "Indent the region and then refill it."
    (interactive)
    (save-excursion
      (cond ((derived-mode-p 'org-mode)
             (org-indent-region (point-min) (point-max))
             (org-fill-element))
            (t
             (indent-region (point-min) (point-max) nil)
             (prog-fill-reindent-defun))))))

(use-package buffer-move
  :demand t
  :custom
  ;; move behavior: move will change current window to previous-buffer
  (buffer-move-behavior 'swap)
  (buffer-move-stay-after-swap nil) ; nil means stay in current window
  ;; TODO: Add other buffer commands in the centaur-tabs package
  :general
  (leader-buffer-menu
   "b" '("switch buffer" . consult-buffer)
   "B" '("other window"  . consult-buffer-other-window)
   "d" '("delete"        . evil-delete-buffer)
   "q" '("kill"          . kill-current-buffer)
   "n" '("next"          . next-buffer)
   "p" '("prev"          . previous-buffer)
   "r" '("reload"        . revert-buffer-quick)
   "i" '("imenu"         . ibuffer)
   ;; movement
   "K" 'buf-move-up
   "J" 'buf-move-down
   "L" 'buf-move-right
   "H" 'buf-move-left))

(use-package dashboard
  :defines
  (day-of-week-image
   config:emacs-config-dir)
  :preface
  (defun dashboard-logo-today ()
    (let* ((image-dir (file-name-concat config:emacs-config-dir
                                        "assets" "images"))
           (today-image (concat (downcase (format-time-string "%A")) ".png")))
      (file-truename (expand-file-name (file-name-concat image-dir today-image)))))
  :defer nil
  :custom
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-navigator
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-footer))
  ;; banner
  (dashboard-startup-banner (dashboard-logo-today))
  ;; navigator
  (dashboard-navigator-buttons
   `((
      ("★" "scratch" "show scratch buffer"
       (lambda (&rest _) (switch-to-buffer "*scratch*")) warning "⋗" " ")
      ("★" "files" "recent files"
       (lambda (&rest _) (recentf-open-files)) warning "⋗" " ")
      ("★" "report" "view use-package report"
       (lambda (&rest _) (use-package-report)) warning "⋗" " ")
      ("★" "packages" "list packages"
       (lambda (&rest _) (list-packages)) warning "⋗" " ")
      ("★" "shell" "project eshell"
       (lambda (&rest _) (project-eshell)) warning "⋗" " ")
      ("★" "restart" "restart emacs"
       (lambda (&rest _) (restart-emacs)) warning "⋗" " ")
      ("★" "manual" "emacs manual"
       (lambda (&rest _) (info-emacs-manual)) warning "⋗" " "))))
  ;; icons
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-heading-icons '((recents   . "nf-oct-history")
                             (bookmarks . "nf-oct-bookmark")
                             (agenda    . "nf-oct-calendar")
                             (projects  . "nf-oct-rocket")
                             (registers . "nf-oct-database")))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  ;; content
  (dashboard-center-content t)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '((recents   . 5)
                     (projects  . 3)
                     (bookmarks . 5)))
  :general
  (leader-view-menu
   "d" '("Dashboard" . dashboard-open))

  :config
  (dashboard-setup-startup-hook))

(use-package harpoon
  :general
  (leader-buffer-menu
   "1" '("harpoon 1" . harpoon-go-to-1)
   "2" '("harpoon 2" . harpoon-go-to-2)
   "3" '("harpoon 3" . harpoon-go-to-3)
   "4" '("harpoon 4" . harpoon-go-to-4)
   "5" '("harpoon 5" . harpoon-go-to-5)
   "6" '("harpoon 6" . harpoon-go-to-6)
   "h" '("harpoon menu" . harpoon-quick-menu-hydra)))

(use-feature ; whitespace
  :preface
  (defun buffer-whitespace-clean ()
    "Remove trailing whitespace and convert any tabs to spaces.
source: `http://steve.yegge.googlepages.com/my-dot-emacs-file'"
    (interactive)
    (untabify (point-min) (point-max))
    (setq delete-trailing-lines t)
    (delete-trailing-whitespace))

  (defun unfill-region (begin end)
    "Remove all linebreaks in a region.

But leave paragraphs,indented text (quotes,code) and lines starting with an
asterix (lists) intact from BEGIN to END."
    (interactive "r")
    (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

  :custom
  (require-final-newline t)
  (tab-width 2)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (standard-indent 2)
  :general
  (leader-toggle-menu
   "w" 'whitespace-mode))

;;;; Structure

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode org-mode) . electric-pair-mode)
  :general
  (leader-toggle-menu
    "e" '("Electric pair" . electric-pair-mode)))

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t))

(use-builtin paren
  :custom-face
  (show-paren-match ((t (:foreground "cyan")))))

(use-package highlight-indent-guides
  :delight
  :hook (prog-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'stack)
  :custom-face
  (highlight-indent-guides-odd-face       ((t (:background "darkgray"))))
  (highlight-indent-guides-even-face      ((t (:background "dimgray"))))
  (highlight-indent-guides-character-face ((t (:foreground "gainsboro")))))

(use-package rainbow-delimiters
  :hook (prog-mode)
  :custom-face
  ;; delimiters follow the Violet, Yellow, Green, Orange, Blue, pattern
  ;; so that they are more destinctly separated
  ;; Red is reserved for `mismatched'
  (rainbow-delimiters-mismatched-face
   ((t (:foreground "LightGray" :background "red"))))
  (rainbow-delimiters-depth-1-face ((t (:foreground "LightSlateBlue"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "LemonChiffon"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "SeaGreen"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "goldenrod"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "SteelBlue"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "MediumOrchid"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "MediumSeaGreen"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "DodgerBlue")))))

(use-builtin outline
  :preface
  (defvar outline-display-table (make-display-table))
  (set-display-table-slot outline-display-table 'selective-display
                          (vector (make-glyph-code ?► 'escape-glyph)))
  (defun set-outline-display-table ()
    "Add glyph to display table."
    (setf buffer-display-table outline-display-table))
  :custom
  (outline-blank-line t)
  (outline-minor-mode-highlight t)
  (outline-default-state 2) ;; default state is folded at level 2
  :general
  (nmap
    :prefix "z"
    "n" 'outline-forward-same-level
    "p" 'outline-backward-same-level)
  :config

  (add-hook 'outline-mode-hook 'set-outline-display-table)
  (add-hook 'outline-minor-mode-hook 'set-outline-display-table))

(use-package backline
  ;; backline is used in conjunction with `outline-minor-mode-highlight' to extend
  ;; the highlight of the outline to the right edge of the window
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package outline-minor-faces
  :after outline
  :hook
  (outline-minor-mode . outline-minor-faces-mode))

;;;; Application modes

(use-package writeroom-mode
  :pretty-hydra
  (write-room-width-hydra
   (:color red
           :quit-key "q"
           :title "Write area width")
   ( "Width"
     (("+" writeroom-increase-width "Wider")
      ("-" writeroom-decrease-width "Narrower"))))
  :general
  (leader-view-menu
   "z" '("Zen mode" . writeroom-mode))
  (nmap write-room-mode-map
    "w"   '("Adjust width"   . write-room-width-hydra/body)
    "M-+" '("increase width" . writeroom-increase-width)
    "M--" '("decrease width" . writeroom-decrease-width)
    "M-#" '("Reset width"    . writeroom-adjust-width)
    "M-?" '("Toggle modeline" . writeroom-toggle-mode-line)))

;;; Hooks & Advice

;;;; Advice

(use-builtin advice
  :custom
  (ad-redefinition-action 'accept))

;;;; Hooks

(use-builtin simple ;; fundemental-mode
  :require ws-butler
  :preface
  (defun fundamental-mode-setup ()
    "Define things that should be set in all modes."
    (setq-default tab-width 2
                  fill-column 80
                  tab-always-indent 'complete)
    (ws-butler-mode)
    (display-line-numbers-mode 1)
    (display-fill-column-indicator-mode 1)
    (auto-fill-mode 1)
    (outline-minor-mode)))

(use-feature ; before-save hook
  (defun setup-before-save ()
    "Actions to take prior to saving the file.")
    ;; replaced with ws-butler
    ;; (buffer-whitespace-clean))
  :hook (before-save . setup-before-save))

(use-builtin text-mode
  :preface
  (defun text-mode-setup ()
    "Function to add commands to `text-mode-hook'."
    (fundamental-mode-setup))
  :hook (text-mode . text-mode-setup))

(use-builtin prog-mode
  :preface
  (defun prog-mode-setup ()
    "Function to add commands to `prog-mode-hook'."
    (fundamental-mode-setup))

  :hook (prog-mode . prog-mode-setup))


;;; Terminals & Shells

(use-builtin term
  :custom
  (term-scroll-to-bottom-on-output 'this)
  (term-input-ring-file-name
   (file-name-concat config:emacs-local-dir "term-history.log")))

(use-package multi-term)

(use-package toggle-term
  :general
  (global-def
    "C-`" '("Toggle term" . toggle-term-shell)))

(use-package term-projectile)

(use-builtin shell
  :defines (explicit-pwsh.exe-args)
  :defer t
  :config
  (when (os-windows-p)
    (setq explicit-shell-file-name (executable-find "pwsh")
          ;;shell-file-name "pwsh.exe"
          explicit-pwsh.exe-args '("-NoLogo" "-NoProfile" "-Interactive")
          shell-prompt-pattern "PS .*>$"))
  :general-config
  (leader-shell-menu
   "p" '("Powershell" . shell)
   "e" '("Emacs shell" . eshell)))

(use-builtin ielm
  :custom
  (ielm-history-file-name (file-name-concat config:emacs-local-dir
                                            "ielm-history.eld"))
  (ielm-prompt "λ")

  :general-config
  (leader-shell-menu
   "i" '("IELM" . ielm))

  (general-def ielm-map
    "C-RET" 'ielm-return
    "RET" 'ielm-return-for-effect))

;;; External Tools & Services

(use-builtin tramp
  :preface
  (defun add-docker-to-tramp ()
    "Add the ability to use tramp to connect to running docker containers."
    (push
     (cons
      "docker"
      '((tramp-login-program "docker")
        (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
        (tramp-remote-shell "/bin/sh")
        (tramp-remote-shell-args ("-i") ("-c"))))
     tramp-methods))

  (defun dotemacs-completion-docker (orig-fun &rest args)
    "Return a list of active Docker container names, followed by colons."
    (if (equal (nth 1 args) "/docker:")
        (let* ((dockernames-raw
                (shell-command-to-string
                 "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          dockernames)
      (apply orig-fun args)))
  (advice-add 'tramp-completion-handle-file-name-all-completions
              :around #'dotemacs-completion-docker)
  :hook (tramp--startup-hook . add-docker-to-tramp))

(use-package gnus
  :custom
  (gnus-inhibit-startup-message t))

(use-package nnhackernews)

(use-package system-packages
  ;; - https://gitlab.com/jabranham/system-packages
  :init
  (add-to-list
   'system-packages-supported-package-managers
   '(winget
     . ((default-sudo . nil)
        (install                     . "winget install")
        (search                      . "winget search")
        (uninstall                   . "winget uninstall")
        (update                      . "winget upgrade")
        (list-installed-packages     . "winget list")
        (list-installed-packages-all . "winget list")
        (log                         . "winget --open-logs")
        (get-info                    . "winget show"))))
  :custom
  (system-packages-package-manager
   (cond ((os-windows-p) 'winget)
         ;; android (termux), or linux
         (t 'apt))))

(use-package yequake
  :custom
  ;; Create a capture frame
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (yequake-org-capture))
      (top    . 10)
      (width  . 0.75)
      (height . 0.5)
      (alpha  . 0.95)
      (frame-parameters . ((undecorated  . t)
                           (skip-taskbar . t)
                           (sticky       . t)))))))

(use-package ripgrep
  :ensure-system-package rg)

(use-package parinfer-rust-mode
  :disabled t
  :init
  (setq parinfer-rust-library-directory
        (file-name-concat (getenv "HOME") ".cargo" "bin"))
  :hook (emacs-lisp-mode clojure-mode)
  :custom
  (parinfer-rust-disable-troublesome-modes t))

;;; Documentation & Help

(use-package helpful
  :custom
  (help-window-keep-selected t)
  (help-window-select t)
  (helpful-max-buffers 3)
  :custom-face
  (helpful-heading ((t (:foreground "#28ABE3" :weight bold))))
  :general
  (leader-help-menu
   "i" '("Info" . consult-info)
   "." '("At point" . helpful-at-point)
   "f" '("Function" . helpful-function)
   "c" '("Command" . helpful-command)
   "v" '("Variable" . helpful-variable)
   "k" '("Key" . helpful-key)
   "s" '("Symbol" . helpful-symbol)
   "q" '("Quit help" . helpful-kill-buffers)))

(use-package info-colors
  :hook (Info-selection-hook . info-colors-fontify-node))

;;; Search

(use-package avy)

(use-package evil-avy)

;;; Text Insertion

(use-feature ; text-encoding
  :init
  (if (os-windows-p)
      (progn
        (set-clipboard-coding-system 'utf-16-le)
        (set-selection-coding-system 'utf-16-le))
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (setq locale-coding-system 'utf-8-unix)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

(use-package undo-fu ;; Needed for C-r to work
  :after evil
  :custom
  (evil-undo-system 'undo-fu)
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  (undo-outer-limit 1006632960))

(use-package surround
  :general
  (general-def 'global
    "C-'" '("Mark surrounding..." . surround-mark)
    "M-'" '("Surround with..."    . surround-insert)
    "C-M-'" 'surround-keymap))

(use-package drag-stuff
  :delight
  :config
  (drag-stuff-global-mode t)
  :general
  ;; TODO: is this a global-def?
  (global-def
    "M-<up>" 'drag-stuff-up
    "M-<down>" 'drag-stuff-down))

(use-package ws-butler)

;;;; Completion

;;;;; Vertico

(use-package vertico
  :general
  (general-def vertico-map
    "?"         'minibuffer-completion-help
    "C-n"       'vertico-next
    "C-p"       'vertico-previous
    "C-N"       'vertico-next-group
    "C-P"       'vertico-previous-group
    "TAB"       'minibuffer-complete
    "M-<DEL>"   'vertico-directory-up
    "M-<enter>" 'minibuffer-force-complete-and-exit
    "<escape>"  'minibuffer-keyboard-quit
    "RET"       'vertico-directory-enter)
  :custom
  (enable-recursive-minibuffers t)
  (vertico-scroll-margin 2) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (setq-default read-extended-command-predicate
                #'command-completion-default-include-p)
  (vertico-mode 1))

(use-package vertico-posframe
  :config
  (vertico-posframe-mode 1))

;;;;; Marginalia

(use-package marginalia
  :functions
  (yas--template-key
   marginalia--full-candidate)
  :defines
  (consult-yasnippet--snippets)
  :preface
  ;; NOTE: Maybe there's a less indirect way to do this :/.
  (defun marginalia-annotate-yasnippet (cand)
    "Add yasnippet CAND annotations to marginalia."
    (when-let* ((cand (marginalia--full-candidate cand))
                (template (alist-get cand
                                     consult-yasnippet--snippets
                                     nil nil #'string-equal))
                (key (yas--template-key template)))
      (concat " " (propertize (concat "[" key "]")
                              'face 'font-lock-type-face))))
  :general
  (general-def minibuffer-local-map
    "M-a" 'marginalia-cycle)
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;;;; Consult

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :general-config
  (leader-search-menu
   "b" '("Bookmark"         . consult-bookmark)
   "g" '("(rip)Grep"        . consult-ripgrep)
   "h" '("History"          . consult-history)
   "l" '("Line in buffer"   . consult-line)
   "L" '("Lines in project" . consult-line-multi)
   "m" '("Mark"             . consult-mark)
   "o" '("Outline heading"  . consult-outline))

  ;; Theme
  (leader-toggle-menu
   "T" '("Theme" . consult-theme))

  :config
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)
                     consult-ripgrep
                     consult-git-grep
                     consult-grep
                     consult-bookmark
                     consult-recent-file
                     consult-xref
                     consult-source-bookmark
                     consult-source-file-register
                     consult-source-recent-file
                     consult-source-project-recent-file
                     :preview-key '(:debounce 0.4 any)))


(use-package consult-ripgrep
  :ensure nil
  :preface
  (defun consult-rg-in-project ()
    "This calls `consult-ripgrep' but with double prefix args.
When called with two prefix args, consult-ripgrep will search the current
project, or ask for a project if not in one."
    (interactive "P")
    (universal-argument)     ; first C-u
    (universal-argument-more); second C-u
    (consult-ripgrep))
  :general
  (global-def
    "C-/" '("Search" . consult-rg-in-project))
  )

(use-package consult-yasnippet
  :defines (consult-yasnippet--snippets)
  :after (:all yasnippet consult embark marginalia)
  :custom
  (consult-yasnippet-use-thing-at-point t)
  (consult-yasnippet-always-overwrite-thing-at-point t))

(use-package consult-projectile)


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-count 12)
  (corfu-scroll-margin 2)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current nil)
  (corfu-preselect 'valid)
  (corfu-on-exact-match nil)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :general
  (general-def corfu-popupinfo-map
    "M-t" 'corfu-popupinfo-toggle
    ;; The scrolling in the popupinfo window is the opposite of
    ;; what I think it would be, so I've switched the keys
    "M-d" 'corfu-popupinfo-scroll-up
    "M-u" 'corfu-popupinfo-scroll-down
    "M-b" 'corfu-popupinfo-beginning
    "M-e" 'corfu-popupinfo-end))

(use-package nerd-icons-corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package embark
  ;; Embark is actually not really about completion, but more like a "right-click"
  ;; menu function.  Perform an action on the given object.
  :general
  (global-def
    "C-RET" '("Embark list" . embark-act)
    "M-."   '("embark"      . embark-dwim)
    "M-S-x" '("Describe bindings" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (vertico-multiform-mode))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package yasnippet-capf
  :preface
  (defun yasnippet-capf-setup ()
    "Add yasnippet completions to the list."
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  :hook
  ((prog-mode text-mode) . yasnippet-capf-setup))

(use-package cape
  :requires (yasnippet-capf)
  :defer nil
  :preface
  (defun cape--add-completions (&rest functions)
    "Add FUNCTIONS to `completion-at-point-functions' list.
If the first function in the list is `cape-capf-super',
insert the given functions after it. Otherwise, insert them at the top."
    (let ((capf-list completion-at-point-functions))
      (if (and capf-list (eq (car capf-list) 'cape-capf-super))
          ;; Insert functions after the first one
          (setcdr capf-list (append functions (cdr capf-list)))
        ;; Insert functions at the top
        (setq completion-at-point-functions (append functions capf-list)))))

  (defun cape-setup ()
    "Setup the cape package."
    ;; These are added to the global completion list.  Buffer local lists
    ;; (setq-local) will be used instead of these, unless the buffer local list
    ;; ends with `t'
    (add-to-list 'completion-at-point-functions #'cape-capf-super)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword))
  :hook
  (emacs-startup . cape-setup)
  :custom
  (cape-file-directory-must-exit nil)
  :general
  (global-def
    :prefix "C-c"
    "p" 'cape-prefix-map))


(use-package citre
  ;; - citre is a completion companion for tags/global/etc.
  :hook
  (find-file . citre-auto-enable-citre-mode)
  :init
  (require 'citre-config)
  :general
  (nmap
    "gp" '("Peek"        . citre-peek)
    "gr" '("Goto ref"    . citre-jump-to-reference)
    "gR" '("Goto ref..." . citre-query-jump-to-reference)))

;;;; Templates

(use-builtin autoinsert
  :demand t
  :requires yasnippet
  :preface
  (defun insert/expand-snippet ()
    "Expand the snippets in the current buffer.
This function is used by `auto-insert-mode' to expand snippets in the
template file."
    (let ((current (buffer-name (current-buffer))))
      (unless (string-match "^CAPTURE" current)
        (yas-expand-snippet (nth 1 (yas--parse-template)) (point-min) (point-max)))))

  :custom
  (auto-insert 'other)
  (auto-insert-query nil)
  (auto-insert-directory config:emacs-templates-dir)
  (auto-insert-alist
   '((("\\.el\\'" . "Emacs Lisp") . ["template.el" insert/expand-snippet])
     (("\\.org\\'" . "org-mode") . ["template.org" insert/expand-snippet])))
  :config
  (auto-insert-mode 1))

(use-package yasnippet
  :functions (yas-expand-snippet yas--parse-template)
  :defines (config:emacs-snippets-dir)
  :init
  (setq yas-snippet-dirs `(,config:emacs-snippets-dir))
  :config
  (yas-global-mode 1)
  :general
  (global-def
    "M-/" '("Select snippet" . consult-yasnippet)))

;;;; Text Correction

(use-builtin ispell
  :custom
  (ispell-choices-win-default-height 6)
  :config
  (when (>= (string-to-number emacs-version) 30)
    (setq text-mode-ispell-word-completion nil))
  (setq ispell-program-name "aspell.exe")
  :general-config
  (leader-text-menu
   "$" '("Correct spelling" . ispell-completion-at-point)))

(use-builtin dictionary
  :custom
  (dictionary-server "dict.org"))

(use-builtin flyspell
  ;; TODO: Add toggle for flyspell
  :custom
  (flyspell-delay-use-timer t)
  (flyspell-delay 3))

(use-package flycheck
  ;; see the sideline package
  :functions
  (flycheck-hl-todo-enabled-p
   flycheck-hl-todo-enable
   flycheck-hl-todo-disable)
  :preface
  (defun flycheck-hl-todo-toggle ()
    (if (flycheck-hl-todo-enabled-p)
        (flycheck-hl-todo-disable)
      (flycheck-hl-todo-enable)))
  :hook (prog-mode)
  :custom
  ;; emacs-lisp
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-emacs-lisp-load-path 'inherit)
  :pretty-hydra
  ;; An "errors hydra" that does not "close" until 'q' in normal mode is hit
  (flycheck-nav-hydra
   (:color red
           :title (concat " " (nerd-icons-octicon "nf-oct-bug") " Errors"))
   ("Navigation"
    (("n"  flycheck-next-error     "Next")
     ("p"  flycheck-previous-error "Previous")
     ("gg" flycheck-first-error    "First")
     ("t"  flycheck-hl-todo-toggle "Toggle todos")
     ("G"  (progn (goto-char (point-max))
                  (flycheck-previous-error)) "Last"))))
  :general
  (general-unbind '(normal insert) "C-c !")
  (leader-error-menu
   "!"   '("Error menu" . flycheck-nav-hydra/body)
   "C-c" 'flycheck-compile
   "C-w" 'flycheck-copy-errors-as-kill
   "?"   'flycheck-describe-checker
   "C"   'flycheck-clear
   "H"   'display-local-help
   "V"   'flycheck-version
   "c"   'flycheck-buffer
   "e"   'flycheck-explain-error-at-point
   "h"   'flycheck-display-error-at-point
   "i"   'flycheck-manual
   "l"   'flycheck-list-errors
   "n"   'flycheck-next-error
   "p"   'flycheck-previous-error
   "s"   'flycheck-select-checker
   "v"   'flycheck-verify-setup
   "x"   'flycheck-disable-checker))

(use-package flycheck-posframe
  :general-config
  (leader-toggle-menu
   "!" '("flycheck posframe" . flycheck-posframe-mode)))

(use-package hl-todo
  ;; TODO: Add a toggle for hl-todo
  :hook (prog-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO" .  "#DB3340")
     ("BUG"   . "#DB3340")
     ("INFO" . "#28ABE3")
     ("DONE" . "#798188"))))

(use-package flycheck-hl-todo
  ;; TODO: Add a toggle for hl-todo in flycheck lists
  :defer 5 ; Need to be initialized after the rest of checkers
  :config
  (flycheck-hl-todo-setup))

(use-package consult-todo)

;;; Filetypes & modes

(use-builtin treesit
  :defines
  (config:emacs-treesitter-grammar-dir)
  :init
  (setopt
   treesit-font-lock-level 4 ; 4 is the max level
   treesit-extra-load-path
   (list
    (file-truename (expand-file-name config:emacs-treesitter-grammar-dir))))
  (dolist (mode-remap
           ;; use all of the builtin ts-modes
           '((c-mode          . c-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (elixir-mode     . elixir-ts-mode)
             (go-mode         . go-ts-mode)
             (heex-mode       . heex-ts-mode)
             (java-mode       . java-ts-mode)
             (json-mode       . json-ts-mode)
             (lua-mode        . lua-ts-mode)
             (php-mode        . php-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (rust-mode       . rust-ts-mode)
             (typescript-mode . typescript-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode-remap)))

(use-package treesit-auto
  :custom
  (treesit-auto-install nil))

;;;; Emacs-lisp

(use-builtin elisp-mode
  :preface
  (defun emacs-lisp-mode-setup ()
    ;; When inside a `:custom' section, variables are not shown like they are when
    ;; you type setq <tab>, so this adds them in, and because we use nerd-icons,
    ;; we can easily distinguish the vars from the functions
    ;; see: https://github.com/jwiegley/use-package/issues/1077
    ;; WARNING: Affects all emacs-lisp completions
    (setq-local completion-at-point-functions
                (list (cape-capf-inside-code #'cape-elisp-symbol)))
    )
  :hook
  (emacs-lisp-mode . emacs-lisp-mode-setup)
  (before-save . check-parens)
  :general
  (make-leader-menu "Eval" "e"
    :keymaps 'emacs-lisp-mode-map
    "b" 'eval-buffer
    "d" 'eval-defun
    "r" 'eval-region
    "T" '("ielm REPL" . ielm)))

;;;; Clojure

(use-package cider
  :custom
  (cider-jack-in-default 'lein)
  ;; TODO: There are a lot more CIDER functions to get into menus, see them with
  ;; describe-keymap cider-mode-map
  :general-config
  (make-leader-menu "Eval" "e"
    :keymaps 'cider-mode-map
    "C-q" '("Quit CIDER" . cider-quit)
    ;; -----------------------------------------------------
    ;; defun D
    ;; TODO: Create a macro for "sub menus".  Its silly to list vD for all of
    ;; these, it should be something like def-submenu "vD" "Defun"
    ;; I think spacemacs has one
    "D" '(:ignore t :which-key "Defun")
    "Dr" '("Read & eval defun at point" . cider-read-and-eval-defun-at-point)
    "D." '("Defun at point" . cider-eval-defun-at-point)
    "D," '("Defun up to point" . cider-eval-defun-up-to-point)
    ;; tap
    "T" '(:ignore t :which-key "Tap")
    "Tl" '("Last sexp" . cider-tap-last-sexp)
    "T." '("Sexp at point" . cider-tap-sexp-at-point)
    ;; sexp
    "." '("Sexp at point" . cider-eval-sexp-at-point)
    ;; context
    "c" '("Sexp at point (context)" . cider-eval-sexp-at-point-in-context)
    "C" '("Last Sexp (context)" . cider-eval-last-sexp-in-context)

    "," '("Sexp up to point" . cider-eval-sexp-up-to-point)
    "e" '("Last sexp" . cider-eval-last-sexp)
    "%" '("Last sexp and replace" . cider-eval-last-sexp-and-replace)

    "l" '("List at point" . cider-eval-list-at-point)
    "s" '("dwim" . cider-eval-dwim)
    "n" '("Eval namespace" . cider-eval-ns-form)
    "r" '("Eval Region" . cider-eval-region)

    "k" '("Kill last result" . cider-kill-last-result)
    ))

(use-package clojure-ts-mode
  :preface
  (defun clojure-ts-mode-setup ()
    "Setup clojure treesitter mode."
    (cider-mode))
  :hook (clojure-ts-mode . clojure-ts-mode-setup)
  :config
  (add-to-list 'org-src-lang-modes '("clojure" . clojure-ts-mode))
  (add-to-list 'org-src-lang-modes '("clj" . clojure-ts-mode))
  ;; Tree-sitter conflicts with CIDER
  ;; (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
  )

(use-package flycheck-clj-kondo
  :hook (clojure-mode . flycheck-mode))

;;;; powershell

(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :interpreter ("pwsh.exe" . powershell-mode)
  :custom
  (powershell-default-langserver-path
   (file-name-as-directory (file-name-concat
                            config:emacs-lsp-server-root-dir "pwsh")))
  (powershell-indent 2)
  (powershell-continuation-indent 1))

(use-package powershell-ts-mode
  :disabled t
  :vc
  (:url "https://github.com/dmille56/powershell-ts-mode.git"
        :branch main)
  :custom
  (powershell-ts-enable-imenu-top-level-vars nil)
  (powershell-ts-command-default 'pwsh)
  (powershell-ts-mode-indent-offset 2)
  :config
  (add-to-list 'major-mode-remap-alist '(powershell-mode . powershell-ts-mode))
  (add-to-list 'org-src-lang-modes '("powershell" . powershell-ts))
  (add-to-list 'org-src-lang-modes '("pwsh"       . powershell-ts)))

;;;; nushell

(use-package nushell-ts-mode
  :mode "\\.nu\\'"
  :config
  (add-to-list 'org-src-lang-modes '("nushell" . nushell-ts-mode)))

;;;; gitconfig

(use-package git-modes)

;;;; makefile

(use-builtin makefile-mode
  :mode "Makefile"
  :config
  (setq-local indent-tabs-mode t))

;;;; mermaid

(use-package mermaid-ts-mode
  :custom
  (mermaid-ts-indent-level 2))

;;;; yaml

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :config
  (add-to-list
   'major-mode-remap-alist
   '(yaml-mode . yaml-ts-mode)))

(use-package outline-yaml
  :vc
  (:url "https://github.com/jamescherti/outline-yaml.el"
        :branch main)
  :hook
  ((yaml-mode    . outline-yaml-minor-mode)
   (yaml-ts-mode . outline-yaml-minor-mode)))

;;; Projects & Repositories

;;;; Language Servers

(use-builtin eglot
  :preface
  (defun eglot-register-powershell ()
    "Add the PSES to the eglot server list"
    (let* ((pwsh-lsp-root (file-name-concat config:emacs-lsp-server-root-dir "pwsh"))
           (start-script (expand-file-name "PowerShellEditorServices/Start-EditorServices.ps1" pwsh-lsp-root))
           (bundled-modules pwsh-lsp-root)
           (logs-root (file-name-concat config:emacs-local-dir ".cache" "eglot"))
           (log-path (expand-file-name "pses/logs" logs-root))
           (session-path (expand-file-name "pses/emacs-eglot-session.json" logs-root))
           (eglot-sync-connect t))
      (add-to-list
       'eglot-server-programs
       `(powershell-mode
         . ("pwsh"
            "-NoLogo" "-NoProfile" "-NonInteractive"
            "-OutputFormat" "Text"
            "-File" ,start-script
            "-HostName" "\"Emacs Host\""
            "-HostProfileId" "Emacs.LSP"
            "-HostVersion" "8.0.1"
            "-LogPath" ,log-path
            "-LogLevel" "Normal"
            "-EnableConsoleRepl"
            "-SessionDetailsPath" ,session-path
            ;; "-AdditionalModules" "@('PowerShellEditorServices.VSCode')"
            "-Stdio"
            "-BundledModulesPath" ,bundled-modules
            "-FeatureFlags" "\"@()\""
            )))))
  :custom
  (eglot-sync-connect t)
  :config
  (eglot-register-powershell))

(use-package lsp-mode
  :custom
  (lsp-idle-delay 0.500)
  (lsp-log-io t)
  (lsp-warn-no-matched-clients nil)
  (lsp-session-file
   (file-name-concat config:emacs-local-dir ".lsp-session-v1"))
  (lsp-server-install-dir
   (file-name-concat (getenv "XDG_DATA_HOME") "lsp"))
  (lsp-progress-via-spinner t)
  (lsp-progress-spinner-type 'horizontal-breathing-long))

(use-package lsp-ui
  :init
  ;; Disable the original in order to use the sideline package
  (setopt lsp-ui-sideline-enable nil))

(use-package lsp-treemacs)

(use-package consult-lsp)

(use-builtin lsp-pwsh ; part of the lsp package
  :custom
  (lsp-pwsh-dir (file-name-concat (getenv "LOCALAPPDATA") "lsp" "pwsh"))
  (lsp-pwsh-ext-path (file-name-concat (getenv "LOCALAPPDATA") "lsp" "pwsh"))
  (lsp-pwsh-code-formatting-preset "OTBS")
  (lsp-pwsh-code-formatting-use-correct-casing t))

;;;; Directory Editor

(use-builtin dired
  :custom
  (dired-auto-revert t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-hide-details-mode t)
  (dired-listing-switches "-alh")
  :general
  (general-unbind 'normal dired-mode-map "SPC"))

(use-builtin dired-x
  :after dired)

(use-package dired-auto-readme
  :defines (dired-auto-readme-file)
  :config
  (dired-auto-readme-mode 1)
  (setq dired-auto-readme-file
        '("readme\\.\\(org\\|rst\\|md\\|markdown\\)")))

(use-package dired-gitignore
  :general
  (general-def
    dired-mode-map
    "h" 'dired-gitignore-global-mode))

(use-package dirvish
  :demand t
  :config
  (dirvish-override-dired-mode t)
  (dirvish-peek-mode t)
  (dirvish-side-follow-mode t)
  :custom
  (dirvish-attributes
   '(vc-state file-size git-msg subtree-state nerd-icons collapse file-time))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (vc-info yank index)))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (dirvish-header-line-height '(25 . 35))
  (dirvish-reuse-session nil)
  (dirvish-side-width 38)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-side-display-alist
   '((side . right) (slot . -1)))
  :general
  (leader-menu
    "0" '("Explorer" . dirvish-side)))

;;;; Projects

(use-package projectile
  :custom
  (projectile-dirconfig-file ".projectile")
  (projectile-dirconfig-comment-prefix ";")
  (projectile-project-search-path
   '( "~/projects"
      "~/repos"
      "~/.dotfiles/packages"))
  :general-config
  (leader-project-menu
   "/" '("ripgrep"   . projectile-ripgrep)
   "%" '("replace"   . projectile-replace)
   ">" '("terminal"  . projectile-run-term)
   "b" '("buffers"   . consult-projectile-switch-to-buffer)
   "E" '("Config" . projectile-edit-dir-locals)
   "f" '("find file" . projectile-find-file)
   "n" '("New"       . projectile-add-known-project)
   "o" '("occur"     . projectile-multi-occur)
   "p" '("commander" . projectile-commander)
   "s" '("toggle"    . projectile-toggle-between-implementation-and-test)
   )
  (add-leader-keys "Run" "r"
    :parent leader-project-menu
    "c" '("Compile" . projectile-compile-project)
    "t" '("Test"    . projectile-test-project)
    "r" '("Run"     . projectile-run-project)
    "i" '("Install" . projectile-install-project)

    )
  (add-leader-keys "Project list" "L"
    :parent leader-project-menu
    "c" '("Cleanup" . projectile-cleanup-known-projects))
  )

(use-package projectile-refactor
  :after projectile
  :load-path config:emacs-user-lisp-dir)

(use-package projectile-ripgrep
  :after projectile)

;;;; Editorconfig

(use-builtin editorconfig
  :config
  (editorconfig-mode 1))

;;;; Repositories

(use-builtin vc
  :general
  (leader-git-menu
    "v" '("Version control" . vc-prefix-map))
  )
(use-package magit
  :after (nerd-icons)
  :custom
  ((magit-format-file-function #'magit-format-file-nerd-icons))
  :custom-face
  (diff-added   ((t (:background "brown"))))
  (diff-removed ((t (:background "DarkOliveGreen"))))
  )

(use-package magit-todos)

(use-package magit-org-todos)

(use-package forge
  :defer t
  :init
  (setq forge-add-default-bindings nil))

(use-package consult-gh
  :after (consult)
  :custom
  (consult-gh-default-clone-directory
   (file-name-concat (getenv "HOME") "projects"))
  (consult-gh-show-preview                 t)
  (consult-gh-preview-buffer-mode          'org-mode)
  (consult-gh-preview-key                  "C-o")
  (consult-gh-repo-action                  #'consult-gh--repo-browse-files-action)
  (consult-gh-issue-action                 #'consult-gh--issue-view-action)
  (consult-gh-pr-action                    #'consult-gh--pr-view-action)
  (consult-gh-code-action                  #'consult-gh--code-view-action)
  (consult-gh-file-action                  #'consult-gh--files-view-action)
  (consult-gh-notifications-action         #'consult-gh--notifications-action)
  (consult-gh-dashboard-action             #'consult-gh--dashboard-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-prioritize-local-folder      'suggest)
  :config
  ;; Remember visited orgs and repos across sessions
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
  (consult-gh-enable-default-keybindings))

(use-package consult-gh-embark
  :after (consult-gh)
  :config
  (consult-gh-embark-mode 1))

(use-package consult-gh-forge
  :after (consult-gh)
  :delight
  :custom
  (consult-gh-forge-timeout-seconds 10)
  :config
  (consult-gh-forge-mode 1))

;;; Personal Information management

;;;; org

(use-builtin org
  :requires (f)
  :mode ("\\.org$" . org-mode)
  :defines (org-save-all-org-buffers)
  :preface

  (setopt org-directory (file-name-concat (getenv "HOME") "vaults" "org"))

  (defun visit-org-inbox ()
    "Load the inbox file."
    (interactive)
    (find-file org-default-notes-file))

  (defun org-mode-setup ()
    "This function sets up `org-mode' using the mode's hook."
    (outline-minor-faces-mode -1)
    (electric-pair-local-mode 1)
    (outline-minor-mode -1)
    ;; dont add pair for `less-than_symbol' in `org-mode'.  It messes up org-tempo
    (add-function :before-until electric-pair-inhibit-predicate
                  (lambda (c) (eq c ?<))))

  (defun org-agenda-nerd-icons (fun prefix alist)
    "Associate nerd-icons with org agenda categories.
    FUN is the icon function, PREFIX is the prefix of the icon name, and ALIST
    is the list of categories associated with the icon name (sans-prefix)"
    (mapcar (pcase-lambda (`(,category . ,icon))
              `(,category
                (,(funcall fun (concat prefix icon) :height 1.0))))
            alist))

  (defun org-convert-md-link ()
    "Convert the Markdown link at point into an Org-mode link."
    (interactive)
    (save-excursion
      ;; Search backward for the opening bracket
      (when (search-backward "[" nil t)
        (let ((start (point)))
          ;; Search forward for the closing parenthesis
          (when (search-forward ")" nil t)
            (let ((end (point)))
              (let ((md-link (buffer-substring-no-properties start end)))
                (when (string-match "\\[\\([^]]+\\)\\](\\([^)]*\\))" md-link)
                  (let ((text (match-string 1 md-link))
                        (url  (match-string 2 md-link)))
                    (delete-region start end)
                    (insert (format "[[%s][%s]]" url text)))))))))))

  (defun org-syntax-convert-keyword-case (&optional upper)
    "Convert the case of all keywords to lowercase by default unless UPPER is non-nil."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (case-fold-search nil))
        (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
          (unless (string-match-p "RESULTS" (match-string 0))
            (if upper
                (replace-match (downcase (match-string 0)) t)
              (replace-match (upcase (match-string 0)) t))
            (setq count (1+ count))))
        (message "Replaced %d occurances" count))))

  :hook (org-mode . org-mode-setup)

  :custom
;;;; locations ---------------------------------------------------------------
  (org-clock-persist-file
   (file-name-concat config:emacs-local-dir "org-clock-save.el"))
  (org-directory (file-name-concat (getenv "HOME") "vaults" "org"))

  (org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (org-archive-location
   (file-name-concat org-directory ".archive/%s_archive.org::"))

;;;; Agenda
  ;;    - NEW :: Needing triage. record a timestamp when leaving the `NEW' state
  ;;    - TODO :: Has been triaged and is available for work
  ;;    - WAIT :: Blocked until something happens. Record a note when entering
  ;;              and a timestamp when leaving
  ;;    - DONE :: No more work to do.  Record a timestamp
  ;;    - DROP :: No longer required, OBE or irrelevant.  Record a note
  (org-todo-keywords
   '((sequence "NEW(N/!)" "TODO(t)" "NEXT(n)" "WAIT(w@/!)")
     (sequence "MEET(m)")
     (sequence "STORY(s@/!)")
     (sequence "|" "DONE(d!)" "DROP(q@)")))

  (org-provide-todo-statistics t)

  (org-scheduled-delay-days 3)
  (org-deadline-warning-days 3)

  (org-habit-show-habits-only-for-today t)

  (org-agenda-category-icon-alist
   (append (org-agenda-nerd-icons
            #'nerd-icons-faicon
            "nf-fa-"
            '(("REVIEW" . "bullseye")
              ("ADMN"   . "terminal")
              ("BLOG"   . "pen_fancy")
              ("Meeting" . "group")
              ("THINK"  . "brain")))
           '(("" '(space . (:width (11)))))))



;;;; Log drawer
  (org-log-into-drawer "LOGBOOK")
  (org-log-note-headings
   '((done        . "Completed on %t")
     (state       . "State %-12s from %-12S %t")
     (note        . "Taken %t")
     (reschedule  . "New schedule from %S on %t")
     (delschedule . "Not scheduled, was %S on %t")
     (redeadline  . "New deadline from %S on %t")
     (deldeadline . "Removed deadline, was %S on %t")
     (refile      . "Refiled on %t")
     (clock-out   . "")))

;;;; Tags
  (org-group-tags t)
  (org-tag-alist
   ;; Contexts
   '(("vpn" . ?v)
     ("web" . ?w)
     ("home" . ?h)
     ("hnet" . ?n)
     ;; Flags
     ("review" . ?r)
     (:startgrouptag)
     ("app")
     (:grouptags)
     ("cli")    ; Command line / Terminal application
     ("svc")    ; an online service
     ("suite")  ; part of a suite of applications
     ("vpkg")   ; a vender package, extension, integration, or plugin
     (:endgrouptag)))

;;;; Format
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  ;; Hard indentation
  (org-startup-indented t)
  (org-adapt-indentation t)
  (org-hide-leading-stars t)
;;;; clocking tasks
  (org-clock-into-drawer t)
  (org-clock-persist 'history)
  (org-clock-out-switch-to-state "WIP") ; change todo state after clocking out
  (org-clock-mode-line-total 'today)

;;;; keybindings
  :general
  ;; TODO: I don't think that o is the right top level menu, is it?
  (make-leader-menu "Org-mode" "o"
    "a" 'org-agenda
    "c" 'org-capture
    "i" 'visit-org-inbox
    "p" 'org-insert-link
    "y" 'org-store-link)
  ;; This menu is under `major-mode-leader', see `config:emacs-local-leader-key'

  (major-mode-menu org-mode-map ; "top-level" in org-mode menu
    "a" '("Agenda" . org-agenda)
    ;; "c" reserved for org-roam-capture
    ;; "R" reserved for org-roam-refile
    "C" '("Capture" . org-capture))
  (nmap org-mode-map
    "C-<Tab>"  '("Cycle"     . org-cycle)
    "C-<Up>"   '("Move up"   . org-move-subtree-up)
    "C-<Down>" '("Move down" . org-move-subtree-down)
    "S-<Up>"   '("Go up"     . org-up-element)
    "S-<Down>" '("Go down"   . org-down-element))
  :config
  (org-clock-persistence-insinuate))

;;;; Faces & formatting

(use-package org-modern
  :custom-face
  (org-level-1 ((t (:height 1.07  :weight regular))))
  (org-level-2 ((t (:height 1.05 :weight regular))))
  (org-level-3 ((t (:height 1.02 :weight regular))))
  (org-level-4 ((t (:height 1.0  :weight regular :slant italic))))
  (org-level-5 ((t (:height 1.0  :weight regular))))
  (org-level-6 ((t (:height 1.0  :weight regular))))
  (org-level-7 ((t (:height 1.0  :weight regular))))
  (org-level-8 ((t (:height 1.0  :weight regular))))
  (org-level-9 ((t (:height 1.0  :weight regular))))
  :custom
  ;; stars
  ;; nil :: no styling
  ;; fold :: use `org-modern-fold-stars'
  ;; replace :: use `org-modern-replace-stars'
  (org-modern-star 'fold)
  (org-modern-replace-stars '("◉" "○" "●" "○" "●" "○" "●"))
  (org-modern-fold-stars '(("" . "")))

  (org-modern-hide-stars 'leading)

  ;; todo
  (org-modern-todo nil)
  (org-modern-todo-faces
   (quote (("NEW"   :foreground "CornflowerBlue")
           ("TODO"  :foreground "sienna")
           ("MEET"  :foreground "SandyBrown")
           ("STORY" :foreground "SandyBrown")
           ("WAIT"  :foreground "IndianRed")
           ("DONE"  :foreground "OliveDrab" :slant 'italic)
           ("DROP"  :foreground "DimGrey"   :slant 'italic))))
  ;; tags
  (org-modern-label-border 0.1)

  ;; timestamps
  ;; Fri 20 Dec 2024 11:15
  ;; note the space left and right of the timestamp
  (org-modern-timestamp '(" %a %d %b %Y " . " %H:%M "))

  ;; blocks
  (org-modern-block-fringe t)
  (org-modern-block-name
   '(("src"     . ("❰" "❱"))
     ("example" . ("»–" "–«"))
     ("quote"   . ("❝" "❞"))
     ("export"  . ("" ""))
     (t         . ("❰" "❱"))))

  ;; keywords
  (org-modern-keyword "")

  ;; tables
  ;; When I turn this on, the cursor gets hidden when I navigate into the table,
  ;; and it is difficult to create tables because it hides so much of the table
  ;; structure
  (org-modern-table nil)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.1)
  :config
  (global-org-modern-mode 1))

(use-package org-modern-indent
  :vc
  (:url "https://github.com/jdtsmith/org-modern-indent.git"
        :branch main))

;;;; Org Agenda

(use-builtin org
  :config
  ;; reset the list of agenda views
  (setq org-agenda-custom-commands '())

  (add-to-list
   'org-agenda-custom-commands
   '("A" "@Agenda"
     (( agenda ""
        ((org-agenda-overriding-header "Upcoming deadlines this week")
         (org-agenda-ndays 7)
         (org-deadline-warning-days 0)
         (org-agenda-start-on-weekday nil)
         (org-agenda-show-all-dates nil)
         (org-agenda-entry-types '(:deadline))
         (org-agenda-skip-deadline-if-done t)
         (org-agenda-warning-days 0)))
      ( agenda ""
        ((org-agenda-overriding-header "Items scheduled this week")
         (org-agenda-ndays 7)
         (org-agenda-start-on-weekday nil)
         (org-agenda-show-all-dates nil)
         (org-agenda-entry-types '(:scheduled))
         (org-habit-show-habits-only-for-today nil)))
      ( agenda ""
        ((org-agenda-overriding-header "One week overview")
         (org-agenda-ndays 7)
         (org-agenda-start-on-weekday nil)
         (org-agenda-repeating-timestamp-show-all t)
         (org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp))
         (org-deadline-warning-days 0))))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-01.CalendarForOneWeek.ps")))


  (add-to-list
   'org-agenda-custom-commands
   '("n" "@Next Actions"
     ((tags-todo "+errand-someday/NEXT"
                 ((org-agenda-overriding-header "@Errand")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      ( tags-todo "+email-someday/NEXT"
        ((org-agenda-overriding-header "@EMail")
         (org-agenda-tags-todo-honor-ignore-options t)
         (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+phone-someday/NEXT"
                 ((org-agenda-overriding-header "@Phone")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+web-someday/NEXT"
                 ((org-agenda-overriding-header "@Online")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+cac-someday/NEXT"
                 ((org-agenda-overriding-header "@PKI")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+office-someday/NEXT"
                 ((org-agenda-overriding-header "@Office")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+home-someday/NEXT"
                 ((org-agenda-overriding-header "@Home")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+laptop-someday/NEXT"
                 ((org-agenda-overriding-header "@Laptop")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "+vault-someday/NEXT"
                 ((org-agenda-overriding-header "@Vault")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))))
      (tags-todo "-CATEGORY=\"Someday\"-CATEGORY=\"REVIEW\"+TAGS=\"\"/NEXT"
                 ((org-agenda-overriding-header "@Uncategorized")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future)))))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-02.AllNextActions.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("u" "@Uncategorized Next Actions" tags-todo "-CATEGORY=\"Someday\"-CATEGORY=\"REVIEW\"+TAGS=\"\"/NEXT"
     ((org-agenda-overriding-header "@Uncategorized Next Actions")
      (org-agenda-todo-keyword-format "")
      (org-agenda-prefix-format "%b\n%i %-12c:")
      (org-agenda-tags-todo-honor-ignore-options t)
      (org-agenda-todo-ignore-scheduled '(future)))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-03.UncategorizedNextActions.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("p" "@Projects" tags-todo "-IGNORE-someday-wait/TODO"
     ((org-agenda-overriding-header "@Projects")
      (org-agenda-tags-todo-honor-ignore-options t)
      (org-tags-match-list-sublevels 'indented)
      (org-agenda-skip-function
       (lambda nil
         (org-agenda-skip-subtree-if (quote nottodo) '("NEXT")))))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-05.ProjectList.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("o" "@Overview"
     ((tags-todo "-IGNORE-someday-wait/TODO"
                 ((org-agenda-overriding-header "@Overview")
                  (org-agenda-tags-todo-honor-ignore-options t)
                  (org-agenda-todo-ignore-scheduled '(future))
                  (org-tags-match-list-sublevels nil)
                  (org-agenda-sorting-strategy '(category-up effort-up))
                  (org-agenda-remove-tags t)))
      (stuck ""))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-06.Overview.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("S" "@Someday/Maybe" tags-todo "+someday/TODO"
     ((org-agenda-overriding-header "Someday/Maybe")
      (org-agenda-prefix-format " - ")
      (org-agenda-todo-keyword-format ""))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-06.SomedayMaybeList.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("R" "@Review Commands"
     ("Ro" "@Review"
      (( tags-todo "REVIEW"
         ((org-agenda-skip-function  '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-overriding-header "Tasks Marked for Review")
          ;;don't list sub levels, just the one we marked
          (org-tags-match-list-sublevels nil)
          (org-agenda-sorting-strategy '(category-up))))
       ( tags "journal+REVIEW"
         ((org-agenda-overriding-header "Journal entries marked for Review")))
       ( tags "CATEGORY=\"REVIEW\"-TODO={[[:upper:]]+}|REVIEW-journal-TODO={[[:upper:]]+}" ;total hack to exclude all todo's
         ((org-agenda-overriding-header "Notes Marked for Review")))

       ( stuck ""))
      ((org-agenda-with-colors t)
       (ps-print-color-p 'black-white))
      ("~/paperPlanner/Agenda-Export/Page-07.TheReview.ps"))
     ("Rp" tags-todo "-someday+Effort=\"\"/NEXT"
      ((org-agenda-overriding-header "Actions that need planning")
       (org-agenda-view-columns-initially t)))))

  (add-to-list
   'org-agenda-custom-commands
   '("w" "@waiting" todo "WAIT"
     ((org-agenda-overriding-header "Waiting For"))
     ((org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-08.WaitingFor.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("l" "@Lists" tags "list"
     ((org-agenda-overriding-header "@Lists"))
     ((org-tags-match-list-sublevels nil)
      (org-agenda-with-colors t)
      (ps-print-color-p 'black-white))
     ("~/paperPlanner/Agenda-Export/Page-09.ListOfLists.ps")))

  (add-to-list
   'org-agenda-custom-commands
   '("y" "Next time im connected" tags "web-someday"
     ((org-agenda-overriding-header "While Connected")))))

;;;; Org refile & capture

(use-builtin org-refile
  :functions
  (org-save-all-org-buffers)
  :custom
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes t)
  (org-refile-targets '((nil :level . 1) ; top-level headings in current file
                        ( org-agenda-files :maxlevel . 2)))

  :config
  (advice-add 'org-refile :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-refile :after #'org-save-all-org-buffers))

(use-builtin org-id
  :custom
  (org-id-locations-file
   (file-name-concat config:emacs-local-dir ".org-id-locations"))
  (org-id-locations-file-relative nil) ; Use the full path to the id location
  (org-id-method 'uuid)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-builtin org-protocol)

(use-package org-context)

(use-builtin org-capture
  :after (org)
  :requires (journal)
  :custom
  (org-capture-use-agenda-date t)
  (org-capture-templates
   '(("t" "New task" entry (file org-default-notes-file)
      "* NEW %?\n%i\n")
     ("n" "New note" entry (file org-default-notes-file)
      "* %?\n")
     ("j" "Journal entry" entry (function journal-get-journal-file)
      "* %?\n%U"
      :empty-lines 1)
     ("w" "Web clip" entry (file org-default-notes-file)
      "* %:description    :web:\n :PROPERTIES:\n :CREATED: %u\n :END:
%:initial

- [[%:link][%:description]]"
      :empty-lines 1
      :immediate-finish 1)
     ("W" "Web clip with note" entry (file org-default-notes-file)
      "* %:description    :web:\n :PROPERTIES:\n :CREATED: %u\n :END:
  %:initial
  %?
  - [[%:link][%:description]]"
      :empty-lines 1
      :immediate-finish 1))))

;;;; org babel

(use-builtin org-src
  :custom
  (org-edit-src-turn-on-auto-save t)
  (org-edit-src-auto-save-idle-delay 5)
  (org-edit-src-persistent-message t)
  (org-src-preserve-indentation nil)
  (org-src-content-indentation 2)
  (org-src-fontify-natively t)
  (org-src-ask-before-returning-to-edit-buffer t)
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively t)
  :config
  (dolist (src-lang
           '(("c"          . c-ts)
             ("cmake"      . cmake-ts)
             ("dockerfile" . dockerfile-ts)
             ("elixir"     . elixir-ts)
             ("go"         . go-ts)
             ("heex"       . heex-ts)
             ("java"       . java-ts)
             ("json"       . json-ts)
             ("lua"        . lua-ts)
             ("php"        . php-ts)
             ("ruby"       . ruby-ts)
             ("rust"       . rust-ts)
             ("typescript" . typescript-ts)))
    (add-to-list 'org-src-lang-modes src-lang)))

(use-package ob-rust)

(use-package ob-powershell
  :custom
  (org-babel-powershell-os-command "pwsh.exe"))

(use-builtin ob
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (quote ((emacs-lisp . t)
           (python     . t)
           (powershell . t)
           (rust       . t)))))

;;;; templates

(use-builtin org-tempo
  :custom
  (org-tempo-keywords-alist '(("L" . "latex")
                              ("H" . "html")
                              ("A" . "ascii")
                              ("i" . "index")
                              ("t" . "title")))
  (org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("x" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("p" . "src powershell")
     ("l" . "src emacs-lisp")
     ("v" . "verse"))))

;;;; Org add-ons


(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :custom
  (org-recur-finish-done t)
  (org-recur-finish-archive t))

(use-package org-cliplink)

(use-package org-download
  :custom
  (org-download-method 'directory)
  (org-download-image-dir
   (file-name-concat org-directory "assets" "images"))
  (org-download-backend "curl")
  (org-download-screenshot-basename "")
  (org-download-timestamp "%Y-%m-%d-%H-%M-%S")
  (org-download-screenshot-method "imagemagick/convert"))

(use-package org-clock-today)

(use-package org-transclusion)

(use-package org-kanban)

(use-package org-ql)

;;;; Org-Roam

(use-package org-roam
  :custom
  (org-roam-list-files-commands '(fd elisp rg find))
  (org-roam-db-location (file-name-concat org-directory "db" "org-roam.db"))
  (org-roam-directory org-directory)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . left)
                 (window-width . 0.20)
                 (window-height . fit-window-to-buffer)))
  :general-config
  (imap
    org-mode-map
    "C-/" '("Insert node" . org-roam-node-insert))
  (major-mode-menu
    org-mode-map
    "v"   '("Roam buffer" . org-roam-buffer-toggle)
    "c"   '("Capture"     . org-roam-capture)
    "/"   '("Find node"   . org-roam-node-find)
    "C-i" '("Insert ID"   . org-id-get-create) ; will not insert if one exists already
    "R"   '("Refile"      . org-roam-refile)
    "x"   '("Extract"     . org-roam-extract-subtree)
    "i" '("Insert node"   . org-roam-node-insert))
  (major-mode-menu
    org-mode-map
    :infix "M-r"
    ""   '(:ignore t :which-key "Reference")
    "a"  '("Add"  . org-roam-ref-add)
    "/"  '("Find" . org-roam-ref-find)
    "d"  '("Delete" . org-roam-ref-remove)))

(use-builtin org-roam-node
  :preface
  (require 'org-roam)

  (defvar org-roam-node-name-append-id nil)

  (cl-defmethod org-roam-node-current-file ((node org-roam-node))
    "Gets node file-name-base by file name"
    (file-name-base (org-roam-node-file node)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Get the PKM type of NODE.  See `data.pkm.org' for details."
    (let* ((name (org-roam-node-current-file node))
           (parts (string-split name "\\.")))
      (when (>= (length parts) 1) (nth 0 parts))))

  (cl-defmethod org-roam-node-domain ((node org-roam-node))
    "Get the PKM domain of NODE.  See `data.pkm.org' for details."
    (let* ((name (org-roam-node-current-file node))
           (parts (string-split name "\\.")))
      (when (>= (length parts) 2) (nth 1 parts))))

  (cl-defmethod org-roam-node-subject ((node org-roam-node))
    "Get the PKM subject of NODE.  See `data.pkm.org' for details."
    (let* ((name (org-roam-node-current-file node))
           (parts (string-split name "\\.")))
      ;; I know it is less efficient to call each of the functions in the body
      ;; of the conditions below, but i feel like it makes it more explicit to
      ;; show that the \"depth\" of the hierarchy determines the subject.
      (cond
       ((length= parts 1)
        (org-roam-node-type node))
       ((length= parts 2)
        (org-roam-node-domain node))
       ((length> parts 3)
        (last parts)))))

  (cl-defmethod org-roam-node-subtitle ((node org-roam-node))
    "Get the subtitle for the NODE or ORG-ROAM-NODE."
    (cdr (assoc-string "SUBTITLE" (org-roam-node-properties node))))

  (cl-defmethod org-roam-node-name ((node org-roam-node))
    "Create a name rather than a slug"
    (let ((title (org-roam-node-title node)))
      (dolist (pair '(("[ \t]" . ".")
                      ("[^[:alnum:] _-]" . "")))
        (setq title (string-replace (car pair) (cdr pair) title)))
      (downcase title)))

  (cl-defmethod org-roam-node-parent ((node org-roam-node))
    "Determine the parent of the current NODE or ORG-ROAM-NODE."
    (let* ((name (org-roam-node-current-file node))
           (parts (string-split name "\\.")))
      (if (length> parts 1 )
          (string-join (butlast parts) ".")
        "root")))

  (cl-defmethod org-roam-node-parent-file ((node org-roam-node))
    "Determine the parent file name of the current node."
    (let* ((dir (file-name-directory (org-roam-node-file node)))
           (parent (org-roam-node-parent node))
           (ext "org"))
      (when parent
        (file-name-concat dir (string-join '(parent ext) ".")))))

  (defun org-roam-convert-to-link (&optional arg)
    "Replace word at point with an Org-roam link.
Without prefix ARG: look up node by title/alias, or create one if missing.
With prefix ARG prompt with `completing-read` to choose a node."
    (interactive "P")
    (let* ((word (thing-at-point 'word t)))
      (delete-region (beginning-of-thing 'word)
                     (end-of-thing 'word))
      (cond
       ;; With prefix: let user choose
       (arg
        (let ((node (org-roam-node-read)))
          (insert (org-roam-node-insert node))))
       ;; Without prefix: auto lookup or create
       (t
        (let ((node (org-roam-node-from-title-or-alias word)))
          (if node
              (insert (org-roam-node-insert node))
            (let ((new-node (org-roam-capture-
                             :node (org-roam-node-create :title word)
                             :props '(:immediate-finish t))))
              (insert (org-roam-node-insert new-node)))))))))
  :custom
  (org-roam-link-auto-replace t) ; replace roam: with id:

  (org-roam-complete-everywhere t)
  (org-roam-node-display-template
   (concat "${title:24} (${file:24}) "
           (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-extract-new-file-path "${name}.org"))

(use-builtin org-roam-capture
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file "${name}.org")
      :unnarrowed t))))

(use-package org-roam-dailies
  :ensure nil
  :custom
  (org-roam-dailies-directory "journals/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target
      (file+head "%<%Y-%m-%d>.org"
                 "#+TITLE: Journal: %<%b %d %Y (%a)>\n#+SUBTITLE: Daily Journal\n\n")
      :unnarrowed t)))
  :pretty-hydra
  (hydra-roam-journal-nav
   (:color red :hint nil :title "Journal" :quit-key "q")
   ("Navigate Journal"
    (("g" org-roam-dailies-goto-date "Date")
     ("n" org-roam-dailies-goto-next-note "Next")
     ("p" org-roam-dailies-goto-previous-note "Previous")
     ("y" org-roam-dailies-goto-yesterday "Yesterday")
     ("." org-roam-dailies-goto-today "Tomorrow")
     ("T" org-roam-dailies-goto-tomorrow "Tomorrow"))))
  :general-config
  (major-mode-menu org-mode-map
    :infix "C-j"
    ""  '(:ignore t :which-key "Journal")
    "." '("Today" . org-roam-dailies-capture-today)
    "g" '("Goto"  . hydra-roam-journal-nav/body)))

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :general-config
  (major-mode-menu
    org-mode-map
    "V"   '("Roam UI" . org-roam-ui-open)))

;; Used to list ids, etc. using org-mem
(use-package inspector)

(use-package org-mem
  :preface
  (defun my-set-agenda-files (&rest _)
    (setq org-agenda-files
          (cl-loop
           for file in (org-mem-all-files)
           unless (string-search "archive" file)
           when (seq-find (lambda (entry)
                            (or (org-mem-entry-active-timestamps entry)
                                (org-mem-entry-todo-state entry)
                                (org-mem-entry-scheduled entry)
                                (org-mem-entry-deadline entry)))
                          (org-mem-entries-in file))
           collect file)))
  :hook (org-mem-post-full-scan-functions . my-set-agenda-files)
  :custom
  (org-mem-watch-dirs '("~/vaults/org"))
  (org-mem-do-sync-with-org-id t)
  (org-roam-db-update-on-save nil)
  (org-mem-roamy-do-overwrite-real-db t)
  :config
  (org-mem-updater-mode)
  (org-mem-roamy-db-mode))

;;;; Deft

(use-package deft
  :custom
  ;; Selecting files
  (deft-directory (file-name-as-directory org-directory))
  ;; The first element in this list will be the `deft-default-extension'
  ;; when deft is `required'
  (deft-extensions '("org" "md"))
  (deft-generation-rules '(("org" . "tex")))
  (deft-recursive nil)
  (deft-archive-directory ".archive/")
  (deft-current-sort-method 'title)
  (deft-incremental-search t)
  (deft-case-fold-search t)
  (deft-file-limit 40)
  (deft-recursive-ignore-dir-regexp
   (concat "\\(?:" "^\\..*" "\\)$"))
  (deft-ignore-file-regexp
   (concat "\\(?:" "^\\..*" "\\)"))
  ;; Deft buffer display
  (deft-strip-title-regexp
   (concat "\\(?:"
           "^%+"
           "\\|^#\\+title:[\t ]*"
           "\\|^[#* ]+"
           "\\|-\\*-[[:alpha:]]+-\\*-"
           "\\|^Title:[\t  ]*"
           "\\|#+$"
           "\\)"))
  ;; I don't want to exclude all keywords
  ;; "\\|^#\\+[[:upper:]_]+:.*$"
  (deft-strip-summary-regexp
   (concat "\\("
           "[\n\t]" ; blank lines
           ;; Strip the whole line of these
           "\\|^#\\+" ; start of property line
           "\\(?:"
           "setupfile"
           "\\|startup"
           "\\|title"
           "\\|property"
           "\\|index"
           "\\|link"
           "\\|category"
           "\\|filetags"
           "\\|id"
           "\\|updated"
           "\\|created"
           "\\)"
           ":.*$" ; end of property line

           ;; Properties block
           "\\|^:PROPERTIES:\\(.\\|\r?\n\\)+:END:.*$"

           ;; strip the keyword but not the
           ;; content
           "\\|^#\\+desc: "
           "\\|^#\\+description: "
           "\\|^#\\+subtitle: "
           "\\|^#\\+summary: "
           ;; blocks
           "\\|^[\t ]*#\\+\\(?:" ; start of property line
           "\\|begin_"
           "\\|end_"
           "\\).*$"
           ;; any line starting with something other than #
           "\\|^[^#].*$"
           "\\)"))
  ;; Creating new files
  (deft-file-naming-rules '((noslash . ".")
                            (nospace . ".")
                            (case-fn . downcase)))
  (deft-auto-save-interval 60) ; when idle for 1 minute
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-org-mode-title-prefix t)
  (deft-time-format "%Y.%m.%d")

  :general
  (leader-app-menu "d" '("Deft" . deft))
  (major-mode-menu 'deft-mode-map
    ;; Filtering
    "C-l" 'deft-filter
    "C-c" 'deft-filter-clear
    "C-y" 'deft-filter-yank
    ;; File creation
    "n" 'deft-new-file
    "N" 'deft-new-file-named
    "<C-return>" 'deft-new-file-named
    ;; File management
    "D" 'deft-delete-file
    "R" 'deft-rename-file
    "f" 'deft-find-file
    "C-a" 'deft-archive-file
    ;; Settings
    "C-t" 'deft-toggle-incremental-search
    "s" 'deft-toggle-sort-method
    ;; Miscellaneous
    "g" 'deft-refresh
    "q" 'quit-window
    "<tab>" 'forward-button
    "<backtab>" 'backward-button
    "<S-tab>" 'backward-button
    "C-o" 'deft-open-file-other-window))


;;; Finalize

(use-feature ; load custom file
  :init
  ;; Put customizations into their own file
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (unless (file-exists-p custom-file)
    (with-temp-file custom-file
      (insert "(custom-set-variables)")))
  (load custom-file))

;;; init.el ends here
