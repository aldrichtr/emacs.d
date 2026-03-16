;;; init.el --- My emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Preliminaries

;;;; User Information

(setopt
 user-full-name "Timothy R. Aldrich"
 user-mail-address "timothy.r.aldrich@gmail.com")

;;;; Config options
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-options)

;;;; Package system

(require 'package-macros)

(require 'package-metadata)

;;;; Keybinding system

(require 'leader-key-system)

;;;; Dependencies

(dolist (pkg '(f s ts dash buttercup))
  (eval `(use-package ,pkg
           :ensure t
           :defer nil)))

;;; Colors & Themes

;;;; Themes

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

(use-package doric-themes)

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
  :defer nil
  :custom
  (custom-theme-directory config:emacs-custom-themes-dir)
  :init
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
    "<enter>"   'evil-multiedit-toggle-or-restrict-region)
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


;;;; Which-key

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

;;;; Leader-Menu system

(use-builtin config-keybindings
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
     ;;("g")
     ("h" "help"    "Help")
     ("p" "project")
     ("t" "toggle")
     ("v" "git" "Version Control")
     ("w" "window")
     ("x" "text"    "Text")
     ;; uppercase
     ("F" "frame")
     ("P" "package")
     ("V" "view")
     ("Z" "quit"    "Quit")))
  :config
  (leader-key-menu-initialize))

;;; Disaster Recovery

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

;;;; Recent files list

(use-builtin recentf
  :defer 0.1
  :custom
  (recentf-save-file config:emacs-recent-files-file)
  :config
  (recentf-mode))

;;;; Auto save and backups

(use-builtin files
  :demand t
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
   (file-name-concat config:emacs-auto-save-dir "sessions" ".saves-"))
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
    ;; TODO: This should move to the dirvish config
    "d"   '("Dired"          . dirvish)
    "D"   '("Delete file"    . delete-file-and-buffer)
    "f"   '("Find file"      . find-file)
    "M-r" '("Rename file"    . rename-visited-file)
    ;; TODO: These should move to the evil config
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
    "Setup the package list buffer."
    (hl-line-mode 1))
  :hook (package-list-mode . package-list-mode-setup)
  :general
  (leader-package-menu
    "l" '("List packages" . package-list-packages)
    "L" '("List packages(no fetch)" . package-list-packages-no-fetch)
    "R" '("Report" . use-package-report)))

;;;; Projects


(use-builtin files
  :defer nil
  :preface
  (add-to-list 'safe-local-variable-values
               '(compilation-read-command . nil)))

(use-package fancy-compilation
  :preface
  (defun compilation-mode-setup ()
    "Setup compilation mode.  Add the ability to render ascii color"
    (fancy-compilation-mode))
  :hook (compilation-mode . compilation-mode-setup))


(use-package projectile
  :custom
  (projectile-dirconfig-file ".projectile")
  (projectile-known-projects-file (config/emacs-local-dir "projectile-bookmarks.eld"))
  (projectile-dirconfig-comment-prefix ";")
  (projectile-completion-system 'default)
  (projectile-per-project-compilation-buffer t)
  (projectile-project-search-path
   '( "~/projects"
      "~/repos"
      "~/.dotfiles/packages"))
  :config
  ;; this ensures projectile and project play well together
  (add-hook 'project-find-functions #'project-projectile)
  (projectile-mode 1)

  :general
  (leader-project-menu
    "/" '("ripgrep"     . projectile-ripgrep)
    "%" '("replace"     . projectile-replace)
    ">" '("terminal"    . projectile-run-term)
    "b" '("buffers"     . consult-projectile-switch-to-buffer)
    "E" '("Edit config" . projectile-edit-dir-locals)
    "f" '("find file"   . projectile-find-file)
    "o" '("occur"       . projectile-multi-occur)
    "p" '("commander"   . projectile-commander)
    "s" '("toggle"      . projectile-toggle-between-implementation-and-test)
    "q" '("Quit"        . projectile-kill-buffers))

  (add-leader-keys "Run" "r"
    :parent leader-project-menu
    "c" '("Compile" . projectile-compile-project)
    "t" '("Test"    . projectile-test-project)
    "r" '("Run"     . projectile-run-project)
    "i" '("Install" . projectile-install-project))

  (add-leader-keys "Project list" "L"
    :parent leader-project-menu
    "s" '("Switch"  . projectile-switch-project)
    "c" '("Cleanup" . projectile-cleanup-known-projects)))

;;;; Repository

(use-package magit
  :after (nerd-icons)
  :custom
  ((magit-format-file-function #'magit-format-file-nerd-icons))
  :custom-face
  (diff-added   ((t (:background "brown"))))
  (diff-removed ((t (:background "DarkOliveGreen"))))
  :general
  (leader-git-menu
    "s" '("Status" . magit-status)))

;; This shows TODO comments in the magit status buffer
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

;; If there is a `todo.org' file in the root, this will show todos in the magit status buffer

(use-package magit-org-todos
  :after magit
  :config
  (config-org-todos-autoinsert))

(use-package forge
  :after magit
  :custom
  (auth-sources (file-truename "~/.secrets/authinfo.gpg"))

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
    (consult-gh-notifications-show-unread-only t)
    :config
    ;; Remember visited orgs and repos across sessions
    (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
    (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
    :general
    (add-leader-keys "h" "Github"
      :parent leader-git-menu
      "L"  '("Login" . consult-gh-auth-switch)
      "d"  '("Dashboard" . consult-gh-dashboard)
      "n"  '("Notifications" . consult-gh-notifications)
      ;;--------------------------------------
      "w"  '(:ignore t :which-key "Workflows")
      "wl" '("List"    . consult-gh-workflow-list)
      "wc" '("Create"    . consult-gh-workflow-create)
      "we" '("Edit"    . consult-gh-workflow-edit)
      ;;--------------------------------------
      "wr" '(:ignore t :which-key "Run")
      "wra" '("Run Action"    . consult-gh-workflow-run)
      "wrl" '("List"   . consult-gh-run-list)
      "wrv" '("View"   . consult-gh-run-view)
      "wrR" '("ReRun"   . consult-gh-run-rerun)
      ;;--------------------------------------
      "i"  '(:ignore t :which-key "Issues")
      "i/" '("Search"  . consult-gh-search-issues)
      "il" '("List"    . consult-gh-issue-list)
      "ic" '("Create"  . consult-gh-issue-create)
      "iC" '("Close"   . consult-gh-issue-close)
      "ie" '("Edit"    . consult-gh-issue-edit)
      "iL" '("Lock"    . consult-gh-issue-lock)
      ;;--------------------------------------
      "p"  '(:ignore t :which-key "PRs")
      "p/" '("Search"  . consult-gh-search-prs)
      "pl" '("List"    . consult-gh-pr-list)
      "pc" '("Create"  . consult-gh-pr-create)
      "pC" '("Close"   . consult-gh-pr-close)
      "pe" '("Edit"    . consult-gh-pr-edit)
      ;;--------------------------------------
      "r"  '(:ignore t :which-key "Repos")
      "r/" '("Search" . consult-gh-search-repos)
      "rl" '("List"   . consult-gh-repo-list)
      "rc" '("Clone"  . consult-gh-repo-clone)
      "rf" '("Fork"   . consult-gh-repo-fork)
      "rn" '("New"    . consult-gh-repo-create)
      "rm" '("Mine"   . consult-gh-user-repos)
      ;;--------------------------------------
      "f"  '(:ignore t :which-key "Files")
      "fc" '("Create" . consult-gh-create-file)
      "fe" '("Edit"   . consult-gh-edit-file)
      "fr" '("Rename" . consult-gh-rename-file)
      "fd" '("Dired"  . consult-gh-dired)
      "fD" '("Delete" . consult-gh-delete-file)
      "fu" '("Upload" . consult-gh-upload-files)
      ;;--------------------------------------
      "/"  '(:ignore t :which-key "Search")
      "/r" '("Repos" . consult-gh-search-repos)
      "/i" '("Issues"   . consult-gh-search-issues)
      "/p" '("PRs"   . consult-gh-search-prs)
      "/f" '("Files" . consult-gh-find-file)
      "/c" '("Code"   . consult-gh-search-code)
      "/m" '("Commits"   . consult-gh-search-commits))))

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

;;;; File & Directory manager

(use-builtin dired
  :custom
  (dired-auto-revert t)
  (dired-kill-when-opening-new-dired-buffer t "Dont create buffers for each directory")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-dwim-target t)
  ;; Use the mouse with dired
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program)
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
  :init (dirvish-override-dired-mode)
  :custom
  ;; dirvish main window
  (dirvish-attributes
   '(vc-state file-size git-msg subtree-state nerd-icons collapse file-time))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (vc-info yank index)))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (dirvish-header-line-height '(25 . 35))
  (dirvish-reuse-session nil "Kill all dired buffers when quitting")
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; dirvish side window
  (dirvish-side-attributes '(nerd-icons collapse subtree-state))
  (dirvish-side-width 38)
  (dirvish-side-display-alist
   '((side . right) (slot . -1)))
  :general
  (leader-menu
    "0" '("Explorer" . dirvish-side))
  :config
  (dirvish-override-dired-mode t)
  (dirvish-peek-mode t)
  (dirvish-side-follow-mode t))

;;;;; Treemacs

(use-package treemacs
  :functions (treemacs-load-theme treemacs-fringe-indicator-mode)
  :custom
  (treemacs-persist-file
   (file-name-concat config:emacs-local-dir ".cache" "treemacs" "persist"))
  (treemacs-last-error-persist-file
   (file-name-concat config:emacs-local-dir ".cache" "treemacs" "last-error"))
  (treemacs-position 'right)
  (treemacs-is-never-other-window t)
  (treemacs-hide-dot-git-directory t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode nil)
  :config
  (treemacs-fringe-indicator-mode 'only-when-focused)
  (treemacs-load-theme "nerd-icons")
  (treemacs-git-mode 'extended)
  :general
  (leader-menu
    "]" '("Treemacs" . treemacs))
  (major-mode-menu treemacs-mode
    "w" '("Workspace" . treemacs-workspace-map)
    "p" '("Project"   . treemacs-project-map)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons))

(use-package treemacs-evil
  :after (treemacs evil))

;;; Emacs Application

(use-feature ; emacs
  :custom
  (yes-or-no-prompt "(y)es or n(o) ")
  (use-short-answers t))

(use-builtin server
  :functions (server-running-p)
  :defines (config:emacs-server-run)
  :config
  (unless (and (server-running-p)
               config:emacs-server-run)
    (add-hook 'after-init-hook 'server-start t)))


(use-builtin filesystem-x)

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
  (leader-quit-menu
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
    "q" '("off"      . menu-bar--display-line-numbers-mode-none)))

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
  :custom
  ;; We want the window manager to control the window (frame) size
  (frame-inhibit-implied-resize t)
  (frame-title-format
   '(""
     invocation-name
     " " emacs-version
     " - "
     user-login-name " - "
     (:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))))))
  :init
  (set-face-attribute 'default nil
                      :background "#000000")
  (set-frame-font config:emacs-font nil t)
  (add-to-list 'initial-frame-alist
               `(font . ,config:emacs-font))
  (add-to-list 'default-frame-alist
               `(font . ,config:emacs-font))
  :general
  (leader-frame-menu
    "f" '("New" . make-frame-command)
    "c" '("Clone" . clone-frame)
    "q" '("Quit" . delete-frame)))

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
            (not (file-name-extension name))))))

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
    "q" '("Quit"       . centaur-tabs-kill-all-buffers-in-current-group)))


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
  (global-menu
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
  (aw-ignored-buffers '(which-key-mode))
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
    "<enter>" '("Jump to"     . ace-window)
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
    "q"   '("Quit"   . evil-quit))

    ;; Tabs
    ;; T          tab-window-detach
    ;; g T        tab-bar-switch-to-prev-tab
    ;; g t        evil-tab-next

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

(use-feature ; buffer indenting
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
             (prog-fill-reindent-defun)))))
  :general
  (leader-menu
    "=" '("Indent buffer" . buffer-indent)))

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
  (default ((t (:background "gray23"))))
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

;;;; Outlines

(use-builtin outline
  :functions (outline-font-lock-fontify-region)
  :preface
  (defvar outline-display-table (make-display-table))
  (set-display-table-slot outline-display-table 'selective-display
                          (vector (make-glyph-code ?► 'escape-glyph)))
  (defun set-outline-display-table ()
    "Add glyph to display table."
    (setf buffer-display-table outline-display-table))
  (defun outline-mode-setup ()
    "Setup the outline(-minor)? mode."
    (set-outline-display-table)
    (jit-lock-unregister #'outline-font-lock-fontify-region))
  :custom
  (outline-blank-line t)
  (outline-minor-mode-highlight t)
  (outline-default-state 2) ;; default state is folded at level 2
  :general
  (nmap
    :prefix "z"
    "n" 'outline-forward-same-level
    "p" 'outline-backward-same-level))

(use-package backline
  ;; backline is used in conjunction with `outline-minor-mode-highlight' to extend
  ;; the highlight of the outline to the right edge of the window
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package outline-minor-faces
  :after outline
  :hook
  (outline-minor-mode . outline-minor-faces-mode))

(use-package outshine)

(use-package outorg)

(use-package navi-mode)

;;;; Structure

(use-builtin treesit
  :defines
  (config:emacs-treesitter-grammar-dir)
  :init
  (setopt
   treesit-font-lock-level 4 ; 4 is the max level
   treesit-extra-load-path
   (list
    (file-truename config:emacs-treesitter-grammar-dir)))
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

(use-builtin simple
  :defer nil
  :requires (ws-butler)
  :preface
  (defun fundamental-mode-setup ()
    "Define things that should be set in all modes."
    (setq-default tab-width 2
                  fill-column 80
                  tab-always-indent 'complete)
    (ws-butler-mode)
    (display-line-numbers-mode 1)
    (display-fill-column-indicator-mode 1)
    (auto-insert-mode 1)
    (auto-fill-mode 1)))

(use-builtin text-mode
  :defer nil
  :preface
  (defun text-mode-setup ()
    "Function to add commands to `text-mode-hook'."
    (fundamental-mode-setup))
  :hook (text-mode . text-mode-setup))

(use-builtin prog-mode
  :defer nil
  :preface
  (defun prog-mode-setup ()
    "Function to add commands to `prog-mode-hook'."
    (fundamental-mode-setup))
  :hook (prog-mode . prog-mode-setup))



(use-builtin json-mode
  :preface
  (defun json-mode-setup ()
    "Setup the json mode.")
  :hook (json-mode . json-mode-setup))


;;; Terminals & Shells

(use-builtin term
  :custom
  (term-scroll-to-bottom-on-output 'this)
  (term-input-ring-file-name
   (file-name-concat config:emacs-local-dir "term-history.log")))

(use-package multi-term)

(use-package toggle-term
  :general
  (global-menu
    "C-`" '("Toggle term" . toggle-term-shell)))

(use-package term-projectile)

;; (use-builtin shell
;;   :defines (explicit-pwsh.exe-args)
;;   :defer t
;;   :config
;;   (when (os-windows-p)
;;     (setq explicit-shell-file-name (executable-find "pwsh")
;;           ;;shell-file-name "pwsh.exe"
;;           explicit-pwsh.exe-args '("-NoLogo" "-NoProfile" "-Interactive")
;;           shell-prompt-pattern "PS .*>$"))
;;   :general-config
;;   (leader-shell-menu
;;     "p" '("Powershell" . shell)
;;     "e" '("Emacs shell" . eshell)))

(use-builtin ielm
  :custom
  (ielm-history-file-name (file-name-concat config:emacs-local-dir
                                            "ielm-history.eld"))
  (ielm-prompt "λ")

  :general-config
  (leader-shell-menu
    "i" '("IELM" . ielm))

  (general-def ielm-map
    "C-<enter>" 'ielm-return
    "<enter>" 'ielm-return-for-effect))

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
  ;; in order to make this work, I needed to compile it myself
  ;; gh clone justinbarclay/parinfer-rust
  ;; cd parinfer-rust
  ;; cargo build --release --features emacs
  ;; Then copy from `target' to `~/.config/emacs/parinfer-rust/parinfer-rust-windows.dll'
  ;; Although there are customization options for that path, they dont work and
  ;; the library only looks to that path.
  :requires (flycheck)
  :preface
  (defun parinfer-rust-mode-setup ()
    "Setup the parinfer-rust-mode.")
  :hook (clojure-mode . parinfer-rust-mode)
        (emacs-lisp-mode . parinfer-rust-mode)
  :custom
  (parinfer-rust-disable-troublesome-modes t)
  :config
  (when (flycheck-running-p)
    (flycheck-add-next-checker 'emacs-lisp 'parinfer-rust t))
  :general-config
  (make-leader-menu "p" "Parinfer"
    :parent leader-toggle-menu
    "p" '("Toggle paren mode" . parinfer-rust-toggle-paren-mode)
    "m" '("Switch mode"       . parinfer-rust-switch-mode)
    "q" '("Disable"           . parinfer-rust-toggle-disable)))

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
    "." '("At point" . helpful-at-point)
    "f" '("Function" . helpful-function)
    "c" '("Command" . helpful-command)
    "v" '("Variable" . helpful-variable)
    "k" '("Key" . helpful-key)
    "s" '("Symbol" . helpful-symbol)
    "q" '("Quit help" . helpful-kill-buffers)))

(use-package info-colors
  :hook (Info-selection-hook . info-colors-fontify-node))


;;;; Transient menu system

(use-package transient)

;; An opinionated menu system built on top of `transient'

(use-package casual)

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
  :preface
  (defun surround-with-parens ()
    "Insert parens around the current thing."
    (interactive)
    (surround-insert "("))

  (defun surround-with-double-quotes ()
    "Insert quotes around the current thing."
    (interactive)
    (surround-insert "\""))

  (defun surround-with-curly ()
    "Insert quotes around the current thing."
    (interactive)
    (surround-insert "{"))
  :general
  (global-map
   "C-'"   '("Mark"       . surround-mark)
   "M-'"   '("Insert"     . surround-insert)
   "C-M-'" '("Surround"   . surround-keymap)
   "M-("   '("Add Parens" . surround-with-parens)
   "M-{"   '("Add Curly"  . surround-with-parens)
   "M-\""  '("Add Quotes" . surround-with-double-quotes)))

(use-package drag-stuff
  :delight
  :config
  (drag-stuff-global-mode t)
  :general
  (global-menu
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
    "<enter>"       'vertico-directory-enter)
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
  (leader-help-menu
    "i" '("Info" . consult-info))
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
  (global-menu
    "C-/" '("Search" . consult-rg-in-project)))


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
  :defer nil
  :general
  (global-menu
    "C-<enter>" '("Embark list"       . embark-act)
    "M-."   '("embark"            . embark-dwim)
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
  (global-menu "M-c" '("Cape" . cape-prefix-map)))


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

(use-package yasnippet
  :demand t
  :functions (yas-expand-snippet yas--parse-template)
  :defines (config:emacs-snippets-dir)
  :init
  (setq yas-snippet-dirs `(,config:emacs-snippets-dir))
  :config
  (yas-global-mode 1)
  :general
  (global-menu
    "M-/" '("Select snippet" . consult-yasnippet)))

(use-builtin autoinsert
  :demand t
  :requires (yasnippet)
  :preface
  (defun insert/expand-snippet ()
    "Expand the snippets in the current buffer.
This function is used by `auto-insert-mode' to expand snippets in the
template file."
    (let ((current (buffer-name (current-buffer))))
      ;; Don't bump heads with capture templates
      (unless (string-match "^CAPTURE" current)
        (yas-expand-snippet (nth 1 (yas--parse-template)) (point-min) (point-max)))))
  :custom
  (auto-insert 'other)
  (auto-insert-query nil)
  (auto-insert-directory config:emacs-templates-dir)
  (auto-insert-alist
   '((("\\.el\\'" . "Emacs Lisp") . ["template.el" insert/expand-snippet])
     (("\\.clj\\'" . "Clojure") . ["template.clj"  insert/expand-snippet])
     (("\\.org\\'" . "org-mode") . ["template.org" insert/expand-snippet])))
  :config
  (auto-insert-mode 1))

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
  :after (flycheck hl-todo)
  :config
  (flycheck-hl-todo-setup))

(use-package consult-todo)

;;; Language Server

(use-package lsp-mode
  :disabled
  :hook (powershell-ts-mode . lsp)
  (lsp-mode lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

;;; Filetypes & modes

;;;; Emacs-lisp

(use-builtin elisp-mode
  :preface
  (defun emacs-lisp-mode-setup ()
    "Setup `emacs-lisp-mode'"
    (outline-minor-mode 1)
    ;; When inside a `:custom' section, variables are not shown like they are when
    ;; you type setq <tab>, so this adds them in, and because we use nerd-icons,
    ;; we can easily distinguish the vars from the functions
    ;; see: https://github.com/jwiegley/use-package/issues/1077
    ;; WARNING: Affects all emacs-lisp completions
    (setq-local completion-at-point-functions
                (list (cape-capf-inside-code #'cape-elisp-symbol))))
  :hook (emacs-lisp-mode . emacs-lisp-mode-setup)
  :hook (before-save . check-parens)
  :general
  (major-mode-menu emacs-lisp-mode-map
    "e"  '(:ignore t :which-key "Eval")
    "eb" '("Buffer" . eval-buffer)
    "ed" '("Defun"  . eval-defun)
    "er" '("Region" . eval-region)
    "T" '("ielm REPL" . ielm)))

;;;; Clojure

(use-package clojure-mode
  :custom
  ;; `always-align' is the community style-guide recommended indentation
  (clojure-indent-style 'always-align)
  (clojure-indent-keyword-style 'always-align)
  (clojure-use-backtracking-indent t)
  (clojure-docstring-fill-column 110)
  (clojure-docstring-fill-prefix 2)
  (clojure-build-tool-files
   '("project.clj"
     "deps.edn" "build.clj"
     "bb.edn" "nbb.edn"
     "deps-clr.edn"))
  (clojure-preferred-build-tool "deps.edn"))

(use-package cider
  :custom
  (cider-jack-in-default 'clj)
  :general
  (major-mode-menu clojure-mode-map
    "<tab>" '("Complete" . complete-symbol)
    "?"     '("Selector" . cider-selector)
    ;; Goto references
    "g"     '(:ignore t :which-key "Goto")
    "gd"    '("Definition"    . cider-find-var)
    "gn"    '("Namespace"     . cider-find-ns)
    "gk"    '("Keyword"       . cider-find-keyword)
    "gr"    '("Resource"      . cider-find-resource)
    "gR"    '("Referenced"    . cider-xref-fn-refs)
    "g M-r" '("Referenced by" . cider-xref-fn-refs-select)
    "gD"    '("Depends"       . cider-xref-fn-deps)
    "g M-d" '("Depends on"    . cider-xref-fn-deps-select)
    ;; Return from goto
    "]"     '("Pop back"      . cider-pop-back)
    ;; Docs
    "d"     '("Docs"     . cider-doc-map)
    "D"     '("Describe conn" . cider-describe-connection)


    ;; Evaluation
    "e"     '("Eval"        . cider-eval-commands-map)
    ";"     '("To comment"  . cider-eval-defun-to-comment)
    "i"     '("Insert"      . cider-insert-commands-map)
    "j"     '(:ignore t :which-key "Jack in")
    "ji"     '("Jack in"       . cider-jack-in)
    "jc"     '("Clj"       . cider-jack-in-clj)
    "js"    '("Cljs"     . cider-jack-in-cljs)
    "r"     '(:ignore t :which-key "Repl")
    "re"    '("Eval"        . cider-eval-last-sexp-to-repl)
    "ri"    '("Insert"      . cider-insert-last-sexp-in-repl)
    "rr"    '("Switch to"   . cider-switch-to-repl-buffer)
    "rc"    '("Clear"       . cider-find-and-clear-repl-output)
    "rb"    '("Load buffer" . cider-load-buffer-and-switch-to-repl-buffer)
    "R"     '("Read & eval" . cider-read-and-eval)
    ;;
    "u"     '("Undef"            . cider-undef)
    "U"     '("Undef All"        . cider-undef-all)
    "m"     '("Macro expand"     . cider-macroexpand-1)
    "M"     '("Macro expand all" . cider-macroexpand-all)
    "n"     '("Namespace"        . cider-ns-map)
    "i"     '("Inspect"          . cider-inspect)
    ;; Tracing
    "T"     '(:ignore t :which-key "Trace")
    "Tv"    '("Var"       . cider-toggle-trace-var)
    "Tn"    '("Namespace" . cider-toggle-trace-ns)
    "p"     '("Profile"   . cider-profile-map)
    ;; Load
    "L"     '(:ignore t :which-key "Load")
    "Lb"    '("Buffer" . cider-load-buffer)
    "Lf"    '("File" . cider-load-file)
    "LA"    '("All files" . cider-load-all-files)
    ;; Testing
    "t"     '("Test" . cider-test-commands-map)
    ;; Logging
    "l"     '(:ignore t :which-key "Log")
    "la"    '("Append"    . cider-log-appender)
    "lc"    '("Consume"   . cider-log-consumer)
    "le"    '("Event"     . cider-log-event)
    "ls"    '("Show"      . cider-log-show)
    "lf"    '("Framework" . cider-log-framework)
    "li"    '("Info"      . cider-log-info)
    "ll"    '("Log"       . cider-log)
    ;; Quit
    "z"     '("Interrupt" . cider-interrupt)
    "Z"     '(:ignore t :which-key "Quit")
    "Zz"    '("Quit" . cider-quit)
    "Zr"    '("" . cider-restart)))

(use-package clj-refactor
  :preface
  (defun clj-refactor-setup ()
    "Setup clj-refactor mode."
    (clj-refactor-mode 1))
  :hook (clojure-mode . clj-refactor-setup))


(use-package flycheck-clj-kondo
  :hook (clojure-mode . flycheck-mode))

;;;; Dotnet

(use-package csproj-mode)

(use-package dotnet)

;;;; powershell

(use-builtin config-eglot-powershell
  :autoload config/eglot-register-powershell)

(use-package powershell-ts-mode
  :mode "\\.ps1\\'"
  :vc (:url "https://github.com/dmille56/powershell-ts-mode.git"
            :branch main)
  :init (config/eglot-register-powershell)
  :hook (eglot-ensure))

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

;;;; json

(use-package json-mode
  :hook (json-mode-hook . json-mode-setup)
  :mode "\\.json\\'")

;;; Personal Information management

;;;; org


(use-builtin config-org-agenda
  :after org
  :commands
  (config/agenda-rebuild-views!
   config/agenda-rebuild-file-list!
   config/agenda-apply-category-icons!)
  :init
  (config/agenda-rebuild-views!)
  (config/agenda-apply-category-icons!)
  (config/agenda-rebuild-file-list!)
  :general
  (major-mode-menu org-mode-map
    "a" '("Agenda" . org-agenda)))

(use-builtin config-org-todo
  :after org
  :commands
  (config/todo-apply-keywords!
   config/todo-define-effort-estimates!)
   :init
  (config/todo-apply-keywords!)
  (config/todo-define-effort-estimates!)
  :general
  (major-mode-menu org-mode-map
    "t" '("Todo" . org-todo)))

(use-builtin org
  :mode ("\\.org$" . org-mode)
  :preface

  (setopt org-directory (f-join (getenv "HOME") "vaults" "org"))

  (defun visit-org-inbox ()
    "Load the inbox file."
    (interactive)
    (find-file org-default-notes-file))

  (defun org-mode-setup ()
    "This function sets up `org-mode' using the mode's hook."
    (outline-minor-faces-mode -1)
    (electric-pair-local-mode 1)
    (outline-minor-mode -1)
    (setq-local fill-column 110
                tab-always-indent 'complete)
    (auto-fill-mode 1)
    (display-fill-column-indicator-mode 1)
    ;; dont add pair for `less-than_symbol' in `org-mode'.  It messes up org-tempo
    (add-function :before-until electric-pair-inhibit-predicate
                  (lambda (c) (eq c ?<))))

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
    "Convert the case of all keywords in current file.
Defaults to lowercase, unless UPPER is non-nil."
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


  (org-scheduled-delay-days 3)
  (org-deadline-warning-days 3)

  (org-habit-show-habits-only-for-today t)


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
  (org-modern-keyword (quote (("title"     . "")
                              ("file_tags" . "")
                              (t           . ""))))

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

;; [[https://github.com/thisirs/org-context][Contextual agenda and capture]]
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
  (org-src-ask-before-returning-to-edit-buffer nil)
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
             ("jsonc"      . json-ts)
             ("lua"        . lua-ts)
             ("php"        . php-ts)
             ("ruby"       . ruby-ts)
             ("rust"       . rust-ts)
             ("powershell" . powershell-ts-mode)
             ("pwsh"       . powershell-ts-mode)
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
  (org-download-screenshot-basename "screenshot.png")
  (org-download-link-format-function)
  (org-download-screenshot-file (expand-file-name
                                 org-download-screenshot-basename
                                 (config/emacs-local-dir ".cache")))
  (org-download-backend "curl")
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
      (if (length> parts 1)
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

(use-package consult-org-roam
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-."))

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
    "C-l" '("filter"       . deft-filter)
    "C-c" '("clear filter" . deft-filter-clear)
    "C-y" '("Copy filter"  . deft-filter-yank)
    ;; File creation
    "n"   '("New file"       . deft-new-file)
    "N"   '("New file..."    . deft-new-file-named)
    ;; File management
    "D"   '("Delete"         . deft-delete-file)
    "R"   '("Rename"         . deft-rename-file)
    "f"   '("Open"           . deft-find-file)
    "C-a" '("Archive"        . deft-archive-file)
    ;; Settings
    "C-t" '("Toggle inc search" . deft-toggle-incremental-search)
    "s"   '("Toggle sort"         . deft-toggle-sort-method)
    ;; Miscellaneous
    "g" '("Refresh"             . deft-refresh)
    "q" '("Quit"                . quit-window)
    "<tab>" '("Forward"         . forward-button)
    "<backtab>" '("Back"        . backward-button)
    "C-o" '("Open other win"    . deft-open-file-other-window)))


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
