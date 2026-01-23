;;; config-options.el --- Personal customization options for emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets the options that I feel will change regularly. It creates several customization options
;; in the `config:' group.

;;; Code:

(defgroup config:emacs nil
  "Personal Emacs configuration settings & options."
  :tag "config"
  :group 'initialization)

;;; Home directories
(defcustom config:emacs-config-dir
  (let  ((config-dir (or (getenv "XDG_CONFIG_HOME")
                         (getenv "APPDATA")
                         "~/.config")))
    (if config-dir
        (file-name-concat config-dir "emacs")
      (file-name-as-directory user-emacs-directory)))
  "The directory where Emacs should save configuration files.
Defaults to `XDG_CONFIG_HOME/emacs', `APPDATA/emacs', or `~/.emacs.d'."
  :type 'directory
  :group 'config:emacs)

(defcustom config:emacs-local-dir
  (let  ((local-dir (or (getenv "XDG_DATA_HOME")
                        (getenv "LOCALAPPDATA")
                        "~/.local/share")))
    (if local-dir
        (file-name-concat local-dir "emacs")
      (file-name-as-directory user-emacs-directory)))
  "The directory where Emacs should save local files.
Defaults to `XDG_DATA_HOME/emacs', `LOCALAPPDATA/emacs', or `~/.emacs.d'."
  :type 'directory
  :group 'config:emacs)

;;;; Also provided as functions for convenience
(defun config/emacs-config-dir (&optional file)
  "Return path to `config:emacs-config-dir'.

When FILE is given, concat file and config-dir"
  (if file
      (file-name-concat config:emacs-config-dir file)
    config:emacs-config-dir))

(defun config/emacs-local-dir (&optional file)
  "Return path to `config:emacs-local-dir'.

When FILE is given, concat file and local-dir"
  (if file
      (file-name-concat config:emacs-local-dir file)
    config:emacs-local-dir))
;; Home directories:4 ends here

;;; Load path additions

;;;; User-lisp
(defcustom config:emacs-user-lisp-dir
  (file-name-concat config:emacs-config-dir "user-lisp")
  "Where local, user Lisp files are located."
  :type 'directory
  :group 'config:emacs)

(unless (file-exists-p config:emacs-user-lisp-dir)
  (mkdir config:emacs-user-lisp-dir t))
(add-to-list 'load-path config:emacs-user-lisp-dir)
;; Load path additions:1 ends here

;;; History
(defcustom config:emacs-history-file
  (config/emacs-local-dir "history")
  "The file that Emacs stores minibuffer, etc. history."
  :type 'file
  :group 'config:emacs)

(unless (file-exists-p config:emacs-history-file)
  (write-region "" nil  config:emacs-history-file))

;;; File backups
(defcustom config:emacs-backup-dir
  (config/emacs-local-dir "backups")
  "The directory where Emacs stores file backups."
  :type 'directory
  :group 'config:emacs)
(unless (file-exists-p config:emacs-backup-dir)
  (mkdir config:emacs-backup-dir t))

;;; Autosave
(defcustom config:emacs-auto-save-dir
  (config/emacs-local-dir "sessions")
  "The directory where Emacs stores auto-save info."
  :type 'directory
  :group 'config:emacs)

(unless (file-exists-p config:emacs-auto-save-dir)
  (mkdir config:emacs-auto-save-dir t))

;;; Recent files list
(defcustom config:emacs-recent-files-file
  (config/emacs-local-dir "recentf")
  "The file that Emacs stores recently visited files in."
  :type 'file
  :group 'config:emacs)

(unless (file-exists-p config:emacs-recent-files-file)
  (write-region "" nil config:emacs-recent-files-file))

;;; Compiler cache
(defcustom config:emacs-compiler-cache-dir
  (config/emacs-local-dir "eln-cache")
  "The file that Emacs stores recently visited files in."
  :type 'file
  :group 'config:emacs)

(unless (file-exists-p config:emacs-compiler-cache-dir)
  (mkdir config:emacs-compiler-cache-dir t))

;;; Elpa Packages
(defcustom config:emacs-package-dir
  (config/emacs-local-dir "packages")
  "The directory where Emacs stores packages."
  :type 'directory
  :group 'config:emacs)

;;; Themes
(defcustom config:emacs-custom-themes-dir
  (config/emacs-config-dir "themes")
  "The directory where Emacs stores themes."
  :type 'directory
  :group 'config:emacs)

(unless (file-exists-p config:emacs-custom-themes-dir)
  (mkdir config:emacs-custom-themes-dir t))

;;; Templating

;;;; Snippets
(defcustom config:emacs-snippets-dir
  (config/emacs-config-dir "snippets")
  "The directory where `yasnippet' loads snippets from."
  :type 'directory
  :group 'config:emacs)

(unless (file-exists-p config:emacs-snippets-dir)
  (mkdir config:emacs-snippets-dir t))

;;;; Autoinsert
(defcustom config:emacs-templates-dir
  (config/emacs-config-dir "templates")
  "The directory where `autoinsert' loads templates from."
  :type 'directory
  :group 'config:emacs)

(unless (file-exists-p config:emacs-templates-dir)
  (mkdir config:emacs-templates-dir t))



;;; Treesitter parsers

(defcustom config:emacs-treesitter-grammar-dir
(config/emacs-local-dir "tree-sitter")
"The directory where treesitter grammars are stored."
:type 'directory
:group 'config:emacs)

(unless (file-exists-p config:emacs-treesitter-grammar-dir)
(mkdir config:emacs-treesitter-grammar-dir))

;;; fonts

;;;; Core fonts

(defcustom config:emacs-mono-font-name
"AnonymicePro Nerd Font Mono"
"Set the default monospaced font."
:type 'string
:group 'config:emacs)

(defcustom config:emacs-propo-font-name
"AnonymicePro Nerd Font Propo"
"Set the default proportional font."
:type 'string
:group 'config:emacs)


;;;; Default font

(defcustom config:emacs-default-font-name
config:emacs-mono-font-name
"The default font used across Emacs."
:type 'string
:group 'config:emacs)


(defcustom config:emacs-default-font-size
(cond ((eq system-type 'android) "14")
(t "12"))
"The default font size used across Emacs."
:type 'string
:group 'config:emacs)


(defcustom config:emacs-default-font
(concat config:emacs-default-font-name
"-"
config:emacs-default-font-size)
"The default font string used across Emacs."
:type 'string
:group 'config:emacs)

;;;; Icon font

(defcustom config:emacs-icon-font-name
"FiraCode Nerd Font Propo"
"The default font string used for icons."
:type 'string
:group 'config:emacs)

(defcustom config:emacs-icon-font-size
"10"
"The default font size used for icons."
:type 'string
:group 'config:emacs)


(defcustom config:emacs-icon-font
(concat config:emacs-icon-font-name
"-"
config:emacs-icon-font-size)
"The default font string used for icons."
:type 'string
:group 'config:emacs)

;;; theme

(defcustom config:emacs-default-theme
'zenburn
"The default theme used across Emacs."
:type 'symbol
:group 'config:emacs)


;;; Window layout

(defcustom config:emacs-fill-column-docs 120
"How wide the buffer should be for document type buffers."
:type 'integer
:group 'config)

(defcustom config:emacs-fill-column-prog 100
"How wide the buffer should be for programming type buffers."
:type 'integer
:group 'config)

;;; Server

(defcustom config:emacs-server-run t
"Should EMACS start the server on init."
:type 'boolean
:group 'config:emacs)

(defcustom config:emacs-lsp-server-root-dir
          "~/.local/share/lsp"
"Root diretory for Language Server Protocol Servers."
:type 'directory
:group 'config:emacs)

;;; Profile

(defcustom config:emacs-profile
(or (getenv "EMACS_PROFILE") "default")
"The profile used across Emacs."
:type 'string
:group 'config:emacs)

(defcustom config:emacs-leader-key "SPC"
"The key used to trigger the leader menu."
:type 'string
:group 'config:emacs)


(defcustom config:emacs-major-mode-leader-key ","
"The key used to trigger the leader menu for the current major-mode."
:type 'string
:group 'config:emacs)


(provide 'config-options)

;;; config-options.el ends here
