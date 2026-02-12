;;; use-config.el --- Macro for configuring initialization files -*- lexical-binding: t; -*-
;;

;;; Commentary:
;; I found this concept in the chrisbarrett/.emacs.d repo on github.  I've added
;; a little bit of my own ideas.

;;; Code:

(require 'config-options)

;;; User config variables

(defcustom config:user-config-dir (file-name-concat config:emacs-user-lisp-dir "config")
  "The root directory to look for user config packages."
  :type 'directory
  :group 'config:emacs)

;;; Use-config variables

(defcustom use-config-init-hook nil
  "Hooks to run when initializing `use-config'."
  :type 'hook
  :group 'config:emacs)

(defcustom use-config-log-config-files nil
  "When non-nil, record a message that the given config file was loaded."
  :type 'boolean
  :group 'config:emacs)

;;; Functions

(defun create-directory-maybe (dir)
  "Create the directory DIR if is not already."
  (unless (file-exists-p dir)
    (mkdir dir)))

(defun create-file-maybe (file)
  "Create the FILE if is not already."
  (unless (file-exists-p file)
    (write-region "" nil file)))

(defun create-config-directory-maybe ()
  "Create the directories that are specified in config."
  (dolist (config-dir
           '(config:emacs-backup-dir
             config:emacs-auto-save-dir
             config:emacs-compiler-cache-dir
             config:emacs-snippets-dir
             config:emacs-templates-dir
             config:emacs-treesitter-grammar-dir
             config:emacs-custom-themes-dir))
    `(create-directory-maybe ,config-dir )))


;;; Main functions

(defun use-config-initialize ()
  "Perform initialization functions when starting `use-config'."
  (run-hooks use-config-init-hook)
  (let* ((dir (file-truename config:user-config-dir))
         (default-directory dir))
    (normal-top-level-add-subdirs-to-load-path)))

(defmacro use-config (feature &rest use-package-args)
  "Load FEATURE from `config:user-package-dir'.
USE-PACKAGE-ARGS are optional additional arguments forwarded to `use-package'."
  (declare (indent defun))
  (let* ((file (car (directory-files-recursively config:user-config-dir
                                                 (format "%s.el" feature))))
         (parsed-args (byte-run--parse-body use-package-args nil))
         ;; Allow a docstring in the `use-config' body, but it isn't used here
         ;; it is only for adding context for loading the config
         (docstring (nth 0 parsed-args))
         (body (nth 3 parsed-args)))
    (unless body
      (setq body use-package-args))
    (unless (plistp body)
      (progn
        (setq docstring body)
        (setq body nil)))
    (message "%s parsed to %s" feature parsed-args)
    (message "docstring: %s" docstring)
    (message "body: %s" body)
    (cl-assert (file-exists-p file) t
               "use-config could not find file for %s in %s"
               feature
               config:user-config-dir)
    (if use-config-log-config-files
        (message "Loading config file %s" file))
    `(use-package ,feature
       :load-path ,file
       :ensure nil
       :demand t
       ,@body)))

;; (font-lock-add-keywords 'emacs-lisp-mode
;;                         `((,(rx "("
;;                                 (group "use-config") symbol-end (* (any space))
;;                                 (group (+ (or (syntax word) (syntax symbol))))
;;                                 (? ")"))
;;                            (1 font-lock-keyword-face)
;;                            (2 font-lock-constant-face nil t))))



(provide 'use-config)


;;; use-config.el ends here
