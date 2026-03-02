;;; package-macros.el --- Convenience macros for use-package -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; Feature - uses the `emacs' package

(require 'use-package)
(require 'config-options)

;;;###autoload
(defmacro use-feature (&rest args)
  "A `use-package' definition that represents a core Emacs feature.
This is the same as calling `use-package Emacs ARGS' with ensure nil,
defer nil."
  (declare (indent defun))
  `(use-package emacs
     :ensure nil
     :defer nil
     ,@args))

;;;; Built-in - No install required

;;;###autoload
(defmacro use-builtin (name &rest args)
  "Like `use-package' but for builtin packages.
NAME and ARGS are the same as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))
    
;;;; Load config files in `user-lisp'

;;;###autoload
(defmacro use-config (name &rest args)
  "Load the config file NAME optionally passings ARGS"
  (declare (indent defun))
  (let ((config-name (concat "config-" name)))
    (load-library config-name)))
    

(provide 'package-macros)

;;; package-macros.el ends here
