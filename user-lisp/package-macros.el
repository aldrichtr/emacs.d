;;; package-macros.el --- Convenience macros for use-package -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; Feature - uses the `emacs' package

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

(defmacro use-builtin (name &rest args)
  "Like `use-package' but for builtin packages.
NAME and ARGS are the same as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(provide 'package-macros)

;;; package-macros.el ends here
