;;; projectile-refactor.el --- Refactor names in project -*- lexical-binding: t -*-

;;; Commentary:
;; Perform refactoring functions on names within the current project.

;;; Code:

(require 'projectile)
(require 'fileloop)
(require 's)

(defun projectile--refactor-replace (current new)
  "Replace every occurance of CURRENT to NEW in project."
  (let* ((dir (projectile-acquire-root))
         (files (projectile-files-with-string current dir)))
    (fileloop-initialize-replace current new files 'default)
    (fileloop-continue)))

(defun projectile--refactor-case (case-function)
    "Convert all occurances of current symbol using CASE-FUNCTION."
    (let* ((old (read-string
                 (projectile-prepend-project-name "Replace: ")
                 (projectile-symbol-or-selection-at-point)))
           (new (case-function old)))
      (projectile--refactor-replace old new)))

(defun projectile-refactor-variable-case-camel ()
  "Convert all occurances of current symbol to camelCase."
  (projectile--refactorcase #'s-lower-camel-case))

(defun projectile-refactor-variable-case-pascal ()
  "Convert all occurances of current symbol to PascalCase."
  (projectile--refactorcase #'s-upper-camel-case))

(defun projectile-refactor-variable-case-snake ()
  "Convert all occurances of current symbol to snake_case."
  (projectile--refactorcase #'s-snake-case))

(defun projectile-refactor-variable-case-kebab ()
  "Convert all occurances of current symbol to kebab-case."
  (projectile--refactorcase #'s-dashed-words))

(defun projectile-refactor-variable-case-upper ()
  "Convert all occurances of current symbol to UPPERCASE."
  (projectile--refactorcase #'s-upcase))

(defun projectile-refactor-variable-case-lower ()
  "Convert all occurances of current symbol to lowercase."
  (projectile--refactorcase #'s-downcase))


(provide 'projectile-refactor)
;;; projectile-refactor.el ends here