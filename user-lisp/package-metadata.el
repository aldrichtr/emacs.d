;;; package-metadata.el --- Package customize option metadata -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar package-customizations-format 'use-package-custom
  "Set the default format for output of `package-customizations'.")

(declare-function custom-group-members cust-edit)
(declare-function custom-variable-current-value cust-edit)

(defun package--list-package-customizations (package &optional output-type)
  "Retrieve customizations for PACKAGE and format them as OUTPUT-TYPE.
If PACKAGE is nil, prompt the user interactively.
OUTPUT-TYPE should be either `setopt' or `use-package-custom'.  If nil defaults
to `package-customizations-format'."
  (interactive
   (list (intern (read-library-name))
         package-customizations-format))
  (let* ((options (seq-filter #'custom-variable-p
                              (mapcar #'car (custom-group-members package nil))))
         (output-type (or output-type package-customizations-format))
         (formatted-options ""))
    (setq formatted-options
          (cond
           ((eq output-type 'setopt)
            (format "(setopt %s)"
                    (mapconcat (lambda (opt)
                                 (format "%S %S" opt (default-value opt)))
                               options "\n        ")))
           ((eq output-type 'use-package-custom)
            (format ":custom\n%s"
                    (mapconcat (lambda (opt)
                                 (format "(%S %S)" opt (default-value opt)))
                               options "\n")))))
    (print formatted-options)))

(defun package-insert-package-customizations (package &optional output-type)
  "Insert the customizations for PACKAGE at point in OUTPUT-TYPE format.
Uses `package-customizations' to generate output."
  (interactive
   (list (intern (read-library-name)) package-customizations-format))
  (insert (package--list-package-customizations package output-type)))

(defun use-package--package-name-at-point ()
  "Retrieve the package name being used in the current `use-package' form."
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'defun)))
      (when bounds
        (goto-char (car bounds)) ;; Move to the beginning of the list
        (forward-char 1) ;; Skip the opening parenthesis
        (when (or (looking-at "use-package")
                  (looking-at "use-builtin"))
          (forward-sexp) ;; Move past 'use-package'
          (forward-char 1) ;; Move to the symbol
          (symbol-at-point))))))

(defun package-metadata (package)
  "Return the metadata (package desc) for PACKAGE."
  ;; this returns the package-desc
  (cadr (assq package package-alist)))

(defun use-package-browse-package-url ()
  "Browse to the url for the package in the current `use-package' form."
  (interactive)
  (let* ((metadata (package-metadata (use-package--package-name-at-point)))
         (extras (and metadata (package-desc-extras metadata)))
         (url (cdr (assoc :url extras))))
    (if url
        (browse-url url))))

(provide 'package-metadata)

;;; package-metadata.el ends here
