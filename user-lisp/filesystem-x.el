;;; filesystem-x.el --- File and directory extras -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

(defun create-config-file-maybe ()
  "Create the files that are specified in config."
  (dolist (config-file
           '(config:emacs-history-file
             config:emacs-recent-files-file
             ))
    `(create-file-maybe ,config-file)))

(defun delete-file-and-buffer ()
  "Delete the file that the current buffer is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        (when
            (yes-or-no-p
             (format "Are you sure you want to delete the file %s? " filename))
          (delete-file filename)
          (kill-buffer))
      (message "Buffer is not visiting a file or file does not exist."))))

(defun rename-file-and-buffer (buffer new-name)
  "Rename the selected BUFFER and its file to NEW-NAME.
If the buffer is not visiting a file, just rename the buffer."
  (interactive
   (list
    (read-buffer "Select buffer (default: current): " (buffer-name) t)
    (read-file-name "New name: ")))
  (let ((selected-buffer (get-buffer buffer))
        (old-name buffer)
        (old-filename (buffer-file-name (get-buffer buffer))))
    (if (not selected-buffer)
        (error "Buffer '%s' does not exist" buffer)
      (with-current-buffer selected-buffer
        (if (not old-filename)
            (rename-buffer new-name)
          (progn
            (if (file-exists-p old-filename)
                (rename-file old-filename new-name 1))
            (set-visited-file-name new-name)
            (rename-buffer new-name)))))
    (message "Renamed buffer '%s' and file to '%s'" old-name new-name)))

(defvar config:emacs-config-dir)

(defun visit-emacs-file (fname &optional other-window)
  "Visit the given FNAME found in the Emacs directory.  Defaults to `init.el'.
If OTHER-WINDOW open the file in a separate window."
  (let* ((file-name (or fname "init.el"))
         (emacs-file (file-name-concat config:emacs-config-dir file-name)))
    (if (file-exists-p emacs-file)
        (if other-window
            (find-file-other-window emacs-file)
          (find-file emacs-file)))))

(defun visit-emacs-init (&optional other-window)
  "Open the Emacs `init.el' file.
If OTHER-WINDOW open the file in a separate window."
  (interactive)
  (visit-emacs-file "init.el" other-window))

(defun visit-emacs-early-init (&optional other-window)
  "Open the Emacs `early-init.el' file.
If OTHER-WINDOW open the file in a separate window."
  (interactive)
  (visit-emacs-file "early-init.el" other-window))

(defun visit-emacs-custom-file (&optional other-window)
  "Open the Emacs `custom.el' file.
If OTHER-WINDOW open the file in a separate window."
  (interactive)
  (visit-emacs-file "custom.el" other-window))


(provide 'filesystem-x)

;;; filesystem-x.el ends here
