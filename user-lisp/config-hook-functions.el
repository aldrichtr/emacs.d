;;; config-hook-functions.el --- Hook functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun fundamental-mode-setup ()
  "Define things that should be set in all modes."
  (setq-default tab-width 2
                fill-column 80
                tab-always-indent 'complete)
  (ws-butler-mode)
  (display-line-numbers-mode 1)
  (display-fill-column-indicator-mode 1)
  (auto-fill-mode 1))

(defun before-save-setup ()
  "Actions to take prior to saving the file.")

;;;###autoload
(defun text-mode-setup ()
  "Function to add commands to `text-mode-hook'."
  (fundamental-mode-setup))

;;;###autoload
(defun prog-mode-setup ()
  "Function to add commands to `prog-mode-hook'."
  (fundamental-mode-setup)
  (outline-minor-mode))


;;;###autoload
(defun package-list-mode-setup()
    "Setup the package list buffer."
    (hl-line-mode 1))

;;;###autoload
(defun json-mode-setup ()
  "Setup the json mode.")

(provide 'config-hook-functions)

;;; config-hook-functions.el ends here
