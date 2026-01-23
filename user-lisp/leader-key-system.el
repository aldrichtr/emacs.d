;;; leader-key-system.el --- Leader key system -*- lexical-binding: t -*-

;;; Commentary:
;; This is my leader key system based on the `general.el' package

;;; Code:

(require 'general)
(require 'plist-x) ; Provides `plist-remove'

(defvar leader-menu-prefix-alist
  `(("leader-menu" . ,config:emacs-leader-key)))

(defun get-leader-menu-prefix (menu)
  "Return the prefix of the given leader MENU."
  (interactive
   (list
    (intern
     (completing-read
      "Select leader menu: "
      (let (symbol-list)
        (mapatoms (lambda (sym)
                    (let ((name (symbol-name sym)))
                      (when (string-prefix-p "leader-" name)
                        (push name symbol-list)))))
        symbol-list)
      nil t))))
  (unless (stringp menu)
    (setq menu (symbol-name menu)))
  (cdr (assoc menu leader-menu-prefix-alist)))


(defun get-leader-menu-map (menu)
  "Return the map associated with MENU."
  (interactive
   (list
    (intern
     (completing-read
      "Select leader menu: "
      (let (symbol-list)
        (mapatoms (lambda (sym)
                    (let ((name (symbol-name sym)))
                      (when (string-prefix-p "leader-" name)
                        (push name symbol-list)))))
        symbol-list)
      nil t))))
  (unless (stringp menu)
    (setq menu (symbol-name menu)))
  (string-replace "-menu" "-map" menu))


(defun set-leader-menu-prefix (menu prefix)
  "Add the (MENU . PREFIX) to the `leader-menu-prefix-alist'."
  (push (cons menu prefix) leader-menu-prefix-alist))
;;(add-to-list 'leader-menu-prefix-alist '(menu . prefix))

;;; Keybinding macros


(defmacro make-leader-menu (name key &rest body)
  "Create a keymap and general-define-key function for KEY.
Creates a keymap named `leader-NAME-map' and a `general-definer' macro
named `leader-NAME-menu'.

NOTE that this relies on there being a definer created called `leader-menu' as
the default.

BODY will be passed on to the definer macro, with the exception of these keys:

- `:PARENT' The definer that we want to wrap
- `:DISPLAY-NAME' The name displayed in which-key menus if not NAME."

  (declare (indent defun))
  (let* ((display-name  (or (plist-get body :display-name) name))
         (parent        (or (plist-get body :parent) 'leader-menu))
         ;; convert name to a lowercase-kebab string
         ;; File --> leader-file-menu , leader-file-map
         (base-name     (concat "leader-"
                                (replace-regexp-in-string " " "-"
                                                          (downcase name))))
         (definer-name  (concat base-name "-menu"))
         (keymap-name   (concat base-name "-map"))
         (parent-prefix (or (get-leader-menu-prefix (symbol-name parent)) ""))
         (prefix        (concat parent-prefix " " key))
         ;;(prefix (concat (get-leader-menu-prefix 'parent) " " key))
         ;; remove keys that do not belong to the general-create-definer macro
         (body (plist-remove '(:display-name :parent) body)))
    (set-leader-menu-prefix definer-name prefix)
    ;; Create and execute the general definer
    `(progn
       (general-create-definer ,(intern definer-name)
  ;;; Originally, I was using
         ;; :wrapping ,(intern (symbol-name parent))
         ;;:infix ,key
  ;;; to create the leader-menu hierarchy, but that didn't work past the
  ;;; first level. I think that is because the definer "unwraps" all the
  ;;; way up the stack to the `leader-menu' definer, instead of the
  ;;; parent.
         :prefix-map (quote ,(intern keymap-name))
         :states '(normal)
         :prefix ,prefix
         :wk-full-keys nil
         "" '(:ignore t :which-key ,display-name))
       (,(intern definer-name)
        ,@body))))


(defmacro add-leader-keys (name key &rest body)
  "This is like `make-leader-menu' but it does not create a definer or map.
  NAME is the which-key name, KEY is key to bind, and BODY is the `general-def'
  body.

  - `:PARENT' The definer that we want to wrap
  - `:DISPLAY-NAME' The name displayed in which-key menus if not NAME."
  (declare (indent defun))
  (let* ((display-name  (or (plist-get body :display-name) name))
         (parent        (or (plist-get body :parent) 'leader-menu))
         (parent-prefix (or (get-leader-menu-prefix (symbol-name parent)) ""))
         (body          (plist-remove '(:display-name :parent) body)))
    `(progn
       (general-define-key
        :states '(normal)
        :prefix ,parent-prefix
        :infix ,key
        "" '(:ignore t :which-key ,display-name)
        ,@body))))

(provide 'leader-key-system)

;;; leader-key-system.el ends here
