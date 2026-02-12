;;; leader-key-system.el --- Leader key system -*- lexical-binding: t -*-

;;; Commentary:
;; This is my leader key system based on the `general.el' package.  The
;; top-level leader menu is `leader-menu'.  This is what is called when the
;; leader-key is pressed.  All other leader-menus are created in a tree below
;; this one.
;;
;; There are two macros that do the bulk of the work and are intended to be used
;; in the `:general' sections of the `use-package' definitions:
;;
;; `make-leader-menu' provide a name, key and optionally a general-definition,
;; which will create a new menu in the hierarchy, and a keymap.
;;
;; `add-leader-keys' provide a name key and optional general-definition, which
;; will add keys to the given map and menu, without creating a new map.

;;; Code:

(require 'general)
(require 'plist-x) ; Provides `plist-remove'
(require 'config-options)

;; Create the `global-def' to add global keys; keys available in every mode.

;; TODO: maybe this should be `global-menu'?
(general-create-definer global-menu
  :keymap 'global-map
  :states
  '(normal visual motion operator
           insert replace emacs hybrid iedit-insert))

;; Create the `leader-menu' keybinding definition. functionality where commands
;; are organized behind the leader `config:emacs-leader-key'
;; This definer is used by the `make-leader-menu' macro
(general-create-definer leader-menu
  :states
  '(normal visual motion operator
           insert replace emacs hybrid iedit-insert)
  :prefix config:emacs-leader-key
  :non-normal-prefix config:emacs-leader-non-normal-key)

;; Create the `major-mode-menu' keybinding definition.  Organize major-mode
;; bindings behind the major-mode leader `config:emacs-local-leader-key'.
(general-create-definer major-mode-menu
  :states '(normal)
  :prefix config:emacs-local-leader-key)


;;; Leader menu system

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

(defun set-leader-menu-prefix (menu prefix)
  "Add the (MENU . PREFIX) to the `leader-menu-prefix-alist'."
  (push (cons menu prefix) leader-menu-prefix-alist))

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

;;; Keybinding macros

;;;###autoload
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
  (let* ((display-name  (or (plist-get body :display-name) (concat (capitalize name) "s")))
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

         ;; remove keys that do not belong to the `general-create-definer' macro
         (body (plist-remove '(:display-name :parent) body)))

    (set-leader-menu-prefix definer-name prefix)
    ;; Create and execute the general definer
    `(progn
       (message "Creating def for %s as %s under %s" ,definer-name ,display-name ,parent-prefix)
       (general-create-definer ,(intern definer-name)
         :prefix-map (quote ,(intern keymap-name))
         :states '(normal)
         :prefix ,prefix
         :wk-full-keys nil
         "" '(:ignore t :which-key ,display-name))
       (,(intern definer-name)
        ,@body))))

;;;###autoload
(defmacro add-leader-keys (name key &rest body)
  "This is like `make-leader-menu' but it does not create a definer or map.
NAME is the which-key name, KEY is key to bind, and BODY is the `general-def'
body.

- `:PARENT' The definer that we want to wrap
- `:DISPLAY-NAME' The name displayed in which-key menus if not NAME."
  (declare (indent defun))
  (let* ((display-name  (or (plist-get body :display-name) (concat (capitalize name) "s")))
         (parent        (or (plist-get body :parent) 'leader-menu))
         (parent-prefix (or (get-leader-menu-prefix (symbol-name parent)) ""))
         ;; remove keys that do not belong to the `general-create-definer' macro
         (body          (plist-remove '(:display-name :parent) body)))
    `(progn
       (general-def
        :states '(normal)
        :prefix ,parent-prefix
        :infix ,key
        :wk-full-keys nil
        "" '(:ignore t :which-key ,display-name)
        ,@body))))

(provide 'leader-key-system)

;;; leader-key-system.el ends here
