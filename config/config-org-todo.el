;;; config-org-todo.el --- Configure org-mode todos -*- lexical-binding: t; -*-

;;; Commentary:
;; - NEW :: Needing triage.  Record a timestamp when leaving the `NEW' state
;; - TODO :: Has been triaged and is available for work
;; - WAIT :: Blocked until something happens.  Record a note when entering
;;           and a timestamp when leaving
;; - MEET :: A Meeting is different than a task
;; - DONE :: No more work to do.  Record a timestamp
;; - DROP :: No longer required, OBE or irrelevant.  Record a note

;;; Code:

(require 'org)

(defun config/todo-apply-keywords! ()
  "Arrange the keywords for `org-mode' todos."
  (setopt org-todo-keywords
          '((sequence "NEW(N/!)" "TODO(t)" "NEXT(n)" "WAIT(w@/!)")
            (sequence "MEET(m)")
            (sequence "STORY(s@/!)")
            (sequence "|" "DONE(d!)" "DROP(q@)"))

          org-provide-todo-statistics t
          org-use-fast-todo-selection 'auto))

(defun config/todo-define-effort-estimates! ()
  "Set the Effort estimates used by `org-mode' if they have changed."
  (let* ((time-blocks "0 0:10 0:30 1:00 2:00 3:00 4:00")
         (current-val (alist-get "Effort_ALL" org-global-properties nil nil #'string-equal)))
    (unless (string-equal time-blocks current-val)
      (setf (alist-get "Effort_ALL" org-global-properties nil nil #'string-equal)
            time-blocks))))


(provide 'config-org-todo)


;;; config-org-todo.el ends here
