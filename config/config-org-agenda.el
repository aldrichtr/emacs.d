;;; config-org-agenda.el --- Configure org-agenda -*- lexical-binding: t; -*-


;;; Commentary:


;;; Code:

;;; Requirements

(require 'f)
(require 'org)
(require 'org-mem)
(require 'nerd-icons)

;;; Agenda files

(defun config/agenda-file-filter (file)
  "Match FILEs that should not be included in the agenda files."
  (when (or (string-match "archive" file)
            (string-match "^\\." file))
    file))

(defun config/file-has-agenda-entries-p (file)
  "Determine if the given FILE has any entries that should be in the agenda.
Any open todo, active timestamp, schedule or deadline returns true."
  (seq-find (lambda (entry)
              (or (org-mem-entry-active-timestamps entry)
                  (and (not (org-mem-entry-closed entry))
                       (or (org-mem-entry-todo-state entry)
                           (org-mem-entry-scheduled entry)
                           (org-mem-entry-deadline entry)))))
            (org-mem-entries-in file)))

(defun config/find-all-files-with-id ()
  "List the files that `org-mem' finds.
If FILTER is given remove any that match."
  (cl-loop for file in (org-mem-all-files)
           unless (config/agenda-file-filter file)
           when (config/file-has-agenda-entries-p file)
           collect file))


(defun config/agenda-rebuild-file-list! ()
  "Rebuild the list of `org-agenda' files."
  (interactive)
  (setopt org-agenda-files (config/find-all-files-with-id)))

;;; Agenda Custom Commands

(defun config/agenda-clear-views! ()
  "Remove all entries from `org-agenda-custom-commands'."
  (interactive)
  (setopt org-agenda-custom-commands '()))


;;;; View elements


(defun config/agenda-weekly-section (head types)
  "Return an `org-mode' agenda section.
HEAD is the section header and TYPES is a list of the entry-types to
include.  valid types are: :timestamp :sexp :scheduled :deadline."
  `(agenda ""
           ((org-agenda-overriding-header ,head)
            (org-agenda-ndays 7)
            (org-deadline-warning-days 0)
            (org-agenda-start-on-weekday nil)
            (org-agenda-show-all-dates nil)
            (org-agenda-entry-types ,types)
            (org-agenda-skip-deadline-if-done t)
            (org-agenda-warning-days 0))))

(defun config/agenda-set-calendar-view ()
  "Build the calendar view."
  `("A" "@Agenda"
    (,(config/agenda-weekly-section "Upcoming deadlines" '(:deadline))
     ,(config/agenda-weekly-section "Scheduled this week" '(:scheduled))
     ,(config/agenda-weekly-section "One week overview" '(:timestamp :sexp)))))

(defun config/agenda-next-action-lists ()
  "Build the Next action lists."
  '("n" "@Next Actions"
    ((tags-todo "+errand-someday/NEXT"
                ((org-agenda-overriding-header "@Errand")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future))))
     (tags-todo "+phone-someday/NEXT"
                ((org-agenda-overriding-header "@Phone")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future))))
     (tags-todo "+web-someday/NEXT"
                ((org-agenda-overriding-header "@Online")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future))))
     (tags-todo "+gps-someday/NEXT"
                ((org-agenda-overriding-header "@GuidePoint")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future))))
     (tags-todo "+home-someday/NEXT"
                ((org-agenda-overriding-header "@Home")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future))))
     (tags-todo "+dev-someday/NEXT"
                ((org-agenda-overriding-header "@Dev")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future))))
     (tags-todo "-CATEGORY=\"Someday\"-CATEGORY=\"REVIEW\"+TAGS=\"\"/NEXT"
                ((org-agenda-overriding-header "@Uncategorized")
                 (org-agenda-tags-todo-honor-ignore-options t)
                 (org-agenda-todo-ignore-scheduled '(future)))))))


(defun config/agenda-project-list ()
  "Build the project list.
A project is:
- A heading with the state of `TODO'
- That has children that are state `NEXT'"
  '("p" "@Projects"
    tags-todo "-IGNORE-someday-wait/TODO"
    ((org-agenda-overriding-header "@Projects")
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-tags-match-list-sublevels 'indented)
     (org-agenda-skip-function
      (lambda nil
        (org-agenda-skip-subtree-if (quote nottodo) '("NEXT")))))))

(defun config/agenda-waiting-for ()
  "Build the waiting for list."
  '("w" "@waiting"
    todo "WAIT"
    ((org-agenda-overriding-header "Waiting For"))))

(defun config/agenda-review ()
  "Build the review list."
  '("R" . "@Review Commands")
  '("Rb" "The Backlog" todo "NEW")
  '("Rn" "Review notes"
    tags-todo "REVIEW"
    ((org-agenda-overriding-header "Notes Marked for Review")
     (org-agenda-skip-function  '(org-agenda-skip-entry-if 'todo))
     ;;don't list sub levels, just the one we marked
     (org-tags-match-list-sublevels nil)
     (org-agenda-sorting-strategy '(category-up))))
  '("Rp" "Stuck projects" stuck ""))

(defun config/agenda-someday-maybe ()
  "Build the someday maybe list."
  '("S" "@Someday/Maybe"
    tags-todo "+someday/TODO"
    ((org-agenda-overriding-header "Someday/Maybe"))))

;;;; Combiner

(defcustom config:agenda-view-builders
  '(config/agenda-calendar-view
    config/agenda-next-action-lists
    config/agenda-project-list
    config/agenda-waiting-for
    config/agenda-review
    config/agenda-someday-maybe)
  "Functions that add views to the `org-agenda' dispatch."
  :group 'config
  :type '(repeat function))

(defun config/agenda-rebuild-views! (&optional views)
  "Rebuild the variables that affect `org-agenda'.
Use VIEWS or `config:agenda-view-builders'."
  (interactive)
  (config/reload-agenda-config)
  (let* ((views (or views config:agenda-view-builders))
         ;; Dereference the symbols into a list of views
         (resolved (mapcar #'funcall views)))
    (config/agenda-clear-views!)
    (setopt org-agenda-custom-commands resolved)))

;;; Agenda category icons

(defun config/agenda-apply-prefix-format! (&optional fmt)
  "Set the `org-agenda-prefix-format'.
FMT is an alist of view to format string."
  (let ((fmt (or fmt '((agenda . " %-2i %-12:c%?-12t% s")
                       (todo   . " %-2i %-12:c")
                       (tags   . " %-2i %-12:c")
                       (search . " %-2i %-12:c")))))
   (setopt org-agenda-prefix-format fmt)))

(defvar config:icon-dispatch-table
  '(("^nf-fae?"                . nerd-icons-faicon)    ; Font Awesome
    ("^nf-dev"                 . nerd-icons-devicon)   ; Devicons
    ("^nf-\\(seti\\|custom\\)" . nerd-icons-sucicon)   ; Seti-UI + Custom
    ("^nf-md"                  . nerd-icons-mdicon)    ; Material Design
    ("^nf-weather"             . nerd-icons-wicon)     ; Weather
    ("^nf-oct"                 . nerd-icons-octicon)   ; Octicons
    ("^nf-ple?"                . nerd-icons-powerline) ; Powerline
    ("^nf-iec"                 . nerd-icons-ipsicon)   ; IEC Power Symbols
    ("^nf-linux"               . nerd-icons-flicon)    ; Font logos
    ("^nf-pom"                 . nerd-icons-pomicon)   ; Pomicons
    ("^nf-cod"                 . nerd-icons-codicon))  ; Codicons
  "Mapping of nerd‑font prefixes to icon functions.")


(defun config/icon-get-nerd-font-icon (name &optional args)
  "Return the nerd-font icon symbol NAME.  ARGS added if non-nil."
  (let ((fn (cl-loop for (re . f) in config:icon-dispatch-table
                     when (string-match re name)
                     return f)))
    (unless fn
      (error "Cannot get icon '%s', type not recognized" name))
    (funcall fn name args)))


(defcustom config:agenda-category-icon-properties (list :height 1.0)
  "Font properties to apply to `org-agenda' category icons."
  :group 'config
  :type 'plist)

(defcustom config:agenda-category-icons
  '(("REVIEW"  . "nf-fa-bullseye")
    ("ADMN"    . "nf-fa-terminal")
    ("BLOG"    . "nf-fa-pen_fancy")
    ("Meeting" . "nf-fa-group")
    ("THINK"   . "nf-fa-brain"))
  "Alist of categories and icon names."
  :group 'config
  :type 'alist)

(defun config/assign-category-icons (categories &optional args)
  "Associate nerd-icons with org agenda categories.
CATEGORIES is an alist of categories associated with the icon name.
ARGS are font properties to be applied to the icons.  Returns an alist of
category to nerd-icon symbol"
  (let ((args (or args config:agenda-category-icon-properties)))
    (mapcar
     (lambda (x)
       (let ((cat (if (symbolp (car x))
                      (symbol-name (car x))
                    (car x)))
             (icon (list (config/icon-get-nerd-font-icon (cdr x) args))))
         (cons cat icon)))
     categories)))

(defun config/default-category-icon ()
  "Return the default match category.
Sets the default as 11 spaces"
  (list "" (quote '(space . (:width (11))))))

(defun config/agenda-apply-category-icons! (&optional icons args)
  "Applies ICONS to categories.
ARGS will be passed to the icon builder.
Rebuilds the `org-agenda-category-icon-alist'."
  (interactive)
  (config/reload-agenda-config)
  (let ((icons (or icons config:agenda-category-icons)))
    (config/agenda-apply-prefix-format!)
    (setopt org-agenda-category-icon-alist
            (config/assign-category-icons icons args))
    (add-to-list 'org-agenda-category-icon-alist
                 (config/default-category-icon))))


;;; Provide library

(defun config/reload-agenda-config ()
  "Reload this library to refresh changes."
  (unload-feature 'config-org-agenda t)
  (require 'config-org-agenda))


(provide 'config-org-agenda)

;;; config-org-agenda.el ends here
