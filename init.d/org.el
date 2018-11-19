;;; org.el --- Configure Org  -*- lexical-binding: t; -*-

;;
;;; Code:

;;; Capture Templates

(setq org-capture-templates
      '(
        ;; Capture interactively:
        ("t" "Task" entry
         (file+headline "inbox.org" "Tasks")
         "* TODO %?\n%U"  :empty-lines 1)

        ;; Capture in the background:
        ("T" "Task from content"  entry
         (file+headline "inbox.org" "Tasks")
         "* TODO %i\n%U" :immediate-finish t :empty-lines 1)))

;;; org.el ends here
