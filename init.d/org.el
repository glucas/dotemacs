;;; org.el --- Configure Org  -*- lexical-binding: t; -*-

;;
;;; Code:

;;; Capture Templates

(setq org-capture-templates
      '(
        ;; Capture interactively:
        ("t" "Task" entry
         (file "")
         "* TODO %?\n%U"  :empty-lines 1)
        ("n" "Note" entry
         (file "")
         "* %?\n%U"  :empty-lines 1)

        ;; Capture in the background:
        ("T" "Task from content"  entry
         (file "")
         "* TODO %i\n%U" :immediate-finish t :empty-lines 1)
        ("N" "Task from content"  entry
         (file "")
         "* TODO %i\n%U" :immediate-finish t :empty-lines 1)
        ))

;;; Add-ons

(eval-when-compile (require 'use-package))

(use-package ob-html-chrome
  :after org
  ;; :custom (org-babel-html-chrome-chrome-executable "...")
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

;;; org.el ends here
