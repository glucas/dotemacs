;;; file-modes.el --- Configure modes for editing various types of files  -*- lexical-binding: t; -*-

;; 
;;; Code:

(eval-when-compile (require 'use-package))

(use-package nxml-                      ; XML
  :defer
  :custom
  (nxml-child-indent 4)
  (nxml-slash-auto-complete-flag t)
  (rng-nxml-auto-validate-flag nil))

(use-package powershell                 ; Powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :commands (powershell-mode powershell))

(use-package terraform-mode             ; Terraform
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode)
  :commands terraform-mode
  :config
  (add-hook 'terraform-mode-hook
            (defun my/terraform-mode-hook ()
              (when buffer-file-name
                (unless (string-match-p "\\.tfvars$" buffer-file-name)
                  (terraform-format-on-save-mode))
                (electric-pair-local-mode)))))

(use-package markdown-mode              ; Markdown
  :custom
  (markdown-command "pandoc")
  :mode ("\\.md\\'" . markdown-mode)
  :commands markdown-mode)

(use-package lua-mode                   ; Lua
  :mode ("\\.lua\\'" . lua-mode)
  :commands lua-mode)

(use-package restclient                 ; REST client
  :mode ("\\.rest\\'" . restclient-mode)
  :commands restclient-mode)

(use-package gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode)
  :commands gradle-mode)

(use-package groovy-mode
  :mode ("\\.gradle\\'" . groovy-mode)
  :commands groovy-mode)

;;; file-modes.el ends here
