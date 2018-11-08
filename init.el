;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

;; Time-stamp: <2018-11-08 18:00:46 glucas>
;; Author: Greg Lucas <greg@glucas.net>
;; Keywords: dotemacs,init,local

;;; Code:

;; init logging
(message "Starting Emacs %s" emacs-version)
(add-hook 'after-init-hook (lambda () (message "Initialization complete after %s" (emacs-init-time))) t)

;; minimal UI
(setq-default visible-bell t)
(setq inhibit-startup-message t)
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file nil t)

;; load use-package
(eval-when-compile
  (require 'use-package))



;;; Editing

(use-package unfill			; Single key to fill/unfill
  :bind ([remap fill-paragraph] . unfill-toggle))


;;; Modes

(use-package powershell                 ; Powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))


(use-package terraform-mode             ; Terraform
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode)
  :config
  (add-hook 'terraform-mode-hook
            (defun gml/terraform-mode-hook ()
              (when buffer-file-name
                (unless (string-match-p "\\.tfvars$" buffer-file-name)
                  (terraform-format-on-save-mode))
                (electric-pair-local-mode)))))

;;; init.el ends here
