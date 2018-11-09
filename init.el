;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

;; Time-stamp: <2018-11-09 11:50:35 glucas>
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

(use-package no-littering               ; Keep emacs.d organized
  :demand
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


;;; Completion framework

(use-package smex)                      ; Track command frequency

(use-package ivy                        ; Incremental Vertical completYon
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :init
  (ivy-mode))

(use-package swiper                     ; Ivy-based incremental search
  :requires ivy
  :custom
  (swiper-action-recenter t)
  (swiper-include-line-number-in-search t)
  :bind (:map isearch-mode-map
	      ("SPC" . gml/swiper-from-isearch))
  :config
  (defun gml/swiper-from-isearch ()
    "Invoke swiper from isearch, adding a space to the query."
    (interactive)
    (if isearch-regexp
	(setq isearch-regexp (concat isearch-regexp " "))
      (setq isearch-string (concat isearch-string " ")))
    (swiper-from-isearch)))

(use-package counsel			; Ivy commands
  :requires ivy
  :init
  (counsel-mode))


;;; Editing

(use-package magit                      ; Git integration
  :if (executable-find "git")
  :custom
  (magit-repository-directories '(("~/dev/src" . 3)))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

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


;;; Key Bindings

(bind-keys
 ("M-o" . other-window)
 ("C-x k" . kill-this-buffer))

;;; init.el ends here
