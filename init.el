;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

;; Time-stamp: <2018-11-09 16:57:34 glucas>
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

;; trust me
(setq disabled-command-function nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; prefer UTF-8
(prefer-coding-system 'utf-8)

;; keep quiet
(setq ring-bell-function
      (defun my/flash-mode-line ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; load custom settings
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file nil t)

;; load use-package
(setq use-package-compute-statistics t)
(require 'use-package)

;; keep .emacs.d organized
(use-package no-littering
  :demand
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


;;; Configure Packages


  :config

(use-package try                        ; Try packages without installing
  :commands (try try-and-refresh))

;;;; Ivy

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
	      ("SPC" . my/swiper-from-isearch))
  :config
  (defun my/swiper-from-isearch ()
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

;;;; Version Control

(use-package magit                      ; Git integration
;;;; Org

(use-package org                        ; Org mode
  :custom
  (org-log-done 'time)
  (org-startup-indented t)
  (org-use-speed-commands t)
  :custom-face
  (org-meta-line ((t (:height 0.85))))
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  (:map org-mode-map
        ("C-c C-8" . org-ctrl-c-star))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit))
  :config
  (load (locate-user-emacs-file "init.d/org")))

  :if (executable-find "git")
  :custom
  (magit-repository-directories '(("~/dev/src" . 3)))
  :bind (("C-c g" . magit-status)))

;;; Editing Modes

(use-package powershell                 ; Powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))


(use-package terraform-mode             ; Terraform
  :mode ("\\.tf\\(vars\\)?\\'" . terraform-mode)
  :config
  (add-hook 'terraform-mode-hook
            (defun my/terraform-mode-hook ()
              (when buffer-file-name
                (unless (string-match-p "\\.tfvars$" buffer-file-name)
                  (terraform-format-on-save-mode))
                (electric-pair-local-mode)))))


;;; Key Bindings

(use-package unfill			; togle fill/unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(bind-keys
 ("M-o" . other-window)                 ; faster other-window
 ("C-x k" . kill-this-buffer))          ; always kill current buffer

;;; init.el ends here
