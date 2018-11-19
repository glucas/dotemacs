;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

;; Time-stamp: <2018-11-19 17:10:35 glucas>
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

;; load custom settings
(defgroup my/host nil
  "Customizations local to a specific host."
  :group 'initialization)

(defcustom my/source-root-dir "~/src" "Root directory for source repositories." :group 'my/host)

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
  (setq custom-theme-directory (no-littering-expand-etc-file-name "themes/"))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (file-truename no-littering-var-directory))
    (add-to-list 'recentf-exclude (file-truename no-littering-etc-directory))))

;; load OS- or host-specific themes
(dolist (theme (list system-type (intern (downcase (system-name)))))
  (condition-case
      nil (load-theme theme t)
    (error (message "Cannot load theme: %s" theme))))

;; Configure mode line
(load (locate-user-emacs-file "init.d/mode-line"))


;;; Configure Packages

(use-package recentf                    ; Recent files
  :defer
  :custom
  (recentf-max-saved-items 100)
  :config
  (run-at-time t (* 5 60) (lambda () (let ((inhibit-message t)) (recentf-save-list))))
  (add-to-list 'recentf-exclude ".*autoloads.el$")
  (add-to-list 'recentf-exclude (file-truename (file-name-as-directory package-user-dir))))

(use-package dired                      ; Directory listings
  :bind
  ("C-x C-j" . dired-jump)
  ("C-x 4 j" . dired-jump-other-window)
  (:map dired-mode-map
        ("C-k" . my/dired-kill-line))
  :config
  (defun my/dired-kill-line ()
    "Kill the current line or subdirectory."
    (interactive)
    (if (dired-get-subdir)
	(dired-kill-subdir)
      (dired-kill-line 1))))

(use-package eww                        ; Emacs Web Wowser
  :custom
  (shr-use-fonts nil)
  :bind
  (:map eww-mode-map
        ("I" . my/eww-toggle-images))
  (:map eww-link-keymap
        ("I" . my/eww-toggle-images))
  (:map goto-map
        ("B" . eww-list-bookmarks))
  :config
  (setq-default shr-inhibit-images t)

  (defun my/eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on"))))

(use-package ediff                      ; Ediff
  :bind
  ("C-c d" . ediff-current-file)
  :config
  (load (locate-user-emacs-file "init.d/ediff")))

(use-package try                        ; Try packages without installing
  :commands (try try-and-refresh)
  :config
  (add-to-list 'recentf-exclude ".*/try.+.el$"))

(use-package hydra)                     ; Key bindings that stick around

(use-package buffer-protect             ; Protect buffers from being killed
  :preface
  (add-to-list 'load-path (expand-file-name  "github.com/glucas/buffer-protect" my/source-root-dir))
  :demand t)

(use-package yank-temp                  ; Copy text to temp buffer
  :preface
  (add-to-list 'load-path (expand-file-name  "github.com/glucas/yank-temp" my/source-root-dir))
  :bind ("C-c y" . yank-temp-from-clipboard)
  :config

  (defhydra hydra-setup-yank-temp (:color blue :timeout 3 :post (yank-temp-set-revert-point))
    ("l" lisp-interaction-mode "lisp")
    ("j" (progn (js-mode) (json-pretty-print-buffer)) "json")
    ("x" (progn (nxml-mode)) "xml")
    ("t" (progn (turn-on-orgtbl) (org-table-convert-region (point-min) (point-max) nil)) "table")
    ("c" (ansi-color-apply-on-region (point-min) (point-max)) "color"))

  (add-hook 'yank-temp-initialized-hook #'hydra-setup-yank-temp/body))

;;;; Ivy

(use-package smex)                      ; Track command frequency

(use-package ivy                        ; Incremental Vertical completYon
  :delight
  :custom
  (ivy-use-virtual-buffers t)
  (projectile-completion-system 'ivy)
  :bind
  ([remap isearch-backward-regexp] . ivy-resume)
  :init
  (ivy-mode))

(use-package ivy-hydra                  ; Additional key bindings for Ivy
    :requires (ivy hydra))

(use-package swiper                     ; Ivy-based incremental search
  :requires ivy
  :custom
  (swiper-action-recenter t)
  (swiper-include-line-number-in-search t)
  :bind
  ([remap isearch-forward-regexp] . swiper)
  (:map isearch-mode-map
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
  :delight
  :bind
  ("C-c m" . counsel-semantic-or-imenu)
  :init
  (counsel-mode))

;;;; Jump Around

(use-package avy                        ; jump to visible text
  :bind
  (:map search-map
        ("SPC" . avy-goto-word-or-subword-1)))

(use-package avy-zap                    ; jump + zap
  :bind
  ([remap goto-line] . avy-goto-line)
  ("M-z" . zap-up-to-char)
  ("M-Z" . avy-zap-up-to-char))

(use-package jump-char                  ; jump to char
  :bind ("M-m" . jump-char-forward))

(use-package ace-link                   ; jump to links
  :init (ace-link-setup-default))

(use-package ace-window                 ; jump to windows
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  :bind
  ("C-c o" . ace-window))

(use-package goto-last-change           ; jump to last change
  :bind
  ("C-M-z" . goto-last-change-with-auto-marks))

;;;; Editing

(use-package change-inner
  :bind
  ("C-c i" . change-inner)
  ("C-c I" . change-outer))

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

;;;; Projects

(use-package projectile
  :delight
  :custom
  (projectile-indexing-method 'alien)
  (projectile-switch-project-action 'projectile-commander)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :hook
  (prog-mode . projectile-mode)
  :init
  ;; Allow dir-locals to configure projectile
  (put 'projectile-project-name 'safe-local-variable #'stringp)
  (put 'projectile-enable-caching 'safe-local-variable #'booleanp)
  (put 'projectile-indexing-method 'safe-local-variable
       (lambda (arg) (memq arg '(native alien)))))

(use-package bug-reference
  :hook
  (org-mode . bug-reference-mode)
  (prog-mode . bug-reference-prog-mode)
  :init
  (defcustom bug-reference-url-format nil
    "Format used to turn a bug number into a URL."
    :group 'bug-reference)
  (put 'bug-reference-bug-regexp 'safe-local-variable #'stringp)
  (put 'bug-reference-url-format 'safe-local-variable #'stringp))

;;;; External Tools

(use-package magit                      ; git
  :if (executable-find "git")
  :custom
  (magit-repository-directories `((,my/source-root-dir . 3)))
  :bind
  (("C-x g" . magit-status)))

(use-package deadgrep                   ; ripgrep
  :if (executable-find "rg")
  :commands (deadgrep)
  :init
  (defalias 'rg 'deadgrep))

;;; Editing Modes

(load (locate-user-emacs-file "init.d/file-modes"))

;;;; Server

(use-package server                     ; Emacs daemon
  :bind (("C-x C-3" . server-edit)))    ; C-x C-#

(use-package edit-server
  :defer 10                             ; Browser edit server
  :init
  (edit-server-start))


;;; Key Bindings

(use-package unfill			; toggle fill/unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package transpose-frame            ; rotate windows in frame
  :bind ("C-c t" . transpose-frame))

(use-package easy-kill                  ; enhanced kill-ring-save
  :disabled t
  :bind ([remap kill-ring-save] . easy-kill))

(defmacro new-split-command (f)
  "Switch to new window after a split command."
  `(lambda ()
     (interactive)
     (funcall ,f)
     (set-window-buffer (next-window) (other-buffer))
     (other-window 1)))

(bind-key [remap split-window-below] (new-split-command 'split-window-below))
(bind-key [remap split-window-right] (new-split-command 'split-window-right))

(bind-keys
 ([remap list-buffers] . ibuffer-other-window)
 ([remap kill-buffer] . kill-this-buffer))

(bind-keys*
 :filter (not (minibufferp))
 ("M-o" . other-window)
 ("M-i" . mode-line-other-buffer))

;;; init.el ends here
