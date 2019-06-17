;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-17 11:25:11 glucas>
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
(setq use-package-enable-imenu-support t)
(require 'use-package)

;; keep .emacs.d organized
(use-package no-littering
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
(load (locate-user-emacs-file "init.d/my-mode-line"))


;;; Configure Packages

(use-package package
  :bind
  (:map package-menu-mode-map
        ("s" . my/package-menu-filter-by-status)
        ("a" . my/package-menu-find-marks))
  :config
  (defun my/package-menu-find-marks ()
    "Find packages marked for action in *Packages*."
    (interactive)
    (occur "^[A-Z]"))

  (defun my/package-menu-filter-by-status (status)
    "Filter the *Packages* buffer by status."
    (interactive
     (list (completing-read
            "Status: " '("new" "installed" "dependency" "obsolete"))))
    (package-menu-filter (concat "status:" status))))

(use-package recentf                    ; Recent files
  :hook
  (emacs-startup . recentf-mode)
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

(use-package dired-collapse
  :hook
  (dired-mode . dired-collapse-mode))

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

(load (locate-user-emacs-file "init.d/my-browser"))

(use-package ediff                      ; Ediff
  :bind
  ("C-c d" . ediff-current-file)
  :config
  (load (locate-user-emacs-file "init.d/my-ediff")))

(use-package try                     ; Try packages without installing
  :commands (try try-and-refresh)
  :config
  (add-to-list 'recentf-exclude ".*/try.+.el$"))

(use-package hydra)                   ; Key bindings that stick around

(use-package use-package-hydra       ; Define hydras with use-package
  :after hydra)

(use-package buffer-protect           ; Keep buffers from being killed
  :preface
  (add-to-list 'load-path (expand-file-name  "github.com/glucas/buffer-protect" my/source-root-dir)))

(use-package yank-temp                  ; Copy text to temp buffer
  :preface
  (add-to-list 'load-path (expand-file-name  "github.com/glucas/yank-temp" my/source-root-dir))
  :bind ("C-c y" . yank-temp-from-clipboard)
  :config
  (add-hook 'window-setup-hook
            (lambda () (when (get-buffer "*scratch*")
                    (with-current-buffer "*scratch*"
                      (yank-temp-set-revert-point)))))
  :config
  (defhydra hydra-setup-yank-temp (:color blue :timeout 3 :post (yank-temp-set-revert-point))
    ("l" lisp-interaction-mode "lisp")
    ("j" (progn (js-mode) (json-pretty-print-buffer)) "json")
    ("x" (progn (nxml-mode)) "xml")
    ("t" (progn (turn-on-orgtbl) (org-table-convert-region (point-min) (point-max) nil)) "table")
    ("c" (ansi-color-apply-on-region (point-min) (point-max)) "color"))

  (add-hook 'yank-temp-initialized-hook #'hydra-setup-yank-temp/body))

;;;; Appearance

(use-package stripe-buffer              ; Row highlighting in tables/lists
  :hook
  ((archive-moden
    bookmark-bmenu-mode
    dired-mode
    eww-bookmark-mode
    ibuffer-mode
    package-menu-mode
    occur-mode
    vc-dir-mode)
   . stripe-listify-buffer)
  :custom-face
  (stripe-highlight ((t (:background "ghost white"))))
  (stripe-hl-line ((t (:inherit hl-line)))))

(use-package hl-line                    ;  Highlight line
  :commands
  (hl-line-mode)
  :custom-face
  (hl-line ((t (:background "lavender")))))

(use-package show-eol
  :commands
  (show-eol-mode global-show-eol-mode))

;;;; Ivy

(use-package smex)                      ; Track command frequency

(use-package ivy                        ; Incremental Vertical completYon
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
  ([remap isearch-forward-regexp] . swiper-isearch)
  (:map isearch-mode-map
        ("SPC" . my/swiper-from-isearch))
  :config
  (defun my/swiper-from-isearch ()
    "Invoke swiper from isearch, adding a space to the query."
    (interactive)
    (let*
        ((query-string (concat isearch-string " "))
         (query (if isearch-regexp
                    query-string
                  (regexp-quote query-string))))
      (isearch-exit)
      (swiper-isearch query))))

(use-package counsel                    ; Ivy commands
  :requires ivy
  :bind
  ("C-c m" . counsel-semantic-or-imenu)
  :init
  (counsel-mode))

;;;; Jump Around

(use-package avy                        ; jump to visible text
  :bind
  ("M-M" . avy-goto-word-or-subword-1)
  ;; (:map search-map
  ;;       ("SPC" . avy-goto-word-or-subword-1))
  )

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
  :commands goto-last-change-with-auto-marks)

(use-package hl-todo                    ; jump to TODOs
  :hook
  (prog-mode . hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("M-s t" . hydra/hl-todo/body))
  :hydra
  (hydra/hl-todo
   (:hint nil)
   "
TODOs:
_j_: prevoius   _o_ccur
_k_: next       _q_uit
"
   ("o" hl-todo-occur)
   ("j" hl-todo-previous)
   ("k" hl-todo-next)
   ("q" nil nil :color blue)))

;;;; Editing

(use-package iedit                      ; edit all occurences
  :bind
  ("C-;" . iedit-mode))

(use-package change-inner               ; replace semantic units
  :bind
  ("C-c i" . change-inner)
  ("C-c I" . change-outer))

(use-package hilit-chg                  ; highlight recent changes
  :custom
  (highlight-changes-visibility-initial-state nil)
  :hook
  (find-file . highlight-changes-mode-turn-on))

(use-package diff-hl                    ; highlight uncommited changes
  :hook
  (emacs-startup . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode-unless-remote)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;;;; Completion

(use-package company                    ; Complete anything
  :commands
  (company-mode company-complete)
  :hook
  ((prog-mode nxml-mode) . company-mode)
  :custom
  (company-backends '(company-semantic
                      company-capf
                      company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)
                      company-dabbrev))
  :bind
  (:map company-mode-map
        ([remap indent-for-tab-command] . company-indent-or-complete-common))
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("M-." . company-show-location)
        ("C-o" . company-other-backend)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))

  :init
  (add-hook 'nxml-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends) '(company-nxml))))
  (add-hook 'css-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends) '(company-css)))))

(use-package company-quickhelp
  :after company
  :commands (company-quickhelp-mode)
  :config (company-quickhelp-mode 1))

;;;; Org

(use-package org                        ; Org mode
  :custom
  (org-log-done 'time)
  (org-startup-indented t)
  (org-use-speed-commands t)
  (org-directory "~/Dropbox/org")
  (org-default-notes-file "inbox.org")
  (org-agenda-files (list org-directory))
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
  (load (locate-user-emacs-file "init.d/my-org")))


;;;; Projects

(use-package projectile
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

(use-package eyebrowse
  :after
  hydra
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-wrap-around t)
  (eyebrowse-default-workspace-slot 0)
  :bind
  ("C-c C-l" . hydra/eyebrowse/body)
  :hydra
  ;; https://www.wisdomandwonder.com/article/10596/screencast-building-a-little-ui-to-manage-buffers
  (hydra/eyebrowse (:color blue :hint nil :body-pre (eyebrowse-mode 1))
                   "
current eyebrowse slot: %(eyebrowse--get 'current-slot)
 _j_ previous _k_ next _l_ last _u_ close _i_ choose _o_ rename _q_ quit
   _a_ 00 _s_ 01 _d_ 02 _f_ 03 _g_ 04 _z_ 05 _x_ 06 _c_ 07 _v_ 08 _b_ 09"
                   ("j" #'eyebrowse-prev-window-config :exit nil)
                   ("k" #'eyebrowse-next-window-config :exit nil)
                   ("l" #'eyebrowse-last-window-config)
                   ("u" #'eyebrowse-close-window-config :exit nil)
                   ("i" #'eyebrowse-switch-to-window-config)
                   ("o" #'eyebrowse-rename-window-config :exit nil)
                   ("q" nil)
                   ("a" #'eyebrowse-switch-to-window-config-0)
                   ("s" #'eyebrowse-switch-to-window-config-1)
                   ("d" #'eyebrowse-switch-to-window-config-2)
                   ("f" #'eyebrowse-switch-to-window-config-3)
                   ("g" #'eyebrowse-switch-to-window-config-4)
                   ("z" #'eyebrowse-switch-to-window-config-5)
                   ("x" #'eyebrowse-switch-to-window-config-6)
                   ("c" #'eyebrowse-switch-to-window-config-7)
                   ("v" #'eyebrowse-switch-to-window-config-8)
                   ("b" #'eyebrowse-switch-to-window-config-9))
  )

;;;; External Tools

(use-package magit                      ; git
  :if (executable-find "git")
  :custom
  (magit-repository-directories `((,my/source-root-dir . 3)))
  :bind
  (("C-x g" . magit-status)))

(use-package forge
  :after magit
  :init
  (setq emacsql-sqlite-c-compilers '("gcc")))

(use-package deadgrep                   ; ripgrep
  :if (executable-find "rg")
  :commands (deadgrep)
  :init
  (defalias 'rg 'deadgrep))

;;; Editing Modes

(load (locate-user-emacs-file "init.d/my-file-modes"))

;;;; Server

(use-package server                     ; Emacs daemon
  :bind (("C-x C-3" . server-edit)))    ; C-x C-#

(use-package edit-server
  :hook
  (emacs-startup . edit-server-start))

;;;; Help

(use-package which-key
  :custom
  (which-key-idle-delay 2.0)
  :hook
  (emacs-startup . which-key-mode))


;;; Key Bindings

(use-package unfill                     ; toggle fill/unfill
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
 ([remap delete-char] . delete-forward-char)
 ([remap list-buffers] . ibuffer-other-window)
 ([remap kill-buffer] . kill-this-buffer)
 ([remap upcase-word] . upcase-dwim)
 ([remap downcase-word] . downcase-dwim)
 ([remap capitalize-word] . capitalize-dwim)
 ([remap view-hello-file] . (lambda () (interactive) (find-file user-init-file)))
 )

(bind-keys*
 :filter (not (minibufferp))
 ("M-o" . other-window)
 ("M-i" . mode-line-other-buffer))

(defun my/clean-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun my/revert-buffer ()
  "Revert buffer, prompting for confirmation only if the buffer is modified."
  (interactive)
  (revert-buffer :ignore-auto (not (buffer-modified-p)) :preserve-modes))

(bind-keys
 ("C-c w" . my/clean-buffer)
 ("C-c r" . my/revert-buffer))

;; Navigate errors
(defhydra hydra-next-error
  (global-map "C-x")
  "
Errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
  ("`" next-error     nil)
  ("j" next-error     nil :bind nil)
  ("k" previous-error nil :bind nil)
  ("h" first-error    nil :bind nil)
  ("l" (condition-case err
           (while t
             (next-error))
         (user-error nil))
   nil :bind nil)
  ("q" nil            nil :color blue))

;; Navigate changes
(defhydra hydra/goto-changes
  (global-map "C-c" :hint nil
              :pre (highlight-changes-visible-mode 1)
              :post (highlight-changes-visible-mode 0))
  "
Changes:
"
  ("h" goto-last-change-with-auto-marks)
  ("j" diff-hl-previous-hunk)
  ("k" diff-hl-next-hunk)
  ("J" highlight-changes-previous-change)
  ("K" highlight-changes-next-change)
  ("q" nil nil :color blue :bind nil))


(defhydra hydra/page (ctl-x-map "" :pre (widen))
  "page"
  ("]" forward-page "next")
  ("[" backward-page "prev")
  ("n" narrow-to-page "narrow" :bind nil :exit t))

;;; init.el ends here
