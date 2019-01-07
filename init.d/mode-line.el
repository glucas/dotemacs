;;; mode-line.el --- Configure mode line  -*- lexical-binding: t; -*-

;;
;;; Code:

(eval-when-compile (require 'use-package))

;; Flash mode line for 'bell'
(setq ring-bell-function
      (defun my/flash-mode-line ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Mode menu
(use-package minions
  :custom
  (minions-mode-line-delimiters '("" . ""))
  (minions-whitelist
   '((abbrev-mode)
     (auto-fill-mode)
     (auto-revert-mode)
     (auto-revert-tail-mode)
     (display-line-numbers-mode)
     (flyspell-mode)
     (highlight-changes-mode)
     (outline-minor-mode)
     (overwrite-mode)
     (subword-mode)
     (whitespace-mode)))
  (minions-blacklist
   '(eldoc-mode))
  :config
  (minions-mode t))

;; Fancy powerline mode line
(use-package spaceline-config
  :if (image-type-available-p 'xpm)

  :custom-face
  (mode-line ((t (:background "gray85" :foreground "black" :box (:line-width -1 :style released-button)))))
  (which-func ((t ())))
  (powerline-active1 ((t (:inherit mode-line :background "grey22" :foreground "lemon chiffon"))))
  (powerline-active2 ((t (:inherit mode-line :background "grey40" :foreground "white"))))
  (powerline-inactive1 ((t (:inherit mode-line-inactive :background "grey11" :foreground "white"))))
  (spaceline-modified ((t (:background "DarkGoldenrod2" :foreground "#3E3D31" :inherit (quote mode-line)))))
  (spaceline-read-only ((t (:background "light blue" :foreground "#3E3D31" :inherit (quote mode-line)))))
  (spaceline-unmodified ((t (:background "DarkSeaGreen2" :foreground "#3E3D31" :inherit (quote mode-line)))))

  :init
  (require 'spaceline)

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)

  (spaceline-define-segment narrow
    "Show when buffer is narrowed."
    (when (buffer-narrowed-p)
      "Narrowed"))

  (spaceline-define-segment minions
    (if (bound-and-true-p minions-mode)
        (format-mode-line minions-mode-line-modes)
      (spaceline-minor-modes-default)))

  (defun my/spaceline-theme ()
    "Install a variation of `spaceline-emacs-theme'."

    (spaceline-install
      `(((buffer-modified) :face highlight-face)
        ((buffer-id which-function) :separator " @ " :face highlight-face :tight-left t)
        remote-host
        projectile-root
        (version-control :when active))

      `(selection-info
        ((process minions) :when active)
        ((buffer-encoding-abbrev
          point-position
          line-column)
         :separator " | " :when active)
        ((narrow buffer-position hud) :face highlight-face)))

    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

  (defun my/spaceline-buffer-encoding-on ()
    "Show buffer encoding in mode line."
    (setq-local spaceline-buffer-encoding-abbrev-p t))

  (add-hook 'after-init-hook #'my/spaceline-theme)
  (add-hook 'prog-mode-hook #'my/spaceline-buffer-encoding-on))

;;; mode-line.el ends here
