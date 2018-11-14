(use-package delight                    ; Configure mode lighters
  :init
  (delight
   '((eldoc-mode nil eldoc)
     (auto-revert-mode nil autorevert))))

(use-package spaceline-config           ; Powerline
  :if (image-type-available-p 'xpm)

  :custom-face
  (mode-line ((t (:background "gray85" :foreground "black" :box (:line-width -1 :style released-button)))))
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

  (defun my/spaceline-theme ()
    "Install a variation of `spaceline-emacs-theme'."

    (spaceline-define-segment evil-state-custom (upcase (format "%S" evil-state)))

    (spaceline-install
     `(((buffer-modified) :face highlight-face)
       ((buffer-id remote-host) :separator " : " :face highlight-face :tight-left t)
       projectile-root
       (version-control :when active)
       which-function)

     `(selection-info
       (minor-modes :when active :separator "")
       evil-state
       ((process major-mode) :when active)
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
  (add-hook 'prog-mode-hook #'my/spaceline-buffer-encoding-on)

  :config
  (defadvice powerline-major-mode (around delight-powerline-major-mode activate)
    (let ((inhibit-mode-name-delight nil)) ad-do-it)))
