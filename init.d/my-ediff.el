(require 'ediff)

;; Don't create a separate frame
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;; Save/restore window configuration
(defvar ediff-saved-window-configuration nil
  "Window configuration to restore after exiting an Ediff session.")

(defun ediff-save-window-config ()
  (setq ediff-saved-window-configuration (current-window-configuration)))

(defun ediff-restore-window-config ()
  (when ediff-saved-window-configuration
    (set-window-configuration ediff-saved-window-configuration))

  ;; ediff-current-file leaves FILE buffers we don't need
  (kill-matching-buffers "FILE=.+" nil t))

(add-hook 'ediff-before-setup-hook #'ediff-save-window-config)
(add-hook 'ediff-quit-hook #'ediff-restore-window-config t)
(add-hook 'ediff-suspend-hook #'ediff-restore-window-config t)
