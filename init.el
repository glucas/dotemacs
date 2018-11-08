;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

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

;;; init.el ends here
