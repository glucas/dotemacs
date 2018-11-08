;;; init.el --- My personal Emacs configuration.     -*- lexical-binding: t; -*-

;; Author: Greg Lucas <greg@glucas.net>
;; Keywords: dotemacs,init,local

;;; Code:

;; init logging
(message "Starting Emacs %s" emacs-version)
(add-hook 'after-init-hook (lambda () (message "Initialization complete after %s" (emacs-init-time))) t)

;; load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file nil t)

;;; init.el ends here
