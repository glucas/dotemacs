;; Time-stamp: <2018-11-13 15:08:00 glucas>
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(time-stamp))
 '(confirm-kill-emacs 'y-or-n-p)
 '(enable-recursive-minibuffers t)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(ls-lisp-use-insert-directory-program t)
 '(package-selected-packages
   '(ivy-hydra hydra projectile avy-zap ace-link ace-window avy easy-kill transpose-frame restclient lua-mode markdown-mode jump-char deadgrep try smex counsel swiper ivy magit no-littering terraform-mode unfill powershell use-package))
 '(prog-mode-hook '(prettify-symbols-mode show-paren-mode semantic-mode))
 '(scroll-preserve-screen-position 1)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(tab-always-indent 'complete)
 '(truncate-lines t)
 '(user-full-name "Greg Lucas"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 100 :width normal))))
 '(org-meta-line ((t (:height 0.85)))))
