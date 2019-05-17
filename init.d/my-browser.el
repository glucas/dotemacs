;;; my-browser.el --- Configure external browser(s)  -*- lexical-binding: t; -*-

;;
;;; Code:

(require 'browse-url)

(defcustom opera-executable ""
  "The Opera executable."
  :type 'string
  :group 'browse-url)

;;;###autoload
(defun browse-url-opera (url &optional _new-window)
  "Ask the Opera browser to load URL.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  ;; TODO check executable exists
  (let (;; (process-environment (browse-url-process-environment))
        (opera (executable-find opera-executable)))
    (unless opera (error "Cannot find Opera executable: %s" opera-executable))
    (apply 'start-process
           (concat "opera " url)
           nil
           opera
           (append
            nil
            (list url)))))

;;; my-browser.el ends here
