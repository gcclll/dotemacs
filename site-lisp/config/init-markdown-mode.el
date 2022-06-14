;;; init-markdown-mode.el --- Configure for markdown mode.
(require 'markdown-mode)
(require 'olivetti)

(dolist (hook (list
               'markdown-mode-hook
               ))
  (add-hook hook
            #'(lambda ()
                (olivetti-mode 1)
                (olivetti-set-width 120)
                )))

(message "> init-markdown.el")
(provide 'init-markdown-mode)

;;; init-markdown-mode.el ends here
