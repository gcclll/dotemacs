;;; init-iedit.el --- Init for iedit
(require 'iedit)
(require 'iedit-lib)
(require 'maple-iedit)

;;; iedit
;; (setq iedit-toggle-key-default (kbd "s-o"))
(when iedit-toggle-key-default
  (define-key global-map iedit-toggle-key-default 'iedit-mode)
  (define-key isearch-mode-map iedit-toggle-key-default 'iedit-mode-from-isearch)
  (define-key esc-map iedit-toggle-key-default 'iedit-execute-last-modification)
  (define-key help-map iedit-toggle-key-default 'iedit-mode-toggle-on-function))

;; maple-iedit
(setq maple-iedit-ignore-case t)

(defhydra maple/iedit ()
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))

(message "> init-iedit.el")
(provide 'init-iedit)

;;; init-iedit.el ends here
