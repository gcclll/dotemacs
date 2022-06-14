;;; init-rjsx-mode.el

(require 'rjsx-mode)

(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(with-eval-after-load 'rjsx-mode
  ;; (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  ;; (define-key rjsx-mode-map ">" nil)
)

(provide 'init-rjsx-mode)
;;; init-rjsx-mode.el ends here
