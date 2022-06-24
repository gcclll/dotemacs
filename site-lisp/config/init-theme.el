;; -*- coding: utf-8; lexical-binding: t; -*-

(when (display-graphic-p)
  (require 'all-the-icons)
  (require 'all-the-icons-completion)
  
  (all-the-icons-completion-mode))

;; (defface font-lock-negation-char-face '((:weight bold)))
;; (face-remap-add-relative 'font-lock-negation-char-face '((:weight bold)))

(provide 'init-theme)
;;; init-theme.el ends here

