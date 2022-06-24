;;; init-highlight.el --- Config for highlight-parentheses-mode

;;; Require
;; (require 'highlight-parentheses)
(require 'indent-guide)

;;; Code:
;; (setq hl-paren-colors '("DarkOrange" "DeepSkyBlue" "DarkRed"))

(indent-guide-global-mode)
(setq indent-guide-delay 0.1)

;; (face-remap-add-relative 'font-lock-keyword-face '((:weight bold)))

(message "> init-highlight.el")
(provide 'init-highlight)

;;; init-highlight.el ends here
