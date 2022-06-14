;;; init-symbol-overlay.el --- Configuration for symbol-overlay
(require 'symbol-overlay)

;;; Code:

(lazy-load-set-keys
 '(
   ("s" . symbol-overlay-put)
   ("n" . symbol-overlay-jump-next)
   ("p" . symbol-overlay-jump-prev)
   ("w" . symbol-overlay-save-symbol)
   ("t" . symbol-overlay-toggle-in-scope)
   ("e" . symbol-overlay-echo-mark)
   ("d" . symbol-overlay-jump-to-definition)
   ("S" . symbol-overlay-isearch-literally)
   ("r" . symbol-overlay-rename)
   ("R" . symbol-overlay-query-replace)
   ("q" . symbol-overlay-remove-all)
   ("<" . symbol-overlay-jump-first)
   (">" . symbol-overlay-jump-last)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward)
   )
 symbol-overlay-map)

(provide 'init-symbol-overlay)

;;; init-symbol-overlay.el ends here
