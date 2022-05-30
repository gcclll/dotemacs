;;; init-projectile.el ---

;;; Require
(require 'projectile)

;;; Code
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; for mac
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map) ;; for linux/window
(setq projectile-enable-caching t)

(provide 'init-projectile)
;;; init-projectile.el ends here
