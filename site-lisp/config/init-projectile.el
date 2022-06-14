;;; init-projectile.el ---

;;; Require
(require 'projectile)

;;; Code
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; for mac
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map) ;; for linux/window
(setq projectile-enable-caching t
      projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
      projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".lo" ".la" ".out" ".sock"))
(add-to-list 'projectile-globally-ignored-directories "elpa")

(with-eval-after-load 'projectile
  (require 'counsel-projectile)
  (counsel-projectile-mode 1)
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)
  ) 

  (message "> init-projectile.el")
(provide 'init-projectile)
;;; init-projectile.el ends here
