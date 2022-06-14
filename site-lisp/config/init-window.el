;;; init-window.el --- Init window

;;; Require
(require 'windresize)
(require 'window-extension)
(require 'winum)
(require 'ace-window)
(require 'winpoint)
(require 'buffer-move)
(require 'watch-other-window)
(require 'buffer-extension)
(require 'toggle-one-window)

(global-set-key (kbd "C-x o") 'ace-window)
(with-eval-after-load 'ace-window
  (setq aw-background t)
  (setq aw-char-position 'top-left)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(window-point-remember-mode 1)
(setq winpoint-non-restore-buffer-list '("*Group*"))

(provide 'init-window)

;;; init-window.el ends here
