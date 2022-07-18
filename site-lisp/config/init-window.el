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

(defun split-window--select-window (orig-func &rest args)
  "Switch to the other window after a `split-window'"
  (let ((cur-window (selected-window))
        (new-window (apply orig-func args)))
    (when (equal (window-buffer cur-window) (window-buffer new-window))
      (select-window new-window))
    new-window))
(advice-add 'split-window :around #'split-window--select-window)

(message "> init-window.el")
(provide 'init-window)

;;; init-window.el ends here
