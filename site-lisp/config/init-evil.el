;;; init-evil.el ---

(require 'goto-chg)
(require 'evil)
(require 'evil-surround)
;; 所有的按键绑定在 site-lisp/extensions/evil/evil-maps.el 中
(my-run-with-idle-timer 1 #'ivy-mode) ;; it enables ivy UI for `kill-buffer'

(evil-mode 1)
(global-evil-surround-mode 1)

;; 在一些modes 中关闭 evil-mode
(evil-set-initial-state 'snails-mode 'emacs)
(evil-set-initial-state 'eaf-mode 'emacs)
(evil-set-initial-state 'color-rg-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'minibuffer-inactive-mode 'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'special-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'log-edit-mode 'emacs)
(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
;; (evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'woman-mode 'emacs)
(evil-set-initial-state 'profiler-report-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'ranger-mode 'emacs)
(evil-set-initial-state 'compilation-mode 'emacs)
(evil-set-initial-state 'ivy-occur-mode 'emacs)
(evil-set-initial-state 'ivy-occur-grep-mode 'emacs)
(evil-set-initial-state 'messages-buffer-mode 'emacs)
(evil-set-initial-state 'js2-error-buffer-mode 'emacs)
(evil-set-initial-state 'ffip-diff-mode 'emacs)
(evil-set-initial-state 'speedbar-mode 'emacs)
;; (evil-set-initial-state 'vc-msg-mode 'normal)

;; {{ change mode-line color by evil state
(defconst my-default-color (cons (face-background 'mode-line)
                                 (face-foreground 'mode-line)))
(defun my-show-evil-state ()
  "Change mode line color to notify user evil current state."
  (let* ((color (cond ((minibufferp) my-default-color)
                      ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                      ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                      ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                      (t my-default-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))
;; (add-hook 'post-command-hook #'my-show-evil-state)
;; }}

(message "> init-evil.el")
(provide 'init-evil)
;;; init-evil.el ends here
