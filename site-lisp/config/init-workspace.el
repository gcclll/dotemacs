
;; 不好用
;; (require 'perspective)
;; (global-set-key (kbd "C-x C-b") 'persp-list-buffers)
;; (customize-set-variable 'persp-mode-prefix-key (kbd "M-,"))
;; (add-hook 'ibuffer-hook
;;           (lambda ()
;;             (persp-ibuffer-set-filter-groups)
;;             (unless (eq ibuffer-sorting-mode 'alphabetic)
;;               (ibuffer-do-sort-by-alphabetic))))

;; (consult-customize consult--source-buffer :hidden t :default nil)
;; (add-to-list 'consult-buffer-sources persp-consult-source)
;; (persp-mode)

(provide 'init-workspace)
