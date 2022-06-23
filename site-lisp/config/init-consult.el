;;; init-consult.el ---
(require 'consult)
(require 'consult-xref)
(require 'consult-register)
(require 'vertico)
(vertico-mode)
(setq completion-styles '(substring basic))

(setq register-preview-delay 0.5 register-preview-function #'consult-register-format
      xref-show-xrefs-function #'consult-xref)

;; (consult-customize consult-theme 
;;                    :preview-key '(:debounce 0.2
;;                                             any) 
;;                    consult-ripgrep consult-git-grep consult-grep consult-bookmark
;;                    consult-recent-file consult-xref consult--source-bookmark
;;                    consult--source-recent-file consult--source-project-recent-file 
;;                    :preview-key (kbd "M-."))


(message "> init-consult.el")
(provide 'init-consult)
;;; init-consult.el ends here
