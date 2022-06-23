;;; init-consult.el ---
;; https://github.com/minad/consult/wiki

(require 'consult)
;; (require 'consult-imenu)
;; (require 'consult-xref)
;; (require 'consult-register)
;; (require 'consult-org)

(require 'selectrum)
(require 'orderless)

;; (require 'consult-selectrum)
;; (require 'marginalia)
;; (require 'vertico)
;; (require 'consult-org-roam)
;; (require 'prescient)
;; (require 'selectrum-prescient)


;; (selectrum-mode +1)
(setq completion-styles '(substring orderless))
;; (vertico-mode)
;; (marginalia-mode)
;; (consult-org-roam-mode 1)
;; to make sorting and filtering more intelligent
;; (selectrum-prescient-mode +1)
;; to save your command history on disk, so the sorting gets more
;; intelligent over time
;; (prescient-persist-mode +1)

;; (setq consult-org-roam-grep-func #'consult-ripgrep)

;; (setq completion-styles '(substring basic))

;; (setq register-preview-delay 0.5 register-preview-function #'consult-register-format
;;       xref-show-xrefs-function #'consult-xref)

;; (consult-customize consult-theme 
;;                    :preview-key '(:debounce 0.2
;;                                             any) 
;;                    consult-ripgrep consult-git-grep consult-grep consult-bookmark
;;                    consult-recent-file consult-xref consult--source-bookmark
;;                    consult--source-recent-file consult--source-project-recent-file 
;;                    consult-org-roam-forward-links
;;                    :preview-key (kbd "M-."))


(message "> init-consult.el")
(provide 'init-consult)
;;; init-consult.el ends here
