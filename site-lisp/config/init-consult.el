;;; init-consult.el ---
;; https://github.com/minad/consult/wiki

(require 'consult)
(require 'consult-imenu)
;; (require 'consult-xref)
(require 'consult-register)
(require 'consult-org)
(require 'consult-org-roam)
(require 'selectrum)
(require 'orderless)

;; (require 'consult-selectrum)
(require 'marginalia)
(require 'embark)
(require 'embark-consult)
;; (require 'vertico)

;; (require 'prescient)
;; (require 'selectrum-prescient)

(selectrum-mode +1)
(setq completion-styles '(substring orderless))
(setq selectrum-refine-candidates-function #'orderless-filter)
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
;; (vertico-mode)
(marginalia-mode)
(consult-org-roam-mode 1)
;; to make sorting and filtering more intelligent

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

;; replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

(defcustom my-consult-ripgrep-or-line-limit 300000
  "Buffer size threshold for `my-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
  :type 'integer)

(defun my-consult-ripgrep-or-line ()
  "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
  (interactive)
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (ignore-errors
            (file-remote-p buffer-file-name))
          (jka-compr-get-compression-info buffer-file-name)
          (<= (buffer-size)
              (/ my-consult-ripgrep-or-line-limit
                 (if (eq major-mode 'org-mode) 4 1))))
      (consult-line)
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (let ((consult-ripgrep-command
           (concat "rg "
                   "--null "
                   "--line-buffered "
                   "--color=ansi "
                   "--max-columns=250 "
                   "--no-heading "
                   "--line-number "
                   ;; adding these to default
                   "--smart-case "
                   "--hidden "
                   "--max-columns-preview "
                   ;; add back filename to get parsing to work
                   "--with-filename "
                   ;; defaults
                   "-e ARG OPTS "
                   (shell-quote-argument buffer-file-name))))
      (consult-ripgrep))))

(message "> init-consult.el")
(provide 'init-consult)
;;; init-consult.el ends here
