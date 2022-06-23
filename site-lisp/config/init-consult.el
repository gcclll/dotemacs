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
(require 'vertico)
(require 'vertico-reverse)

;; (require 'prescient)
;; (require 'selectrum-prescient)

(selectrum-mode +1)
(setq completion-styles '(substring orderless))
(setq selectrum-refine-candidates-function #'orderless-filter)
(setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
(vertico-mode)
(marginalia-mode)
(consult-org-roam-mode 1)

;; replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)

;; {{ 大文件用line小文件用 ripgrep
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
;; }}

;; {{ org-clock ##
(setq org-clock-persist t)
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))

(defun consult-clock-in (&optional match scope resolve)
  "Clock into an Org heading."
  (interactive (list nil nil current-prefix-arg))
  (require 'org-clock)
  (org-clock-load)
  (save-window-excursion
    (consult-org-heading
     match
     (or scope
         (thread-last org-clock-history
           (mapcar 'marker-buffer)
           (mapcar 'buffer-file-name)
           (delete-dups)
           (delq nil))
         (user-error "No recent clocked tasks")))
    (org-clock-in nil (when resolve
                        (org-resolve-clocks)
                        (org-read-date t t)))))

(consult-customize consult-clock-in
                   :prompt "Clock in: "
                   :preview-key (kbd "M-.")
                   :group
                   (lambda (cand transform)
                     (let* ((marker (get-text-property 0 'consult--candidate cand))
                            (name (if (member marker org-clock-history)
                                      "*Recent*"
                                    (buffer-name (marker-buffer marker)))))
                       (if transform (substring cand (1+ (length name))) name))))
;; }}

;; {{ isearch-like ##
(defun my/consult-line-forward ()
  "Search for a matching line forward."
  (interactive)
  (consult-line))

(defun my/consult-line-backward ()
  "Search for a matching line backward."
  (interactive)
  (advice-add 'consult--line-candidates :filter-return 'reverse)
  (vertico-reverse-mode +1)
  (unwind-protect (consult-line)
    (vertico-reverse-mode -1)
    (advice-remove 'consult--line-candidates 'reverse)))

(with-eval-after-load 'consult
  (consult-customize my/consult-line-backward
                     :prompt "Go to line backward: ")
  (consult-customize my/consult-line-forward
                     :prompt "Go to line forward: "))

;; isearch 就够了啊
;; (global-set-key (kbd "C-s") 'my/consult-line-forward)
;; (global-set-key (kbd "C-r") 'my/consult-line-backward)
;; }}

;; {{ color funcs ##
(defvar consult-colors-history nil
  "History for `consult-colors-emacs' and `consult-colors-web'.")

;; No longer preloaded in Emacs 28.
(autoload 'list-colors-duplicates "facemenu")
;; No preloaded in consult.el
(autoload 'consult--read "consult")

(defun consult-colors-emacs (color)
  "Show a list of all supported colors for a particular frame.\

You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
selected color."
  (interactive
   (list (consult--read (list-colors-duplicates (defined-colors))
                        :prompt "Emacs color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert color))

;; Adapted from counsel.el to get web colors.
(defun counsel-colors--web-list nil
  "Return list of CSS colors for `counsult-colors-web'."
  (require 'shr-color)
  (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

(defun consult-colors-web (color)
  "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
selected color."
  (interactive
   (list (consult--read (counsel-colors--web-list)
                        :prompt "Color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert color))
;; }}

(message "> init-consult.el")
(provide 'init-consult)
;;; init-consult.el ends here
