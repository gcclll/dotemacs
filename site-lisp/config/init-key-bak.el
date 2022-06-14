;;; ### Org ###
;;; --- 笔记管理和组织
(lazy-load-global-keys
 '(
   ("s-s" . one-key-menu-org)      ;Org 文件
   ("C-c r" . org-remember)             ;Org-remeber
   ;; org-roam
   ("C-c n a" . org-id-get-create)
   ("C-c n c" . org-roam-capture)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n r" . org-roam-ref-add)
   ("C-c n u". org-roam-ui-open)
   ;; org-mac-link
   ("C-c 0" . org-mac-link-chrome-insert-frontmost-url)
   ;; agenda
   ("C-c a" . org-agenda)
   )
 "init-org")

;; ### vdiff ###
(lazy-load-global-keys
 '(
   ("M-s-u" . vdiff-buffers))
 "vdiff")


(provide 'init-key)
