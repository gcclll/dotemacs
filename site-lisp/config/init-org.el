;;; init-org.el --- Configure for org-mode

;;;###autoload
(defun gcl/indent-org-block-automatically ()
  (interactive)
  (when (org-in-src-block-p)
   (org-edit-special)
   (indent-region (point-min) (point-max))
   (org-edit-src-exit)))

;;;###autoload
(defun gcl/org-html-export-to-html
   (&optional async subtreep visible-only body-only ext-plist)
   (interactive)
   (let* ((extension (concat "." org-html-extension))
      (file (org-export-output-file-name extension subtreep (concat user-blog-dir "posts/")))
      (org-export-coding-system org-html-coding-system))
     (if async
     (org-export-async-start
         (lambda (f) (org-export-add-to-stack f 'html))
      (let ((org-export-coding-system org-html-coding-system))
         `(expand-file-name
           (org-export-to-file
            'html ,file ,subtreep ,visible-only ,body-only ',ext-plist))))
       (let ((org-export-coding-system org-html-coding-system))
     (org-export-to-file
      'html file subtreep visible-only body-only ext-plist)))))

(dolist (hook (list
               'org-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'eaf)

                     (setq truncate-lines nil) ;默认换行
                     )))

(let ((emacs-font-size 14)
      (emacs-font-name "WenQuanYi Micro Hei Mono"))
  (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
  (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name)))

;; org base
(with-eval-after-load 'org
  ;; 变量设置
  (setq org-log-done t
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-into-drawer "LOGBOOK"
        org-edit-src-content-indentation 0
        org-hide-emphasis-markers t 
        org-src-preserve-indentation t
        org-agenda-start-on-weekday nil
        org-agenda-span 14
        org-agenda-window-setup 'current-window
        org-fast-tag-selection-single-key 'expert
        org-agenda-inhibit-startup t ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-odt-preferred-output-format "docx" ;ODT转换格式默认为docx
        org-tags-column 80
        org-startup-folded nil ;默认展开内容
        org-startup-indented t                 ;默认缩进内容
        org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")
                            (sequence "[ ](t)" "[-](s)" "[?](w)" "|" "[X](x)"))
        org-tag-alist (quote (("@home" . ?h)
                              (:newline)
                              ("CANCELLED" . ?c)))
        org-todo-keyword-faces `(("TODO" . org-warning)
                                 ("DOING" . "yellow")
                                 ("CANCELLED" . (:foreground "blue" :weight bold))
                                 ("NEXT" . "green")
                                 ("[ ]" . "green")
                                 ("[-]" . "yellow")
                                 ("[?]" . "red"))) ;; ends setq

  ;; funcs
  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (concat (file-name-as-directory lazycat-emacs-root-dir)
                                 (file-name-as-directory "template")
                                 "template.docx")))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file
                             ))
      (message "Convert finish: %s" docx-file)))
  
  (defun org-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC Nerd 15")
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))

  (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)

  ;; requires
  (require 'org-special-block-extras)
  (add-hook #'org-mode-hook #'org-special-block-extras-mode)
  
  (require 'org-mac-link)
  (require 'org-protocol)
  (require 'org-download)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (require 'org-sort-tasks)
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (require 'org-super-agenda)
  

  ;; org-roam packages
  (require 'websocket)
  (require 'org-roam)
  (require 'org-roam-ui)
  (setq org-roam-ui-open-on-start nil
        org-roam-ui-update-on-save t
        org-roam-ui-follow t
        org-roam-ui-sync-theme t
        org-roam-ui-browser-function #'xwidget-webkit-browse-url)
  (setq org-roam-capture-templates
        '(
          ("a" "auto export" plain "%?" :target
           (file+head "${slug}.org" "#+SETUPFILE:~/.gclrc/org/hugo_setup.org
#+HUGO_SLUG: ${slug}
#+PROPERTY: header-args:js :exports both
#+PROPERTY: header-args :noweb no-export
#+TITLE: ${title}\n

<badge: GCCLL | Homepage | green | / | gnu-emacs | tinder>
...

\* COMMENT Local Variables       :ARCHIVE:
# Local Variables:
# after-save-hook: gcl/org-html-export-to-html
# End:")
           :unnarrowed t)
          ("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ))
  )

;; 当org文件有变动时自动保存
(with-eval-after-load 'org-agenda
  (advice-add #'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-set-effort :after #'org-save-all-org-buffers)
  (advice-add #'org-schedule :after (lambda (&rest _)
                                      (org-save-all-org-buffers)))
  (advice-add #'org-deadline :after (lambda (&rest _)
                                      (org-save-all-org-buffers)))
  (advice-add #'+org-change-title :after (lambda (&rest _)
                                           (org-save-all-org-buffers)))
  (advice-add #'org-cut-special :after #'org-save-all-org-buffers)
  (advice-add #'counsel-org-tag :after #'org-save-all-org-buffers)
  (advice-add #'org-agenda-todo :after #'aj-org-agenda-save-and-refresh-a)
  (advice-add #'org-todo :after (lambda (&rest _)
                                  (org-save-all-org-buffers)))
  (advice-add #'org-agenda-kill :after #'aj-org-agenda-save-and-refresh-a)
  (advice-add #'org-agenda-refile :after (lambda (&rest _)
                                           "Refresh view."
                                           (if (string-match "Org QL" (buffer-name))
                                               (org-ql-view-refresh)
                                             (org-agenda-redo))))

  (setq org-agenda-tags-column 80
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-start-on-weekday nil
        org-agenda-todo-list-sublevels t
        org-agenda-include-deadlines t
        org-agenda-log-mode-items '(closed clock state)
        org-agenda-block-separator nil
        )
  )

(provide 'init-org)

;;; init-org.el ends here
