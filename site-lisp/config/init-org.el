(require 'org-superstar)
(require 'org-modern)
(require 'org-mac-link)
(require 'org-protocol)
(require 'org-download)
(require 'websocket)
(require 'org-roam)
(require 'org-roam-ui)
(require 'org-extra-emphasis)
(require 'ox-odt)


(defun gcl/indent-org-block-automatically ()
  (interactive)
  (when (org-in-src-block-p)
   (org-edit-special)
   (indent-region (point-min) (point-max))
   (org-edit-src-exit)))

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

(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(with-eval-after-load 'org
  ;; baisc
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
                                 ("[?]" . "red")))

  ;; org-modern
  (setq org-superstar-leading-bullet ?\s)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  ;; org-roam
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
odt_prettify_xml:
#+HTML_HEAD: <style>
#+HTML_HEAD: .textbox { color: #000000; background: #fff8ca; border: 1px solid #000000; padding: 1em; }
#+HTML_HEAD: </style>
#+PROPERTY: header-args:js :exports both
#+PROPERTY: header-args :noweb no-export
#+TITLE: ${title}\n

<badge: GCCLL | Homepage | green | / | gnu-emacs | tinder>
...

\* COMMENT Local Variables       :ARCHIVE:
# Local Variables:
# eval: (buffer-face-mode 1)
# org-hide-emphasis-markers: t
# after-save-hook: gcl/org-html-export-to-html
# End:")
           :unnarrowed t)
          ("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ))
  
  )


;; ox-odt + org-extra-emphasis
(custom-set-faces
 '(org-default ((t ( 
                    :background "#fff8ca"
                    :inherit default
                    ))))
 '(org-extra-emphasis ((t ( 
                           :background "#fff8ca"
                           :height 1.5
                           :inherit default
                           :weight bold
                           :width normal
                           ))))
 '(org-extra-emphasis-01 ((t ( 
                              :family "CherryBomb"
                              :foreground "#A53E2D"
                              :inherit org-extra-emphasis
                              :underline t
                              ))))
 '(org-extra-emphasis-02 ((t ( 
                              :family "Eater"
                              :foreground "#EEB97E"
                              :inherit org-extra-emphasis
                              :weight heavy
                              ))))
 '(org-extra-emphasis-03 ((t ( 
                              :family "Diplomata"
                              :foreground "#683231"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-04 ((t ( 
                              :family "Linux Biolinum Keyboard O"
                              :foreground "#353D8B"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-05 ((t ( 
                              :family "Finger Paint"
                              :foreground "#54436E"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-06 ((t ( 
                              :family "Fuzzy Bubbles"
                              :foreground "#D65D8F"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-07 ((t ( 
                              :family "Gloria Hallelujah"
                              :foreground "#E5C494"
                              :inherit org-extra-emphasis
                              :weight extra-bold
                              ))))
 '(org-extra-emphasis-08 ((t ( 
                              :family "Henny Penny"
                              :foreground "#0B9FCF"
                              :inherit org-extra-emphasis
                              ))))
 '(org-extra-emphasis-09 ((t ( 
                              :family "Humor Sans"
                              :foreground "#C9564C"
                              :inherit org-extra-emphasis
                              )))))

(provide 'init-org)
;;; init-org.el ends here
