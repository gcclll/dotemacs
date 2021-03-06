;;; init-qt.el --- Setup for qt mode
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'cc-mode)
                     (require 'modern-cpp-font-lock)

                     (defun c-mode-style-setup ()
                       (interactive)
                       ;; cpp font lock.
                       (modern-c++-font-lock-global-mode t)

                       ;; base-style
                       (c-set-style "stroustrup")

                       ;; qt keywords and stuff ...
                       ;; set up indenting correctly for new qt kewords
                       (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                                      "\\|protected slot\\|private\\|private slot"
                                                      "\\)\\>")
                             c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                                      "\\|public slots\\|protected slots\\|private slots"
                                                      "\\)\\>[ \t]*:"))
                       (progn
                         ;; modify the colour of slots to match public, private, etc ...
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
                         ;; make new font for rest of qt keywords
                         (make-face 'qt-keywords-face)
                         (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
                         ;; qt keywords
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
                         ))
                     (c-mode-style-setup))))
(message "> init-c.el")
(provide 'init-c)

;;; init-qt.el ends here
