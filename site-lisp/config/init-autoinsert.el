;;; init-autoinsert.el
(require 'autoinsert)
(setq auto-insert 'other
      auto-insert-query nil
      auto-insert-directory (concat user-emacs-directory "templates/")
      auto-insert-alist '(
                          ;; ("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "template.h")
                          ("\\.\\(jsx?\\|tsx?\\)\\'" . "my.js")
                          ("\\.\\(vue\\)\\'" . "my.vue")
                          ))
(add-hook 'find-file-hook #'auto-insert)

(message "> init-autoinsert.el")

(provide 'init-autoinsert)
;;; init-autoinsert.el ends here
