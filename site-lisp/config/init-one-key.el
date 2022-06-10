;;; init-one-key.el --- Init one key

;;; Require
(require 'eaf)
(require 'eaf-file-manager)

;;; Code:

(one-key-create-menu
 "DIRECTORY"
 '(
   (("h" . "Home") . (lambda () (interactive) (eaf-open-in-file-manager "~/")))
   (("b" . "Book") . (lambda () (interactive) (eaf-open-in-file-manager "/data/Book")))
   (("j" . "Picture") . (lambda () (interactive) (eaf-open-in-file-manager "/data/Picture")))
   (("m" . "Music") . (lambda () (interactive) (eaf-open-in-file-manager "/data/Music")))
   ((";" . "Emacs Package") . (lambda () (interactive) (eaf-open-in-file-manager "/home/gcl/.emacs.d/site-lisp/extensions/emacs-application-framework")))
   (("c" . "Config") . (lambda () (interactive) (eaf-open-in-file-manager lazycat-emacs-config-dir)))
   (("'" . "Blog") . (lambda () (interactive) (eaf-open-in-file-manager "/home/gcl/github/mine/blog.cheng92.com")))
   (("v" . "Vue") . (lambda () (interactive) (eaf-open-in-file-manager "/home/gcl/github/vue/vue-next")))
   )
 t)

(provide 'init-one-key)

;;; init-one-key.el ends here
