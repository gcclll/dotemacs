;;; init-web-mode.el --- Init web mode
(require 'web-mode)
;; (require 'js)
(require 'grammatical-edit)
(require 'instant-rename-tag)
(require 'highlight-matching-tag)
(require 'emmet-mode)
(require 'js-doc)
(require 'js2-mode)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; (setenv "NODE_PATH" "/usr/local/lib/node_modules")

(setq web-mode-enable-auto-quoting nil) ;disable automatic insertion of double quotes, not easy to use if cursor in string

(highlight-matching-tag 1)

;; Emmit.
(setq web-mode-tag-auto-close-style 2) ;2 mean auto-close with > and </.
(setq web-mode-markup-indent-offset 2)

;; emmet-mode
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t) ;; 展开后光标停留在标签的里面
;; (setq emmet-move-cursor-after-expanding nil) ;; 展开后光标不动
;; (add-to-list 'emmet-jsx-major-modes 'jsx-major-mode) ;; 在 rjsx 中启用

;;; js-doc
(setq js-doc-mail-address user-mail-address
      js-doc-author (format "%s <%s>" user-full-name js-doc-mail-address)
      js-doc-url user-blog-url
      js-doc-license "MIT")

(one-key-create-menu
 "WEB"
 '(
   (("c" . "Clone") . web-mode-element-clone)
   (("a" . "Beginning") . web-mode-element-beginning)
   (("e" . "End") . web-mode-element-end)
   )
 t)

(provide 'init-web-mode)

;;; init-web-mode.el ends here
