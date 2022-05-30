;;; init-lsp-bridge.el --- Configuration for lsp-bridge
;;; Require
(require 'corfu)
(require 'corfu-info)
(require 'corfu-history)
(require 'cape)
(require 'lsp-bridge)
(require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
(require 'lsp-bridge-icon) ;; show icon for completion items, optional
(require 'lsp-bridge-jdtls)
(require 'tabnine-capf)

;; 打开日志，开发者才需要
;; (setq lsp-bridge-enable-log t)
;; 默认用这三个补全后端
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; 让Corfu适应高分屏
(when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))

;; 开启 history mode
(corfu-history-mode t)
(global-lsp-bridge-mode)
(global-corfu-mode)

;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
   (lsp-bridge-mode
    (lsp-bridge-return-from-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

(defcustom lsp-bridge-lang-server-extension-list
  '(
    (("vue") . "volar")
    (("html") . "vscode")
    (("css") . "vscode")
    )
  "The lang server rule for file extension."
  :type 'cons)

;; org 里面不用补全
(with-eval-after-load 'org
  (lsp-bridge--disable)
  (corfu-mode -1)
  (corfu-history-mode -1))

;; 打开日志，开发者才需要
;; (setq lsp-bridge-enable-log t)

;; (require 'tabnine-capf)
;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
;; (defun lsp-bridge-mix-multi-backends ()
;;   (setq-local completion-category-defaults nil)
;;   (setq-local completion-at-point-functions
;;               (list
;;                (cape-capf-buster
;;                 (cape-super-capf
;;                  #'lsp-bridge-capf
                 
;;                  ;; 我嫌弃TabNine太占用我的CPU了， 需要的同学注释下面这一行就好了
;;                  ;; #'tabnine-completion-at-point
                 
;;                  #'cape-file
;;                  #'cape-dabbrev
;;                  )
;;                 'equal)
;;                )))


;; (dolist (hook lsp-bridge-default-mode-hooks)
;;   (add-hook hook (lambda ()
;;                    (lsp-bridge-mix-multi-backends) ; 通过Cape融合多个补全后端
;;                    )))

(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
