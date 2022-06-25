;;; init-misc.el --- 一些小的不怎么需要配置的包

;;; Commentary:

;;; Require:
(require 'lazycat-toolkit)
(require 'dash-at-point)
(require 'move-text)
(require 'duplicate-line)
(require 'goto-last-change)
(require 'open-newline)
(require 'smart-align)
(require 'restart-emacs)
(require 'recursive-search-references)
(require 'tiny)
(require 'expand-region)
(require 'string-inflection)
(require 'ace-jump-mode)
(require 'pangu-spacing)
(require 'delete-block)
(require 'aweshell)
(require 'rect-mark)
(require 'rect-extension)
(require 'org-mac-link)
(require 'cycle-quotes)
(require 'find-file-in-project)
(require 'whitespace-cleanup-mode)
(require 'rainbow-delimiters)

;;; Code:
;; (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
;; (add-hook 'after-save-hook 'whitespace-cleanup)

;; 英文和中文之间自动插入空格
(global-pangu-spacing-mode 1)

;;括号显示各种着色
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(message "> init-misc.el")
(provide 'init-misc)
;;; init-misc.el ends here.
