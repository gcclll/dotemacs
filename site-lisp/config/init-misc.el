;;; init-misc.el --- 包含了所有用到的一些不怎么需要配置的包

(require 'smart-hungry-delete)
(require 'vimish-fold) 
(require 'format-all)
(require 'buffer-move)
(require 'pangu-spacing)
(require 'restart-emacs)
(require 'expand-region)
(require 'lazycat-toolkit)
(require 'dash-at-point)
(require 'wgrep)
(require 'rg)
(require 'tiny)
(require 'emojify)
(require 'color-rg)
(require 'insert-translated-name)
(require 'visual-regexp)
(require 'duplicate-line)
(require 'shift-number)
(require 'watch-other-window)
(require 'goto-line-preview)
(require 'instant-rename-tag)
(require 'corfu-english-helper)
(require 'symbol-overlay)
(require 'buffer-extension)
(require 'delete-block)
(require 'toggle-one-window)
(require 'vdiff)
(require 'ace-jump-mode)
(require 'aweshell)
(require 'move-text)
(require 'rect-mark)
(require 'rect-extension)
(require 'org-mac-link)
(require 'cycle-quotes)
;; (require 'fastdef)
(require 'find-file-in-project)
(require 'workgroups2)
(require 'dumb-jump)

;; fastdef, 搜索google并插入结果
;; (autoload 'fastdef-insert "fastdef" nil t)
;; (autoload 'fastdef-insert-from-history "fastdef" nil t)

;; emoji
(add-hook 'after-init-hook #'global-emojify-mode)

;; 快捷输入一些规则化的文本, 这个是设置 C-; 按键的
;; 不要这样做，在init-key.el 中配置新的按键
;; (tiny-setup-default) 

;; wgrep 可以在 grep/rg/color-rg 搜索结果中直接批量修改
;; 其实直接使用 color-rg 就够用了
(setq wgrep-auto-save-buffer t) ;; 当 wgrep-finish-edit 完成后自动保存

;; 英文和中文之间自动插入空格
(global-pangu-spacing-mode 1)

;; fold
(vimish-fold-global-mode 1)

;; format-all
(add-hook 'prog-mode-hook 'format-all-mode)

;; smart-hungry-delete
(smart-hungry-delete-add-default-hooks)

(global-set-key [remap backward-delete-char-untabify] 'smart-hungry-delete-backward-char)
(global-set-key [remap delete-backward-char] 'smart-hungry-delete-backward-char)
(global-set-key [remap delete-char] 'smart-hungry-delete-forward-char)     

;; buffer move

(provide 'init-misc)
;;; init-misc.el ends here
