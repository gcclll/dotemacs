 init-key.el --- 所有按键绑定
;; Mac平台下交换 Option 和 Command 键。
(when (featurep 'cocoa)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
 '("C-x C-f" "C-z" "C-q" "s-W" "s-z" "M-h" "C-x C-c" "C-\\" "s-c" "s-x" "s-v" "s-p" "C-6" "C-c i"))

;;; ### iedit ###
;;; --- iedit
(lazy-load-global-keys
 '(
   ("s-o" . iedit-mode)
   )
 "init-iedit")

;;; ### Ace jump ###
(lazy-load-global-keys
 '(
   ("s-<" . ace-jump-word-mode)
   ("s->" . ace-jump-char-mode)
   ("s-?" . ace-jump-line-mode)
   )
 "ace-jump-mode")

;;; ### Python ###
;;; --- Python mode
(eval-after-load 'python-mode
  '(lambda ()
     (lazy-load-local-keys
      '(i
        ("C-S-j" . jump-to-import)
        )
      python-mode-map
      "python-mode-utils")
     ))

;;; ### Ielm ###
;;; --- Emacs Lisp 解释模式
(autoload 'ielm-map "ielm")
(lazy-load-global-keys
 '(
   ("M-s-i" . ielm-toggle)              ;切换ielm
   ("C-c d" . insert-standard-date)
   )
 "lazycat-toolkit")
(eval-after-load 'ielm-mode
  '(lambda ()
     (progn
       (lazy-load-unset-keys
        '("M-p" "M-n")
        ielm-map)                       ;卸载按键
       (lazy-load-set-keys
        '(
          ("C-s-p" . comint-previous-input) ;上一个输入
          ("C-s-n" . comint-next-input)     ;下一个输入
          )
        ielm-map
        )
       )))

;;; ### Man ###
;;; --- Man
(lazy-load-global-keys
 '(
   ("<f1>" . woman))
 "init-woman")

;;; ### Company en words ###
;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . toggle-corfu-english-helper) ;英文助手
   )
 "corfu-english-helper")

;;; ### Ido ###
;;; --- 交互式管理文件和缓存
(add-hook 'ido-setup-hook
          #'(lambda ()
              (interactive)
              (ido-my-keys ido-completion-map)))
(defun ido-my-keys (keymap)
  "Add my keybindings for ido."
  (lazy-load-set-keys
   '(
     ("M-s-p" . ido-prev-match)              ;上一个匹配
     ("M-s-n" . ido-next-match)              ;下一个匹配
     ("M-s-h" . ido-next-work-directory)     ;下一个工作目录
     ("M-s-l" . ido-prev-work-directory)     ;上一个工作目录
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("M-O" . ido-delete-backward-updir)     ;删除字符或进入上一级目录
     )
   keymap
   ))

;;; ### IRC ###
;;; --- 聊天
(lazy-load-global-keys
 '(
   ("C-c o e" . switch-to-erc)            ;切换到IRC或自动登录IRC
   ("C-c o E" . erc-nick-notify-jump-last-channel) ;自动跳转到最后收到消息的频道
   )
 "init-erc")

;;; Elisp
(lazy-load-set-keys
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map
 )

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

;;; ### String Inflection ###
;; --- 单词语法风格快速转换
(lazy-load-global-keys
 '(
   ;; ("C-c C-u" . one-key-string-inflection)
   ("M-i" . string-inflection-toggle)
   )
 "init-string-inflection")

;;; ### Projectile Rails ###
;; Rails 文件快速导航
(lazy-load-global-keys
 '(
   ("s-c p" . one-key-projectile-rails) ;projectile rails
   )
 "init-projectile-rails")

;;; ### Keyboard Macro ###
;;; --- 键盘宏
(lazy-load-global-keys
 '(
   ("M-s-s" . kmacro-start-macro-or-insert-counter) ;开始键盘宏或插入
   ("M-s-d" . kmacro-end-or-call-macro)             ;结束键盘宏或调用
   ("M-s-c" . kmacro-delete-ring-head)              ;删除当前的键盘宏
   ("M-s-w" . kmacro-cycle-ring-next)               ;下一个键盘宏
   ("M-s-e" . kmacro-cycle-ring-previous)           ;上一个键盘宏
   ("M-s-a" . kmacro-edit-macro)                    ;编辑键盘宏
   ("M-s-v" . name-last-kbd-macro)                  ;命令当前键盘宏
   ("M-s-f" . insert-kbd-macro)                     ;插入键盘宏
   ("M-s-q" . apply-macro-to-region-lines) ;应用键盘宏到选择的区域
   )
 "macros+")

;;; ### auto-install ###
(lazy-load-global-keys
 '(
   ("C-s-x" . auto-install-from-emacswiki))
 "init-auto-install")

;;; ### expand-region ###
(lazy-load-global-keys
 '(
   ("C-=" . er/expand-region))
 "expand-region")

;; ### vdiff ###
(lazy-load-global-keys
 '(
   ("M-s-u" . vdiff-buffers))
 "vdiff")

;; git menu
(lazy-load-global-keys
 '(
   ("s-x f" . one-key-menu-git))
 "init-eaf")

;; 切换输入法
(lazy-load-global-keys
 '(
   ("s-m" . toggle-input-method)
   )
 "init-rime")

;; (lazy-load-global-keys
;;  '(
;;    ("M-x" . smex+)
;;    ("C-c C-c M-x" . execute-extended-command)
;;    )
;;  "init-smex")

;; 搜索替换
(lazy-load-global-keys
 '(
   ("C-M-%" . vr/query-replace))
 "init-visual-regexp")



;; lsp-bridge
(lazy-load-global-keys
 '(
   ("C-6" . lsp-bridge-lookup-documentation)
   ("C-7" . lsp-bridge-jump-back)
   ("C-8" . lsp-bridge-jump)
   ("C-9" . lsp-bridge-find-references)
   ("C-0" . lsp-bridge-rename)
   )
 "init-lsp-bridge")

;; 搜索引用
(lazy-load-global-keys
 '(
   ("C--" . recursive-search-references)
   )
 "recursive-search-references")

;; ivy
(lazy-load-global-keys
 '(
   ("M-x" . counsel-M-x)
   ("C-c '" . ivy-resume)
   )
 "init-ivy")

;; window move
(lazy-load-global-keys
 '(
   ("s-x w" . one-key-menu-window-navigation)
   ("C-<left>" . windmove-left)
   ("C-<right>" . windmove-right)
   ("C-<down>" . windmove-down)
   ("C-<up>" . windmove-up)
   )
 "init-window")

;; vimish
(lazy-load-global-keys
 '(
   ;; fold
   ("M-+" . vimish-fold)
   ("M-`" . vimish-fold-toggle)
   ("C-M-`" . vimish-fold-toggle-all)
   ("C-M-n" . vimish-fold-next-fold)
   ("C-M-p" . vimish-fold-previous-fold)
   ("M-<backspace>" . vimish-fold-delete)
   ("M-S-<backspace>" . vimish-fold-delete-all)
   ;; format-all
   ("M-F" . format-all-buffer)
   ;; buf move
   ("C-S-<left>" . buf-move-left)
   ("C-S-<right>" . buf-move-right)
   ("C-S-<up>" . buf-move-up)
   ("C-S-<down>" . buf-move-down)
   ;; tiny
   ("C-c j" . tiny-expand)
   
   ;; emojify
   ("C-c i e" . emojify-insert-emoji)
   ) 
 "init-misc")

;; magit
(lazy-load-global-keys
 '(
   ("s-x F" . one-key-menu-magit)
   )
 "init-git")

(provide 'init-key)
