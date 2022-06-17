;; init-key.el

;; Mac平台下交换 Option 和 Command 键。
(when 
    (featurep 'cocoa) 
  (setq mac-option-modifier 'super) 
  (setq mac-command-modifier 'meta))

;;; unset keys
(lazy-load-unset-keys '("C-x C-f" "C-z" "C-q" "s-W" "M-h" "C-x C-c" "C-\\" "s-c" "s-x" "s-v" "s-p"
                        "C-6" "C-c i" "C-c w" "C-c v"))

;; {{ 各种模式切换 ##
(lazy-load-set-keys '(
                      ("C-c C-t e" . acm-toggle-english-helper)
                      ("C-c C-t p" . lsp-bridge-mode)
                      ("C-c C-t g" . git-gutter-toggle)
                      ))
;; }}

;; {{ 功能键 ##
(lazy-load-set-keys '(
                      ("<f5>" . emacs-session-save) ;退出 Emacs
                      ))
;; }}

;; {{ eaf ##
;; git menu
(lazy-load-global-keys '(("C-c C-a g" . eaf-open-git)
                         ("C-c C-a t" . eaf-open-terminal )
                         ("C-c C-a r" . eaf-open-rss-reader)
                         ("C-c C-a f" . eaf-open-file-manager)
                         ("C-c C-a b" . eaf-open-browser)
                         ("C-c C-a m" . eaf-open-system-monitor)
                         ) "init-eaf")

(unless 
    (featurep 'cocoa) 
  (lazy-load-global-keys '(("M-j" . eaf-open-in-file-manager) 
                           ("s-'" . eaf-open) 
                           ("s-\"" . eaf-open-browser) 
                           ("s-/" . eaf-open-terminal) 
                           ("s-b" . eaf-open-rss-reader)) "init-eaf"))
;; }}

;; {{ window 相关 ##
;; 临时最大当前窗口
(lazy-load-global-keys '(("M-s-o" . toggle-one-window)) "toggle-one-window")

;; window 全局按键 C-c w
(lazy-load-set-keys '(("C-c w x" . delete-window) 
                      ("C-c w ," . delete-other-windows) 
                      ("C-c w v" . split-window-horizontally) 
                      ("C-c w -" . split-window-vertically) 
                      ("C-c w ;" . kill-other-window-buffer)
                      ("C-c w d" . delete-current-buffer-and-window)
                      ;; ("C-M-;" . kill-other-window-buffer)
                      ))

(lazy-load-global-keys '(
                         ("C-<left>" . windmove-left) 
                         ("C-<right>" . windmove-right) 
                         ("C-<up>" . windmove-up) 
                         ("C-<down>" . windmove-down)
                         ;; 切换焦点
                         ("C-c w h" . windmove-left) 
                         ("C-c w l" . windmove-right) 
                         ("C-c w j" . windmove-down) 
                         ("C-c w k" . windmove-up)
                         ;; 交换两个窗口的buffer
                         ("C-S-<left>" . buf-move-left) 
                         ("C-S-<right>" . buf-move-right) 
                         ("C-S-<up>" . buf-move-up) 
                         ("C-S-<down>" . buf-move-down) 
                         ("C-c w H" . buf-move-left) 
                         ("C-c w L" . buf-move-right) 
                         ("C-c w K" . buf-move-up) 
                         ("C-c w J" . buf-move-down)
                         ;; resize
                         ("C-c w =" . windresize-balance-windows)
                         ;; buffer
                         ) "init-window")

;; 滚动其它窗口
(lazy-load-global-keys '(("M-J" . watch-other-window-up) ;向下滚动其他窗口
                         ("M-K" . watch-other-window-down) ;向上滚动其他窗口
                         ("M-<" . watch-other-window-up-line) ;向下滚动其他窗口一行
                         ("M->" . watch-other-window-down-line) ;向上滚动其他窗口一行
                         ) "watch-other-window")
;; }}

;; {{ buffer 操作 ##
;; --- avy jump
(lazy-load-global-keys '(("C-c ," . avy-goto-char-2)) "avy-jump")

;; --- ace jump
(lazy-load-global-keys '(("s-<" . ace-jump-word-mode) 
                         ("s->" . ace-jump-char-mode) 
                         ("s-?" . ace-jump-line-mode) 
                         ) "ace-jump-mode")

;; --- Cycle buffer
(lazy-load-global-keys '(("M-C" . one-key-menu-cycle-buffer) ;特定模式切换
                         ) "init-cycle-buffer")

;; --- 关闭buffer/window
(lazy-load-global-keys '(("C-'" . delete-current-buffer-and-window) 
                         ("C-\"" . delete-current-buffer-window) 
                         ("C-x O" . toggle-window-split)) "window-extension")

;; --- last change
(lazy-load-global-keys '(("C-," . goto-last-change)) "goto-last-change")

(lazy-load-global-keys '(("M-g" . goto-line-preview)) "goto-line-preview")

(lazy-load-set-keys '(("s-," . bury-buffer)       ;隐藏当前buffer
                      ("s-." . unbury-buffer)     ;显示隐藏的buffer
                      ("C-x n" . next-buffer)     ;下一个buffer
                      ("C-x p" . previous-buffer) ;前一个buffer
                      ("C-x k" . kill-this-buffer) 
                      ("C-x K" . kill-buffer) 
                      ("C-x b" . ido-switch-buffer) ;切换buffer
                      ("C-x i" . ido-insert-buffer) ;插入buffer内容到当前buffer
                      ("C-x I" . ido-insert-file) ;插入文件内容到当前buffer
                      ("C-<" . beginning-of-buffer) ;当前buffer最开始位置
                      ("C->" . end-of-buffer) ;当前buffer最后位置
                      ))

;; buffer move
(lazy-load-set-keys '(("C-z k" . beginning-of-buffer) ;缓存开始
                      ("C-z j" . end-of-buffer)       ;缓存结尾
                      ("C-M-f" . forward-paragraph)   ;下一个段落
                      ("C-M-b" . backward-paragraph)  ;上一个段落
                      ("C-M-y" . backward-up-list)    ;向左跳出 LIST
                      ("C-M-o" . up-list)             ;向右跳出 LIST
                      ("C-M-u" . backward-down-list)  ;向左跳进 LIST
                      ("C-M-i" . down-list)           ;向右跳进 LIST
                      ("C-M-a" . beginning-of-defun)  ;函数开头
                      ("C-M-e" . end-of-defun)        ;函数末尾
                      ("C-M-l" . recenter)
                      ))
;;

;; {{ file 文件操作 ##
(lazy-load-set-keys '(
                      ("C-c C-f" . ido-find-file) ;打开文件
                      ("C-c f f" . find-file)     ;打开文件
                      ("C-c f r" . rename-this-file-and-buffer) ;重命名文件和buffer
                      ("C-c f d" . delete-this-file) ;删除当前文件和buffer
                      ("C-c f l" . load-file)                      
                      ("C-c f p" . cp-fullpath-of-current-buffer)
                      ("C-c f n" . cp-filename-of-current-buffer)
                      ))

;; (lazy-load-global-keys '() "init-utils" "C-c")

;; osx-lib
(lazy-load-global-keys '(("f o" . osx-lib-reveal-in-finder)) "init-osx" "C-c")
;; }}

;; {{ 搜索 ##
;; 搜索替换
(lazy-load-global-keys '(("C-M-%" . vr/query-replace)) "init-visual-regexp")

;; --- ivy
(lazy-load-global-keys '(("M-x" . counsel-M-x) 
                         ("C-c '" . ivy-resume)) "init-ivy")

;; --- snails
(lazy-load-global-keys '(("s-y" . snails) 
                         ("s-u" . snails-search-point)) "init-snails")

(lazy-load-set-keys '(
                      ("C-s" . swiper)                      
                      ("C-M-s" . isearch-forward)
                      ))

(lazy-load-global-keys '(("s-R" . re-builder)
                         ;; color-rg
                         ("s-x g" . color-rg-search-symbol) 
                         ("s-x h" . color-rg-search-input) 
                         ("s-x j" . color-rg-search-symbol-in-project) 
                         ("s-x k" . color-rg-search-input-in-project) 
                         ("s-x ," . color-rg-search-symbol-in-current-file) 
                         ("s-x ." . color-rg-search-input-in-current-file)
                         ;; visual-regexp
                         ("M-%" . vr/query-replace)) "init-search")
;; }}

;; {{ 文本操作(标记+注释等) ##
;; --- iedit
(lazy-load-global-keys '(
                         ("s-o" . iedit-mode)
                         ("C-c C-t i" . iedit-mode)) "init-iedit")

;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys '(("M-s-h" . one-key-menu-thing-edit) ;thing-edit 菜单
                         ) "init-thing-edit")

;; --- shift-number, +-数字
(lazy-load-global-keys '(("M--" . shift-number-down) 
                         ("M-=" . shift-number-up)) "shift-number")

;; --- 矩形操作
(lazy-load-global-keys '(("s-M" . rm-set-mark) ;矩形标记
                         ("s-X" . rm-exchange-point-and-mark) ;矩形对角交换
                         ("s-D" . rm-kill-region)    ;矩形删除
                         ("s-S" . rm-kill-ring-save) ;矩形保存
                         ("s-Y" . yank-rectangle)    ;粘帖矩形
                         ("s-O" . open-rectangle) ;用空白填充矩形, 并向右移动文本
                         ("s-C" . clear-rectangle) ;清空矩形
                         ("s-T" . string-rectangle) ;用字符串替代矩形的每一行
                         ("s-I" . string-insert-rectangle) ;插入字符串在矩形的每一行
                         ("s-F" . delete-whitespace-rectangle) ;删除矩形中空格
                         ("s-\"" . copy-rectangle-to-register) ;拷贝矩形到寄存器
                         ("s-:" . mark-rectangle-to-end) ;标记矩形到行末
                         ) "rect-extension")

(lazy-load-global-keys '(("C-o" . open-newline-above) 
                         ("C-l" . open-newline-below) ;; recenter ???
                         ) "open-newline")

;; copy and comment
(lazy-load-global-keys '(("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
                         ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
                         ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
                         ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
                         ("C-:" . comment-or-uncomment-region+) ;注释当前行
                         ) "duplicate-line")

(lazy-load-set-keys '(
                      ("M-;" . comment-dwim)     ;在行尾添加注释
                      ("C-x C-x" . exchange-point-and-mark) ;交换当前点和标记点
                      ("C-M-S-h" . mark-paragraph)          ;选中段落                      
                      ("C-M-S-b" . mark-whole-buffer)

                      ("M-o" . backward-delete-char-untabify) ;向前删除一个字符
                      ("M-SPC" . just-one-space) ;合并空格
                      ("C-/" . undo)             ;回退

                      ("C-=" . er/expand-region)
                      ))

;; delete block
(lazy-load-global-keys '(("M-N" . delete-block-backward) 
                         ("M-M" . delete-block-forward)) "delete-block")

;; symbol-overlay
(lazy-load-global-keys '(("M-s" . symbol-overlay-put)) "init-symbol-overlay")

;; move-text
(lazy-load-global-keys '(("s-P" . move-text-up) 
                         ("s-N" . move-text-down)) "move-text")
;; }}

;; {{ my hydras ##
(lazy-load-set-keys '(
                      ("C-c C-w" . my-hydra-window/body)
                      ("C-c C-g" . my-hydra-git/body)
                      ("C-c C-s" . my-hydra-search/body)
                      ("C-c C-," . my-hydra-file/body)
                      ))
;; }}

;; {{ 其它 ##
;; --- 多标签式的shell
(lazy-load-global-keys '(("s-n" . aweshell-new) 
                         ("s-h" . aweshell-toggle) 
                         ("s-x s-x" . aweshell-dedicated-toggle)) "aweshell")

(lazy-load-set-keys '(
                      ("s-[" . eval-expression) ;执行表达式
                      ("C-c i e" . emoji-insert) ;插入Emacs内置表情包
                      ("C-c i y" . yas-insert-snippet)

                      ("s--" . text-scale-decrease) ;减小字体
                      ("s-=" . text-scale-increase) ;增大字体
                      ))
;; 翻译, C-z
(lazy-load-global-keys '((";" . popweb-dict-bing-input) ;翻译输入的内容
                         ("y" . popweb-dict-bing-pointer) ;翻译当前光标处的内容
                         ("," . insert-translated-name-insert-with-underline) ;翻译输入的中文
                         ("." . insert-translated-name-insert-with-camel) 
                         ("i" . insert-translated-name-insert)) "init-popweb" "C-z")

;; misc
(lazy-load-global-keys '(("C-x y" . dash-at-point) 
                         ("C-x r" . restart-emacs) 
                         ;; ("C-x j" . tiny-expand) 
                         ("C-c j" . tiny-expand)
                         ("M-i" . string-inflection-toggle) 
                         ("<f5>" . restart-emacs) 
                         ("M-'" . cycle-quotes)) "init-misc")
;; }}

;; {{ 工具函数 ##

(lazy-load-global-keys '(("C-c C-t l" . display-line-numbers-mode) ;行号模式切换
                         ("M-s-n" . comment-part-move-down) ;向下移动注释
                         ("M-s-p" . comment-part-move-up) ;向上移动注释
                         ("C-s-n" . comment-dwim-next-line) ;移动到上一行并注释
                         ("C-s-p" . comment-dwim-prev-line) ;移动到下一行并注释
                         ("M-2" . indent-buffer) ;自动格式化当前Buffer
                         ("M-z" . upcase-char) ;Upcase char handly with capitalize-word
                         ("C-x u" . mark-line) ;选中整行
                         ("s-k" . kill-and-join-forward) ;在缩进的行之间删除
                         ("M-G" . goto-column)           ;到指定列
                         ;;("C->" . remember-init)              ;记忆初始函数
                         ;;("C-<" . remember-jump)              ;记忆跳转函数
                         ("M-s-." . point-stack-pop)  ;buffer索引跳转
                         ("M-s-," . point-stack-push) ;buffer索引标记
                         ("s-g" . goto-percent) ;跳转到当前Buffer的文本百分比, 单位为字符
                         ("M-I" . backward-indent)    ;向后移动4个字符
                         ("s-J" . scroll-up-one-line) ;向上滚动一行
                         ("s-K" . scroll-down-one-line) ;向下滚动一行
                         ("<f2>" . refresh-file)        ;自动刷新文件
                         ("s-f" . find-file-root) ;用root打开文件
                         ("s-r" . find-file-smb)  ;访问sambao
                         ) "basic-toolkit")
;; }}

;; {{ Sort-Tab ##
;; --- 多标签浏览
(lazy-load-global-keys '(("M-7" . sort-tab-select-prev-tab) ;选择前一个标签
                         ("M-8" . sort-tab-select-next-tab) ;选择后一个标签
                         ("M-s-7" . sort-tab-select-first-tab) ;选择第一个标签
                         ("M-s-8" . sort-tab-select-last-tab) ;选择最后一个标签
                         ("C-M-7" . sort-tab-turn-on) 
                         ("C-M-8" . sort-tab-turn-off) 
                         ("C-;" . sort-tab-close-current-tab) ;关闭当前标签
                         ("s-q" . sort-tab-close-mode-tabs) ;关闭特定模式的标签
                         ("s-Q" . sort-tab-close-all-tabs) ;关闭所有标签
                         ) "sort-tab")
;; }}

;; {{ 编程相关 ##
;; --- elisp-format
(lazy-load-set-keys '(("M-F" . elisp-format-buffer)) emacs-lisp-mode-map)

(lazy-load-global-keys '(("C-c e n" . flycheck-next-error)
                         ("C-c e p" . flycheck-previous-error)) "flycheck")

;; --- 格式化
(lazy-load-global-keys '(("M-F" . format-all-buffer)) "init-formatter")

;; --- 搜索引用
(lazy-load-global-keys '(("C--" . recursive-search-references)) "recursive-search-references")

;; --- lsp-bridge
(lazy-load-global-keys '(("C-6" . lsp-bridge-lookup-documentation) 
                         ("C-7" . lsp-bridge-jump-back) 
                         ("C-8" . lsp-bridge-jump) 
                         ("C-9" . lsp-bridge-find-references) 
                         ("C-0" . lsp-bridge-rename)) "init-lsp-bridge")

;; --- smart align
(lazy-load-global-keys '(("M-U" . smart-align)) "smart-align")

;; --- 结构化编程
;; 卸载按键
(lazy-load-unset-keys '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)") grammatical-edit-mode-map)
(defvar grammatical-edit-key-alist nil)
(setq grammatical-edit-key-alist '(
                                   ;; 移动
                                   ("M-n" . grammatical-edit-jump-left) 
                                   ("M-p" . grammatical-edit-jump-right)
                                   ;; 符号插入
                                   ("%" . grammatical-edit-match-paren) ;括号跳转
                                   ("(" . grammatical-edit-open-round) ;智能 (
                                   ("[" . grammatical-edit-open-bracket) ;智能 [
                                   ("{" . grammatical-edit-open-curly) ;智能 {
                                   (")" . grammatical-edit-close-round) ;智能 )
                                   ("]" . grammatical-edit-close-bracket) ;智能 ]
                                   ("}" . grammatical-edit-close-curly) ;智能 }
                                   ("\"" . grammatical-edit-double-quote) ;智能 "
                                   ("'" . grammatical-edit-single-quote) ;智能 '
                                   ("=" . grammatical-edit-equal) ;智能 =
                                   ("SPC" . grammatical-edit-space) ;智能 space
                                   ("RET" . grammatical-edit-newline) ;智能 newline
                                   ;; 删除
                                   ("M-o" . grammatical-edit-backward-delete) ;向后删除
                                   ("C-d" . grammatical-edit-forward-delete) ;向前删除
                                   ("C-k" . grammatical-edit-kill) ;向前kill
                                   ;; 包围
                                   ("M-\"" . grammatical-edit-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
                                   ("M-'" . grammatical-edit-wrap-single-quote) ;用 ' ' 包围对象, 或跳出字符串
                                   ("M-[" . grammatical-edit-wrap-bracket) ;用 [ ] 包围对象
                                   ("M-{" . grammatical-edit-wrap-curly) ;用 { } 包围对象
                                   ("M-(" . grammatical-edit-wrap-round) ;用 ( ) 包围对象
                                   ("M-)" . grammatical-edit-unwrap) ;去掉包围对象
                                   ;; 跳出并换行缩进
                                   ("M-:" . grammatical-edit-jump-out-pair-and-newline) ;跳出括号并换行
                                   ;; 向父节点跳动
                                   ("C-j" . grammatical-edit-jump-up)))
(lazy-load-set-keys grammatical-edit-key-alist grammatical-edit-mode-map)

;; }}

;; {{ 版本管理 ##
;; --- 显示当前代码的commit信息
(lazy-load-global-keys '(
                         ("C-c C-t b" . blamer-mode)
                         ("s-i" . blamer-show-posframe-commit-info)) "init-git")
;; }}

;; {{ 代码折叠 ##
;; fold, vimish
(lazy-load-global-keys '(("C-M-+" . vimish-fold) 
                         ("C-M--" . vimish-fold-unfold-all)
                         ("C-M-=" . vimish-fold-refold-all)
                         ("C-M-<backspace>" . vimish-fold-delete)
                         ("C-M-S-<backspace>" . vimish-fold-delete-all)
                         ) "init-fold")
;; }}

;; {{ org-mode ##
(lazy-load-set-keys '(
                      ("C-c n f" . org-roam-node-find)
                      ("C-c n g" . org-roam-graph)
                      ("C-c n i" . org-roam-node-insert)
                      ("C-c n c" . org-roam-capture)
                      ("C-c n j" . org-roam-dailies-capture-today)
                      ("C-c n ," . org-id-get-create)
                      ("C-c n s" . org-roam-db-sync)
                      ("C-c n u" . org-roam-ui-open)
                      ))
;; }}

(message "> init-key.el")
(provide 'init-key)
