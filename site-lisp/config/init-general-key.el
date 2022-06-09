;;; init-general-key.el --- 使用 general 这个包管理按键
;;;
(require 'general)

;; mac 下交换 Option 和 Command
(when (featurep 'cocoa)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(general-evil-setup)
(general-auto-unbind-keys)

;;; 如果要用 general 覆盖 evil 按键，必须指定 `:states'
;; 正常模式下按键
(general-define-key
 :states 'motion
 ;; {{ evil motion mode ##
 "0" 'evil-end-of-line
 "f" 'evil-ace-jump-char-mode
 "F" 'evil-ace-jump-line-mode
 "s" 'evil-ace-jump-word-mode
 ;; }}
 )

;; ;; visual 模式
(general-define-key
 :states 'visual
 ;; {{ evil visual mode ##
 "S" 'evil-surround-region
 ;; }}
 )

;; 输入模式
(general-define-key
 :states 'insert
 ;; {{ evil insert mode ##
 "C-n" 'evil-next-line
 "C-p" 'evil-previous-line
 ;; }}
 )

;; 不分evil 模式，适用于各种组合键
;; 按照功能分类
(general-define-key
 :states '(motion normal insert emacs)
 
 ;; {{ 定义按键在 which-key 中的说明 ##
 "C-c f" '(:ignore t :which-key "File")
 "C-c b" '(:ignore t :which-key "Buffer")
 "C-c i" '(:ignore t :which-key "Insert")
 "C-c o" '(:ignore t :which-key "Open everything")
 "C-c w" '(:ignore t :which-key "Window")
 "C-c s" '(:ignore t :which-key "Search")
 ;; }}

 ;; {{ 功能键 ##
 "<f1>" 'woman                          ;系统帮助
 "<f2>" 'refresh-file                   ;刷新并格式化当前buffer
 "<f5>" 'restart-emacs                  ;这个更常用，mac键盘有问题用的外接，按 f<n> 要结合 fn
 "C-<f5>" 'emacs-session-save
 "<f9>" 'my-next-merge-conflict         ;下一个冲突点
 "S-<f9>" 'my-prev-merge-conflict       ;上一个冲突点
 "C-<f9>" 'my-search-next-merge-conflict ;搜索下一个冲突点
 "C-S-<f9>" 'my-search-prev-merge-conflict ;搜索上一个冲突点
 "<f10>" 'my-search-next-diff-hunk         ;搜索下一个diff hunk
 "S-<f10>" 'my-search-prev-diff-hunk       ;搜索上一个 diff hunk
 ;; }}

 ;; {{ 标记，记忆，跳转 ##
 "C-=" 'er/expand-region                ;快捷选中区域
 "C->" 'remember-init                   ;开始记忆
 "C-<" 'remember-jump                   ;跳到记忆点
 "C-," 'goto-last-change
 "C-e" 'end-of-line
 "C-a" 'beginning-of-line
 "C-c u" 'mark-line
 "M-g" 'goto-line-preview
 "M-G" 'goto-column
 "M-s" 'symbol-overlay-put              ; 高亮显示相同内容(n/p上下跳转)
 "M-s-." 'point-stack-push              ; buffer 索引标记
 "M-s-," 'point-stack-pop               ; buffer 索引跳转
 ;; ace jump, evil 模式下可以直接单字符跳转(f/s/F)      
 "s-<" 'ace-jump-word-mode
 "s->" 'ace-jump-char-mode
  "s-?" 'ace-jump-line-mode
 "s-g" 'goto-percent
 ;; }}

 ;; {{ 复制，粘贴，文本操作 ##
 "C-=" 'er/expand-region
 "C-y" 'yank
 "C-c j" 'tiny-expand
 "M-SPC" 'just-one-space                ;合并多个空格
 "M-=" 'shift-number-up
 "M--" 'shift-number-down
 "M-i" 'string-inflection-all-cycle
 "M-I" 'parrot-rotate-next-word-at-point
 "M-M" 'delete-block-forward
 "M-N" 'delete-block-backward
 "s--" 'text-scale-decrease
 "s-=" 'text-scale-increase
 ;; "C--" 'cnfonts-decrease-fontsize       ;增大字体
 ;; "C-=" 'cnfonts-increase-fontsize       ;减小字体
 "s-k" 'kill-and-join-forward
 "s-N" 'move-text-down
 "s-P" 'move-text-up
 ;; iedit, rectangle
 "s-M" 'rm-set-mark                     ;标记
 "s-X" 'rm-exchange-point-and-mark      ;对角交换位置
 "s-D" 'rm-kill-region                  ;删除标记的区域
 "s-S" 'rm-kill-ring-save
 "s-O" 'open-rectangle                  ;空白填充，文本右移
 "s-C" 'clear-rectangle                 ;清空标记区域
 "s-T" 'string-rectangle                ;用字符串替代矩形的每一行
 "s-I" 'string-insert-rectangle         ;在矩形的每一行插入字符串
 "s-F" 'delete-whitespace-rectangle     ;删除空格
 "s-\"" 'copy-rectangle-to-register     ;拷贝到寄存器
 "s-:" 'mark-rectangle-to-end           ;标记到行末

 "C-c i d" 'insert-standard-date
 "C-c i D" 'insert-changelog-date
 ;; }}

 ;; {{ 标签(sort-tab), 书签(bm)操作 ##
 "C-;" 'sort-tab-close-current-tab      ;关闭当前标签
 "C-M-7" 'sort-tab-turn-on              ;有时候 sort-tab 可能会有问题，先off->on就好了
 "C-M-8" 'sort-tab-turn-off
 "M-7" 'sort-tab-select-prev-tab
 "M-8" 'sort-tab-select-next-tab
 "M-s-7" 'sort-tab-select-first-tab
 "M-s-8" 'sort-tab-select-last-tab
 "s-q" 'sort-tab-close-mode-tabs        ; 关闭特定模式标签
 "s-Q" 'sort-tab-close-all-tabs         ; 关闭所有标签
 ;; }}

 ;; {{ window & frame & buffer & file 操作 ##
 "M-F" 'format-all-buffer
 "M-<" 'watch-other-window-up-line
 "M->" 'watch-other-window-down-line
 "M-J" 'watch-other-window-up
 "M-K" 'watch-other-window-down
 "M-s-o" 'toggle-one-window             ; 窗口最大化/最小化

 "s-;" 'ivy-resume
 "s-," 'bury-buffer
 "s-." 'unbury-buffer
 "s-'" 'next-window-any-frame
 "s-J" 'scroll-up-one-line              ; 向上滚动一行光标不动
 "s-K" 'scroll-down-one-line            ; 向下滚动一行光标不动
 "s-f" 'find-file-root
 "s-r" 'find-file-smb

 "C-v" 'scroll-up-command
 "C-'" 'delete-current-buffer-window      ;删除当前 buffer 所在的 window 但是不关闭buffer
 "C-\"" 'delete-current-buffer-and-window ;删除当前buffer 和 window
 "C-2" 'rename-this-file-and-buffer       ;重命名当前文件及buffer名称
 "C-M-;" 'kill-other-window-buffer
 "C-c <backspace>" 'delete-this-file
 
 ;; buffer
 "C-c b d" 'kill-this-buffer
 "C-c b k" 'kill-other-window-buffer
 "C-c b K" 'delete-other-windows
 "C-c b n" 'next-buffer
 "C-c b p" 'previous-buffer
 "C-c c" 'cp-filename-of-current-buffer
 "C-c C" 'cp-fullpath-of-current-buffer
 "C-c h" 'split-window-horizontally
 "C-c v" 'split-window-vertically
 
 ;; file
 "C-c f o" 'osx-lib-find-file-in-finder
 "C-c f f" 'find-file
 "C-c f d" 'delete-this-file
 
 ;; insert
 "C-c i b" 'ido-insert-buffer
 "C-c i f" 'ido-insert-file
 
 ;; window
 "C-c w h" 'windmove-left
 "C-c w l" 'windmove-right
 "C-c w j" 'windmove-down
 "C-c w k" 'windmove-up
 "C-c w H" 'buf-move-left
 "C-c w L" 'buf-move-right
 "C-c w J" 'buf-move-down
 "C-c w K" 'buf-move-up
 "C-c w d" 'delete-window
 "C-c w D" 'delete-current-buffer-window
 "C-c w k" 'delete-other-windows        ;删除其它窗口
 "C-c w v" 'split-window-horizontally
 "C-c w -" 'split-window-vertically
 "C-c w =" 'windresize-balance-windows
 ;; }}

 ;; {{ 编程相关(lsp,eslint,prettier,flycheck, 补全) ##
 "C-0" 'lsp-bridge-rename               ;重命名当前 tag 名字
 "C-6" 'lsp-bridge-lookup-documentation
 "C-7" 'lsp-bridge-jump-back
 "C-8" 'lsp-bridge-jump
 "C-9" 'lsp-bridge-find-references

 "M-s-j" 'flycheck-next-error
 "M-s-k" 'flycheck-previous-error

 "M-h" 'corfu-insert
 "M-." 'corfu-first
 "M-," 'corfu-last
 "M-j" 'corfu-next
 "M-k" 'corfu-previous

 "M-'" 'cycle-quotes
 "M-\"" 'grammatical-edit-wrap-double-quote
 ;; "M-'" 'grammatical-edit-wrap-single-quote
 "M-{" 'grammatical-edit-wrap-curly
 "M-(" 'grammatical-edit-wrap-round-pair
 "M-[" 'grammatical-edit-wrap-bracket
 "M-)" 'grammatical-edit-unwrap

 ;; }}

 ;; {{ 注释，复制行操作 ##
 "M-;" 'comment-dwim                    ; 在行尾添加注释
 "M-/" 'comment-line
 "C-:" 'comment-or-uncomment-region
 "C-t" 'transpose-chars ;; 当前光标下前后字符位置交换
 "M-s-n" 'comment-part-move-down        ; 注释下移，如果下面也是注释则交换
 "M-s-p" 'comment-part-move-up          ; 注释下移，如果下面也是注释则交换
 "C-s-n" 'comment-dwim-next-line ;; 光标下移并添加注释
 "C-s-p" 'comment-dwim-prev-line ;; 光标上移并添加注释
 "C-S-o" 'duplicate-line-or-region-above ;; 复制行或选中区到上面
 "C-S-l" 'duplicate-line-or-region-below ;; 复制行或选中区到上面
 "C-S-s-o" 'duplicate-line-above-comment ;; 复制行到上面且注释原始行
 "C-S-s-l" 'duplicate-line-below-comment ;; 复制行到下面且注释原始行
 ;; }}

 ;; {{ 搜索，查询(search&replace) ##
 "C-s" 'swiper
 "M-%" 'isearch-query-replace
 "C-M-%" 'vr/query-replace
 "s-R" 're-builder
 ;; color-rg
 "s-x g" 'color-rg-search-symbol
 "s-x h" 'color-rg-search-input
 "s-x j" 'color-rg-search-symbol-in-project
 "s-x k" 'color-rg-search-input-in-project
 "s-x ," 'color-rg-search-symbol-in-current-file
 "s-x ." 'color-rg-search-input-in-current-file

 ;; snails
 "s-u" 'snails-search-point
 "s-y" 'snails
 ;; }}

 ;; {{ vimish, 使用 evil 其实已经自带 fold 了，理论上用不到了 ##
 ;; 但是不排除还会使用的时候，比如一个函数太长，用来折叠 函数内一部分代码时很有用
 "C-M-`" 'vimish-fold-toggle-all
 "C-M-n" 'vimish-fold-next-fold
 "C-M-p" 'vimish-fold-previous-fold
 "C-M-<backspace>" 'vimish-fold-delete ;; 删除当前折叠
 "C-M-S-<backspace>" 'vimish-fold-delete-all ;; 删除所有折叠
 "M-+" 'vimish-fold
 "C-M-+" 'vimish-fold-refold-all
 ;; }}

 ;; {{ 表情符号管理 ##
 "C-c i e" 'emojify-insert-emoji
 "C-c i E" 'emoji-insert
 ;; }}
 
 ;; {{ Emacs 调用外部程序 ##
 "C-c 0" 'org-mac-link-chrome-get-frontmost-url
 "C-c d" 'dash-at-point
 "C-c o e" 'switch-to-erc
 "s-n" 'aweshell-new
 "s-h" 'aweshell-toggle
 "s-x s-x" 'aweshell-dedicated-toggle
 "C-c C-o" 'osx-open-url-at-point       ;打开当前光标下的链接
 ;; }}

 ;; {{ 版本管理 ##
 "M-s-u" 'vdiff-buffers
 "C-c o g" 'magit
 ;; }}

 ;; {{ org-mode 相关 ##
 ;; C-c n, org-roam
 "C-c n" '(:ignore t :which-key "Org Roam")
 "C-c n f" 'org-roam-node-find
 "C-c n g" 'org-roam-graph
 "C-c n i" 'org-roam-node-insert
 "C-c n c" 'org-roam-capture
 "C-c n j" 'org-roam-dailies-capture-today
 "C-c n e" 'org-id-get-create ;; 给当前的header添加id方便被引用
 "C-c n s" 'org-roam-db-sync ;; 同步数据
 "C-c n u" 'org-roam-ui-open ;; 在emacs中打开 ui
 ;; }}

 ;; {{ 翻译相关 ##
 "C-c y" '(:ignore t :which-key "Translate")
 "C-c y y" 'popweb-dict-bing-pointer ;; 翻译当前光标处单词
 "C-c y i" 'popweb-dict-bing-input ;; 翻译输入单词
 "C-c y ," 'insert-translated-name-insert-with-underline
 "C-c y ." 'insert-translated-name-insert-with-camel
 "s-i" 'insert-translated-name-insert
 ;; }}
 

 ;; {{ 模式切换 ##
 "M-x" 'counsel-M-x                     ;用 counsel 可以显示绑定的按键
 "M-r" 'toggle-corfu-english-helper
 "M-s-i" 'ielm-toggle                   ;切换emacs lisp repl buffer
 "s-m" 'toggle-input-method
 "s-o" 'iedit-mode
 ;; }}

 ;; {{ hydra ##
 "C-c C-w" 'my-hydra-window/body
 "C-c C-g" 'my-hydra-git/body
 "C-c C-s" 'my-hydra-search/body
 ;; }}

 )

;; org-mode
(general-define-key
 :keymaps 'org-mode-map
 ;; {{ org-mode ##
 "<tab>" 'org-cycle
 "C-c C-t" 'org-todo
 ;; }}
 )

;; rjsx-mode
(general-define-key
 :keymaps 'rjsx-mode-map
 ;; {{ rjsx-mode ##
 "C-c C-r" 'rjsx-rename-tag-at-point
 ;; }}
 )

;; for web-mode
(general-define-key
 :keymaps 'web-mode-map
 ;; {{ web-mode ##
 "C-j" 'emmet-expand-line
 "M-R" 'instant-rename-tag
 "M-s-SPC" 'web-mode-element-content-select
 "C-s-l" 'web-mode-element-clone
 "C-M-SPC" 'web-mode-mark-and-expand
 "C-:" 'web-mode-comment-or-uncomment
 "C-S-SPC" 'mark-sexp
 "C-c m" '(:ignore t :which-key "web-mode")
 ;; }}
 )

(general-create-definer my-spc-leader-def
  :prefix "SPC"
  :states '(normal visual))

(my-spc-leader-def
  ;; {{ SPC <key>, single key ##
  "SPC" 'counsel-M-x
  "'" 'ivy-resume                       ;恢复之前搜索的minibuffer
  ;; }}

  ;; {{ SPC 0~9, 根据数字选择哪个window ##
  "0" 'winum-select-window-0-or-10
  ;; "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  ;; }}
  

  ;; {{ SPC which-key definition ##
  "a" '(:ignore t :which-key "Apps")
  "b" '(:ignore t :which-key "Buffer")
  "d" '(:ignore t :which-key "Dash")
  "f" '(:ignore t :which-key "File&Find")
  "g" '(:ignore t :which-key "Magit&Go")
  "h" '(:ignore t :which-key "Help")
  "i" '(:ignore t :which-key "Insert")
  "k" '(:ignore t :which-key "Kill")
  "n" '(:ignore t :which-key "Org")
  "o" '(:ignore t :which-key "Open")
  "p" '(:ignore t :which-key "Projectile")
  "s" '(:ignore t :which-key "Search")
  "t" '(:ignore t :which-key "Toggle")
  "w" '(:ignore t :which-key "Window")
  "x" '(:ignore t :which-key "Others")
  "y" '(:ignore t :which-key "Translation")
  ;; }}

  ;; {{ SPC a, applications ##
  "an" '(:ignore t :which-key "aweshell")
  "aan" 'aweshell-new                   ;新建一个shell
  "aat" 'aweshell-toggle                ;切换到新建的shell
  "aax" 'aweshell-dedicated-toggle      ;切换shell
  "ac" '(:ignore t :which-key "Chrome")
  "acc" 'org-mac-link-chrome-get-frontmost-url ;获取当前chrome标签的URL
  "aci" 'org-mac-link-chrome-insert-frontmost-url ;插入当前chrome标签的URL
  "ae" '(:ignore t :which-key "ERC")
  "aee" 'switch-to-erc                  ;切换到ERC聊天窗口
  "ar" 'align-regexp                    ;根据输入的正则进行缩进
  "ah" '(:ignore t :which-key "httpd")
  "ahr" 'httpd-restart-now              ;重启http服务，默认是/var/www中
  "ahd" 'httpd-restart-at-default-directory ;在默认root目录重启http服务
  ;; }}

  ;; {{ SPC b, buffer ##
  "bb" 'switch-to-buffer                ;切换buffer
  "bd" 'kill-this-buffer                ;关闭当前窗口的buffer
  "bD" 'delete-other-windows            ;关掉其它所有窗口
  "bk" 'kill-this-buffer                ;关掉当前窗口的buffer
  "bK" 'kill-other-window-buffer        ;关掉其它窗口的buffer
  "bn" 'next-buffer                     ;切换到下一个buffer
  "bp" 'previous-buffer                 ;切换到上一个buffer
  "bc" 'cp-filename-of-current-buffer   ;拷贝文件名
  "bC" 'cp-fullpath-of-current-buffer   ;拷贝文件全路径
  "bo" 'osx-lib-find-file-in-finder     ;在mac finder 中打开文件
  "bR" 'rename-this-file-and-buffer     ;重命名buffer和文件
  "b," 'bury-buffer                     ;隐藏buffer
  "b." 'unbury-buffer                   ;显示buffer
  ;; }}

  ;; {{ SPC d, dictionay&dash&directory ##
  "dd" 'dash-at-point
  "dr" 'ranger
  ;; }}

  ;; {{ SPC f, file&directory ##
  "ff" 'find-file                       ;查找文件
  "fd" 'delete-this-file
  "fo" 'osx-lib-find-file-in-finder
  "fr" 'refresh-file                    ;会格式化文件
  "fR" 'rename-this-file-and-buffer
  ;; }}

  ;; {{ SPC g, magit & go ##
  "ge" 'eaf-open-git
  ;; magit
  "gg" 'magit                           ;magit版本管理
  "gc" 'git-commit-tracked              ;提交代码不push
  ;; diff
  "gd" 'vdiff-buffers                   ;比较两个buffers
  "gf" 'ffip-show-diff                  ;git diff的一些命令
  ;; conflict
  "g[" (lambda  ()                          ;下一个冲突地方
          (interactive)
          (if (derived-mode-p 'diff-mode) (my-search-next-diff-hunk)
            (my-search-next-merge-conflict)))         
  "g]" (lambda  ()                          ;上一个冲突地方
          (interactive)
          (if (derived-mode-p 'diff-mode) (my-search-prev-diff-hunk)
            (my-search-prev-merge-conflict))) 
  ;; gutter
  "gt" 'git-gutter-mode                  ;切换git-gutter模式
  "g;" 'git-gutter:popup-hunk            ;显示当前文件的改动点
  "g," 'git-gutter:stage-hunk            ;保存修改
  "g." 'git-gutter:revert-hunk           ;还原修改
  ;; vc-msg
  "gv" 'vc-msg-show                     ;显示当前代码提交信息
  ;; 

  ;; {{ SPC h, help&hydra ##
  "h1" 'woman                           ;linxu man方档
  "hw" 'my-hydra-window/body
  "hg" 'my-hydra-git/body
  ;; }}

  ;; {{ SPC i, insert ##
  "ie" 'emojify-insert-emoji            ;加强版表情
  "iE" 'emoji-insert                    ;内置表情
  ;; }}

  ;; {{ SPC k, kill ##
  "kb" 'kill-this-buffer
  ;; }}

  ;; {{ SPC l, workgroup&layout ##
  "ll" 'wg-open-workgroup               ;加载window布局
  "ls" 'wg-create-workgroup             ;保存window布局
  "lk" 'wg-kill-workgroup               ;删除指定window 布局
  ;; }}

  ;; {{ SPC n, org&org-roam ##
  "na"  'org-agenda
  "nc"  '(:ignore t :which-key "Clock")
  "nci" 'org-clock-in                   ;任务开始时间
  "nco" 'org-clock-out                  ;任务结束时间
  "ncr" 'org-clock-report               ;任务时间表
  "nC"  'org-roam-capture                ;创建新的模板
  "ne"  'org-id-get-create               ;给header创建新的id并同步到roam
  "nf"  'org-roam-node-find              ;查找roam中的文件
  "ng"  'org-roam-graph                  ;org-roam图形化界面
  "ni"  'org-roam-node-insert            ;插入新的节点，没有则新建
  "nj"  'org-roam-dailies-capture-today
  "ns"  'org-roam-db-sync                ;同步数据
  "nt"  '(:ignore t :which-key "Toggle")
  "ntl" 'org-toggle-link-display  ; 切换org-mode中链接显示方式
  "nu"  'org-roam-ui-open                ;图形化界面
  "nv"  'org-tags-view                   ;搜索org-agenda中标题带tag的
  "n<"  'org-do-promote                  ;C-c C-<，改变当前目录层级，h2 -> h1
  "n>" 'org-do-demote                   ;C-c C->, 改变当前目录层级，h1 -> h2
  ;; }}

  ;; {{ SPC o, open ##

  ;; }}

  ;; {{ SPC p, Projectile ##
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  "pF" 'projectile-find-file-other-window
  "pd" 'projectile-find-dir
  "pD" 'projectile-find-dir-other-window
  "pr" 'projectile-replace
  "pR" 'projectile-replace-regexp
  ;; }}
  

  ;; {{ SPC q, quit & query... ##
  "qq" 'emacs-session-save
  "qr" 'restart-emacs
  ;;

  ;; {{ SPC s, search ##
  "ss" 'counsel-grep
  ;; fastdef, 使用 w3m 时报错了
  ;; "sf" '(:ignore t :which-key "fastdef")
  ;; "sfi" 'fastdef-insert
  ;; "sfh" 'fastdef-insert-from-history
  ;; color-rg
  "sg" 'color-rg-search-symbol
  "sh" 'color-rg-search-input
  "sj" 'color-rg-search-symbol-in-project
  "sk" 'color-rg-search-input-in-project
  "s," 'color-rg-search-symbol-in-current-file
  "s." 'color-rg-search-input-in-current-file
  ;; regex
  "sr" 're-builder
  "s%" 'isearch-query-replace
  "sv" 'vr/query-replace
  "sn" 'snails
  "sN" 'snails-search-point
  ;; }}

  ;; {{ SPC w, window ##
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wL" 'buf-move-right
  "wH" 'buf-move-left
  "wJ" 'buf-move-down
  "wK" 'buf-move-up
  "wm" 'maximize-window
  "wv" 'split-window-right
  "w-" 'split-window-below
  "wd" 'delete-window
  "wD" 'delete-other-windows
  "w=" 'balance-windows
  "wt" 'toggle-one-window
  ;; }}

  ;; {{ SPC y, Translation&Yasnippet ##
  "yy" 'popweb-dict-bing-pointer        ;翻译当前光标处单词
  "yi" 'popweb-dict-bing-input          ;翻译输入的内容
  "y," 'insert-translated-name-insert-with-underline
  "y." 'insert-translated-name-insert-with-camel
  "yI" 'insert-translated-name-insert
  ;; 本地词典查询，词典目录在 ~/.emacs.d/sdcv-dict/, 通过 install-emacs-dicts 下载
  "yb" 'sdcv-search-input               ;sdcv词典翻译，会打开新buffer显示结果
  "yt" 'sdcv-search-input+              ;sdcv 词典翻译+，以弹窗形式显示
  "yB" 'sdcv-search-pointer             ;本地词典搜索，词典目录.emacs.d/sdcv-dict
  "yT" 'sdcv-search-pointer+            ;本地词典搜索，词典目录.emacs.d/sdcv-dict
  ;; yas-snippets
  "yc" 'aya-create                      ;创建snippet
  "ye" 'aya-expand                      ;展开snippet
  "yo" 'aya-open-line                   ;展开snippet
  ;; }}

  ;; {{ SPC t, Toggle 切换 ##
  "te" 'toggle-corfu-english-helper     ;切换英文字典模式，补全中会出现相关单词
  "ti" 'toggle-input-method             ;切换输入法
  "tf" 'toggle-frame-fullscreen
  ;; }}

  )

(provide   'init-general-key)
;;; init-general-key.el ends here
