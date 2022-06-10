;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)

;; 人个的一些设置
(require 'init-personal)

(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
       ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; 定义一些启动目录，方便下次迁移修改
  (defvar lazycat-emacs-root-dir (file-truename "~/.emacs.d/site-lisp"))
  (defvar lazycat-emacs-config-dir (concat lazycat-emacs-root-dir "/config"))
   (defvar lazycat-emacs-extension-dir (concat lazycat-emacs-root-dir "/extensions") )
 
   (with-temp-message ""              ;抹掉插件启动的输出
     ;;(require 'benchmark-init-modes)
     ;;(require 'benchmark-init)
     ;;(benchmark-init/activate)
 
     (require 'init-proxy)
     (require 'init-utils)
     (require 'init-file-type)
     (require 'init-evil)
    ;; (require 'init-fullscreen)

    (require 'init-generic)
    ;; (require 'init-theme)
    (require 'lazycat-theme)
    ;; (lazycat-theme-load-with-sunrise)
    (lazycat-theme-load-dark)
    (when (featurep 'cocoa)
      (require 'cache-path-from-shell))
    ;; (require 'lazy-load)
    (require 'one-key)
    (require 'grammatical-edit)
    (require 'display-line-numbers)
    (require 'basic-toolkit)
    (require 'redo)

    (require 'init-highlight-parentheses)
    (require 'init-awesome-tray)
    (require 'init-line-number)
    (require 'init-lsp-bridge)
    (require 'init-auto-save)
    (require 'init-mode)
    (require 'init-grammatical-edit)
    (require 'init-indent)
    (require 'init-one-key)
    ;; (require 'init-vi-navigate)
    (require 'init-isearch-mb)
    (require 'init-performance)
    (require 'init-hydra)
    ;; (require 'init-rime) ;; 用的是 squirrel + rime 够用
    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
         (require 'pretty-lambdada)
         (require 'browse-kill-ring)
         (require 'elf-mode)
         (require 'snails)

         (require 'init-tree-sitter)
         (require 'init-eldoc)
         (require 'init-yasnippet)
         (require 'init-smooth-scrolling)
         (require 'init-cursor-chg)
         (require 'init-winpoint)
         ;; (require 'init-info)
         (require 'init-c)
         (require 'init-org)
         (require 'init-idle)
         (require 'init-markdown-mode)
         (require 'init-popweb)

         (require 'init-eaf) ;; 我这mac上依旧卡，干脆不用了
         (require 'init-search-engine)
         (require 'init-ivy)
         (require 'init-which-key)
         (require 'init-projectile)
         (require 'init-window)
         (require 'init-misc)
         (require 'init-string-inflection)
         (require 'init-restclient)
         (require 'init-rjsx-mode)
         (require 'init-osx)
         (require 'init-git)
         (require 'init-web-mode)
         (require 'init-iedit)
         (require 'init-parrot)
         ;;(require 'init-leetcode)
         (require 'init-httpd)
         (require 'init-sdcv)
         (require 'init-ibuffer)        ;ibuffer显示优化
         (require 'init-ranger)
         (require 'init-treemacs)
         (require 'init-formatter)
         (require 'init-speedbar)
         (require 'init-thing-edit)

         ;; Restore session at last.
         (require 'init-session)
         ;; (emacs-session-restore) ;; 要退出了就没必要保存会话了
         (require 'init-sort-tab)
         (require 'init-general-key)
        ;;  (server-start t t)        ;为emacsclient准备使用场景，比如git
         )))
  )

(provide 'init)
