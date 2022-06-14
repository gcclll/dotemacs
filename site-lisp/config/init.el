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
  (defvar lazycat-emacs-extension-dir (concat lazycat-emacs-root-dir "/extensions"))
  (with-temp-message ""              ;抹掉插件启动的输出
    ;;(require 'benchmark-init-modes)
    ;;(require 'benchmark-init)
    ;;(benchmark-init/activate)
    (require 'basic-toolkit) 
    (require 'init-tree-sitter) 
    (require 'grammatical-edit) 
    (when 
        (featurep 'cocoa) 
      (require 'cache-path-from-shell)) 
    (require 'display-line-numbers)
    (require 'init-proxy) 
    (require 'init-utils) 
    (require 'init-file-type) 
    (require 'init-fullscreen)
    ;; (require 'init-evil)
    (require 'init-generic) 
    (require 'lazy-load) 
    (require 'one-key)
    (require 'init-awesome-tray) 
    (require 'init-auto-save) 
    (require 'init-line-number) 
    (require 'init-one-key) 
    (require 'init-key) 
    (require 'init-lsp-bridge) 
    (require 'init-hydra) 
    (require 'init-performance) 
    (require 'init-mode) 
    (require 'init-grammatical-edit) 
    (require 'init-indent)

    ;; (require 'init-theme)
    ;; (require 'lazycat-theme)
    ;; (lazycat-theme-load-with-sunrise)
    ;; (lazycat-theme-load-dark)

    ;; 可以延后加载的
    (run-with-idle-timer 1 nil #'(lambda ()
                                   ;; (require 'pretty-lambdada)
                                   (require 'browse-kill-ring)
                                   (require 'elf-mode) 
                                   (require 'init-popweb) 
                                   (require 'init-eaf) 
                                   (require 'init-ivy) 
                                   (require 'init-window) 
                                   (require 'init-search) 
                                   (require 'init-which-key) 
                                   (require 'init-smooth-scrolling) 
                                   (require 'init-projectile) 
                                   (require 'init-idle) 
                                   (require 'init-misc) 
                                   (require 'init-osx) 
                                   (require 'init-symbol-overlay) 
                                   (require 'init-thing-edit) 
                                   (require 'init-snails) 
                                   (require 'init-visual-regexp) 
                                   (require 'init-formatter) 
                                   (require 'init-iedit) 
                                   (require 'init-woman) 
                                   (require 'init-org) 
                                   ;; (require 'init-sort-tab)
                                   ;; (require 'init-eldoc)
                                   ;; (require 'init-yasnippet)

                                   (require 'init-cursor-chg)
                                   (require 'init-markdown-mode)
                                   
                                   ;; 开发语言相关
                                   (require 'init-c)
                                   (require 'init-web-mode)
                                   (require 'init-rjsx-mode)

                                   ;; (require 'init-restclient)

                                   ;; (require 'init-git)

                                   ;; (require 'init-parrot)
                                   ;; ;;(require 'init-leetcode)
                                   ;; (require 'init-httpd)
                                   ;; (require 'init-sdcv)
                                   ;; (require 'init-ibuffer)        ;ibuffer显示优化
                                   ;; (require 'init-ranger)
                                   ;; (require 'init-treemacs)

                                   ;; (require 'init-speedbar)


                                   ;; ;; Restore session at last.
                                   ;; (require 'init-session)
                                   ;; (emacs-session-restore) ;; 要退出了就没必要保存会话了

                                   ;; (require 'init-general-key)

	                                 ;;  (server-start t t)        ;为emacsclient准备使用场景，比如git
	                                 (message "Emacs Ready, Enjoy It !!")))))
(provide 'init)
