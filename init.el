;;; init.el --- 初始化
;;; 这份配置是 fork 自 https://github.com/manateelazycat/lazycat-emacs 
;;; 只适用于 macos，ELisp 正在努力学习中，额外添加的配置只在自己的 mac 上跑过
;;; 修改和增加了一些配置，主要参考配置：
;;; 1. https://github.com/redguardtoo/emacs.d
;;; 按键是基于 one-key 创建对应的 menu 进行管理(大部分按键都是一个特殊键+字符键)
;;; 按键绑定查询 http://blog.cheng92.com/posts/emacs_all_keybindings.html

(require 'cl-lib)

(tool-bar-mode -1)                      ;禁用工具栏
(menu-bar-mode -1)                      ;禁用菜单栏
(scroll-bar-mode -1)                    ;禁用滚动条

(setq *is-a-mac* (eq system-type 'darwin))
(setq *no-memory* (cond
                   (*is-a-mac*
                    ;; @see https://discussions.apple.com/thread/1753088
                    ;; "sysctl -n hw.physmem" does not work
                    (<= (string-to-number (shell-command-to-string "sysctl -n hw.memsize"))
                        (* 4 1024 1024)))
                   (*linux* nil)
                   (t nil)))

(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是文件的都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 目录匹配下面规则的都移除
                   (member subdir '("." ".." ;Linux当前目录和父目录
                                    "dist" "node_modules" "__pycache__" ;语言相关的模块目录
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ;版本控制目录
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))
          
          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")
(require 'exec-path-from-shell)
;; 设成nil 则不从 .zshrc 读 只从 .zshenv读（可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中）
;; (setq exec-path-from-shell-check-startup-files nil) ;
;; (setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
(exec-path-from-shell-initialize)

;; (add-subdirs-to-load-path "/usr/share/emacs/lazycat")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'init)
