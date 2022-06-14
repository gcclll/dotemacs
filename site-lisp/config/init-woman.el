;;; init-woman.el --- Init woman module
(require 'woman)
(require 'lazy-load)

;;; Code:

(lazy-load-set-keys
 '(
   ("J" . scroll-up-one-line)           ;向上滚动一行
   ("K" . scroll-down-one-line)         ;向下滚动一行
   ("," . end-of-buffer)                ;buffer末尾
   ("." . beginning-of-buffer)          ;buffer开始
   ("M-n" . Man-next-section)           ;下一节
   ("M-p" . Man-previous-section)       ;上一节
   ("g" . Man-goto-section)             ;跳转到某一节
   ("G" . Man-goto-see-also-section)    ;跳转到 see-also
   ("f" . Man-follow-manual-reference)  ;当前处的man手册引用
   ("F" . man-follow)                   ;某man手册引用
   ("N" . Man-next-manpage)             ;下一个页面
   ("P" . Man-previous-manpage)         ;上一个页面
   ("q" . Man-quit)                     ;隐藏
   ("Q" . Man-kill)                     ;退出
   )
 Man-mode-map
 )

(setq woman-default-indent 7            ;缩进格式
      woman-fill-frame t                ;填充满屏幕
      woman-use-own-frame nil           ;同一个frame
      woman-cache-level 3)              ;缓存级别, 最快

(message "> init-woman.el")
(provide 'init-woman)

;;; init-woman.el ends here
