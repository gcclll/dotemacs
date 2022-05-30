;;; init-search-engine.el


;;; Require
(require 'engine-mode)

(engine/set-keymap-prefix (kbd "C-c s"))
(defengine baidu "https://www.baidu.com/s?wd=%s"
           :keybinding "b")
(defengine github
           "https://github.com/search?ref=simplesearch&q=%s"
           :keybinding "g")
(defengine qwant
           "https://www.qwant.com/?q=%s"
           :docstring "什么都能搜到哦~~😍😍"
           :keybinding "q")
(defengine rfcs
           "http://pretty-rfc.herokuapp.com/search?q=%s"
           :keybinding "r")
(defengine stack-overflow
           "https://stackoverflow.com/search?q=%s"
           :keybinding "s")
(defengine twitter
           "https://twitter.com/search?q=%s"
           :keybinding "t")
(defengine wolfram-alpha
           "http://www.wolframalpha.com/input/?i=%s"
           :docstring "数学搜索引擎，公式，坐标图等。"
           :keybinding "w")
(defengine google
           "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
           :keybinding "/")
(defengine youtube
           "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
           :keybinding "y")

(engine-mode t)
;;; Code

(provide 'init-search-engine)
;;; init-search-engine.el ends here

