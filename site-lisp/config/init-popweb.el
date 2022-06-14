;;; init-popweb.el --- Configure for popweb

;;; Require
(require 'popweb-dict-bing)
(require 'popweb-dict-youdao)
(require 'insert-translated-name)

;;; Code:

(setq popweb-proxy-type "socks5")
(setq popweb-proxy-host "127.0.0.1")
(setq popweb-proxy-port "1080")

(dolist (hook (list
               'markdown-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'insert-translated-name)
                     (insert-translated-name-use-original-translation))))

(message "> init-popweb.el")
(provide 'init-popweb)

;;; init-popweb.el ends here
