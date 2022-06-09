(require 'prettier)

;; 找不到 prettier-el.js.gz.base64 问题: https://github.com/radian-software/straight.el/issues/621
;; 这个只对 straight 有用啊!!!
;; cd straight/repos/prettier.el/
;; npm install --frozen-lockfile
;; make
;; cd ../../build/prettier
;; cp ../../repos/prettier.el/dist/prettier-el.js.gz.base64 .
;; cp ../../repos/prettier.el/dist/bootstrap-min.js .

;; (add-hook 'after-init-hook #'global-prettier-mode)

(provide 'init-formatter)
