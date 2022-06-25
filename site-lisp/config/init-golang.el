;;; init-golang.el --- Extensions for go lang mode

(require 'go-mode)
(require 'gotest)

;;; Code:

(defun go-run-buffer()
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

;;; ### Golang ###
;; (lazy-load-unset-keys
;;  '("C-k" "M-o")
;;  go-mode-map)
;; (lazy-load-set-keys
;;  '(
;;    ("C-c C-c" . go-run-buffer)
;;    ("C-c C-f" . gofmt)
;;    ("C-c C-d" . godoc)
;;    ("C-c C-a" . go-import-add)
;;    ("C-c t" . go-test-current-test)
;;    ("C-c f" . go-test-current-file)
;;    ("C-c p" . go-test-current-project)
;;    ("C-c b" . go-test-current-benchmark))
;;  go-mode-map)

(message "> init-golang.el")
(provide 'init-golang)

;;; init-golang.el ends here
