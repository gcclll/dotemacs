(require 'perspective)
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "M-,"))
(persp-mode)

(provide 'init-workspace)
