;;; init-ivy.el ---

;;; Require
(require 'ivy)
(require 'counsel)

;;; Code
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(message "> init-ivy.el")
(provide 'init-ivy)
;;; init-ivy.el ends here
