;;; init-awesome-tray.el --- Configuration for awesome tray

;;; Require
(require 'awesome-tray)

;;; Code:
(awesome-tray-mode 1)
(setq awesome-tray-active-modules '(
                                    "location"
                                    "belong"
                                    "file-path"
                                    "mode-name"
                                    "battery"
                                    ;; "date"
                                    "git"
                                    "input-method"
                                    "evil"
                                    "belong"
                                    ;; "flymake"
                                    ))
(provide 'init-awesome-tray)

;;; init-awesome-tray.el ends here
