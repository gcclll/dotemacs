;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'simple-httpd)

(setq httpd-port 4444)
(defun httpd-restart-now ()
  (interactive)
  (httpd-stop)
  (httpd-start)
  (message "http://localhost:%d/ at %s restarted"
           httpd-port httpd-root))

(defun httpd-restart-at-default-directory ()
  (interactive)
  (setq httpd-root default-directory)
  (httpd-restart-now))

(provide 'init-httpd)
