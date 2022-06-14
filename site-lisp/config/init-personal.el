;;; init-personal.el ---


;;; Require

;;; Code

(setq user-full-name "Zhicheng Lee")
(setq user-mail-address "gccll.love@gmail.com")
(setq user-blog-url "https://blog.cheng92.com")
(setq user-github-dir "~/github/mine/")
(setq user-blog-dir (concat user-github-dir "blog.cheng92.com/"))
(setq user-blog-posts (concat user-blog-dir "posts/"))
(setq org-directory (concat user-blog-dir "org/"))
(setq org-roam-directory org-directory)
(setq org-html-doctype "html5"
      org-latex-listings 'minted
      org-html-htmlize-output-type 'css)

(message "> init-personal.el")
(provide 'init-personal)
