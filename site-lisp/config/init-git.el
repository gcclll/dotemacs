;;; init-git.el --- Init for git

;;; Require
(require 'git-gutter)
(require 'vc-msg)
(require 'blamer)
;; (require 'vdiff)

;; {{ git-gutter
(with-eval-after-load 'git-gutter (unless (fboundp 'global-display-line-numbers-mode) 
                                    (git-gutter:linum-setup)) 
                      (setq git-gutter:update-interval 2) 
                      (setq git-gutter:disabled-modes '(asm-mode org-mode outline-mode markdown-mode
                                                                 image-mode)))

(custom-set-variables
 ;; '(git-gutter:window-width 2)
 ;; '(git-gutter:modified-sign "☁")
 ;; '(git-gutter:added-sign "☀")
 ;; '(git-gutter:deleted-sign "☂")
 ;; '(git-gutter:separator-sign "|")
 '(git-gutter:hide-gutter t)       ;没有变更的时候不显示gutter
 ;; '(git-gutter:diff-option "-w")    ;忽略所有空格
 '(git-gutter:ask-p nil)           ;总是提示commit/revert ?
 ;; '(git-gutter:verbosity 0)         ;不需要log/message
 ;; '(git-gutter:unchanged-sign " ")
 )
;; (set-face-background 'git-gutter:unchanged "yellow")
;; (set-face-foreground 'git-gutter:separator "yellow")

;; diff information is updated at hooks in git-gutter:update-hooks.
;; (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
;; diff information is updated after command in git-gutter:update-commands executed. 
;; (add-to-list 'git-gutter:update-commands 'other-window)


(defun my-git-gutter-reset-to-head-parent () 
  "Reset gutter to HEAD^." 
  (interactive) 
  (let* ((filename (buffer-file-name)) 
         (cmd (concat "git --no-pager log --oneline -n1 --pretty=\"format:%H\" " filename)) 
         (parent (cond ((eq git-gutter:vcs-type 'svg) "PREV") 
                       (filename (concat (shell-command-to-string cmd) "^")) 
                       (t "HEAD^")))) 
    (git-gutter:set-start-revision parent) 
    (message "git-gutter:set-start-revision HEAD^")))

(defun git-gutter-toggle () 
  "Toggle git gutter." 
  (interactive) 
  (git-gutter-mode -1)
  ;; git-gutter-fringe doesn't seem to
  ;; clear the markup right away
  (sit-for 0.1) 
  (git-gutter:clear))
(defun git-gutter-reset-to-default () 
  "Restore git gutter to its original status.
Show the diff between current working code and git head." 
  (interactive) 
  (git-gutter:set-start-revision nil) 
  (message "git-gutter reset"))
(my-run-with-idle-timer 2 #'global-git-gutter-mode)
;; }}

;; {{ git functions
(defun git-get-current-file-relative-path () 
  "Get relative path of current file for Git."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory)) ""
                            buffer-file-name))

(defun git-checkout-current-file () 
  "Git checkout current file." 
  (interactive) 
  (when (and (buffer-file-name) 
             (yes-or-no-p (format "git checkout %s?" (file-name-nondirectory (buffer-file-name))))) 
    (let* ((filename (git-get-current-file-relative-path))) 
      (shell-command (concat "git checkout " filename)) 
      (message "DONE! git checkout %s" filename))))

(defvar git-commit-message-history nil)
(defun git-commit-tracked () 
  "Run 'git add -u' and commit." 
  (interactive) 
  (let* ((hint "Commit tracked files. Please input commit message (Enter to abort):") 
         (msg (read-from-minibuffer hint nil nil nil 'git-commit-message-history))) 
    (cond ((and 
            msg
            (> (length msg) 3)) 
           (shell-command "git add -u") 
           (shell-command (format "git commit -m \"%s\"" msg)) 
           (message "Tracked files is committed.")) 
          (t (message "Do nothing!")))))

(defun git-add-current-file () 
  "Git add file of current buffer." 
  (interactive) 
  (when buffer-file-name (let* ((filename (git-get-current-file-relative-path))) 
                           (shell-command (concat "git add " filename)) 
                           (message "DONE! git add %s" filename))))
;; }}



;; {{ merge conflict
(defvar my-goto-merge-conflict-fns '(("n" my-next-merge-conflict) 
                                     ("p" my-prev-merge-conflict)))

(defun my-next-merge-conflict () 
  "Go to next merge conflict." 
  (interactive) 
  (my-goto-merge-conflict-internal t))

(defun my-prev-merge-conflict () 
  "Go to previous merge conflict." 
  (interactive) 
  (my-goto-merge-conflict-internal nil))

(defun my-goto-merge-conflict-internal (forward-p) 
  "Goto specific hunk.  If forward-p is t, go in forward direction."
  ;; @see https://emacs.stackexchange.com/questions/63413/finding-git-conflict-in-the-same-buffer-if-cursor-is-at-end-of-the-buffer#63414
  (my-ensure 'smerge-mode) 
  (let ((buffer (current-buffer)) 
        (hunk-fn (if forward-p 'smerge-next 'smerge-prev))) 
    (unless (funcall hunk-fn) 
      (vc-find-conflicted-file) 
      (when (eq buffer (current-buffer)) 
        (let ((prev-pos (point))) 
          (goto-char (if forward-p (point-min) 
                       (1- (point-max)))) 
          (unless (funcall hunk-fn) 
            (goto-char prev-pos) 
            (message "No conflicts found")))))))

(defun my-search-next-merge-conflict () 
  "Search next merge conflict." 
  (interactive) 
  (my-setup-extra-keymap my-goto-merge-conflict-fns "Goto merge conflict: [n]ext [p]revious [q]uit"
                         'my-goto-merge-conflict-internal t))

(defun my-search-prev-merge-conflict () 
  "Search previous merge conflict." 
  (interactive) 
  (my-setup-extra-keymap my-goto-merge-conflict-fns "Goto merge conflict: [n]ext [p]revious [q]uit"
                         'my-goto-merge-conflict-internal nil))
;; }}



;; {{ diff hunk
(defvar my-goto-diff-hunk-fns '(("n" diff-hunk-next) 
                                ("p" diff-hunk-prev)))

(defun my-search-next-diff-hunk () 
  "Search next diff hunk." 
  (interactive) 
  (my-setup-extra-keymap my-goto-diff-hunk-fns "Goto diff hunk: [n]ext [p]revious [q]uit"
                         'diff-hunk-next))

(defun my-search-prev-diff-hunk () 
  "Search previous diff hunk." 
  (interactive) 
  (my-setup-extra-keymap my-goto-diff-hunk-fns "Goto diff hunk: [n]ext [p]revious [q]uit"
                         'diff-hunk-prev))

;; }}

;; {{ git-gutter use ivy
(defun my-reshape-git-gutter (gutter) 
  "Re-shape gutter for `ivy-read'."
  (let* ((linenum-start (aref gutter 3)) 
         (linenum-end (aref gutter 4)) 
         (target-line "") 
         (target-linenum 1) 
         (tmp-line "") 
         (max-line-length 0)) 
    (save-excursion (while (<= linenum-start linenum-end) 
                      (goto-line linenum-start) 
                      (setq tmp-line (replace-regexp-in-string "^[ \t]*" "" 
                                                               (buffer-substring 
                                                                (line-beginning-position) 
                                                                (line-end-position)))) 
                      (when (> (length tmp-line) max-line-length) 
                        (setq target-linenum linenum-start) 
                        (setq target-line tmp-line) 
                        (setq max-line-length (length tmp-line)))
                      (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s" (if (eq 'deleted (aref gutter 1)) "-" "+") target-linenum target-line)
          target-linenum)))

(defun my-goto-git-gutter () 
  (interactive) 
  (if git-gutter:diffinfos (ivy-read "git-gutters:" (mapcar 'my-reshape-git-gutter
                                                            git-gutter:diffinfos) 
                                     :action (lambda (e) 
                                               (unless (numberp e) 
                                                 (setq e (cdr e))) 
                                               (goto-line e))) 
    (message "NO git-gutters!")))

;; }}

;; {{ git rebase
(defun my-git-extract-based (target lines) 
  "Extract based version from TARGET."
  (let* (based (i 0) break) 
    (while (and (not break) 
                (< i (length lines))) 
      (cond ((string-match (regexp-quote target) 
                           (nth i lines)) 
             (setq break t)) 
            (t 
             (setq i (1+ i)))))
    ;; find child of target commit
    (when (and (< 0 i) 
               (< i (length lines))) 
      (setq based (replace-regexp-in-string "^tag: +" "" (car (split-string (nth (1- i) lines) "
+"))))) based))

;; }}

;; {{ git commit

(defun my-git-commit-id () 
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'") 
         (collection (my-nonempty-lines (shell-command-to-string git-cmd))) 
         (item (completing-read "git log:" collection))) 
    (when item (car (split-string item "|" t)))))

(defun my-git-show-commit-internal () 
  "Show git commit."
  (let* ((id (my-git-commit-id))) 
    (when id (shell-command-to-string (format "git show %s" id)))))

(defun my-git-show-commit () 
  "Show commit using ffip." 
  (interactive) 
  (let* ((ffip-diff-backends '(("Show git commit" . my-git-show-commit-internal)))) 
    (ffip-show-diff 0)))
;; }}

;;; Code:
(when 
    (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell) 
  (exec-path-from-shell-initialize))
(load-library "with-editor")

;; blamer, show commit message like vscode
;; (global-blamer-mode 1) ; 影响操作性能，有需要使用 s-i 查看
(setq blamer-idle-time 0.3 blamer-min-offset 70 blamer-author-formatter " ✎ %s "
      blamer-datetime-formatter "[%s]" blamer-commit-formatter " ● %s")

(message "> init-git.el")
(provide 'init-git)

;;; init-git.el ends here
