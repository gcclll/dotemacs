;;; init-git.el --- Init for git

;;; Require
(require 'magit)
(require 'git-gutter)
(require 'vc-msg)
(require 'blamer)
;; (require 'vdiff)

;; {{ git-gutter
(with-eval-after-load 'git-gutter
  (unless (fboundp 'global-display-line-numbers-mode)
    (git-gutter:linum-setup))
  (setq git-gutter:update-interval 2)
  (setq git-gutter:disabled-modes
        '(asm-mode
          org-mode
          outline-mode
          markdown-mode
          image-mode))
  )

(defun my-git-gutter-reset-to-head-parent ()
  "Reset gutter to HEAD^."
  (interactive)
  (let* ((filename (buffer-file-name))
         (cmd (concat "git --no-pager log --oneline -n1 --pretty=\"format:%H\" " filename))
         (parent (cond
                  ((eq git-gutter:vcs-type 'svg)
                   "PREV")
                  (filename
                   (concat (shell-command-to-string cmd) "^"))
                  (t
                   "HEAD^"))))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))
(defvar my-prefer-lightweight-magit t)
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
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

(defun git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (message "DONE! git checkout %s" filename))))

(defvar git-commit-message-history nil)
(defun git-commit-tracked ()
  "Run 'git add -u' and commit."
  (interactive)
  (let* ((hint "Commit tracked files. Please input commit message (Enter to abort):")
         (msg (read-from-minibuffer hint
                                    nil
                                    nil
                                    nil
                                    'git-commit-message-history)))
    (cond
     ((and msg (> (length msg) 3))
      (shell-command "git add -u")
      (shell-command (format "git commit -m \"%s\"" msg))
      (message "Tracked files is committed."))
     (t
      (message "Do nothing!")))))

(defun git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (when buffer-file-name
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))
;; }}



;; {{ merge conflict
(defvar my-goto-merge-conflict-fns
  '(("n" my-next-merge-conflict)
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
          (goto-char (if forward-p (point-min) (1- (point-max))))
          (unless (funcall hunk-fn)
            (goto-char prev-pos)
            (message "No conflicts found")))))))

(defun my-search-next-merge-conflict ()
  "Search next merge conflict."
  (interactive)
  (my-setup-extra-keymap my-goto-merge-conflict-fns
                         "Goto merge conflict: [n]ext [p]revious [q]uit"
                         'my-goto-merge-conflict-internal
                         t))

(defun my-search-prev-merge-conflict ()
  "Search previous merge conflict."
  (interactive)
  (my-setup-extra-keymap my-goto-merge-conflict-fns
                         "Goto merge conflict: [n]ext [p]revious [q]uit"
                         'my-goto-merge-conflict-internal
                         nil))
;; }}



;; {{ diff hunk
(defvar my-goto-diff-hunk-fns
  '(("n" diff-hunk-next)
    ("p" diff-hunk-prev)))

(defun my-search-next-diff-hunk ()
  "Search next diff hunk."
  (interactive)
  (my-setup-extra-keymap my-goto-diff-hunk-fns
                         "Goto diff hunk: [n]ext [p]revious [q]uit"
                         'diff-hunk-next))

(defun my-search-prev-diff-hunk ()
  "Search previous diff hunk."
  (interactive)
  (my-setup-extra-keymap my-goto-diff-hunk-fns
                         "Goto diff hunk: [n]ext [p]revious [q]uit"
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
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (ivy-read "git-gutters:"
                (mapcar 'my-reshape-git-gutter git-gutter:diffinfos)
                :action (lambda (e)
                          (unless (numberp e) (setq e (cdr e)))
                          (goto-line e)))
    (message "NO git-gutters!")))

;; }}

;; {{ git rebase
(defun my-git-extract-based (target lines)
  "Extract based version from TARGET."
  (let* (based (i 0) break)
    (while (and (not break) (< i (length lines)))
      (cond
       ((string-match (regexp-quote target) (nth i lines))
        (setq break t))
       (t
        (setq i (1+ i)))))
    ;; find child of target commit
    (when (and (< 0 i)
               (< i (length lines)))
      (setq based
            (replace-regexp-in-string "^tag: +"
                                      ""
                                      (car (split-string (nth (1- i) lines)
                                                         " +")))))
    based))

(defun my-git-rebase-interactive (&optional user-select-branch)
  "Rebase interactively on the closest branch or tag in git log output.
If USER-SELECT-BRANCH is not nil, rebase on the tag or branch selected by user."
  (interactive "P")
  (let* ((cmd "git --no-pager log --decorate --oneline -n 1024")
         (lines (my-lines-from-command-output cmd))
         (targets (delq nil
                        (mapcar (lambda (e)
                                  (when (and (string-match "^[a-z0-9]+ (\\([^()]+\\)) " e)
                                             (not (string-match "^[a-z0-9]+ (HEAD " e)))
                                    (match-string 1 e)))
                                lines)))
         based)
    (cond
     ((or (not targets) (eq (length targets) 0))
      (message "No tag or branch is found to base on."))
     ((or (not user-select-branch)) (eq (length targets) 1)
      ;; select the closest/only tag or branch
      (setq based (my-git-extract-based (nth 0 targets) lines)))
     (t
      ;; select the one tag or branch
      (setq based (my-git-extract-based (completing-read "Select based: " targets)
                                        lines))))

    ;; start git rebase
    (when based
      (magit-rebase-interactive based nil))))

;; }}

;; {{ git commit

(defun my-git-commit-id ()
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (collection (my-nonempty-lines (shell-command-to-string git-cmd)))
         (item (completing-read "git log:" collection)))
    (when item
      (car (split-string item "|" t)))))

(defun my-git-show-commit-internal ()
  "Show git commit."
  (let* ((id (my-git-commit-id)))
    (when id
      (shell-command-to-string (format "git show %s" id)))))

(defun my-git-show-commit ()
  "Show commit using ffip."
  (interactive)
  (let* ((ffip-diff-backends '(("Show git commit" . my-git-show-commit-internal))))
    (ffip-show-diff 0)))

(defun my-git-find-file-in-commit (&optional level)
  "Find file in previous commit with LEVEL.
If LEVEL > 0, find file in previous LEVEL commit."
  (interactive "P")
  (my-ensure 'magit)
  (let* ((rev (concat "HEAD" (if (and level (> level 0)) (make-string level ?^))))
         (pretty (string-trim (shell-command-to-string (format "git --no-pager log %s --oneline --no-walk" rev))))
         (prompt (format "Find file from commit [%s]: " pretty))
         (cmd (my-git-files-in-rev-command rev level))
         (default-directory (my-git-root-dir))
         (file (completing-read prompt (my-lines-from-command-output cmd))))
    (when file
      (find-file file))))

(defun my-git-cherry-pick-from-reflog ()
  "Cherry pick a commit from git reflog."
  (interactive)
  (let* ((cmd "git --no-pager reflog --date=short")
         (lines (my-lines-from-command-output cmd))
         (selected (completing-read "Commit to cherry pick:" lines))
         (commit-id (and selected (car (split-string selected)))))
    (when commit-id
      (my-ensure 'magit)
      (magit-cherry-copy commit-id))))
;; }}

;;; Code:
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(load-library "with-editor")

;; Magit configuration.
(setq magit-commit-ask-to-stage nil)    ;don't ask stage question
(setq magit-display-buffer-noselect t) ;don't select magit buffer default

;; Make path column have enough space to display.
(setq magit-submodule-list-columns
      '(("Path"     80 magit-modulelist-column-path   nil)
        ("Version"  30 magit-repolist-column-version  nil)
        ("Branch"   20 magit-repolist-column-branch   nil)
        ("B<U" 3 magit-repolist-column-unpulled-from-upstream   ((:right-align t)))
        ("B>U" 3 magit-repolist-column-unpushed-to-upstream     ((:right-align t)))
        ("B<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
        ("B>P" 3 magit-repolist-column-unpushed-to-pushremote   ((:right-align t)))
        ("B"   3 magit-repolist-column-branches                 ((:right-align t)))
        ("S"   3 magit-repolist-column-stashes                  ((:right-align t)))))

;; (one-key-create-menu
;;  "MAGIT"
;;  '(
;;    (("s" . "Magit status") . magit-status+)
;;    (("c" . "Magit checkout") . magit-checkout)
;;    (("C" . "Magit commit") . magit-commit)
;;    (("u" . "Magit push to remote") . magit-push-current-to-pushremote)
;;    (("p" . "Magit delete remote branch") . magit-delete-remote-branch)
;;    (("i" . "Magit pull") . magit-pull-from-upstream)
;;    (("r" . "Magit rebase") . magit-rebase)
;;    (("e" . "Magit merge") . magit-merge)
;;    (("l" . "Magit log") . magit-log-all)
;;    (("L" . "Magit blame") . magit-blame+)
;;    (("b" . "Magit branch") . magit-branch)
;;    (("m" . "Magit submodule add") . magit-submodule-add+)
;;    (("d" . "Magit submodule remove") . magit-submodule-remove+)
;;    (("M" . "Magit submodule list") . magit-list-submodules)
;;    (("D" . "Magit discarded") . magit-discard)
;;    (("," . "Magit init") . magit-init)
;;    (("." . "Magit add remote") . magit-remote-add)
;;    )
;;  t)

(defun magit-submodule-add+ (url)
  (interactive "sURL: ")
  (let ((parent-dir (cadr (split-string (file-name-as-directory lazycat-emacs-extension-dir) (expand-file-name (cdr (project-current)))))))
    (magit-submodule-add
     url
     (concat parent-dir (file-name-base url))
     (file-name-base url))))

(defun magit-submodule-remove+ ()
  (interactive)
  (magit-submodule-remove (list (magit-read-module-path "Remove module")) "--force" nil))

(defun magit-status+ ()
  (interactive)
  (magit-status)
  (other-window 1))

(defun magit-blame+ ()
  (interactive)
  (setq magit-blame--style
        '(margin
          (margin-format " %s%f" " %C %a" " %H")
          (margin-width . 42)
          (margin-face . magit-blame-margin)
          (margin-body-face magit-blame-dimmed)))
  (magit-blame))

(defun magit-delete-remote-branch ()
  (interactive)
  (when (y-or-n-p (format "Delete remote branch (%s): " (magit-get-current-branch)))
    (magit-run-git-async "push" "origin" (format ":%s" (magit-get-current-branch)))))

;; blamer, show commit message like vscode
;; (global-blamer-mode 1) ; 影响操作性能，有需要使用 s-i 查看
(setq blamer-idle-time 0.3
      blamer-min-offset 70
      blamer-author-formatter " ✎ %s "
      blamer-datetime-formatter "[%s]"
      blamer-commit-formatter " ● %s"
      )

(provide 'init-git)

;;; init-git.el ends here
