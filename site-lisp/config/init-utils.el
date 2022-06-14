;;; init-utils.el --- utility functions
;;; 函数来自 https://github.com/redguardtoo/emacs.d

(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

(defun my-nonempty-lines (str)
  "Split STR into lines."
  (split-string str "[\r\n]+" t))

(defun my-lines-from-command-output (command)
  "Return lines of COMMAND output."
  (let* ((output (string-trim (shell-command-to-string command)))
         (cands (my-nonempty-lines output)))
    (delq nil (delete-dups cands))))

(defun my-git-root-dir ()
  "Git root directory."
  (locate-dominating-file default-directory ".git"))

(defun my-git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (unless level (setq level 0))
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string level ?^)))

(defun my-setup-extra-keymap (extra-fn-list hint fn &rest args)
  "Map EXTRA-FN-LIST to new keymap and show HINT after calling FN with ARGS."
  (let ((echo-keystrokes nil))
    (when fn (apply fn args))
    (message hint)
    (set-transient-map
     (let ((map (make-sparse-keymap))
           cmd)
       (dolist (item extra-fn-list)
         (setq cmd (nth 1 item))
         (setq cmd (cond
                    ((commandp cmd)
                     cmd)
                    (t
                     `(lambda ()
                        (interactive)
                        (if (functionp ,cmd) (funcall ,cmd) ,cmd)))))
         (define-key map (kbd (nth 0 item)) cmd))
       map)
     t)))

;; Handier way to add modes to auto-mode-alist
(defun my-add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (push (cons pattern mode) auto-mode-alist)))

(defun my-add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (push (cons pattern mode) interpreter-mode-alist )))

(defvar my-disable-idle-timer (daemonp)
  "Function passed to `my-run-with-idle-timer' is run immediately.")

(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun my-write-string-to-file (string file)
  "Write STRING to FILE."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file)))

(defun my-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* (win64-clip-program
         ssh-client)
    (my-write-string-to-file str-val "/tmp/clipboard")))
    
(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (my-pclip msg)
  msg)

(defun cp-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n (format "%s:%s" filename (line-number-at-pos)) filename)))
      (copy-yank-str s)
      (message "%s => clipboard&kill-ring" s))))

(defun cp-fullpath-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (copy-yank-str (file-truename buffer-file-name))
    (message "file full path => clipboard & yank ring")))

(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun my-selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun my-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond
   ((region-active-p)
    (my-selected-str))
   ((or (not hint) (string= "" hint))
    (thing-at-point 'symbol))
   (t
    (read-string hint))))

(defun my-lookup-doc-in-man ()
  "Read man by querying keyword at point."
  (interactive)
  (man (concat "-k " (my-use-selected-string-or-ask))))

(defun my-delete-selected-region ()
  "Delete selected region."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))))

(message "> init-utils.el")

(provide 'init-utils)
;;; init-utils.el ends here
