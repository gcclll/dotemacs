;;; init-hydra.el --
;;; keybindings
(require 'hydra)

;; {{ everything
(defhydra gcl-everything (:color blue :columns 3 :hint nil)
  "🗯 做任何你想不到的事情~~~~ 👁👁👁👁👁👁👁👁👁
🌻"
  ("e" maple/iedit/body "Simple Iedit")
  )
;;

;; {{ window managemnt
;; @see https://github.com/abo-abo/hydra/wiki/Window-Management

;; helpers from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defun hydra-move-split-left (arg)
  "Move window split left."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-split-right (arg)
  "Move window split right."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-split-up (arg)
  "Move window split up."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-split-down (arg)
  "Move window split down."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra my-hydra-window ()
  "
Movement^^   ^Split^         ^Switch^     ^Resize^       ^Bufmove
-------------------------------------------------------------------
_h_ Left     _v_ertical      _b_uffer     _q_ X left     _L_ Right
_j_ Down     _x_ horizontal  _f_ind files _w_ X Down     _H_ Left
_k_ Top      _z_ undo        _a_ce 1      _e_ X Top      _J_ Down
_l_ Right    _Z_ reset       _s_wap       _r_ X Right    _K_ Up
_F_ollow     _D_elete Other  _S_ave       max_i_mize
_SPC_ cancel _o_nly this     _d_elete     _=_ Balance
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right)
  ("L" buf-move-right)
  ("H" buf-move-left)
  ("J" buf-move-down)
  ("K" buf-move-up)
  ("=" balance-windows)
  ("q" hydra-move-split-left)
  ("w" hydra-move-split-down)
  ("e" hydra-move-split-up)
  ("r" hydra-move-split-right)
  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'my-hydra-window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'my-hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'my-hydra-window/body)))
  ("o" delete-other-windows)
  ("i" ace-delete-other-windows)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))
;; }}

;; {{ git-gutter, @see https://github.com/abo-abo/hydra/wiki/Git-gutter
(defhydra my-hydra-git (:body-pre
                     (progn
                       (git-gutter-mode 1)
                       (setq git-link-use-commit t))
                     :after-exit (setq git-link-use-commit nil)
                     :color blue)
"
Git:
[_dd_] Diff               [_ri_] Rebase closest             [_vs_] Show commit message
[_dc_] Diff staged        [_s_] Show commit                 [_vb_] Blamer Commit message
[_dr_] Diff range         [_rr_] Reset gutter               [_ob_] Blamer mode
[_au_] Add modified       [_rh_] Gutter => HEAD
[_cc_] Commit             [_l_] Log selected/file
[_ca_] Amend              [_b_] Branches
[_ja_] Amend silent       [_k_] Git commit link
[_tt_] Stash              [_Q_] Quit gutter
[_ta_] Apply stash        [_cr_] Cherry pick from reflog
[_f_] Find file in commit

"
  ("ri" my-git-rebase-interactive)
  ("rr" git-gutter-reset-to-default)
  ("rh" my-git-gutter-reset-to-head-parent)
  ("s" my-git-show-commit)
  ("l" magit-log-buffer-file)
  ("b" magit-show-refs-popup)
  ("k" git-link)
  ("g" magit-status)
  ("ta" magit-stash-apply)
  ("tt" magit-stash)
  ("dd" magit-diff-dwim)
  ("dc" magit-diff-staged)
  ("dr" (magit-diff-range (my-git-commit-id)))
  ("cc" magit-commit-create)
  ("ca" magit-commit-amend)
  ("ja" (magit-commit-amend '("--reuse-message=HEAD" "--no-verify")))
  ("au" magit-stage-modified)
  ("Q" git-gutter-toggle)
  ("f" my-git-find-file-in-commit)
  ("cr" my-git-cherry-pick-from-reflog)
  ("vs" vc-msg-show)
  ("vb" blamer-show-posframe-commit-info)
  ("ob" blamer-mode)
  ("q" nil))
;; }}

;; {{ search
(defhydra my-hydra-search ()
  "
 ^Search^         ^Dictionary^
-----------------------------------------
_g_ Google        _b_ English => English
_/_ Github        _t_ English => Chinese
_s_ StackOverflow _y_ popweb 翻译
_m_ Man
"
  ("b" sdcv-search-input)
  ("t" sdcv-search-input+)
  ("y" popweb-dict-bing-pointer)
  ("/" engine/search-google)
  ("g" engine/search-github)
  ("s" enine/search-stack-overflow)
  ("m" woman)
  ("q" nil))
;; }}

;; {{ file ##
(defhydra my-hydra-file (:columns 4)
  "
文件操作
-------------------------------------------------------------------
"
  ("d" delete-this-file "Delete")
  ("r" rename-this-file-and-buffer "Rename")
  ("c" cp-filename-of-current-buffer "Filename")
  ("p" cp-fullpath-of-current-buffer "Full Path")
  ("f" osx-lib-find-file-in-finder "Open in finder"))
;; }}

;; {{ 各种跳转 ##
(defhydra my-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
;; }}

(message "> init-hydra.el")
(provide 'init-hydra)
;;; init-hydra.el ends here
