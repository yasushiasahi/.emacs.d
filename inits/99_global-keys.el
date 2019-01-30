;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; キーバインド（一般）
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-unset-key (kbd "C-u")) ;C-uは一旦無効化
(global-unset-key (kbd "C-t")) ; デフォルトのC-tを無効化
(global-unset-key (kbd "C-q")) ; デフォルトのC-q(特殊文字入力)を無効化
(global-unset-key (kbd "C-\\")) ;C-\(日本語入力)を無効化
(global-set-key (kbd "C-m") 'newline-and-indent) ; 改行してインデント
(global-set-key (kbd "C-x ?") 'help-command) ; ヘルプコマンド
(global-set-key (kbd "C-^") 'universal-argument) ; defaultのC-u
(global-set-key (kbd "C-M-d") 'kill-word) ; 単語ごとに削除
(global-set-key (kbd "C-c l") 'toggle-truncate-lines) ; 折り返しをトグル
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>")) ; C-hでバックスペース


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; ウィンドウ操作
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun split-window-horizontally-n (num_wins)
  "任意の数だけ横に分割"
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))


(defhydra hydra-window ()
  ("b" windmove-left)
  ("n" windmove-down)
  ("p" windmove-up)
  ("f" windmove-right)
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("C-<left>" shrink-window-horizontally)
  ("C-<right>" enlarge-window-horizontally)
  ("C-<up>" shrink-window)
  ("C-<down>" enlarge-window)
  ("s" window-swap-states)
  ("-" split-window-below)
  ("\\" split-window-right)
  ("0" delete-window)
  ("1" delete-other-windows)
  ("3" (lambda ()
	 "3分割"
	 (interactive)
	 (split-window-horizontally-n 3)))
  ("4" (lambda ()
	 "4分割"
	 (interactive)
	 (split-window-horizontally)
	 (split-window-vertically)
	 (setq i 0)
	 (while (< i 1)
	   (windmove-right)
	   (split-window-vertically)
	   (setq i (+ 1 i)))))
  ("6" (lambda ()
	 "6分割"
	 (interactive)
	 (split-window-horizontally-n 3)
	 (split-window-vertically)
	 (setq i 0)
	 (while (< i 2)
	   (windmove-right)
	   (split-window-vertically)
	   (setq i (+ 1 i))))))

(global-set-key (kbd "C-t") 'hydra-window/body)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @curx
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package crux
	:bind (("C-u" . crux-smart-open-line-above) ;; 現在行を改行せずに上に空行を作ってその行に移動
		("C-j" . crux-smart-open-line) ;; 現在行を改行せずに下に空行を作ってその行に移動
		("M-k" . crux-kill-whole-line) ;; 現在行全体を削除して詰める
		("M-h" . crux-kill-line-backwards) ;; インデント位置からカーソル位置までを削除
		("C-a" . crux-move-beginning-of-line) ;; C-a連打で行頭→行の最初のインデント位置への移動を繰り返す
		("M-d" . crux-duplicate-current-line-or-region) ;; 現在行or選択行を下に複製
		("M-\\" . crux-duplicate-and-comment-current-line-or-region))) ;; 現在行or選択行を下に複製してコメントアウト


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @multiple-cursors
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package multiple-cursors
  :bind ("C-\\" . hydra-multiple-cursors/body)
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil))
  )
