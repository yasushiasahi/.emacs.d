;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 見た目
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; カラーテーマをsolarizedにする
(use-package color-theme-solarized
	:config
	(set-terminal-parameter nil 'background-mode 'dark)
	(load-theme 'solarized t))

;; 左端に行番号を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")
(set-face-background 'linum "brightblack")
(set-face-foreground 'linum "brightgreen")

;; モードライン
(column-number-mode t) ; カラム番号を表示
(size-indication-mode t) ; ファイスサイズを表示
(set-face-foreground 'mode-line "cyan") ; カレントバッファはシアン
(set-face-foreground 'mode-line-inactive "green") ; カレント以外のバッファは緑

;; 対応する括弧の強調表示
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
(setq show-paren-style 'parenthesis) ; カッコのみをハイライト
(set-face-foreground 'show-paren-match "brightblack") ; 背景
(set-face-background 'show-paren-match "green") ; 文字色

;; リージョンの色
(set-face-foreground 'region "white") ; 背景
(set-face-background 'region "brightgreen") ; 文字色

