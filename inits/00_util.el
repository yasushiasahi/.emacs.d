;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 一般設定
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t) 

;; yes/noはすべてy/nで答える
(defalias 'yes-or-no-p 'y-or-n-p)

;; 一行ずつスクロール
(setq scroll-conservatively 1)

;; 保存前にバッファ残体の行末の空行を削除する
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 文字コード
(set-language-environment "Japanese") ; 日本語推奨環境
(prefer-coding-system 'utf-8) ; utf-8が最優先
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; メニューバー非表示
(menu-bar-mode 0) 

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t) 

;; 入力補完
(electric-pair-mode t) ; 閉じ括弧自動挿入

;; 画面からはみ出た文字を折り返さいない C-l で変更可能
(setq-default truncate-lines t)