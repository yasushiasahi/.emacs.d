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

;; フォント
(set-face-attribute 'default nil
                   :family "Ricty Diminished"
                   :height 140)

(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty Diminished"))

;; スタートアップメッセージを非表示
(setq inhibit-startup-screen t)

;; 入力補完
(electric-pair-mode t) ; 閉じ括弧自動挿入

;; 画面からはみ出た文字を折り返さいない C-l で変更可能
(setq-default truncate-lines t)

(when (eq window-system 'ns)
  (tool-bar-mode 0) ;toolbarを非表示
  (scroll-bar-mode 0) ;scrollbarを非表示
  )

(when (eq window-system 'nil)
  ;; コピペの設定darwin専用
  (defun copy-from-osx ()
   (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
   (let ((process-connection-type nil))
       (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
         (process-send-string proc text)
         (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )
