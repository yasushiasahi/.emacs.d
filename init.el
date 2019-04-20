;;; init.el ---  -*- coding: utf-8 ; lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; straightとuse-packageでコードを管理
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


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
                   :height 130)

(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty Diminished"))

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



;;; ++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 環境変数
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))




;;; ++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;  オートセーブ、バックアップ
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/")) ; バックアップファイルはbackups/へ保存
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/backups/") t))) ; オートセーブファイルもbackups/へ保存

;; 更新されたファイルを自動で読み直す
(global-auto-revert-mode t)
(setq create-lockfiles nil)




;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; 見た目
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  (setq solarized-high-contrast-mode-line t) ; make the modeline high contrast
  (setq x-underline-at-descent-line t)
  (setq solarized-emphasize-indicators nil) ; Use less colors for indicators such as git:gutter, flycheck and similar
  )

;; 左端に行番号を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")

;; モードライン
(column-number-mode t) ; カラム番号を表示
(size-indication-mode t) ; ファイスサイズを表示

;; 現在行のハイライト
(global-hl-line-mode t)

;; 対応する括弧の強調表示
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
(setq show-paren-style 'mixed) ; カッコのみをハイライト

;; カーソルの色
(set-cursor-color "#ff00ff")


(when (eq window-system 'nil)
  ;; カラーテーマをsolarizedにする
  (use-package color-theme-solarized
    :config
    (set-terminal-parameter nil 'background-mode 'dark)
    (load-theme 'solarized t))

  (set-face-foreground 'mode-line "cyan") ; カレントバッファはシアン
  (set-face-foreground 'mode-line-inactive "green") ; カレント以外のバッファは緑

  ;;リージョンの色
  (set-face-foreground 'region "white") ; 背景
  (set-face-background 'region "brightgreen") ; 文字色

  (set-face-foreground 'show-paren-match "brightblack") ; 背景
  (set-face-background 'show-paren-match "green") ; 文字色

  (set-face-background 'linum "brightblack")
  (set-face-foreground 'linum "brightgreen")
  )




;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; このファイル内で繰り返し使う関数定義
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))





;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @hydra
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package hydra)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @goto-chg
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package goto-chg
  :bind (("M-[" . hydra-goto-chg/goto-last-change)
	 ("M-]" . hydra-goto-chg/goto-last-change-reverse))
  :config
  (defhydra hydra-goto-chg ()
    ("[" goto-last-change)
    ("]" goto-last-change-reverse))
  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @expand-region
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package expand-region
  :bind (("C-o" . er/expand-region))
  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @helm
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package helm
  :init
  (require 'helm-config)
  :bind (("M-y". 'helm-show-kill-ring)
	 ("M-x". 'helm-M-x)
	 ("C-x C-b". 'helm-for-files)
	 ("C-x C-i". 'helm-mini))
  :config
  (custom-set-variables
   '(helm-truncate-lines t)
   '(helm-delete-minibuffer-contents-from-point t)
   '(helm-mini-default-sources '(helm-source-buffers-list
				 helm-source-recentf
				 helm-source-projectile-projects
				 helm-source-ghq
				 helm-source-files-in-current-dir)))
  )

;; (use-package helm-swoop
;;   :bind(("C-c s". 'helm-swoop)))
(use-package helm-swoop
  :straight (helm-swoop :type git :host github :repo "ashiklom/helm-swoop")
  :bind(("C-c s". 'helm-swoop))
  )



(use-package helm-ghq
  :bind(("C-x q". 'helm-ghq))
  )

(use-package wgrep)
(use-package helm-git-grep
  :bind (("C-c g g". 'helm-git-grep)
	 ("C-c g a". 'helm-git-grep-at-point))
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @projectile
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :config
  (setq helm-projectile-fuzzy-match t)
  (helm-projectile-on))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @avy
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package avy
  :bind(("M-g g". 'avy-goto-line))
  :config
  (setq avy-all-windows nil)
  )


;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @ace-isearch
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package ace-isearch
  :config
  (global-ace-isearch-mode +1))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @undo-tree
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package undo-tree
  :config
  (global-undo-tree-mode))




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @quickrun
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package quickrun
  :bind (("C-c q b" . quickrun)
	 ("C-c q r". quickrun-region)))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @open-junk-file
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package open-junk-file
  :bind ("C-c j" . open-junk-file)
  :config
  (setq open-junk-file-find-file-function 'find-file)
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @popwin
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package popwin
  :config
  (popwin-mode 1)
  (push '("*Google Translate*" :height 0.4)  popwin:special-display-config)
  (push '("*quickrun*" :height 0.3) popwin:special-display-config)
  ;(push '("godoc" :regexp t :height 0.4 :position top) popwin:special-display-config)
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @google-translate
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; https://solist.work/blog/posts/google-translate/
(use-package google-translate
  :bind ("C-c t" . google-translate-auto)
  :config
  (require 'google-translate-default-ui)
  (defvar toggle-translate-flg nil
    "Toggle flg.")

  (defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))

  (defun toggle-translate ()
    "Toggle translate function."
    (interactive)
    (if toggle-translate-flg
	(progn
	  (bind-key "C-c t" 'google-translate-auto)
	  (setq toggle-translate-flg nil))
      (progn
	(bind-key "C-c t" 'chromium-translate)
	(setq toggle-translate-flg t))))


  (defun google-translate-auto ()
    "Automatically recognize and translate Japanese and English."
    (interactive)
    (if (region-active-p)
	(progn (setq mark-active nil)
	       (if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
				 (buffer-substring (region-beginning) (region-end)))
		   (google-translate-translate
		    "en" "ja"
		    (buffer-substring
		     (region-beginning) (region-end)))
	         (google-translate-translate
		  "ja" "en"
		  (buffer-substring
		   (region-beginning) (region-end)))))
      (let ((string (read-string "Google Translate: ")))
	(if (string-match
	     (format "\\`[%s]+\\'" "[:ascii:]")
	     string)
	    (google-translate-translate
	     "en" "ja"
	     string)
	  (google-translate-translate
	   "ja" "en"
	   string)))))
  )




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @restclient
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package restclient)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; magit
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package magit)

















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
	 (split-window-horizontally-n 4)))
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
  :bind ("C-q" . hydra-multiple-cursors/body)
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




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @company
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (global-company-mode) ; 全バッファで有効にする
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)) ;; 使用履歴＆ソート順
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq completion-ignore-case nil) ; 大文字、小文字を区別しない Emacs自体の設定
  (setq company-dabbrev-downcase nil) ; lower caseで補完で保管されるのを防ぐ
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fでも候補を設定
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  )

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package company-go
  :after company
  (push 'company-go company-backends))



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @prettier-js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package prettier-js)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @prettier-js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package rainbow-delimiters)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @yasnippet
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package yasnippet
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @flycheck
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package flycheck)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; add-node-modules-path
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package add-node-modules-path  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; tide(tsserver language server) for js or typescript
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package tide
  :config
  (setq company-tooltip-align-annotations t)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; lsp
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (use-package lsp-mode :commands lsp)
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; webmode for html php JSX TSX VUE
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'" "\\.mustache\\'" "\\.eex\\'")
  :init
  (define-derived-mode web-vue-mode web-mode "WebVUE")
  (define-derived-mode web-jsx-mode web-mode "WebJSX"
    (setq web-mode-content-type "jsx"))

  :config
  (setq web-mode-attr-indent-offset nil)              ; 属性ごとのインデントを浅く
  (setq web-mode-markup-indent-offset 2)              ; htmlのインデント幅
  (setq web-mode-css-indent-offset 2)                 ; cssのインデント幅
  (setq web-mode-code-indent-offset 2)                ; js、php、rubyとかのインデント幅
  (setq web-mode-sql-indent-offset 2)                 ; sqlのインデント幅
  (setq indent-tabs-mode nil)                         ; インデントをタブではなくスペースに
  (setq tab-width 2)                                  ; タブの幅をスペース2つに
  (setq web-mode-script-padding 0)                    ; <script>タグ直下のインデント幅
  (setq web-mode-style-padding 0)                     ; <style>タグ直下のインデント幅
  (setq web-mode-block-padding 0)                     ; php、erbとかコードブロックのインデント幅
  (setq web-mode-enable-current-element-highlight t)  ; カーソル位置にある要素に対応するタグをハイライト
  (setq web-mode-enable-current-column-highlight t)   ; カーソル位置にある要素に対応するタグまで縦線を表示
  (setq web-mode-enable-auto-closing t)               ; <タグ名>の直後に</と自動で</タグ名>を挿入してカーソル位置は><の間に移動
  (setq web-mode-enable-auto-expanding t)             ; d/ => <div></div> c/ <div class=""></div> みたいな感じになる
  (setq web-mode-comment-style 2)                     ; pnpとかのコメントのスタイルがいい感じに

  (push '("javascript" . "//") web-mode-comment-formats)

  (defun change-jsx-mode ()
    (interactive)
    (web-tsx-mode))

  (defun change-vue-mode ()
    (interactive)
    (web-vue-mode))

  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode '("\\.html\\'" . prettier-js-mode))
                               (enable-minor-mode '("\\.php\\'" . prettier-js-mode))
			       (yas-minor-mode)
			       (rainbow-delimiters-mode)
			       ))

  (add-hook 'web-vue-mode-hook #'(lambda ()
                               (add-node-modules-path)
			       (setup-tide-mode)
			       (prettier-js-mode)
			       (yas-minor-mode)
			       (rainbow-delimiters-mode)
			       ))


  (add-hook 'web-jsx-mode-hook #'(lambda ()
                               (add-node-modules-path)
			       (setup-tide-mode)
			       (prettier-js-mode)
			       (yas-minor-mode)
			       (rainbow-delimiters-mode)
			       ))

  ;; (add-hook 'web-vue-mode-hook #'add-node-modules-path)
  ;; (add-hook 'web-vue-mode-hook #'prettier-js-mode)
  ;; (add-hook 'web-vue-mode-hook #'setup-tide-mode)

  ;; (add-hook 'web-jsx-mode-hook #'add-node-modules-path)
  ;; (add-hook 'web-jsx-mode-hook #'prettier-js-mode)
  ;; (add-hook 'web-jsx-mode-hook #'setup-tide-mode)
  )

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-vue-mode))


(global-set-key (kbd "C-c m x") 'change-jsx-mode)
(global-set-key (kbd "C-c m v") 'change-vue-mode)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; CSS/SCSS
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package scss-mode
  :mode ("\\.scss\\'")
  :config
  (setq css-indent-offset 2)



  (add-hook 'scss-mode-hook #'(lambda ()
                               (add-node-modules-path)
			       (prettier-js-mode)
			       (yas-minor-mode)
			       (rainbow-delimiters-mode)
			       ))
  )


(defun change-scss-mode ()
    (interactive)
    (scss-mode))
(global-set-key (kbd "C-c m s") 'change-scss-mode)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; js2-mode for js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil)
  (add-hook 'js2-mode-hook #'(lambda ()
                               (add-node-modules-path)
			       (setup-tide-mode)
			       (prettier-js-mode)
			       (yas-minor-mode)
			       (rainbow-delimiters-mode)
			       ))
  )




;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; typescript-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2) ;スペースは２つ、デフォルトは4
  (add-hook 'typescript-mode-hook #'(lambda ()
                               (add-node-modules-path)
			       (setup-tide-mode)
			       (prettier-js-mode)
			       (yas-minor-mode)
			       (rainbow-delimiters-mode)
			       ))
  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; go-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package go-guru)
(use-package go-mode
  :config
  (defun setup-go-mode ()
    "Hooks for Go mode."
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (go-guru-hl-identifier-mode)
    (go-eldoc-setup)
    (setq company-go-show-annotation t)
    (setq company-begin-commands '(self-insert-command))
    (flycheck-mode)
    )
  (add-hook 'go-mode-hook 'setup-go-mode)
  )



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Elixir
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package elixir-mode)
(use-package alchemist)



;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; multi-term
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package multi-term
  :config
  (setq multi-term-program "/usr/local/bin/zsh")
  (add-to-list 'term-unbind-key-list '"M-x")
  (add-to-list 'term-unbind-key-list '"C-t")
  :bind (:map term-mode-map
	 ("C-p" . term-send-previous-line)
	 ("C-n". term-send-next-line)
	 ("C-b". term-send-backward-char)
	 ("C-f". term-send-forward-char)
	 ("C-h". term-send-backspace)
	 ("C-y". term-paste)
	 ("C-t 9". multi-term-next)
	 ("C-t 8". multi-term-prev)
	 ("C-t c". multi-term)
	 )
  )































(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
