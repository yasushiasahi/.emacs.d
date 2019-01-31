;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @hydra
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package hydra)

;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @goto-chg
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package goto-chg
  :bind (("M-." . goto-last-change)
	 ("M-," . goto-last-change-reverse)))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @expand-region
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package expand-region
  :bind (("C-o" . er/expand-region))
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @prettier-js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package prettier-js
  :hook ((css-mode scss-mode js2-mode typescript-mode web-vue-mode) . prettier-js-mode)
  :config
  (setq prettier-js-args '(
			   "--no-semi" "false"
			   "--jsx-bracket-same-line" "true"))
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @helm
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package helm
  :bind (("M-y". 'helm-show-kill-ring)
	 ("C-x C-b". 'helm-for-files)
	 ("M-x". 'helm-M-x))
  )

(use-package helm-swoop
  :bind(("C-c s". 'helm-swoop)))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @yasnippet
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)


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
  (push 'company-yasnippet company-backends)
  (require 'company-css)
  )



(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  )

(use-package company-go
  :after company
  (push 'company-go company-backends))

(use-package helm-company
  :after company)


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @avy
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package avy
  :bind ("M-g g" . avy-goto-line)
  )


;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @ace-isearch
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package ace-isearch
  :after (helm-swoop avy)
  :config
  (global-ace-isearch-mode +1))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @undo-tree
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package undo-tree
  :config
  (global-undo-tree-mode))


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
  (helm-projectile-on))


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
  :bind ("C-c j" . open-junk-file))


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
;;; @flycheck
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package flycheck)
