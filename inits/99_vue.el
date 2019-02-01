;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @web-vue-mode extended from web-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-vue-mode))

(global-set-key (kbd "C-c m v") '(lambda ()
				   (interactive)
				   (web-vue-mode)))
