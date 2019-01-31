;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; @web-vue-mode extended from web-mode
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-vue-mode))

(global-set-key (kbd "C-c m v") '(lambda ()
				   (interactive)
				   (web-vue-mode)))

(add-hook 'web-vue-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-css))))

;; (add-hook 'wev-mode-hook '(lambda ()
;; 			    (set (make-local-variable 'company-backends) '(company-lsp company-css))))


;;   (set (make-local-variable 'company-backends) '((company-tide
;;   						  company-yasnippet
;; 						  company-files
;;   						  company-dabbrev
;;   						  company-keywords
;;   						  company-capf
;;   						  )
;;   						 (company-abbrev company-dabbrev)
;;   						 ))
