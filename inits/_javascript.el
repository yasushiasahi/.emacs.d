;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; hydra for js ts jsx tsx vue
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defhydra hydra-js-mode-map (:color blue
				    :hint nil)
  "
^tide commands^
^-^----------------^-^---------------------^-^-----------------------------
_rs_: restart-server                 Restart tsserver. This would come in handy after you edit tsconfig.
_dp_: documentation-at-point         Show documentation for the symbol at point.
_rf_: references                     List all references to the symbol at point in a buffer.
_pe_: project-errors                 List all errors in the project. Errors can be navigated using n and p.
_rs_: rename-symbol                  Rename all occurrences of the symbol at point.
_rf_: rename-file                    Rename current file and all it's references in other files.
_fo_: format                         Format the current region or buffer.
_fi_: fix                            Apply code fix for the error at point. When invoked with a prefix arg,
_td_: add-tslint-disable-next-line   If the point is on one or more tslint
_re_: refactor                       Refactor code at point or current region.
_jt_: jsdoc-template                 Insert JSDoc comment template at point.
_vs_: verify-setup                   Show the version of tsserver.
_oi_: organize-imports               Organize imports in the file.
_jd_: jump-to-definition
_jb_: jump-back
"
  ("rs" tide-restart-server)
  ("dp" tide-documentation-at-point)
  ("rf" tide-references)
  ("pe" tide-project-errors)
  ("rs" tide-rename-symbol)
  ("rf" tide-rename-file)
  ("fo" tide-format)
  ("fi" tide-fix)
  ("td" tide-add-tslint-disable-next-line)
  ("re" tide-refactor)
  ("jt" tide-jsdoc-template)
  ("vs" tide-verify-setup)
  ("oi" tide-organize-imports)
  ("jd" tide-jump-to-definition)
  ("jb" tide-jump-back))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; tide-mode for js ts jsx tsx vue
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package tide
  :hook ((js2-mode typescript-mode web-mode) . tide-mode)
  :config
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;(company-mode +1)
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; js2-mode for js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :bind (:map js2-mode-map ("C-q". 'hydra-js-mode-map/body))
  :config
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil)
  (setq-default js2-global-externs '("module" "require" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "__dirname" "console" "JSON" "location" "fetch")) ;指定した文字列の警告をオフ
					;(setq js2-mode-show-parse-errors nil)
					;(setq js2-mode-show-strict-warnings nil)
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; typescript-mode for ts
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package typescript-mode
  :mode "\\.ts\\'"
  :interpreter "typescript"
  :bind (:map typescript-mode-map ("C-q". 'hydra-js-mode-map/body))
  :config
  (setq indent-tabs-mode nil)
  (setq typescript-indent-level 2))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; webmode for jsx tsx vue
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package web-mode
  :mode ("\\.jsx\\'" "\\.tsx\\'" "\\.vue\\'" )
					;:bind (:map web-mode-map ("C-q". 'hydra-js-mode-map/body))
  :bind (:map web-mode-map
	      (lambda ()
		(when (string-equal "jsx" (file-name-extension buffer-file-name))
		  ("C-q". 'hydra-js-mode-map/body))
		(when (string-equal "tsx" (file-name-extension buffer-file-name))
		  ("C-q". 'hydra-js-mode-map/body))
		(when (string-equal "vue" (file-name-extension buffer-file-name))
		  ("C-q". 'hydra-js-mode-map/body))))
  :config
  (setq web-mode-content-types-alist'(("jsx" . "\\.(jsx|tsx|vue)\\'")))
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)

  (setq web-mode-block-padding 0)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-closing 2)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-auto-quoting t)

  (custom-set-faces
   '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
   '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
   '(web-mode-html-tag-bracket-face  ((t (:foreground "#4A8ACA"))))
   '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
   '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
   '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
   '(web-mode-comment-face           ((t (:foreground "#587F35"))))
   '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))
   '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
   '(web-mode-comment-face           ((t (:foreground "#587F35"))))
   '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
   '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
   '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
   '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
   )
  )



;; tide-mode
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;;(add-hook 'before-save-hook 'tide-format-before-save) ;; formats the buffer before saving
;;   (define-key tide-mode-map (kbd "C-q") 'hydra-js-mode-map/body)
;;   (company-mode +1)
;;   (prettier-js-mode)
;;   (set (make-local-variable 'company-backends) '((company-tide
;;   						  company-yasnippet
;; 						  company-files
;;   						  company-dabbrev
;;   						  company-keywords
;;   						  company-capf
;;   						  )
;;   						 (company-abbrev company-dabbrev)
;;   						 ))
;;   )

;; (defun change-js2-mode ()
;;   (interactive)
;;   (js2-mode))
;; (global-set-key (kbd "C-c m j") 'change-js2-mode)


;; ;; typescript-mode
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; (defun setup-typescript-mode ()
;;   (setq indent-tabs-mode nil) ;インデントはタブではなくスペース
;;   (setq typescript-indent-level 2) ;スペースは２つ、デフォルトは4
;;   )
;; (add-hook 'typescript-mode-hook 'setup-typescript-mode )
