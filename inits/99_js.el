;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; js2-mode for js
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  (setq js2-strict-missing-semi-warning nil)
  )

(use-package add-node-modules-path
  :config
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'web-vue-mode-hook #'add-node-modules-path)
  (add-hook 'js2-mode-hook #'prettier-js-mode)
  (add-hook 'web-vue-mode-hook #'prettier-js-mode)
  )



(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (setq company-tooltip-align-annotations t)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-hook 'web-vue-mode-hook #'setup-tide-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  )


;; (use-package tide
;;   :after (typescript-mode js-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;; 	 (js2-mode . tide-setup)
;;          (js2-mode . tide-hl-identifier-mode)

;;          ))
