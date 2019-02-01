(use-package lsp-mode
  ;after (web-mode scss-mode)
  :hook ((web-vue-mode scss-mode js2-mode web-mode) . lsp)
  :config
  (require 'lsp-clients)
  (lsp-register-client                                                                              ; web-vue-modeを-vue-language-serverに紐付け
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-vue--ls-command)
  		    :major-modes '(web-vue-mode)
  		    :priority -1
  		    :ignore-messages '("readFile .*? requested by Vue but content not available")
  		    :server-id 'vls))
  (setq lsp-prefer-flymake nil)                                                                     ; flymakeではなくflycheckを使う
  )


(use-package lsp-ui
  :config
  (require 'lsp-ui-flycheck)
  (lsp-ui-flycheck-enable t)
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point)

  )
