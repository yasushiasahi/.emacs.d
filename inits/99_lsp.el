(use-package lsp-mode
  ;after (web-mode scss-mode)
  :hook ((web-vue-mode scss-mode js2-mode web-mode typescript-mode web-tsx-mode web-jsx-mode) . lsp)
  :config
  (require 'lsp-clients)
  (lsp-register-client                                                                              ; web-vue-modeを-vue-language-serverに紐付け
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-vue--ls-command)
  		    :major-modes '(web-vue-mode)
  		    :priority -1
  		    :ignore-messages '("readFile .*? requested by Vue but content not available")
  		    :server-id 'vls))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     (-const `(,lsp-clients-javascript-typescript-server
                                               ,@lsp-clients-typescript-javascript-server-args)))
                    :major-modes '(web-tsx-mode web-jsx-mode typescript-mode)
                    :priority -3
                    :ignore-messages '("readFile .*? requested by TypeScript but content not available")
                    :server-id 'jsts-ls))

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
