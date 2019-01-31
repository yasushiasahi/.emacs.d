(use-package lsp-mode
  ;after (web-mode scss-mode)
  :hook ((web-vue-mode scss-mode js2-mode web-mode) . lsp)
  :init
  (defcustom emmet.showExpandedAbbreviation
    "always"
    "Controls the Emmet suggestions that show up in the suggestion/completion list."
    :group 'lsp-vue
    :type  '(radio
             (const "never" :tag  "Never show Emmet abbreviations in the suggestion list")
             (const "inMarkupAndStylesheetFilesOnly" :tag "Emmet abbreviations in the suggestion list for languages that are markup and stylesheet based ('html','pug','slim','haml','xml','xsl','css','scss','sass','less','stylus'")
             (const "always" "Emmet abbreviations in the suggestion list in languages that are markup and stylesheet based as well as javascriptreact, typescriptreact and any other language that has been mapped in the new setting emmet.includeLanguages.")
             )
    )

  (defcustom emmet.useNewEmmet
    nil
    "Use new emmet model"
    :group 'lsp-vue
    :type  'boolean
    )

  (defcustom emmet.includeLanguages
    '((vue . "html"))
    "emmet - include languages"
    :group 'lsp-vue
    :type 'sexp
    )

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
