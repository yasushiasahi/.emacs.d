;; (use-package go-mode
;;   :mode ("\\.go\\'")
;;   ;n:init
;;   :config
;;   ;(setq c-basic-offset 4)
;;   (setq tab-width 4)
;;   ;; (add-hook 'before-save-hook 'lsp-format-buffer)
;;   ;; (add-hook 'go-mode-hook #'lsp)
;;   )

(use-package go-mode)

(defun setup-go-mode ()
  "Hooks for Go mode."
  (setq tab-width 4)
  (lsp)
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (add-hook 'go-mode-hook #'lsp)
  )

(add-hook 'go-mode-hook 'setup-go-mode)
