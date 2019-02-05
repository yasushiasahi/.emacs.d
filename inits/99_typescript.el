(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq indent-tabs-mode nil) ;インデントはタブではなくスペース
  (setq typescript-indent-level 2) ;スペースは２つ、デフォルトは4
  )

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-tsx-mode))
