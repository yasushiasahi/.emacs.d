;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; CSS/SCSS
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun setup-css-mode()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2))

(use-package scss-mode
  :mode ("\\.scss\\'")
  )

(add-hook 'css-mode-hook 'setup-css-mode)
(add-hook 'scss-mode-hook 'setup-css-mode)


(defun change-scss-mode ()
  (interactive)
  (scss-mode))
(global-set-key (kbd "C-c m s") 'change-scss-mode)
