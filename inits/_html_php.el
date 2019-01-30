;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; hydra for html php
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defhydra hydra-markup-mode-map (:color blue
				     :hint nil)
  "
^web-mode commands^
^-^--------------------------^-^------------------------^-^-------------------------
_eb_: element-beginning        _be_: block-end              _ab_: attribute-beginning
_ee_: element-end              _bn_: block-next             _ap_: attribute-previous
_en_: element-next             _bk_: block-kill             _at_: attribute-transpose
_ep_: element-previous         _bb_: block-beginning        _ae_: attribute-end
_ec_: element-child            _bc_: block-close            _ai_: attribute-insert
_el_: element-clone            _bs_: block-select           _as_: attribute-select
_eo_: element-close            _bp_: block-previous         _an_: attribute-next
_ew_: element-wrap                                          _ak_: attribute-kill
_es_: element-select           _tn_: tag-next               _cu_: comment-or-uncomment
_er_: element-rename           _tp_: tag-previous           _ci_: comment-indent-new-line
_ep_: element-parent           _tb_: tag-beginning          _ct_: toggle-comments
_ev_: element-vanish           _te_: tag-end
_ei_: element-insert           _tm_: tag-match              _dn_: dom-normalize
_ek_: element-kill             _ta_: tag-attributes-sort    _dq_: dom-quotes-replace
_et_: element-transpose        _ts_: tag-select             _da_: dom-apostrophes-replace
_em_: element-mute-blanks                                   _de_: dom-entities-replace
_cs_: element-content-select                                _dx_: dom-xpath
_ef_: element-children-fold-or-unfold                       _dt_: dom-traverse
"
  ("ee" web-mode-element-end)
  ("ek" web-mode-element-kill)
  ("en" web-mode-element-next)
  ("ec" web-mode-element-child)
  ("el" web-mode-element-clone)
  ("eo" web-mode-element-close)
  ("ew" web-mode-element-wrap)
  ("es" web-mode-element-select)
  ("er" web-mode-element-rename)
  ("ep" web-mode-element-parent)
  ("ev" web-mode-element-vanish)
  ("ei" web-mode-element-insert)
  ("ep" web-mode-element-previous)
  ("et" web-mode-element-transpose)
  ("eb" web-mode-element-beginning)
  ("em" web-mode-element-mute-blanks)
  ("cs" web-mode-element-content-select)
  ("ef" web-mode-element-children-fold-or-unfold)
  ("be" web-mode-block-end)
  ("bn" web-mode-block-next)
  ("bk" web-mode-block-kill)
  ("bb" web-mode-block-beginning)
  ("bc" web-mode-block-close)
  ("bs" web-mode-block-select)
  ("bp" web-mode-block-previous)
  ("tn" web-mode-tag-next)
  ("tp" web-mode-tag-previous)
  ("tb" web-mode-tag-beginning)
  ("te" web-mode-tag-end)
  ("tm" web-mode-tag-match)
  ("ta" web-mode-tag-attributes-sort)
  ("ts" web-mode-tag-select)
  ("ab" web-mode-attribute-beginning)
  ("ap" web-mode-attribute-previous)
  ("at" web-mode-attribute-transpose)
  ("ae" web-mode-attribute-end)
  ("ai" web-mode-attribute-insert)
  ("as" web-mode-attribute-select)
  ("an" web-mode-attribute-next)
  ("ak" web-mode-attribute-kill)
  ("cu" web-mode-comment-or-uncomment)
  ("ci" web-mode-comment-indent-new-line)
  ("ct" web-mode-toggle-comments)
  ("dn" web-mode-dom-normalize)
  ("dq" web-mode-dom-quotes-replace)
  ("da" web-mode-dom-apostrophes-replace)
  ("de" web-mode-dom-entities-replace)
  ("dx" web-mode-dom-xpath)
  ("dt" web-mode-dom-traverse)
  )


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; webmode for html php
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'")
  :bind (:map web-mode-map
	      (lambda ()
		(when (string-equal "html" (file-name-extension buffer-file-name))
		  ("C-q". 'hydra-markup-mode-map/body))
		(when (string-equal "php" (file-name-extension buffer-file-name))
		  ("C-q". 'hydra-markup-mode-map/body))))
  :config
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
   '(web-mode-css-string-face        ((t (:foreground "#D78181"))))))
