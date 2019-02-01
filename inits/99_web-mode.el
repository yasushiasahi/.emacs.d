(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'" "\\.mustache\\'" "\\.eex\\'")
  :init
  (define-derived-mode web-vue-mode web-mode "WebVUE"
    "Major mode for editing Web & VUE templates.\\{web-vue-map}")

  :config
  (setq web-mode-attr-indent-offset nil)              ; 属性ごとのインデントを浅く
  (setq web-mode-markup-indent-offset 2)              ; htmlのインデント幅
  (setq web-mode-css-indent-offset 2)                 ; cssのインデント幅
  (setq web-mode-code-indent-offset 2)                ; js、php、rubyとかのインデント幅
  (setq web-mode-sql-indent-offset 2)                 ; sqlのインデント幅
  (setq indent-tabs-mode nil)                         ; インデントをタブではなくスペースに
  (setq tab-width 2)                                  ; タブの幅をスペース2つに
  (setq web-mode-script-padding 0)                    ; <script>タグ直下のインデント幅
  (setq web-mode-style-padding 0)                     ; <style>タグ直下のインデント幅
  (setq web-mode-block-padding 0)                     ; php、erbとかコードブロックのインデント幅

  (setq web-mode-enable-current-element-highlight t)  ; カーソル位置にある要素に対応するタグをハイライト
  (setq web-mode-enable-current-column-highlight t)   ; カーソル位置にある要素に対応するタグまで縦線を表示
  (setq web-mode-enable-auto-closing t)               ; <タグ名>の直後に</と自動で</タグ名>を挿入してカーソル位置は><の間に移動
  (setq web-mode-enable-auto-expanding t)             ; d/ => <div></div> c/ <div class=""></div> みたいな感じになる
  ;; (setq web-mode-enable-auto-quoting t)            ; <~>の中で=を打つと直後に""を挿入する。でも、スニペットと競合する
  (setq web-mode-comment-style 2)                     ; pnpとかのコメントのスタイルがいい感じに

  (push '("javascript" . "//") web-mode-comment-formats)

  (custom-set-faces
   '(web-mode-html-tag-face ((t (:foreground "blue")))))

  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode '("\\.html\\'" . prettier-js-mode))
                               (enable-minor-mode '("\\.php\\'" . prettier-js-mode))))
  )
