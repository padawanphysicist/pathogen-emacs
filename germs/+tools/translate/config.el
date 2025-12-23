;; https://github.com/lorniu/gt.el
(use-package gt
  :ensure t
  ;; Initialize the default translator, let it translate between en and pt-br via Google Translate,
  ;; and the result will be displayed in the Echo Area.
  :custom
  ((gt-langs '(en pt-br))
  (gt-default-translator (gt-translator :engines (gt-google-engine)))))
