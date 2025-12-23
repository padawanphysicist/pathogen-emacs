(use-package dirvish
  :ensure t
  :bind ("C-x d" . dirvish)
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))
