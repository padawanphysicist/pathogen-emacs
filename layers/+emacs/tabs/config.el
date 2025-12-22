(use-package nerd-icons
  :ensure t)

;; Add tabs
;; https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :after nerd-icons
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 35)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t t" . centaur-tabs-mode))
  :config
  (centaur-tabs-mode t))

(elpaca-wait)
