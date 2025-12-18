(use-package emacs
  :ensure nil
  :custom
  (auto-revert-use-notify nil)
  :config
  (global-auto-revert-mode t)
  (global-hl-line-mode t))

;; https://github.com/jdtsmith/ultra-scroll
(use-package ultra-scroll)

