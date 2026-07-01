;;; Adiciona linhas guia nos buffers
;;
;; - https://github.com/jdtsmith/indent-bars
(use-package indent-bars
  :ensure t
  :hook
  ((python-mode yaml-mode) . indent-bars-mode))

;; Programação em Haskell (Basicamente o XMonad no meu caso)
(use-package haskell-mode
  :ensure t)

;; Programação em Lua
(use-package lua-mode
  :ensure t)

(use-package geiser-guile
  :ensure t
  :config
  (with-eval-after-load
    'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/.guix-profile/share/guile/site/3.0"))

(with-eval-after-load
    'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/site/3.0"))
  )

(require 'pathogen-emacs-lisp)

(provide 'pathogen-programming)
