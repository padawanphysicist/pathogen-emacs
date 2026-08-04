;;; Code

;;; Markup

(use-package org
  ;; Forces Elpaca to install Org before continuing
  :ensure (:wait t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🔗 [[https://jblevins.org/projects/markdown-mode/][markdown-mode]]
;;
;; O modo principal para editar arquivos Markdown. Configurado para
;; usar GitHub Flavored Markdown (GFM) em arquivos README e suporte a
;; renderização externa.
;;
(use-package markdown-mode
  :ensure t
  ;; Usa o modo do GitHub para arquivos README
  :mode ("README\\.md\\'" . gfm-mode)
  :init 
  ;; Define o comando externo para exportação/preview (ex: multimarkdown ou pandoc)
  (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;; YAML
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package outline-yaml
  :ensure (:type git
           :host github
           :repo "jamescherti/outline-yaml.el")
  :hook
  ((yaml-mode . outline-yaml-minor-mode)
   (yaml-ts-mode . outline-yaml-minor-mode)))


;;;; TeX/LaTeX/ConTeXt
(use-package auctex :ensure t
  :config
  ;; Set .ctx files to ConTeXt mode
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . ConTeXt-mode))
  (add-to-list 'auto-mode-alist '("\\.mkiv\\'" . ConTeXt-mode))
  ;; Set default ConTeXt engine to ConTeXt (instead of texexec)
  (setq-default TeX-command-default "ConTeXt")

  ;; Set ConTeXt engine version to Mark IV or LMTX (IV by default, change to VI if desired)
  (setq-default ConTeXt-Mark-version "IV"))

(use-package cdlatex
  :ensure t
  :after (latex-mode org-mode) ; Load after these modes
  :config
  (define-key cdlatex-mode-map (kbd "TAB") 'cdlatex-tab)
  ;; Add any other specific configurations here
  ;; For example, to enable CDLaTeX in Org mode:
  (add-hook 'org-mode-hook 'cdlatex-mode)
  ;; Or to enable it in LaTeX mode:
  (add-hook 'latex-mode-hook 'cdlatex-mode))


;;;; HTML/CSS
;; 4. CONFIGURAÇÃO DE CSS (NATIVO + RAINBOW MODE)
(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

;; Mostra as cores reais diretamente no código (ex: #ff0000 fica vermelho)
(use-package rainbow-mode
  :ensure t
  :hook (css-mode . rainbow-mode))

;; 5. CONFIGURAÇÃO DE HTML & TEMPLATES (WEB-MODE)
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.jsp\\'" "\\.asax\\'" "\\.erb\\'" "\\.mustache\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t))

(add-hook 'css-mode-hook
          (lambda ()
            ;; Define que qualquer linha começando com "/* " seguido de texto é um cabeçalho
            (setq-local outline-regexp "/\\* [^*]")
            ;; Ativa o outline-minor-mode automaticamente para CSS
            (outline-minor-mode 1)))

;;;; Folding

(use-package kirigami
  :ensure t
  :bind (:map kirigami-mode-map
         ("C-c z TAB"    . kirigami-toggle-fold)
         ("C-c z <left>"  . kirigami-close-fold)
         ("C-c z <right>" . kirigami-open-fold)
         ("C-c z <up>"    . kirigami-close-folds)
         ("C-c z <down>"  . kirigami-open-folds))
  :hook
  ((css-mode    . my/kirigami-css-setup)
   (scheme-mode . my/kirigami-scheme-setup)
   (latex-mode  . my/kirigami-latex-setup)
   (LaTeX-mode  . my/kirigami-latex-setup)
   (ConTeXt-mode . my/kirigami-latex-setup)
   (emacs-lisp-mode . my/kirigami-emacs-lisp-setup)
   (lua-mode . my/kirigami-lua-setup))
  :config
  (defun my/kirigami-common-setup ()
    "Configuração genérica para ativar o Kirigami e seus backends."
    (outline-minor-mode 1)
    (hs-minor-mode 1)
    (kirigami-mode 1))

  (defun my/kirigami-css-setup ()
    (setq-local outline-regexp "/\\* [^*]")
    (my/kirigami-common-setup))

  (defun my/kirigami-emacs-lisp-setup ()
    (setq-local outline-regexp "^;;;\\*+")
    (my/kirigami-common-setup))

  (defun my/kirigami-scheme-setup ()
    (setq-local outline-regexp ";;;\\*+\\|\\`")
    (my/kirigami-common-setup))

  (defun my/kirigami-latex-setup ()
    ;; 1. Define a regex do cabeçalho
    (setq-local outline-regexp "^%\\*+")
    
    ;; 2. Calcula o nível do cabeçalho para evitar o erro "Unrecognized header"
    (setq-local outline-level
                (lambda ()
                  (save-excursion
                    (looking-at outline-regexp)
                    (- (match-end 0) (match-beginning 0)))))
    
    ;; 3. Ativa o outline e o kirigami
    (outline-minor-mode 1)
    (kirigami-mode 1))

    (defun my/kirigami-lua-setup ()
    ;; 1. Define a regex do cabeçalho
    (setq-local outline-regexp "^--\\*+")
    
    ;; 2. Calcula o nível do cabeçalho para evitar o erro "Unrecognized header"
    (setq-local outline-level
                (lambda ()
                  (save-excursion
                    (looking-at outline-regexp)
                    (- (match-end 0) (match-beginning 0)))))
    
    ;; 3. Ativa o outline e o kirigami
    (outline-minor-mode 1)
    (kirigami-mode 1))

  ;; Configurações globais adicionais
  (remove-hook 'prog-mode-hook #'outline-minor-mode)
  (setq outline-minor-mode-cycle nil))
;; (use-package kirigami
;;   :ensure t
;;   :bind (:map kirigami-mode-map
;;          ("C-c z TAB"    . kirigami-toggle-fold)
;;          ("C-c z <left>"  . kirigami-close-fold)
;;          ("C-c z <right>" . kirigami-open-fold)
;;          ("C-c z <up>"    . kirigami-close-folds)
;;          ("C-c z <down>"  . kirigami-open-folds))
;;   :hook
;;   ((css-mode . my/kirigami-css-setup)
;;    (scheme-mode . my/kirigami-scheme-setup)
;;    (latex-mode . my/kirigami-latex-setup)
;;    (ConTeXt-mode . my/kirigami-latex-setup))
;;   :config
;;   ;; Funções auxiliares organizadas
;;   (defun my/kirigami-common-setup ()
;;     "Configuração genérica para ativar o Kirigami e seus backends."
;;     (outline-minor-mode 1)
;;     ;;(hs-minor-mode 1)
;;     (kirigami-mode 1))

;;   (defun my/kirigami-css-setup ()
;;     (setq-local outline-regexp "/\\* [^*]")
;;     (my/kirigami-common-setup))

;;   (defun my/kirigami-scheme-setup ()
;;     (setq-local outline-regexp ";;;\\*+\\|\\`")
;;     (my/kirigami-common-setup))

;;   (defun my/kirigami-latex-setup ()
;;     (setq-local outline-regexp "%%%+\\|\\`")
;;     (my/kirigami-common-setup))

;;   ;; Configurações globais adicionais
;;   (remove-hook 'prog-mode-hook #'outline-minor-mode)
;;   (setq outline-minor-mode-cycle nil))

;; (use-package kirigami
;;   :ensure t
;;   :bind (;; Define atalhos unificados que vão funcionar no CSS e em qualquer outro modo
;;          ("C-c z TAB" . kirigami-toggle-fold)
;;          ("C-c z <left>" . kirigami-close-fold)
;;          ("C-c z <right>" . kirigami-open-fold)
;;          ("C-c z <up>" . kirigami-close-folds)    ; Fecha tudo no arquivo
;;          ("C-c z <down>" . kirigami-open-folds))  ; Abre tudo no arquivo
;;   :hook
;;   ((css-mode-hook .
;;                         (lambda ()
;;                           ;; Mantém a regex para reconhecer os comentários "/* " como títulos
;;                           (setq-local outline-regexp "/\\* [^*]")
;;                           ;; Ativa os backends que o Kirigami gerencia
;;                           (outline-minor-mode 1)
;;                           (hs-minor-mode 1)
;;                           ;; Ativa o Kirigami localmente no buffer de CSS
;;                           (kirigami-mode 1)))
;; (scheme-mode-hook .
;;                         (lambda ()
;;                           ;; Mantém a regex para reconhecer os comentários "/* " como títulos
;;                           (setq-local outline-regexp ";;;\\*+\\|\\`")
;;                           ;; Ativa os backends que o Kirigami gerencia
;;                           (outline-minor-mode 1)
;;                           (hs-minor-mode 1)
;;                           ;; Ativa o Kirigami localmente no buffer de CSS
;;                           (kirigami-mode 1)))
;;    )
;;   :config
;;   (remove-hook 'prog-mode-hook 'outline-minor-mode)
;;   (setq outline-minor-mode-cycle nil))

;;; Programming languages


;;;;  Haskell

(use-package haskell-mode
  :ensure t)

;;;; Lua
(use-package lua-mode
  :ensure t)

;;;; Guile Scheme

(use-package geiser-guile
  :ensure t
  :config
  (add-to-list 'geiser-guile-load-path "~/.guix-profile/share/guile/site/3.0")
  (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/site/3.0"))

;;;; Common Lisp

;;;; Python

(use-package pyvenv :ensure (:host github :repo "jorgenschaefer/pyvenv"))


;;; Adiciona linhas guia nos buffers
;;
;; - https://github.com/jdtsmith/indent-bars
(use-package indent-bars
  :ensure t
  :hook
  ((python-mode yaml-mode) . indent-bars-mode))

;;; Docker

(use-package dockerfile-mode
  :ensure t)

(provide 'setup-programming-and-markup)
;;; setup-programming-and-markup.el ends here



