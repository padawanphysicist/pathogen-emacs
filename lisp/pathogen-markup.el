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


;; TeX/LaTeX/ConTeXt
(use-package auctex :ensure t
  :config
  ;; Set .ctx files to ConTeXt mode
  (add-to-list 'auto-mode-alist '("\\.ctx\\'" . ConTeXt-mode))
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


(provide 'pathogen-markup)
