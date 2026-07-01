;; o [[https://github.com/vedang/pdf-tools][pdf-tools]] é uma
;; biblioteca de visualização de PDFs muito superior ao =doc-view=
;; (padrão), permitindo anotações e busca real.
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; Integração com LaTeX (AUCTeX)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :custom
  (pdf-annot-activate-created-annotations t "Ativa anotações automaticamente"))

;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-directions-alist
   '(("pt-br" . "en") ("en" . "pt-br")))
  :config
  (require 'google-translate-smooth-ui)
  :bind
  ("C-c g" . google-translate-smooth-translate))

(use-package olivetti
  :ensure t
  :bind
  ("C-M-z" . olivetti-mode)
  :custom
  (olivetti-body-width 80)
  ;;:hook
  ;; (org-agenda-mode . olivetti-mode)
  ;; (text-mode olivetti-mode)
)

(provide 'pathogen-writing)
;;; pathogen-writing.el ends here
