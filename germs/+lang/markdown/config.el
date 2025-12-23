;(use-package markdown-mode
;  :ensure t
;  :mode ("\\.md\\'" . markdown-mode)
;  :bind (:map markdown-mode-map
;         ("C-c C-e" . markdown-export)))


(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown"
        markdown-default-variant 'gfm)
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-export-and-open)))

;(use-package markdown-mode
;  :ensure t
;  :mode ("\\.md\\'" . gfm-mode)
;  :init (setq markdown-command "multimarkdown")
;  :bind (:map markdown-mode-map
;         ("C-c C-e" . markdown-do)))
