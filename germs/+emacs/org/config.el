(use-package org
  :ensure t
  ;; :init
  ;; Set org-directory from layer variable (before package loads)
  ;; (setq org-directory pathogen-emacs-org/base-directory)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-latex-preview))
  :custom
  ((org-directory pathogen-emacs-org/base-directory)
   (org-ellipsis " [+]")
   (org-link-elisp-skip-confirm-regexp ".*")
   ;;(org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "STOP(s)")))
  
   ;; (org-refile-targets `((nil :maxlevel . 9)
   ;;                       (org-agenda-files :maxlevel . 9)
   ;;                       ;; MOCs específicos
   ;;                       (,(expand-file-name "collections/trabalho.org" pathogen-productivity-memex/root-directory) :maxlevel . 3)
   ;;                       (,(expand-file-name "collections/pessoal.org" pathogen-productivity-memex/root-directory) :maxlevel . 3)
   ;;                       (,(expand-file-name "collections/projetos.org" pathogen-productivity-memex/root-directory) :maxlevel . 3)
   ;;                       (,(expand-file-name "collections/bullet.org" pathogen-productivity-memex/root-directory) :maxlevel . 2)))
   ;;(org-refile-use-outline-path t)
   ;; https://lists.gnu.org/archive/html/emacs-orgmode/2024-09/msg00209.html
   ;;(org-yank-image-save-method (expand-file-name "assets/img/" org-directory))
   ;;(org-preview-latex-image-directory "~/.cache/emacs/org/ltxpng/")
   ;;(org-hide-emphasis-markers t "hide the emphasis marker characters.")
   (org-startup-indented t "turn on ‘org-indent-mode’ on startup.")
   ;;(org-startup-with-link-previews t)
   ;;(org-highlight-latex-and-related '(native latex script entities))
   ;;(org-startup-with-inline-images t "show inline images when loading a new Org file."))
   )
  ;;:config
  ;;(add-hook 'org-after-todo-state-change-hook #'pathogen-productivity-memex/log-todo-next-creation-date)
  ;; https://notes.alexkehayias.com/emacs-inline-macro-in-the-buffer/
  ;; Display macros inline in buffers
  ;;(add-to-list 'font-lock-extra-managed-props 'display)

  ;(font-lock-add-keywords
  ; 'org-mode
  ; '(("\\({{{[a-zA-Z#%)(_-+0-9]+}}}\\)" 0
  ;    `(face nil display
  ;           ,(format "%s"
  ;                    (let* ((input-str (match-string 0))
  ;                           (el (with-temp-buffer
  ;                                 (insert input-str)
  ;                                 (goto-char (point-min))
  ;                                 (org-element-context)))
  ;                           (text (org-macro-expand el org-macro-templates)))
  ;                      (if text
  ;                          text
  ;                        input-str)))))))
  )

;; (use-package org-modern-indent
;;   :after org
;;   :ensure (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
;;   :custom
;;   (
;;    (org-modern-todo-faces '(("TODO" :background "#FF6347" :foreground "#FFFFFF")
;;                             ("NEXT" :background "#FFC107" :foreground "#212529")
;;                             ("DONE" :background "#28A745" :foreground "#FFFFFF")
;;                             ("STOP" :background "#6C757D" :foreground "#FFFFFF")))
;;    (org-modern-priority-faces
;;     '((?A :background "#C82333" :foreground "#FFFFFF")
;;       (?B :background "#007BFF" :foreground "#FFFFFF")
;;       (?C :background "#E9ECEF" :foreground "#212529")))
;;    )
;;   :config
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for PKM
;; These settings aim to provide a neat acess to my files
;;(use-package org-agenda
;;  :after org
;;  :ensure nil
;;  :custom
;;  (org-agenda-files memex-agenda-files)
;;  (org-agenda-custom-commands
;;   '(;; Dashboard completo
;;     ("d" "Dashboard"
;;      ((agenda "" ((org-agenda-span 'day)
;;                   (org-agenda-overriding-header "📅 Hoje")))
;;       (tags-todo "+PRIORITY=\"A\""
;;                  ((org-agenda-overriding-header "🔥 Alta Prioridade")))
;;       (tags-todo "+TODO=\"PROG\""
;;                  ((org-agenda-overriding-header "⏭️  Em Progresso")))
;;       (tags-todo "+TODO=\"TODO\""
;;                  ((org-agenda-overriding-header "📋 A Fazer")))))
;;
;;     ;; Contexto: Trabalho
;;     ("w" "Trabalho" tags-todo "+work|+arco"
;;      ((org-agenda-overriding-header "💼 Tarefas de Trabalho")))
;;
;;     ;; Contexto: Pessoal
;;     ("p" "Pessoal" tags-todo "+personal-work-arco"
;;      ((org-agenda-overriding-header "🏠 Tarefas Pessoais")))
;;
;;     ;; Review semanal
;;     ("r" "Review Semanal"
;;      ((todo "DONE"
;;             ((org-agenda-overriding-header "✅ Completados Esta Semana")
;;              (org-agenda-span 'week)))
;;       (tags-todo "-TODO=\"DONE\"-TODO=\"STOP\""
;;                  ((org-agenda-overriding-header "📋 Todos os TODOs Ativos")))))))
;;  :config
;;    (defun memex-agenda-files-from-directory (dir)
;;    "Get all Org files from DIR recursively."
;;    (when (and dir (file-directory-p dir))
;;      (directory-files-recursively dir "\\.org$")))
;;
;;  (defun memex-agenda-from-directory (dir)
;;    "Run agenda with all Org files from DIR."
;;    (let ((org-agenda-files (directory-files-recursively dir "\\.org$")))
;;      (org-agenda nil "t")))
;;
;;
;;  (defun memex-agenda-dashboard ()
;;    (interactive)
;;    (org-agenda nil "d")
;;    (delete-other-windows))
;;
;;  (defun memex-agenda-work ()
;;    (interactive)
;;    (org-agenda nil "w")
;;    (delete-other-windows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture - Captura rápida de notas e tarefas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package org-capture                                                                                                                                                                                                                                                                                                                       ;;
;;   :after org                                                                                                                                                                                                                                                                                                                                   ;;
;;   :ensure nil                                                                                                                                                                                                                                                                                                                                  ;;
;;   :config                                                                                                                                                                                                                                                                                                                                      ;;
;;   ;;:custom                                                                                                                                                                                                                                                                                                                                    ;;
;;   ;; (org-capture-templates                                                                                                                                                                                                                                                                                                                    ;;
;;   ;;  `(;; TODO rápido (vai para inbox mobile)                                                                                                                                                                                                                                                                                                 ;;
;;   ;;    ;; ("t" "TODO Rápido" entry                                                                                                                                                                                                                                                                                                            ;;
;;   ;;    ;;  (file ,(expand-file-name "inbox/mobile.org" pathogen-productivity-memex/root-directory))                                                                                                                                                                                                                                           ;;
;;   ;;    ;;  "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"                                                                                                                                                                                                                                                                                   ;;
;;   ;;    ;;  :empty-lines 1)                                                                                                                                                                                                                                                                                                                    ;;
;;                                                                                                                                                                                                                                                                                                                                                ;;
;;   ;;    ;; Nota (vai para inbox desktop)                                                                                                                                                                                                                                                                                                       ;;
;;   ;;    ;; ("t" "Anotação rápida" entry                                                                                                                                                                                                                                                                                                        ;;
;;   ;;    ;;  (file ,(expand-file-name "productivity/inbox.org" pathogen-productivity-memex/root-directory))                                                                                                                                                                                                                                     ;;
;;   ;;    ;;  "* TODO %U %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n"                                                                                                                                                                                                                                                            ;;
;;   ;;    ;;  :empty-lines 1)                                                                                                                                                                                                                                                                                                                    ;;
;;                                                                                                                                                                                                                                                                                                                                                ;;
;;   ;;    ;; Journal entry (vai para journal/YYYY/)                                                                                                                                                                                                                                                                                              ;;
;;   ;;    ("j" "Journal" entry                                                                                                                                                                                                                                                                                                                   ;;
;;   ;;     (file+olp+datetree                                                                                                                                                                                                                                                                                                                    ;;
;;   ;;      ,(expand-file-name (format-time-string "journal/%Y/%Y-%m-%d.org") pathogen-productivity-memex/root-directory))                                                                                                                                                                                                                       ;;
;;   ;;     "* %<%H:%M> %?\n"                                                                                                                                                                                                                                                                                                                     ;;
;;   ;;     :empty-lines 1)                                                                                                                                                                                                                                                                                                                       ;;
;;                                                                                                                                                                                                                                                                                                                                                ;;
;;   ;;    ;; Meeting notes (vai para trabalho.org MOC)                                                                                                                                                                                                                                                                                           ;;
;;   ;;    ("m" "Meeting" entry                                                                                                                                                                                                                                                                                                                   ;;
;;   ;;     (file+headline ,(expand-file-name "collections/trabalho.org" pathogen-productivity-memex/root-directory) "Tarefas")                                                                                                                                                                                                                   ;;
;;   ;;     "* MEETING %? :meeting:\nSCHEDULED: %^T\n** Participantes\n\n** Notas\n\n** Action Items\n- [ ] \n"                                                                                                                                                                                                                                   ;;
;;   ;;     :empty-lines 1)                                                                                                                                                                                                                                                                                                                       ;;
;;                                                                                                                                                                                                                                                                                                                                                ;;
;;   ;;    ;; Zettel note                                                                                                                                                                                                                                                                                                                         ;;
;;   ;;    ("z" "Zettel Note" entry                                                                                                                                                                                                                                                                                                               ;;
;;   ;;     (file ,(expand-file-name (format "notes/%s.org" (format-time-string "%Y%m%d%H%M%S"))                                                                                                                                                                                                                                                  ;;
;;   ;;                               pathogen-productivity-memex/root-directory))                                                                                                                                                                                                                                                                ;;
;;   ;;     "#+title: %^{Título}\n#+date: %U\n#+setupfile: ../assets/tags.org\n#+filetags: \n\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n\n* %?\n\n* Links Relacionados\n\n* Backlinks\n# Adicione manualmente links para notas que referenciam esta\n# Use: M-x memex/find-backlinks para buscar automaticamente\n\n* References\n" ;;
;;   ;;     :empty-lines 1)))                                                                                                                                                                                                                                                                                                                     ;;
;;   )                                                                                                                                                                                                                                                                                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-modern
  :ensure t
  :after org
  :custom
  (org-modern-fold-stars '(("◉" . "◉") ("○" . "○") ("✸" . "✸") ("◉" . "◉") ("○" . "○") ("✸" . "✸")))
  :config
  (global-org-modern-mode))

;; Reveals hidden elements interactively.
;; https://github.com/awth13/org-appear
(use-package org-appear
  :after org-modern
  :custom
  ((org-appear-autolinks nil)
   (org-appear-inside-latex t))
  :hook (org-mode . org-appear-mode))

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

;; (use-package org-ql
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org ID - Links estáveis (ESSENCIAL para Zettelkasten)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package org-id
;;   :after org
;;   :ensure nil
;;   :custom
;;   (org-id-track-globally t)
;;   (org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory))
;;   (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consult - Busca interativa (RECOMENDADO para navegação)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package consult
;  :ensure t
;  :bind (("C-c o s" . consult-org-heading)      ; Buscar heading em arquivo atual
;         ("C-c o S" . consult-org-agenda)       ; Buscar em agenda files
;         ("C-c o g" . consult-ripgrep)))        ; Buscar texto em notes/

;; Keybindings
;(use-package emacs
;  :ensure nil 
;  :hook (after-init . memex/help)
;  :bind (:prefix-map memex-submap
;                     :prefix-docstring "Memex custom map"
;                     :prefix "C-c o"
;                     ("h" . #'memex/help)
;                     ("a" . memex-agenda-submap)
;                     ("s" . memex-search-submap)
;                     ("c" . org-capture)
;                     )
;  :bind (:prefix-map memex-agenda-submap
;                     :prefix-docstring "Memex agenda"
;                     :prefix "C-c o a"
;                     ("a" . org-agenda)
;                     ("d" . #'memex-agenda-dashboard)
;                     ("w" . #'memex-agenda-work)
;                     )
;  :bind (:prefix-map memex-search-submap
;                     :prefix-docstring "Memex Search"
;                     :prefix "C-c o s"
;                     ("h" . consult-org-heading)
;                     ("g" . #'memex/consult)))

;; Wait for Elpaca to process packages before continuing
;; This ensures org is available in config.el
(elpaca-wait)

