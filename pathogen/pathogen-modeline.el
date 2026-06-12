;; Check { C-h v mode-line-format RET }
;;
;; Default:
;; ("%e" mode-line-front-space
;;  (:propertize
;;   ("" mode-line-mule-info mode-line-client mode-line-modified
;;    mode-line-remote mode-line-window-dedicated)
;;   display (min-width (6.0)))
;;  mode-line-frame-identification mode-line-buffer-identification "   "
;;  mode-line-position (project-mode-line project-mode-line-format)
;;  (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info
;;  mode-line-end-spaces)
;;

(when (version<= "30.0" emacs-version)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Indicadores de Status ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (setopt mode-line-modified
          '((:eval (if buffer-read-only "🔒 " ""))
            (:eval (if (buffer-modified-p) "📝 " "💾 "))))
  (setopt mode-line-remote
          '(:eval (if (file-remote-p default-directory) "☁️ " "")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Indicador de projeto ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setopt project-mode-line-format " 📁 %s")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Indicadores de posição ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; "L%l" vira apenas "L%l:%c" se line-number-mode e
  ;; column-number-mode estiverem ativos
  (setopt mode-line-position-column-line-format '(" %l:%c "))
  (setopt mode-line-position-line-format '(" L%l "))
  
  ;; Alinhamento à Direita (Emacs 30+)
  (setopt mode-line-right-align-edge 'window)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Define o layout da modeline ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq-default mode-line-format
        '("%e"
          mode-line-front-space
          mode-line-client
          (:eval (list "[" (winum-get-number-string) "] "))
          ;; Status do Arquivo (Modificado/Remoto)
          mode-line-modified
          mode-line-remote
          mode-line-window-dedicated
          " " ; Substituído o "\t" por um espaço limpo
          mode-line-frame-identification
          ;; Identificação do Buffer (Nome do arquivo) com destaque em negrito
          (:propertize mode-line-buffer-identification face bold)
          "   "
          mode-line-position
          ;; Tudo a partir daqui vai para a extrema direita
          mode-line-format-right-align
          ;; Contexto do Trabalho (Projeto e Git)
          ;; (project-mode-line project-mode-line-format)
          ;; PROJETO (Dinâmico: Só aparece se o arquivo estiver em um projeto)
          (:eval (when-let ((proj (project-current)))
                   (list " 📁 " (project-name proj))))
          ;; Git
          ;; (vc-mode vc-mode)
          ;; CONTROLE DE VERSÃO / GIT (Versão robusta com Regex)
          (:eval (when (and buffer-file-name (file-exists-p buffer-file-name))
                   (when-let ((branch (vc-git--symbolic-ref buffer-file-name)))
                     ;; string-trim garante que não existam quebras de linha ocultas
                     (list " 🔀 " (string-trim branch)))))

          "  "
          mode-line-misc-info
          "  "
          mode-line-end-spaces)))

(provide 'pathogen-modeline)
;; pathogen-modeline.el ends here
