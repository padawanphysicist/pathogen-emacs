;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package nerd-icons-dired                                         ;;
;;   :ensure t                                                           ;;
;;   :after nerd-icons                                                   ;;
;;   :preface                                                            ;;
;;   (defun my/nerd-icons-icon-for-file (file)                           ;;
;;     (nerd-icons-icon-for-file file :height 0.9 :v-adjust 0.45))       ;;
;;                                                                       ;;
;;   (defun my/nerd-icons-icon-for-dir (dir)                             ;;
;;     (nerd-icons-icon-for-dir dir :height 0.9 :v-adjust 0.45))         ;;
;;                                                                       ;;
;;   :custom                                                             ;;
;;   (nerd-icons-dired-file-icon-function #'my/nerd-icons-icon-for-file) ;;
;;   (nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir)   ;;
;;   :hook                                                               ;;
;;   (dired-mode . nerd-icons-dired-mode))                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :preface
  ;; Usar &rest permite aceitar os argumentos adicionais que o dired envia
  (defun my/nerd-icons-icon-for-file (file &rest _)
    (nerd-icons-icon-for-file file :height 0.9 :v-adjust 0.45))

  (defun my/nerd-icons-icon-for-dir (dir &rest _)
    (nerd-icons-icon-for-dir dir :height 0.9 :v-adjust 0.45))

  :custom
  (nerd-icons-dired-file-icon-function #'my/nerd-icons-icon-for-file)
  (nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'pathogen-dired)
