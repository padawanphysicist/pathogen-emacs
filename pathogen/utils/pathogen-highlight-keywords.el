;;; pathogen-highlight-keywords.el --- Highlight TODO and similar keywords -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup +highlight-keywords nil
  "Customizações para o pathogen-highlight-keywords."
  :group 'convenience)

(defcustom +highlight-keywords-faces
  '(("TODO" . error)
    ("FIXME" . error)
    ("HACK" . warning)
    ("NOTE" . warning)
    ("HERE" . success)) ; <-- Alterado de 'compilation-info para 'success
  "Alist of keywords to highlight and their face."
  :group '+highlight-keywords
  :type '(alist :key-type (string :tag "Keyword")
                :value-type (symbol :tag "Face"))
  :set (lambda (sym val)
         (dolist (face (mapcar #'cdr val))
           (unless (facep face)
             (error "Invalid face: %s" face)))
         (set-default sym val)
         (when (fboundp '+highlight-keywords--update-matcher)
           (+highlight-keywords--update-matcher))))

(defvar +highlight-keywords--keywords nil
  "Keywords gerados dinamicamente para o font-lock.")

(defun +highlight-keywords--update-matcher ()
  "Atualiza a regex interna baseada nas faces customizadas."
  (setq +highlight-keywords--keywords
        (when +highlight-keywords-faces
          (let ((keywords (mapcar #'car +highlight-keywords-faces)))
            `((,(regexp-opt keywords 'words)
               (0 (when (nth 8 (syntax-ppss))
                    (cdr (assoc (match-string 0) +highlight-keywords-faces)))
                  prepend)))))))

;; Inicializa a variável de keywords
(+highlight-keywords--update-matcher)

(defun pathogen/highlight-keywords-mode-on ()
  "Ativa o font-lock para as palavras-chave."
  (when (and buffer-file-name
             (not (string-match-p "^\\*" (buffer-name))))
    (font-lock-add-keywords nil +highlight-keywords--keywords t)
    (font-lock-flush)))

(defun pathogen/highlight-keywords-mode-off ()
  "Remove o font-lock das palavras-chave."
  (font-lock-remove-keywords nil +highlight-keywords--keywords)
  (font-lock-flush))

;;;###autoload
(define-minor-mode pathogen/highlight-keywords-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter " +HL"
  :group '+highlight-keywords
  (if pathogen/highlight-keywords-mode
      (pathogen/highlight-keywords-mode-on)
    (pathogen/highlight-keywords-mode-off)))

;;;###autoload
(defun pathogen/highlight-keywords-initialize ()
  "Função para ser adicionada aos hooks (ex: prog-mode-hook)."
  (pathogen/highlight-keywords-mode 1))

(provide 'pathogen-highlight-keywords)
;;; pathogen-highlight-keywords.el ends here
