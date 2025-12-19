(defun pathogen-productivity-memex/log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(defun memex/help ()
  (interactive)
  (find-file (expand-file-name "README.org" pathogen-productivity-memex/root-directory)))

(defun pathogen-productivity-memex/open-folder ()
  (interactive)
  ;; TODO add something like "if dirvish is loaded"
  (dirvish org-directory))

(defun memex/consult (initial)
  (interactive "P")
  (consult-ripgrep pathogen-productivity-memex/root-directory initial))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções Customizadas para Zettelkasten
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Criar submap para os atalhos do memex
(define-prefix-command 'memex-submap)
(define-prefix-command 'memex-dailies-submap)
(define-prefix-command 'memex-agenda-submap)
(define-prefix-command 'memex-search-submap)

;; (defun memex--dailies-filename (offset)
;;     "RETURN file name for journal OFFSET days from today.  OFFSET needs to be an integer."
;;     (if (integerp offset)
;;         (let* ((target-time (+ (float-time) (* 86400 offset)))
;;                (year (format-time-string "%Y" target-time))
;;                (filename (format-time-string "%Y-%m-%d.org" target-time)))
;;           (expand-file-name (concat year "/" filename) memex-notes-dailies-directory))
;;       (error "Invalid offset value '%s'.  Expecting an integer" offset)))

;; (defun memex--dailies-get-template-content ()
;;   "Lê e formata o arquivo de template diário.
;; Substitui os especificadores de tempo/data."
;;   (let ((memex-dailies-template-file (expand-file-name "assets/templates/daily.org" pathogen-productivity-memex/root-directory)))
;;     (if (not (file-exists-p memex-dailies-template-file))
;;         (progn
;;           (message "Arquivo de template não encontrado: %s" memex-dailies-template-file)
;;           "") ; Retorna uma string vazia se o template não existir
;;       ;; Arquivo existe, vamos lê-lo e formatá-lo
;;       (with-temp-buffer
;;         (insert-file-contents memex-dailies-template-file)
;;         ;; format-time-string processa o buffer inteiro
;;         ;; substituindo códigos como %Y, %m, %d, %A, etc.
;;         (format-time-string (buffer-string))))))

;; (defun memex/dailies-goto-today ()
;;   "Abre a nota diária de hoje.
;; Se o arquivo não existir, cria-o usando `memex/dailies-template-file`."
;;   (interactive)
;;   (let* ((filename (memex--dailies-filename 0))
;;          (file-exists (file-exists-p filename)))
    
;;     ;; 1. Garante que o diretório pai existe
;;     ;(unless file-exists
;;     ;  (make-directory (file-name-directory filename) :parents t))
    
;;     ;; 2. Abre o arquivo (ou cria um novo buffer para ele)
;;     (find-file filename)
    
;;     ;; 3. Se o arquivo não existia, insere o conteúdo do template
;;     (unless file-exists
;;       (insert (memex--dailies-get-template-content)))))

;(defun memex/dailies-goto-today ()
;    "Open today's daily."
;    (interactive)
;    (find-file (memex--dailies-filename 0)))

  ;; (defun memex/dailies-goto-yesterday ()
  ;;   "Open yesterday's daily."
  ;;   (interactive)
  ;;   (find-file (memex--dailies-filename -1)))

  ;; (defun memex/dailies-goto-tomorrow ()
  ;;   "Open tomorrows's daily."
  ;;   (interactive)
  ;;   (find-file (memex--dailies-filename 1)))

  ;; (defun memex/dailies-goto-date ()
  ;;   "Read a date and open daily for that particular date."
  ;;   (interactive)
  ;;   (find-file (format "%s.org" (org-read-date))))

  (defun memex/open-bujo ()
    "Open Bullet Journal."
    (interactive)
    (find-file (expand-file-name "collections/bullet.org" org-directory)))

  (defun memex/edit-tags ()
    "Edit tag sets."
    (interactive)
    (find-file (expand-file-name "assets/tags.org" org-directory)))

;;; Buscar backlinks da nota atual
(defun memex/find-backlinks ()
  "Encontra todas as notas que linkam para a atual usando rgrep."
  (interactive)
  (let* ((id (org-id-get))
         (search-pattern (format "\\[\\[id:%s\\]" id)))
    (if id
        (progn
          (rgrep search-pattern "*.org" memex-notes-directory)
          (message "Buscando backlinks para ID: %s" id))
      (message "Esta nota não tem :ID: property!"))))

;;; Inserir link para nota por título
(defun memex/insert-note-link ()
  "Busca nota por título e insere link [[id:xxx][Título]]."
  (interactive)
  (let* ((files (directory-files-recursively memex-notes-directory "\\.org$"))
         (titles-alist (mapcar
                        (lambda (file)
                          (cons (memex--get-title file) file))
                        files))
         (selected-title (completing-read "Nota: " titles-alist))
         (selected-file (cdr (assoc selected-title titles-alist)))
         (id (memex--get-id selected-file)))
    (if id
        (insert (format "[[id:%s][%s]]" id selected-title))
      (message "Nota selecionada não tem :ID: property!"))))

;;; Funções auxiliares
(defun memex--get-title (file)
  "Extrai #+title: do FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:\\s-*\\(.*\\)$" nil t)
        (match-string 1)
      (file-name-base file))))

(defun memex--get-id (file)
  "Extrai :ID: property do FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^:ID:\\s-*\\(.*\\)$" nil t)
        (match-string 1)
      nil)))

;;; Listar notas por tag
(defun memex/notes-by-tag (tag)
  "Lista todas as notas com TAG."
  (interactive "sTag: ")
  (let ((files (directory-files-recursively memex-notes-directory "\\.org$")))
    (with-current-buffer (get-buffer-create "*Notes by Tag*")
      (erase-buffer)
      (org-mode)
      (insert (format "* Notas com tag :%s:\n\n" tag))
      (dolist (file files)
        (when (memex--file-has-tag-p file tag)
          (let ((title (memex--get-title file))
                (id (memex--get-id file)))
            (if id
                (insert (format "- [[id:%s][%s]]\n" id title))
              (insert (format "- [[file:%s][%s]]\n" file title))))))
      (switch-to-buffer (current-buffer)))))

(defun memex--file-has-tag-p (file tag)
  "Verifica se FILE tem TAG."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward (format ":%s:" tag) nil t)))

;;; Keybindings para funções Zettelkasten
(define-key memex-submap (kbd "b") 'memex/find-backlinks)
(define-key memex-submap (kbd "l") 'memex/insert-note-link)
(define-key memex-submap (kbd "t") 'memex/notes-by-tag)

;; Usar: C-c m b (backlinks), C-c m l (insert link), C-c m t (by tag)

(defun memex--hyperbole-change-default-action-key ()
  ;;(hkey-set-action-key "\M-S-RET")
  (hkey-set-key "<M-S-Return>" 'hkey-either)
  ;;(add-hook 'hyperbole-init-hook (lambda () (hkey-set-key <KEY> '<CMD>)
  )

(defun memex--hyperbole-disable-key-bindings-after-init-hook ()
  "The table below is what I found defined by non-Hyperbole modes, which
  Hyperbole's global minor mode overrides based upon the bindings listed by
  https://emacs.stackexchange.com/a/75065/15483 :
  
  Binding         Hyperbole                             Without Hyperbole
  --------------------------------------------------------------------------------
  C-h A           hkey-help                             counsel-apropos
  M-RET           hkey-either                           org-meta-return
  ESC <return>    hkey-either                           org-meta-return
  M-o             hkey-operate                          not defined
  C-c RET         hui-select-thing                      org-ctrl-c-ret
  C-c .           hui-select-goto-matching-delimiter    org-time-stamp
  C-c /           hui-search-web                        org-sparse-tree
  C-c @           hycontrol-windows-grid                org-mark-subtree
  C-c \           hycontrol-enable-windows-mode         org-match-sparse-tree
  C-x r s         hui-copy-to-register                  copy-to-register
  
  Thus as my usage of Hyperbole grows, I'll discover which ones
  have to be undefined, as done below.
  
  Undefine the `C-c /` key in the hyperbole mode map, allowing
  the existing binding in the Org mode map (C-c / runs the
  command org-sparse-tree) to be active, but temporarily
  orphaning `hui-search-web`:"
  (define-key hyperbole-mode-map (kbd "C-c /") nil)
  (define-key hyperbole-mode-map (kbd "M-RET") nil))
