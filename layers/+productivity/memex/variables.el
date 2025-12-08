(defvar memex-directory (expand-file-name "~/2.areas/org/"))

;; Recursively find all .org files in org-directory
;; EXCLUINDO cemetery/, tmp/, archive/ para melhor performance
(defvar memex-agenda-files
  (seq-filter
   (lambda (file)
     (not (string-match-p "\\(cemetery\\|tmp\\|archive\\)/" file)))
   (directory-files-recursively memex-directory "\\.org$")))

;; Nova estrutura Zettelkasten
(defvar memex-notes-dailies-directory  (expand-file-name "journal/" memex-directory))
(defvar memex-notes-directory (expand-file-name "notes/" memex-directory))
(defvar memex-collections-directory (expand-file-name "collections/" memex-directory))
(defvar memex-inbox-directory (expand-file-name "inbox/" memex-directory))
