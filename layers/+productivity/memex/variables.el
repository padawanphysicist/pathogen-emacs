(defvar pathogen-productivity-memex/base-directory "~/org/"
  "Root directory for memex.")

;; Recursively find all .org files in org-directory
;; EXCLUINDO cemetery/, tmp/, archive/ para melhor performance
(defvar pathogen-productivity-memex/agenda-files
  (seq-filter
   (lambda (file)
     (not (string-match-p "\\(cemetery\\|tmp\\|archive\\)/" file)))
   (directory-files-recursively pathogen-productivity-memex/base-directory "\\.org$")))

;; Nova estrutura Zettelkasten
;(defvar memex-notes-dailies-directory  (expand-file-name "journal/" pathogen-productivity-memex/root-directory))
;(defvar memex-notes-directory (expand-file-name "notes/" pathogen-productivity-memex/root-directory))
;(defvar memex-collections-directory (expand-file-name "collections/" pathogen-productivity-memex/root-directory))
;;(defvar memex-inbox-directory (expand-file-name "inbox/" pathogen-productivity-memex/root-directory))
