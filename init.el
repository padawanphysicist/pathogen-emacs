(add-to-list 'load-path (expand-file-name "pathogen" (file-name-directory load-file-name)))
(require 'pathogen)

;; Define the base germ
(define-germ base-germ
  :vars '(:experimental-mode t)
  :enabled t)

;; Define a dependent germ
(define-germ mutation-germ
  :deps '(base-germ)
  :vars '(:power-level 9000))

