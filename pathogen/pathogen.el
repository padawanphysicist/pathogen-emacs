;;; pathogen.el --- Master Control

(add-to-list 'load-path (expand-file-name "core" (file-name-directory load-file-name)))
;;(add-to-list 'load-path (expand-file-name "protocols" (file-name-directory load-file-name)))

(require 'pathogen-dna)
;; (require 'pathogen-sequence)
;; (require 'pathogen-infection)
;; (require 'pathogen-quarantine)

;; (defun pathogen-start-infection ()
;;   "Begin the system-wide infection based on the Genome."
;;   (interactive)
;;   (let ((path (pathogen-sequence-dna)))
;;     (mapc #'pathogen-incubate-germ path)))

(provide 'pathogen)
