;;; pathogen-versioning.el --- Version Control Configuration -*- lexical-binding: t; -*-

;; Transient: Required dependency for Magit to prevent version mismatches
(use-package transient
  :ensure t)

;; Magit: A spectacular Git interface for Emacs
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(provide 'pathogen-versioning)
;;; pathogen-versioning.el ends here
