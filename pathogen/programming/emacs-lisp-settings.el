;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun pathogen-elisp-mode-hook ()
  "Custom settings for Emacs Lisp editing."
  (show-paren-mode 1)
  (column-number-mode 1)
  (electric-pair-local-mode 1)
  (eldoc-mode 1)
  (setq show-trailing-whitespace t)
  (outline-minor-mode 1)
  (setq outline-regexp ";;;+")
  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1)
  (setq fill-column 70))

(add-hook 'emacs-lisp-mode-hook #'pathogen-elisp-mode-hook)

(provide 'emacs-lisp-settings)

