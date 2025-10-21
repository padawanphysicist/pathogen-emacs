;; Make the hook functional by advising text scaling functions
(defun pathogen--run-after-text-scale-hook (&rest _args)
  "Run `after-text-scale-hook' after text scaling."
  (run-hooks 'pathogen-after-text-scale-hook))

;; Use modern advice-add instead of deprecated defadvice (obsolete since Emacs 24.4)
(defun pathogen--run-after-load-theme-hook (&rest _args)
  "Run `after-load-theme-hook' after loading a theme."
  (run-hooks 'pathogen-after-load-theme-hook))


