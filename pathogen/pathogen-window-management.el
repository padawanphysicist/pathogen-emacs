;;; pathogen-window-management --- Window and Workspace Layouts -*- lexical-binding: t; -*-

;; Popper: Keeps temporary/popup buffers tightly managed
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;; Winum: Window number selection (M-1, M-2, etc.)
(use-package winum
  :ensure t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-²") 'winum-select-window-by-number)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config
  ;; Consistency fix: Prevent winum from numbering popper's popup windows
  (setq winum-ignored-buffers-regexp '("\\*Messages\\*" "\\*Help\\*" "\\*Compile-Log\\*"))
  (winum-mode +1))

;; Transpose-frame: Rotate, flip, or transpose window layouts
(use-package transpose-frame
  :ensure t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

(provide 'pathogen-window-management)
;;; pathogen-window-management.el ends here
