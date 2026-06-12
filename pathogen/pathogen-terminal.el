;;; pathogen-terminal.el

;; Eat and Eat powered Eshell, fast featureful terminal inside Emacs:
;; https://emacsconf.org/2023/talks/eat/

(use-package eat
  :ensure t
  :after project
  :custom (eat-term-name "xterm-256color")
  :hook (eshell-load . eat-eshell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Project + Eat Terminal Integration
;;
;; Problem: defining eat-project inside `use-package eat :config` meant the
;; function and project-switch-commands entry only existed after eat was loaded
;; — which happens lazily, i.e.  only when first required. So the menu entry was
;; never registered at startup.
;;
;; Additionally, wrapping everything in `with-eval-after-load 'project` inside
;; eat's :config block was redundant: because eat declared `:after project`, by
;; the time :config ran, project was already loaded — but eat itself still
;; wasn't guaranteed to load early enough for the side effects to take.
;;
;; Solution: move the integration into a standalone `with-eval-after-load
;; 'project` block, completely outside any use-package declaration. Since
;; project is loaded eagerly via `:demand t`, this block fires reliably during
;; init.
;;
;; The `require 'eat` call inside eat-project handles lazy loading of eat itself
;; — it loads only when the terminal is actually invoked, not at startup. This
;; keeps startup fast while ensuring the menu entry and keybinding are always
;; registered.
;;
(use-package emacs
  :ensure nil
  :config
  (with-eval-after-load 'project
    (defun eat-project ()
      "Open the Eat terminal in the root of the current project."
      (interactive)
      (require 'eat)
      (let ((default-directory (project-root (project-current t))))
        (eat)))

    (define-key project-prefix-map "t" '("Eat terminal" . eat-project))

    (add-to-list 'project-switch-commands
                 '(eat-project "Eat terminal" ?t) t)))

(provide 'pathogen-terminal)
;;; pathogen-terminal.el ends here
