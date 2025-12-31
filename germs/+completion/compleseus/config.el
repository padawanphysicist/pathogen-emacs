;;; Completion framework configuration. -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;
;;
;; https://github.com/minad/vertico
;;
;; Vertico provides a performant and minimalistic vertical completion UI based
;; on the default completion system. The main focus of Vertico is to provide a
;; UI which behaves correctly under all circumstances. By reusing the built-in
;; facilities system, Vertico achieves full compatibility with built-in Emacs
;; completion commands and completion tables.
;;
;; Here I use a "complete" vertico ecossytem:
;;   - Marginalia: Rich annotations in the minibuffer
;;   - Consult: Useful search and navigation commands
;;   - Embark: Minibuffer actions and context menu
;;   - Orderless: Advanced completion style
(use-package vertico
  :custom
  (vertico-cycle t "Enable cycling for `vertico-next' and `vertico-previous'")
  (read-file-name-completion-ignore-case t "Ignores case during file name completion")
  (read-buffer-completion-ignore-case t "Ignores case during buffer name completion")
  :init
  (vertico-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :after vertico
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)))

;;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after vertico)

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-initialism orderless-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corfu
;;
;;
;; https://github.com/minad/corfu
;;
;; Completion Overlay Region FUnction
;; Modern in-buffer completion UI that works seamlessly with vertico/consult
(use-package corfu
  :custom
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.2 "Delay before showing popup")
  (corfu-auto-prefix 2 "Minimum prefix length for auto completion")
  (corfu-cycle t "Enable cycling through candidates")
  (corfu-quit-no-match 'separator "Don't quit if no match, except at separator")
  (corfu-preview-current nil "Don't preview current candidate")
  (corfu-preselect 'prompt "Preselect the prompt")
  :init
  (global-corfu-mode))

;;; config.el ends here

