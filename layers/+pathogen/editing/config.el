;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unkillable-scratch
;;
;; Disallow the *scratch* buffer from being killed
;;
;;  https://github.com/EricCrosson/unkillable-scratch
;;
(use-package unkillable-scratch
  :ensure t
  :config
  (unkillable-scratch t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
;;
;;
;; https://github.com/abo-abo/avy
;;
;; Easy navigation within buffers
;;
(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 1)
  (avy-case-fold-search nil) ;; Case sensitive search
  :bind (("C-;" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual regex search on steroids
;;
;;
;; https://github.com/benma/visual-regexp-steroids.el
;; Deferred: only loads when keybinding is used
;;
(use-package visual-regexp-steroids
  :defer t
  :bind
  (("C-c q" . vr/query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enable transposing frames
;;
;; https://melpa.org/#/transpose-frame
;;
;; Deferred: only loads when transpose-frame commands are called
;;
(use-package transpose-frame
  :defer t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dims inactive windows to highlight the active one
;;
;; https://melpa.org/#/dimmer 
;;
;; Load after init to avoid slowing down startup
;;
(use-package dimmer
  :defer 2
  :custom
  (dimmer-fraction 0.5)
  :bind ("C-c t d" . dimmer-mode)  ; Toggle dimmer on/off
  :config
  (dimmer-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yasnippet
;;
;; https://github.com/joaotavora/yasnippet
;;
;; Template system for Emacs. Allows quick insertion of code templates with
;; tab-stop fields for customization. Essential productivity tool for reducing
;; boilerplate code and maintaining consistent patterns.
;;
;; Examples:
;;   - Type "for" + TAB → expands to full for-loop structure
;;   - Type "def" + TAB → expands to function definition
;;   - Type "class" + TAB → expands to class template
;;
;; Works seamlessly with LSP and completion frameworks for enhanced productivity.
;;
(use-package yasnippet
  :defer 2
  :bind (:map yas-minor-mode-map
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1))

;; Collection of standard snippets for many languages
(use-package yasnippet-snippets
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
;;
;; https://www.flycheck.org/
;;
;; On-the-fly syntax checking for GNU Emacs
;; Provides real-time error highlighting and linting for 50+ languages
;;
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   "Check on save, after idle time, and when mode is enabled")
  (flycheck-idle-change-delay 2.0
   "Wait 2 seconds after typing stops before checking")
  (flycheck-display-errors-delay 0.5
   "Show error messages after 0.5 seconds")
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! v" . flycheck-verify-setup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple cursors for Emacs 
;;
;; https://melpa.org/#/multiple-cursors
;;
(use-package multiple-cursors
  :config
  (defun mc/toggle-cursor-at-point ()
    "Add or remove a cursor at point."
    (interactive)
    (if multiple-cursors-mode
        (message "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
      (let ((existing (mc/fake-cursor-at-point)))
        (if existing
            (mc/remove-fake-cursor existing)
          (mc/create-fake-cursor-at-point)))))
  (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)
  :bind (;; Mouse and custom bindings
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-SPC" . mc/toggle-cursor-at-point)
         ("<C-S-return>" . multiple-cursors-mode)
         ;; Standard multiple-cursors bindings
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C->" . mc/skip-to-next-like-this)
         ("C-c C-<" . mc/skip-to-previous-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winum
;;
;; https://github.com/deb0ch/emacs-winum
;;
(use-package winum
  :init
  (setq winum-keymap
    (let ((map (make-sparse-keymap)))
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
  (winum-mode))

