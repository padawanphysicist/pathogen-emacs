;;; 03-setup-packages.el --- Core packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Module: Setup Packages (03)
;; Purpose: External package configurations
;; Dependencies: 02-package-manager.el (elpaca, use-package)
;; Provides: Modern IDE-like features and workflows
;;
;; This module configures all external packages including the completion stack
;; (vertico, consult, embark, corfu, orderless), navigation tools (avy,
;; ace-window), development tools (eglot, flycheck, treesit-auto, magit), and
;; productivity packages (which-key, multiple-cursors, dimmer).
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dashboard
;;
;;
;; https://github.com/emacs-dashboard/emacs-dashboard
;;
;;  An extensible emacs dashboard.
(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Pathogen Emacs" "Set dashboard title")
  (dashboard-startup-banner (concat user-emacs-directory "logo/pathogen-emacs.png") "Set initial banner")
  (dashboard-center-content t "Center contents by default")
  :config
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key
;;
;;
;; https://github.com/justbur/emacs-which-key
;;
;; One of the core ideas of Emacs is discoverability. It is a self-documented
;; editor. To see this, check =C-h ?=.
;;
;; However, after enabling a whole plethora of available packages you can get
;; lost by the messiness of the enabled shortcuts.
;;
;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup. This
;; provides a way to discover shortcuts globally.
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.4 "Delay before showing keybinding hints")
  :config
  (which-key-mode 1))

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
  :bind (;; C-c bindings (mode-specific-map)
         ("C-x C-r" . consult-recent-file)
         ("C-x C-t" . consult-theme)
         ;; ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-win) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :after avy
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winum
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin
;;
;;
;; https://github.com/emacsorphanage/popwin
;;
;; Nice popup management
;;
(use-package popwin
  :config
  (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
;;
;;
;; https://github.com/abo-abo/avy
;;
;; Easy navigation within buffers
(use-package avy
  :custom
  (avy-timeout-seconds 1)
  (avy-case-fold-search nil) ;; Case sensitive search
  :bind (("C-;" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit
;;
;;
;; https://magit.vc/
;;
;; A Git Porcelain inside Emacs
;; https://github.com/progfolio/elpaca/issues/324
(use-package transient)
(use-package magit
  :after transient
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual regex search on steroids
;;
;;
;; https://github.com/benma/visual-regexp-steroids.el
;; Deferred: only loads when keybinding is used
(use-package visual-regexp-steroids
  :defer t
  :bind
  (("C-c q" . vr/query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable transposing frames
;;
;;
;; Deferred: only loads when transpose-frame commands are called
(use-package transpose-frame
  :defer t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

;; Load after init to avoid slowing down startup
(use-package dimmer
  :defer 2
  :custom
  (dimmer-fraction 0.5)
  :config
  (dimmer-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings unrelated to any package
;;
;;
(use-package emacs
  :ensure nil
  :custom
  ;; `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t "Enable context menu.")
  (enable-recursive-minibuffers t "Enable recursive minibuffers")
  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  ;;(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; TAB cycle if there are only few candidates
  ;;(setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (global-completion-preview-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; project.el
;;
;;
;; Built-in project management (Emacs 27+)
;; Provides project detection, file finding, and project-scoped operations
(use-package project
  :ensure nil
  :bind (("C-x p f" . project-find-file)
         ("C-x p F" . project-or-external-find-file)
         ("C-x p g" . project-find-regexp)
         ("C-x p d" . project-find-dir)
         ("C-x p p" . project-switch-project)
         ("C-x p b" . project-switch-to-buffer)
         ("C-x p k" . project-kill-buffers)
         ("C-x p c" . project-compile)
         ("C-x p e" . project-eshell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
;;
;;
;; https://www.flycheck.org/
;;
;; On-the-fly syntax checking for GNU Emacs
;; Provides real-time error highlighting and linting for 50+ languages
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
;;; eglot
;;
;;
;; Built-in LSP client (Emacs 29+, available via package for earlier versions)
;; Provides IDE-like features: code completion, jump to definition,
;; documentation, refactoring, and more.
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t "Shutdown server when last buffer is killed")
  (eglot-sync-connect nil "Connect asynchronously")
  (eglot-events-buffer-size 0 "Disable event logging for performance")
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l o" . eglot-code-action-organize-imports)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; treesit-auto
;;
;;
;; Automatically install and use tree-sitter grammars
;; Provides better syntax highlighting and structural navigation
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt "Ask before installing grammars")
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

(provide '03-setup-packages)
;;; 03-setup-packages.el ends here
