;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; which-key is a minor mode for Emacs that displays the key bindings
;; following your currently entered incomplete command (a prefix) in a
;; popup. This provides a way to discover shortcuts globally.
;;
(use-package which-key
  :ensure nil
  :diminish which-key-mode
  :custom
  (which-key-add-column-padding 1)
  (which-key-allow-multiple-replacements t)
  (which-key-echo-keystrokes 0.02)
  (which-key-idle-delay 0.4)
  (which-key-idle-secondary-delay 0.01)
  (which-key-max-description-length 32)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-prevent-C-h-from-cycling t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-sort-uppercase-first nil)
  (which-key-special-keys nil)
  (which-key-use-C-h-for-paging t)
  :config
  (which-key-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; project.el
;;
;;
;; Built-in project management (Emacs 27+)
;; Provides project detection, file finding, and project-scoped operations
;;
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
;;; emacs
;;
;;
;; Settings unrelated to any package
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
