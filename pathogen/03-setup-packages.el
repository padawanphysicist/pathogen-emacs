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
;;
;;; Code:

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
(use-package
  which-key
  :init
  ;; Don't wait too much for help buffer popup
  (setq which-key-idle-delay 0.1)
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
  :init
  (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))
  (vertico-mode)
  :custom
  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  :config
  (setq completion-styles '(substring orderless)
          read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult;; :quelpa (consult :fetcher github :repo "minad/consult")
  :bind
  (("C-x r x" . consult-register)
   ("C-x r b" . consult-bookmark)
   ("C-c k" . consult-kmacro)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ("M-g o" . consult-outline)
   ("M-g h" . consult-org-heading)
   ("M-g a" . consult-org-agenda)
   ("M-g m" . consult-mark)
   ("C-x b" . consult-buffer)
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-project-imenu)
   ("M-g e" . consult-error)
   ;; M-s bindings (search-map)
   ("M-s f" . consult-find)
   ("M-s L" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch)
   ("M-g l" . consult-line)
   ("M-s m" . consult-multi-occur)
   ("C-x c o" . consult-multi-occur)
   ("C-x c SPC" . consult-mark)
   :map isearch-mode-map
   ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
   ("M-s l" . consult-line))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<")
;; Use `consult-completion-in-region' if Vertico is enabled.
(add-hook 'vertico-mode-hook (lambda ()
                           (setq completion-in-region-function
                                 (if vertico-mode
                                     #'consult-completion-in-region
                                   #'completion--in-region))))

  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package orderless
;;   :init (icomplete-mode)
;;   :custom
;;   (completion-styles '(orderless))
;;   (completion-category-defaults nil)
;;   (completion-category-overrides '((file (styles partial-completion))))
;;   (orderless-matching-styles '(orderless-initialism orderless-flex orderless-literal)))
(use-package orderless
  :init (icomplete-mode)
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-strict-full-initialism orderless-regexp)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general
;;
;;
;; https://github.com/noctuid/general.el
;;
;; general.el provides a more convenient method for binding keys in emacs (for
;; both evil and non-evil users). Like use-package, which provides a convenient,
;; unified interface for managing packages, general.el is intended to provide a
;; convenient, unified interface for key definitions. While this package does
;; implement some completely new functionality (such as the ability to make
;; vim-style keybindings under non-prefix keys with an optional timeout), its
;; primary purpose is to build on existing functionality to make key definition
;; more clear and concise. general-define-key is user-extensible and supports
;; defining multiple keys in multiple keymaps at once, implicitly wrapping key
;; strings with (kbd ...), using named prefix key sequences (like the leader key
;; in vim), and much more.
(use-package general)

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
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
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
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  (global-set-key (kbd "M-g f") 'avy-goto-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit
;;
;;
;; https://magit.vc/
;;
;; A Git Porcelain inside Emacs
(use-package magit
  :bind (("<f12>" . magit-status)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :after magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual regex search on steroids
;;
;;
;; https://github.com/benma/visual-regexp-steroids.el
(use-package visual-regexp-steroids
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable transposing frames
;;
;;
(use-package transpose-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;
;;
;; Install themes
(use-package atom-one-dark-theme)
;;(use-package dracula-theme)
(use-package monokai-pro-theme
  :config
  (load-theme 'monokai-pro t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template system
;;
;;
;; https://github.com/joaotavora/yasnippet
;;
(use-package yasnippet
  :init       (yas-global-mode 1)
  :config  
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

(use-package yasnippet-snippets
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Viewing
;;
;;
;; https://github.com/politza/pdf-tools
;;
(use-package pdf-tools
  ;;:pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1)))
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile
;;
;;
;; https://github.com/bbatsov/projectile
;;
(use-package projectile
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide '03-setup-packages)
;;; 03-setup-packages.el ends here
