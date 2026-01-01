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
  ;; :bind (("C-x p f" . project-find-file)
  ;;        ("C-x p F" . project-or-external-find-file)
  ;;        ("C-x p g" . project-find-regexp)
  ;;        ("C-x p d" . project-find-dir)
  ;;        ("C-x p p" . project-switch-project)
  ;;        ("C-x p b" . project-switch-to-buffer)
  ;;        ("C-x p k" . project-kill-buffers)
  ;;        ("C-x p c" . project-compile)
  ;;        ("C-x p e" . project-eshell))
  )

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
  ;; :bind (:map eglot-mode-map
  ;;             ("C-c l r" . eglot-rename)
  ;;             ("C-c l a" . eglot-code-actions)
  ;;             ("C-c l f" . eglot-format)
  ;;             ("C-c l d" . eglot-find-declaration)
  ;;             ("C-c l i" . eglot-find-implementation)
  ;;             ("C-c l t" . eglot-find-typeDefinition)
  ;;             ("C-c l o" . eglot-code-action-organize-imports))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save minibuffer history
;;
;;
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo/Redo window configuration
;;
;;
(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recent files
;;
;;
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Show matching parenthesis
;;
;;
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modus themes
;;
;; https://github.com/protesilaos/modus-themes
(use-package modus-themes
  :ensure t
  :config
  (modus-themes-load-theme +distributions/base-default-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs
;;
;;
;; Settings unrelated to any package
;;
(use-package emacs
  :ensure nil
  :init
  
  ;; Enable modern auto-save
  (auto-save-visited-mode 1)          
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.  
  (advice-add #'completing-read-multiple :filter-args #'+distributions/base-crm-indicator)

  (global-completion-preview-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Death to tabs
  ;;
  ;; Yep, I don't like tabs:
  ;;
  ;;    1. It's hard to align code beyond simple indenting.
  ;;    2. When using tabs, spaces are still valid characters. Did you just
  ;;       accidentally indent with spaces instead of tabs? You never know. You
  ;;       have to enable a (noisy) visual whitespace in your editor to see it.
  ;;    3. Someone, somewhere will display your code expanding tabs to 8
  ;;      spaces. Try diff or cat on the command line.
  ;;    4. If you ban tabs, it is easy to write a pre-commit hook (or an editor
  ;;       macro, or a command-line tool) to check that no tabs are being
  ;;       added. It’s much harder (or even impossible) to verify that the
  ;;       indentation is correct when using tabs.
  ;;    5. If you can always get #1 and #2 right, one of your collegues or
  ;;       contributors won’t.
  ;; Therefore, death to them!
  ;;
  ;; However, historically tabs are a character to indent to the next 8-character
  ;; offset; specifying anything else might cause *mass* confusion, as it will
  ;; change the appearance of every existing file.  In some cases (python), even
  ;; worse -- it will change the semantics (meaning) of the program.
  ;;
  ;; Emacs modes usually provide a standard means to change the indentation width
  ;; -- eg. c-basic-offset: use that to adjust your personal indentation width,
  ;; while maintaining the style (and meaning) of any files you load.
  ;;
  ;; We also enable TAB to have a double purpose: first tries to indent the
  ;; current line, and if the line was already indented, then try to complete the
  ;; thing at point.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq tab-always-indent 'complete)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Electric indentation
  ;;
  ;;
  ;; electric-indent-mode is enabled by default in Emacs 24.4+. It automatically
  ;; reindents the current line when you press RET or type certain characters
  ;; like closing braces, brackets, or semicolons.
  ;;
  ;; This provides convenient automatic formatting as you type. If you find it
  ;; intrusive, you can disable it with: (electric-indent-mode -1)
  (electric-indent-mode 1)  ; Explicit, though enabled by default

  ;; An archaic default in the age of widescreen 4k displays? I disagree. We
  ;; still frequently split our terminals and editor frames, or have them
  ;; side-by-side, using up more of that newly available horizontal
  ;; real-estate.
  (setq-default fill-column 79)
  
  ;; Delete whatever is selected if typing starts This reflects the behavior
  ;; of other editors.
  (delete-selection-mode 1))

;; (use-package bind-key
;;   :ensure nil
;;   :config
;;   ;; Basic keymaps
;;   (defvar +distributions/base-file-map (make-sparse-keymap))
;;   (defvar +distributions/base-buffer-map (make-sparse-keymap))
;;   (defvar +distributions/base-ui-map (make-sparse-keymap))
;;   (defvar +distributions/base-search-map (make-sparse-keymap))
;;   (defvar +distributions/base-help-map (make-sparse-keymap))
;;   (defvar +distributions/base-quit-map (make-sparse-keymap))
;;   (defvar +distributions/base-window-map (make-sparse-keymap))
;;   (defvar +distributions/base-project-map (make-sparse-keymap))

;;   ;; Attach keymaps to leader key (C-c)
;;   (keymap-set global-map "C-c f" +distributions/base-file-map)
;;   (keymap-set global-map "C-c b" +distributions/base-buffer-map)
;;   (keymap-set global-map "C-c u" +distributions/base-ui-map)
;;   (keymap-set global-map "C-c s" +distributions/base-search-map)
;;   (keymap-set global-map "C-c h" +distributions/base-help-map)
;;   (keymap-set global-map "C-c q" +distributions/base-quit-map)
;;   (keymap-set global-map "C-c w" +distributions/base-window-map)
;;   (keymap-set global-map "C-c p" +distributions/base-project-map)

;;   ;; Define how the keymaps are labelled in `which-key-mode'
;;   (which-key-add-keymap-based-replacements global-map
;;     "C-c f" `("File" . ,+distributions/base-file-map)
;;     "C-c b" `("Buffer" . ,+distributions/base-buffer-map)
;;     "C-c u" `("UI" . ,+distributions/base-ui-map)
;;     "C-c s" `("Search" . ,+distributions/base-search-map)
;;     "C-c h" `("Help" . ,+distributions/base-help-map)
;;     "C-c q" `("Quit" . ,+distributions/base-quit-map)
;;     "C-c w" `("Window" . ,+distributions/base-window-map)
;;     "C-c p" `("Project" . ,+distributions/base-project-map))

;;   ;; ;; Add this immediately after the bind-keys block:
;;   (which-key-add-keymap-based-replacements +distributions/base-file-map
;;     "f" '("Find File" . find-file)
;;     "r" '("Find Recent File" . recentf-open)
;;     "s" '("Save File" . save-buffer)
;;     "p" '("Find Within Project" . project-find-file))

;;   (which-key-add-keymap-based-replacements +distributions/base-buffer-map
;;     "b" '("Switch Buffer" . switch-to-buffer)
;;     "k" '("Kill Buffer" . kill-current-buffer))

;;   (which-key-add-keymap-based-replacements +distributions/base-ui-map
;;     "t" '("Load Theme" . load-theme))

;;   (which-key-add-keymap-based-replacements +distributions/base-help-map
;;     "f" '("Describe Function" . describe-function)
;;     "v" '("Describe Variable" . describe-variable))

;;   (which-key-add-keymap-based-replacements +distributions/base-quit-map
;;     "q" '("Quit Emacs" . save-buffers-kill-terminal))

;;   (which-key-add-keymap-based-replacements +distributions/base-window-map
;;     "/" '("Split Vertically" . split-window-right)
;;     "-" '("Split Horizontally" . split-window-below)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
;;
;;
;; https://github.com/abo-abo/avy
;;
;; Easy navigation within buffers
;;
(when +distributions/base-enable-avy
  (use-package avy
    :ensure t    
    :bind (("M-j" . avy-goto-char-timer)
           ("M-g g" . avy-goto-line)
           ("M-g l" . +reflex/jump-to-line)
           ("M-g M-g" . avy-goto-line))))

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
  :bind ("C-c t d" . dimmer-mode)  ; Toggle dimmer on/off
  :config
  (dimmer-mode t))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple cursors for Emacs 
;;
;; https://melpa.org/#/multiple-cursors
;;
(when +distributions/base-enable-mc
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
           ("C-c C-<" . mc/skip-to-previous-like-this))))

;; Unkillable Scratch 
(when +distributions/base-unkillable-scratch
  (add-hook 'kill-buffer-query-functions #'+distributions/base-make-scratch-unkillable))

(use-package ultra-scroll)
