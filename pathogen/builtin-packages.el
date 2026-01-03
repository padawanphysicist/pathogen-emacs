(defvar pathogen--base-cache-dir
  (cond ((getenv "PATHOGEN_CACHE_DIR") (getenv "PATHOGEN_CACHE_DIR"))
        (t (expand-file-name "cache/" user-emacs-directory)))
  "Directory where Pathogen stores cache files.
This includes savehist, recentf, and other persistent data.
Users can customize this to store cache in a different location. Can be configured using the environment variable PATHOGEN_CACHE_DIR.")

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
  (which-key-add-column-padding 2)
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
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-events-buffer-size 0)
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
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l o" . eglot-code-action-organize-imports)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save minibuffer history
;;
;;
(use-package savehist
  :ensure nil
  :custom
  (savehist-file (expand-file-name "savehist" pathogen--base-cache-dir))
  (savehist-save-minibuffer-history t)
  ;; Auto-save history every 5 minutes to protect against crashes.
  ;; Setting to nil would only save on Emacs exit, risking data loss.
  ;; 300 seconds (5 minutes) balances protection with minimal I/O overhead.
  (savehist-autosave-interval 300)
  (savehist-additional-variables
   '(
     ;; persist clipboard
     kill-ring
     ;; persist macros
     register-alist
     ;; persist marks
     mark-ring global-mark-ring
     ;; persist searches
     search-ring regexp-search-ring))
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
  :custom
  ;; Store recentf file in cache directory for cleaner organization
  (recentf-save-file (expand-file-name "recentf" pathogen--base-cache-dir))
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  ;; Cleanup recent files list periodically during idle time.
  ;; This removes deleted/moved files automatically without impacting startup.
  ;; The value is in seconds - 600 (10 minutes) provides automatic cleanup
  ;; without being too aggressive.
  ;;
  ;; Options:
  ;;   'mode   - Cleanup at startup (default, can slow startup)
  ;;   'never  - No automatic cleanup (use if working with remote files)
  ;;   NUMBER  - Cleanup after N seconds of idle time
  (recentf-auto-cleanup 600)
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
  ;;(advice-add #'completing-read-multiple :filter-args #'+distributions/base-crm-indicator)

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





(provide 'builtin-packages)
