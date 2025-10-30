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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save minibuffer history
;;
;;
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :custom
  (savehist-file (expand-file-name "savehist" pathogen--cache-dir))
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
     search-ring regexp-search-ring)))

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
  (recentf-mode 1)
  :custom
  ;; Store recentf file in cache directory for cleaner organization
  (recentf-save-file (expand-file-name "recentf" pathogen--cache-dir))
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
  (recentf-auto-cleanup 600))

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
  :custom
  ;; uses shorter answers "y" or "n".
  (use-short-answers t)
  ;; `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t "Enable context menu.")
  (enable-recursive-minibuffers t "Enable recursive minibuffers")
  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; TAB cycle if there are only few candidates
  ;;(setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; The POSIX standard defines a line is "a sequence of zero or more
  ;; non-newline characters followed by a terminating newline", so files
  ;; should end in a newline. Windows doesn't respect this (because it's
  ;; Windows), but we should, since programmers' tools tend to be POSIX
  ;; compliant (and no big deal if not).
  (require-final-newline t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Clipboard/kill-ring
  ;;
  ;;
  ;; Remove duplicates in the kill ring to reduce bloat
  ;; and make the kill ring easier to peruse (with
  ;; `counsel-yank-pop' or `helm-show-kill-ring'.
  (kill-do-not-save-duplicates t)
  
  ;; Allow UTF or composed text from the clipboard, even
  ;; in the terminal or on non-X systems (like Windows or
  ;; macOS), where only `STRING' is used.
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; Display current column in modeline
  (column-number-mode t)
  
  ;; Store all backup and autosave files in the tmp dir
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Just-in-time syntax highlighting
  ;;
  ;;
  ;; JIT Lock mode is the default font-lock support mode in Emacs. It fontifies
  ;; (highlights syntax) text on-demand as it becomes visible.
  ;;
  ;; The `jit-lock-stealth-time' variable controls when Emacs fontifies text that
  ;; is not currently visible. By default, it's set to a high value (16 seconds),
  ;; meaning Emacs waits a long time before fontifying off-screen text.
  ;;
  ;; Setting this to a lower value (0.2 seconds) improves responsiveness when
  ;; scrolling through large files, as more text will already be fontified.
  ;;
  ;; Trade-off:
  ;;   - Lower values: Better scrolling experience, slightly more CPU usage
  ;;   - Higher values: Less CPU usage, potential delay when scrolling
  ;;
  ;; For modern systems, 0.2 seconds provides a good balance.
  (jit-lock-stealth-time 0.2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Custom init file
  ;;
  ;;
  ;; By default, Emacs stores any configuration you make through its UI by writing
  ;; custom-set-variables invocations to your init file, or to the file specified
  ;; by custom-file. Though this is convenient, it's also an excellent way to
  ;; cause aggravation when the variable you keep trying to modify is being set in
  ;; some custom-set-variables invocation.
  ;;
  ;; We set custom-file to a separate file to keep init.el clean. Using
  ;; user-emacs-directory ensures portability across different systems.
  (custom-file (concat user-emacs-directory "custom.el"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Auto-save configuration
  ;;
  ;;
  ;; Modern auto-save using auto-save-visited-mode (Emacs 26+).
  ;;
  ;; Unlike traditional auto-save which creates #filename# backup files,
  ;; auto-save-visited-mode saves the actual file periodically. This provides
  ;; a cleaner, more modern editor experience similar to VS Code, Sublime, etc.
  ;;
  ;; BEHAVIOR:
  ;;   - Saves actual files every N seconds of idle time
  ;;   - No #filename# clutter in file system
  ;;   - File is always up-to-date on disk
  ;;   - No recovery needed after crash (file already saved)
  ;;
  ;; BENEFITS:
  ;;   - Clean file system (no auto-save backup files)
  ;;   - Modern UX (like contemporary editors)
  ;;   - Works naturally with version control
  ;;   - Simpler mental model (one source of truth)
  ;;
  ;; TRADE-OFFS:
  ;;   - No separate recovery file (file IS the recovery)
  ;;   - May save broken/incomplete code
  ;;   - Can trigger file watchers frequently
  ;;   - Less control over when changes persist
  ;;
  ;; The 5-second interval balances protection with performance. For more
  ;; conservative behavior, increase the interval (e.g., 10 or 30 seconds).
  ;;
  ;; TRADITIONAL ALTERNATIVE:
  ;; If you prefer traditional #filename# auto-save with recovery mechanism:
  ;;   (setq auto-save-default t)
  ;;   (setq auto-save-interval 200)
  ;;   (setq auto-save-timeout 20)
  ;;
  (auto-save-default nil)        ; Disable traditional auto-save  
  (auto-save-visited-interval 5) ; Save every 5 seconds

  
  :init
  (auto-save-visited-mode 1)          ; Enable modern auto-save
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  ;;(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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
  (delete-selection-mode 1)
  

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom hooks
;;
;;
;; Define hook run after font resize
(advice-add 'text-scale-increase :after #'pathogen--run-after-text-scale-hook)
(advice-add 'text-scale-decrease :after #'pathogen--run-after-text-scale-hook)
(advice-add 'text-scale-set :after #'pathogen--run-after-text-scale-hook)
(advice-add 'load-theme :after #'pathogen--run-after-load-theme-hook)

