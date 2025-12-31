(defvar +distributions/base-cache-dir
  (cond ((getenv "PATHOGEN_CACHE_DIR") (getenv "PATHOGEN_CACHE_DIR"))
        (t (expand-file-name "cache/" user-emacs-directory)))
  "Directory where Pathogen stores cache files.
This includes savehist, recentf, and other persistent data.
Users can customize this to store cache in a different location. Can be configured using the environment variable PATHOGEN_CACHE_DIR.")

;; Toggles
(defvar +distributions/base-enable-avy t
  "Enable fast jumping across the screen.")
(defvar +distributions/base-enable-mc t
  "Enable multiple-cursors for simultaneous editing.")
(defvar +distributions/base-unkillable-scratch t
  "Prevent the *scratch* buffer from being killed.")
(defvar +distributions/base-default-theme 'modus-vivendi-tinted
  "Default theme for Pathogen")

;; Which-key
(pathogen--defvars-with-aliases! +distributions/base
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
  (which-key-use-C-h-for-paging t))

;; Eglot
(pathogen--defvars-with-aliases!
 +distributions/base
 (eglot-autoshutdown t)
 (eglot-sync-connect nil)
 (eglot-events-buffer-size 0))

;; savehist
(pathogen--defvars-with-aliases!
 +distributions/base
 (savehist-file (expand-file-name "savehist" +distributions/base-cache-dir))
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

;; recentf
(pathogen--defvars-with-aliases!
 +distributions/base
 ;; Store recentf file in cache directory for cleaner organization
 (recentf-save-file (expand-file-name "recentf" +distributions/base-cache-dir))
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

;; dimmer
(pathogen--defvars-with-aliases!
 +distributions/base
 (dimmer-fraction 0.5))

;; emacs default vars
(pathogen--defvars-with-aliases!
 +distributions/base
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
 )

(pathogen--defvars-with-aliases!
 +distributions/base
 (avy-timeout-seconds 1)
 ;; Case sensitive search
 (avy-case-fold-search nil))
