;;; 01-editor.el --- Enhanced core editing experience -*- lexical-binding: t; -*-
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
;; Module: Editor (01)
;; Purpose: Core editor behavior and built-in features
;; Dependencies: None (built-in features only)
;; Provides: Editor fundamentals, custom hooks
;;
;; Settings to enhance the basic Emacs editing experience using only built-in
;; features. This module configures indentation, file handling, history, and
;; other fundamental editor behaviors.
;;
;;; Code:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard/kill-ring
;;
;;
;; Remove duplicates in the kill ring to reduce bloat
;; and make the kill ring easier to peruse (with
;; `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)
;; Allow UTF or composed text from the clipboard, even
;; in the terminal or on non-X systems (like Windows or
;; macOS), where only `STRING' is used.
(setq
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor tweaks
;;
;;
;; An archaic default in the age of widescreen 4k displays? I disagree. We
;; still frequently split our terminals and editor frames, or have them
;; side-by-side, using up more of that newly available horizontal
;; real-estate.
(setq-default fill-column 80)
;; The POSIX standard defines a line is "a sequence of zero or more
;; non-newline characters followed by a terminating newline", so files
;; should end in a newline. Windows doesn't respect this (because it's
;; Windows), but we should, since programmers' tools tend to be POSIX
;; compliant (and no big deal if not).
(setq require-final-newline t)
;; Delete whatever is selected if typing starts This reflects the behavior
;; of other editors.
(delete-selection-mode 1)
;; Display current column in modeline
(setq column-number-mode t)
;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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
(setq auto-save-default nil)        ; Disable traditional auto-save
(auto-save-visited-mode 1)          ; Enable modern auto-save
(setq auto-save-visited-interval 5) ; Save every 5 seconds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in plugins
;;
;;
;; These packages are part of GNU Emacs and therefore don't require any package
;; management.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save minibuffer history
;;
;;
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :custom
  (savehist-file (concat pathogen-cache-directory "savehist"))
  (savehist-save-minibuffer-history t)
  ;; save on kill only
  (savehist-autosave-interval nil)
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
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Show matching parenthesis
;;
;;
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom hooks
;;
;;
;; Define hook run after font resize
(defvar after-text-scale-hook nil
  "Hook run after text is rescaled.
This hook is triggered after `text-scale-increase', `text-scale-decrease',
or `text-scale-set' commands are executed.

Example usage:
  (add-hook 'after-text-scale-hook
            (lambda ()
              (message \"Text scaled to: %s\" text-scale-mode-amount)))")

;; Make the hook functional by advising text scaling functions
(defun pathogen--run-after-text-scale-hook (&rest _args)
  "Run `after-text-scale-hook' after text scaling."
  (run-hooks 'after-text-scale-hook))

(advice-add 'text-scale-increase :after #'pathogen--run-after-text-scale-hook)
(advice-add 'text-scale-decrease :after #'pathogen--run-after-text-scale-hook)
(advice-add 'text-scale-set :after #'pathogen--run-after-text-scale-hook)

;; Define hook run after theme loading
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

;; Use modern advice-add instead of deprecated defadvice (obsolete since Emacs 24.4)
(defun pathogen--run-after-load-theme-hook (&rest _args)
  "Run `after-load-theme-hook' after loading a theme."
  (run-hooks 'after-load-theme-hook))

(advice-add 'load-theme :after #'pathogen--run-after-load-theme-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sticky keys
;;
;;
;; Note: Emacs does not have built-in sticky keys support. The variable
;; `modifier-keys-are-sticky` does not exist in standard Emacs and has been
;; removed as it had no effect.
;;
;; If you need sticky keys functionality:
;;   - Use OS-level sticky keys (recommended for system-wide support)
;;     * macOS: System Preferences → Accessibility → Keyboard
;;     * Linux: Settings → Universal Access → Typing Assist
;;     * Windows: Settings → Accessibility → Keyboard
;;   - Consider Emacs packages like `god-mode` or `key-chord` for modal editing
;; See also: https://www.emacswiki.org/emacs/StickyModifiers

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
(setq jit-lock-stealth-time 0.2)

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
(setq custom-file (concat user-emacs-directory "custom.el"))

(provide '01-editor)
;;; 01-editor.el ends here
