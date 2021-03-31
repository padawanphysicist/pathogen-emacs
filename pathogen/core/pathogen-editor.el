;;; pathogen-editor.el --- Enhanced core editing experience -*- lexical-binding: t; -*-
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
;;; Code:

(defvar pathogen-editor-hook nil
  "Enhance editing experience.")

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
(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
     (setq-default indent-tabs-mode nil)
     (setq-default tab-width 8)
     (setq tab-always-indent 'complete))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clipboard/kill-ring
;;
;;
(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
     ;; Remove duplicates in the kill ring to reduce bloat
     ;; and make the kill ring easier to peruse (with
     ;; `counsel-yank-pop' or `helm-show-kill-ring'.
     (setq kill-do-not-save-duplicates t)
     ;; Allow UTF or composed text from the clipboard, even
     ;; in the terminal or on non-X systems (like Windows or
     ;; macOS), where only `STRING' is used.
     (setq x-select-request-type
           '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor tweaks
;;
;;
(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
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
     ;; Default to soft line-wrapping in text modes. It is more sensibile for
     ;; text modes, even if hard wrapping is more performant.
     (visual-line-mode 1)
     ;; Delete whatever is selected if typing starts This reflects the behavior
     ;; of other editors.
     (delete-selection-mode 1)
     ;; Store all backup and autosave files in the tmp dir
     (setq backup-directory-alist
           `((".*" . ,temporary-file-directory)))
     (setq auto-save-file-name-transforms
           `((".*" ,temporary-file-directory t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in plugins
;;
;;
;; These packages are part of GNU Emacs and therefore don't require any package
;; management.

;;; Auto revert
;;
;;
;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, overuse
;; file watchers or constantly poll your buffer list. Too many watchers can
;; bring Emacs to a halt if you preform expensive or batch processes on files
;; outside of Emacs (e.g. their mtime changes), and polling your buffer list is
;; terribly inefficient as your buffer list grows into the hundreds.
;;
;; The strategy here is do like Doom and do this lazily. i.e. All visible
;; buffers are reverted immediately when
;;    - a file is saved, or
;;    - Emacs is refocused (after using another app).
;; Meanwhile, buried buffers are reverted only when they are switched to. This
;; way, Emacs only ever has to operate on, at minimum, a single buffer and, at
;; maximum, ~10 buffers.
;;;###autoload
(defun pathogen-visible-buffers ()
  "Return a list of currently visible buffers."
  (cl-remove-if-not
   (lambda (b)
     (get-buffer-window b))
   (buffer-list)))

;;;###autoload
(defun pathogen-auto-revert-buffer ()
  "Auto revert current buffer, if necessary."
  (unless (or auto-revert-mode (active-minibuffer-window))
    (let ((auto-revert-mode t))
      (auto-revert-handler))))

;;;###autoload
(defun pathogen-auto-revert-buffers ()
  "Auto revert stale buffers in visible windows, if necessary."
  (mapc
   (lambda (b)
     (with-current-buffer b
       (pathogen-auto-revert-buffer)))
   (pathogen-visible-buffers)))

(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
     (setq
      ;; Let us know when it happens
      auto-revert-verbose t
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil
      ;; Only prompts for confirmation when buffer is unsaved.
      revert-without-query (list "."))
     (add-hook 'focus-in-hook #'pathogen-auto-revert-buffers)
     (add-hook 'after-save-hook #'pathogen-auto-revert-buffers)
     ;; Revert buffers automatically when underlying files are changed
     ;; externally
     (global-auto-revert-mode t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recent files
;;
;;
(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
     (setq
      recentf-filename-handlers
      '(;; Text properties inflate the size of recentf's files, and there is
        ;; no purpose in persisting them, so we strip them out.
        substring-no-properties   
        ;; Replace $HOME with ~, which is more portable, and reduces how much
        ;; horizontal space the recentf listing uses to list recent files.
        abbreviate-file-name)
      recentf-save-file (concat pathogen/cache-dir "recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 10
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
     (recentf-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save minibuffer history
;;
;;
(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
     (setq savehist-file (concat pathogen/cache-dir "savehist"))
     (savehist-mode 1)
     (setq savehist-save-minibuffer-history t
           ;; save on kill only
           savehist-autosave-interval nil    
           savehist-additional-variables
           ;; persist clipboard
           '(kill-ring                       
             ;; persist macros
             register-alist
             ;; persist marks
             mark-ring global-mark-ring       
             ;; persist searches
             search-ring regexp-search-ring)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save location history
;;
;;
(add-hook
 'pathogen-editor-hook
 (lambda ()
   (progn
     (setq save-place-file (concat pathogen/cache-dir "saveplace"))
     (save-place-mode 1)
     (setq save-place-limit 100))))

(provide 'pathogen-editor.el)
;;; pathogen-editor.el ends here
