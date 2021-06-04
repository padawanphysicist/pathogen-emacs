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
;;  Settings for enhance basic emacs experience.
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
(setq-default tab-width 8)
(setq tab-always-indent 'complete)

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
(setq x-select-request-type
    '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

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
    `((".*" ,temporary-file-directory t)))

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
(setq savehist-file (concat pathogen-cache-directory "savehist"))
(savehist-mode 1)
(setq
    savehist-save-minibuffer-history t
    ;; save on kill only
    savehist-autosave-interval nil    
    savehist-additional-variables
    '(
         ;; persist clipboard
         kill-ring  
	 ;; persist macros
	 register-alist
	 ;; persist marks
	 mark-ring global-mark-ring       
	 ;; persist searches
	 search-ring regexp-search-ring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo/Redo window configuration
;;
;;
(winner-mode 1)

(provide '01-editor)
;;; 01-editor.el ends here
