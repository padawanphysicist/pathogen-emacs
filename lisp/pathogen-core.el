;;; pathogen-core.el --- Pathogen core configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code

;;;; Built-in package configuration

;;;; Minibuffer & Command History Persistence (Savehist)
;;
;; `savehist' is a built-in feature that provides Emacs with a long-term memory.
;; It persists your minibuffer history, search inputs, and complex variables
;; across sessions, ensuring you never lose your command context when restarting.
;;
;; Key features enabled below:
;; - Persistence: Saves all historical data into the Emacs directory.
;; - Auto-save: Periodically flushes history to disk every 5 minutes to prevent
;;   data loss during sudden closures.
;; - Extended Context: Beyond basic commands, it explicitly backs up crucial
;;   global states including the `kill-ring' (clipboard history), registers,
;;   markers, and search rings (both standard and regex).

(use-package savehist
  :ensure nil
  :custom
  (savehist-file (expand-file-name "savehist" pathogen-cache-directory))
  (savehist-save-minibuffer-history t)
  ;; Automatically save at each 5min
  (savehist-autosave-interval 300)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     mark-ring global-mark-ring
     search-ring regexp-search-ring))
  :init
  (savehist-mode 1))

;;;; Recent Files History (Recentf)
;;
;; `recentf' is a built-in minor mode that maintains a persistent history of
;; recently opened files across Emacs sessions. It acts as an editing dashboard,
;; allowing you to quickly jump back into past workflows.
;;
;; Key features enabled below:
;; - Persistence: Saves your recent file list directly into the Emacs directory.
;; - Capacity: Increases the tracked history limit to 50 items for both menus
;;   and saved lists.
;; - Idle Cleanup: Automatically prunes non-existent or deleted files from the
;;   history after 10 minutes of inactivity to keep the list clean.
;; - Fast Access: Binds `C-x C-r' to provide an instant interface for searching
;;   and reopening your recent files.

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file (expand-file-name "recentf" pathogen-cache-directory))
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  ;; Automatic file cleaning after 10min inactivity
  (recentf-auto-cleanup 600)
  :init
  (recentf-mode 1)
  :bind
  ("C-x C-r" . recentf))

;;;; Window Layout History & Undo/Redo (Winner Mode)
;;
;; `winner' is a built-in global minor mode that records changes in the window
;; configurations (how your screen is split). It allows you to "undo" and
;; "redo" window layouts seamlessly if a package or command accidentally
;; disrupts your preferred workspace setup.
;;
;; Default Keybindings:
;; - `C-c <left>'  : Undo the last window change (restore previous layout).
;; - `C-c <right>' : Redo the window change (go forward in layout history).

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

;;;; project.el

(use-package project
  :ensure nil
  :demand t)

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))


(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;;;; Which-key (Built-in on Emacs >=30.1)
;;
;; https://github.com/justbur/emacs-which-key
;;
;; One of the core philosophies of Emacs is discoverability; it is a famously
;; self-documenting editor (try `C-h ?` to see this in action).
;;
;; However, as you enable more packages and features, keeping track of the
;; absolute plethora of available shortcuts can become overwhelming.
;;
;; `which-key' is a minor mode that solves this by displaying a popup with
;; all possible key bindings that can follow your currently entered prefix.
;; It provides an excellent, contextual way to discover shortcuts globally.

(use-package which-key
  :ensure
  :defer t
  :hook
  (after-init-hook . which-key-mode)
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
  ;; Inspired by: https://gist.github.com/mmarshall540/a12f95ab25b1941244c759b1da24296d
  ;;
  ;; By default, Which-key doesn't give much help for prefix-keys. It
  ;; either shows the generic description, "+prefix", or the name of a
  ;; prefix-command, which usually isn't as descriptive as we'd like.
  ;;
  ;; Here are some descriptions for the default bindings in `global-map'
  ;; and `org-mode-map'.
  (which-key-add-key-based-replacements
    "<f1> 4" "help-other-win"
    "<f1>" "help"
    "<f2>" "2column"
    "C-c" "mode-and-user"
    "C-c !" "flymake"
    "C-c g" "git-gutter"
    "C-h 4" "help-other-win"
    "C-h" "help"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x 6" "2-column"
    "C-x 8" "insert-special"
    "C-x 8 ^" "superscript (⁰, ¹, ², …)"
    "C-x 8 _" "subscript (₀, ₁, ₂, …)"
    "C-x 8 a" "arrows & æ (←, →, ↔, æ)"
    "C-x 8 e" "emojis (🫎, 🇧🇷, 🇮🇹, …)"
    "C-x 8 *" "common symbols ( , ¡, €, …)"
    "C-x 8 =" "macron (Ā, Ē, Ḡ, …)"
    "C-x 8 N" "macron (№)"
    "C-x 8 O" "macron (œ)"
    "C-x 8 ~" "tilde (~, ã, …)"
    "C-x 8 /" "stroke (÷, ≠, ø, …)"
    "C-x 8 ." "dot (·, ż)"
    "C-x 8 ," "cedilla (¸, ç, ą, …)"
    "C-x 8 '" "acute (á, é, í, …)"
    "C-x 8 `" "grave (à, è, ì, …)"
    "C-x 8 \"" "quotation/dieresis (\", ë, ß, …)"
    "C-x 8 1" "†, 1/…"
    "C-x 8 2" "‡"
    "C-x 8 3" "3/…"
    "C-x C-k C-q" "kmacro-counters"
    "C-x C-k C-r a" "kmacro-add"
    "C-x C-k C-r" "kmacro-register"
    "C-x C-k" "keyboard-macros"
    "C-x RET" "encoding/input"
    "C-x a i" "abbrevs-inverse-add"
    "C-x a" "abbrevs"
    "C-x n" "narrowing"
    "C-x p" "projects"
    "C-x r" "reg/rect/bkmks"
    "C-x t ^" "tab-bar-detach"
    "C-x t" "tab-bar"
    "C-x v M" "vc-mergebase"
    "C-x v b" "vc-branch"
    "C-x v" "version-control"
    "C-x w ^" "window-detach"
    "C-x w" "window-extras"
    "C-x x" "buffer-extras"
    "C-x" "extra-commands"
    "M-g" "goto-map"
    "M-s h" "search-highlight"
    "M-s" "search-map"))

;;;; Dired (Directory Editor)
;;
;; Dired is Emacs' built-in, visual directory editor. Far more than a simple
;; file browser, it treats directory listings as editable buffers where you
;; can copy, move, delete, and rename files using standard Emacs commands.
;;
;; Key workflows enabled below:
;; - `wdired' (Writable Dired): Press `C-x C-q' to edit filenames like text,
;;   allowing powerful multi-file renaming using multiple cursors or regex.
;; - Reuse buffers: Prevent Dired from spawning a new buffer for every single
;;   directory you navigate into, keeping the buffer list clean.
;; - Modern defaults: Enable human-readable file sizes, sort directories first,
;;   and handle automatic updates when files change on disk.
;;

(use-package dired
  :ensure nil
  :defer t
  :custom
  ;; Behavior & Performance
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)                                    ; Guess target directory if 2 dired buffers are open
  (dired-kill-when-opening-new-dired-buffer t)             ; Keep buffer list clean (Emacs 28+)

  ;; Visuals & Sorting
  (dired-listing-switches "-alh --group-directories-first")
  (dired-hide-details-hide-absolute-location t)            ; Clean header (Emacs 31+)
  (image-dired-dir pathogen-cache-directory)

  ;; Wdired (Writable Dired - integrated settings)
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)

  :config
  ;; Enable Dired-X features (like omitting files)
  (use-package dired-x
    :ensure nil
    :custom
    ;; Hide dotfiles but preserve navigation to `.` and `..`
    (dired-omit-files "^\\.?#\\|^\\.[^.]"))

  ;; Automatically enable omitting when opening Dired (C-x M-o)
  :hook (dired-mode . dired-omit-mode))

;;;; Syntax Delineation & Auto-Pairing (Electric & Paren)
;;
;; This section manages the automatic pairing and visual highlighting of
;; structural delimiters (parentheses, brackets, braces, and quotes).
;;
;; Packages configured:
;; - `electric-pair': Automatically inserts matching closing delimiters as you
;;   type, keeping code structures balanced without manual repetition.
;; - `paren' (`show-paren-mode'): Instantly highlights the matching counterpart
;;   of the delimiter under the cursor. It includes offscreen context support
;;   to show matching headers when the opening pair is out of view.

(use-package electric-pair
  :ensure nil
  :defer
  :hook (after-init-hook . electric-pair-mode))

(use-package paren
  :ensure nil
  :hook (after-init-hook . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t))

(use-package pathogen-goto-line-numbers
  :load-path "utils/"
  :config
  (pathogen-goto-line-numbers-mode))


(provide 'pathogen-core)
;;; pathogen-core.el ends here
