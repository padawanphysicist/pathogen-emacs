;;; pathogen-better-defaults.el --- Better defaults for Emacs -*- lexical-binding: t; -*-

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
;; A collection of sensible defaults to improve Emacs ergonomics.
;; This module avoids external dependencies, focusing on core
;; improvements.

;;; Code:

(defgroup better-defaults nil
  "Customizations for improved Emacs defaults."
  :group 'convenience)

;;; Standardizing defaults
(use-package emacs
  :ensure nil
  :custom
  ;; UI: Cleanup visual clutter
  ;; (menu-bar-mode -1)
  ;; (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  ;;(column-number-mode t)

    ;; --- Visuals & Cursor ---
  (x-stretch-cursor nil)                      ; Do not stretch cursor to fit wide characters
  ;; Set indicators for wrapped lines in the fringe margins
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  ;; --- Mouse & Keyboard Scrolling ---
  ;; Ensure the mouse wheel scrolls the window directly underneath the pointer
  (mouse-wheel-follow-mouse t)
  ;; Scroll exactly 1 line at a time to prevent jarring page jumps
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  ;; Horizontal scrolling increment
  (mouse-wheel-scroll-amount-horizontal 2)
  ;; Disable progressive speed (prevents acceleration based on scroll velocity)
  (mouse-wheel-progressive-speed nil)
  ;; Keyboard scrolling step increment (1 line at a time)
  (scroll-step 1)
  ;; Prevent sudden view recentering when the cursor moves past window edges
  (scroll-conservatively 101)
  (scroll-margin 0)
  ;; Keep the cursor at the same screen position when page-scrolling
  (scroll-preserve-screen-position t)

  (project-list-file (expand-file-name "project" pathogen-cache-directory))

  ;; --- Interface & Minibuffer ---
  (column-number-mode t)                      ; Show column number in the mode-line
  (use-short-answers t)                       ; Use short "y/n" answers instead of "yes/no"
  (context-menu-mode t)                       ; Enable right-click context menu
  (enable-recursive-minibuffers t)            ; Allow opening a minibuffer within another minibuffer
  ;; Hide commands that do not work in the current major mode from `M-x'
  (read-extended-command-predicate #'command-completion-default-include-p)
  (use-dialog-box nil)                        ; Avoid graphical GUI dialog boxes
  (echo-keystrokes 0.02)                      ; Show current key sequence in minibuffer immediately
  (resize-mini-windows nil)                   ; Keep the echo/minibuffer area at a fixed height
     ;; --- Backup & Auto-Save ---
  ;; Activate standard auto-save safety mechanism
  (auto-save-default t)

  ;; Save the buffer directly upon 5 seconds of inactivity
  (auto-save-visited-mode 1)
  (auto-save-visited-interval 5)

  ;; Centralize all auto-save (#file#) files into the custom cache directory
  (auto-save-file-name-transforms `((".*" ,pathogen-cache-directory t)))

  ;; Centralize all backup (file~) files into the custom cache directory
  (backup-directory-alist `((".*" . ,pathogen-cache-directory)))

  :config

  ;; Editing: Modern habits
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (fset 'yes-or-no-p #'y-or-n-p)

  ;; Clipboard & System Integration
  (setq select-enable-clipboard t
        mouse-yank-at-point t)


  (global-auto-revert-mode t))

(provide 'pathogen-better-defaults)
;;; pathogen-better-defaults.el ends here
