;;; 04-custom-functions.el --- Custom functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; Module: Custom Functions (04)
;; Purpose: User-facing commands and utility functions
;; Dependencies: Can use any previous module
;; Provides: pathogen/* commands available to users
;;
;; Functions to be available to all custom configuration. This module provides
;; interactive commands and helper functions used across the configuration.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font Configuration Example
;;
;; The pathogen/set-font function below helps you set fonts with fallback
;; support. However, it's important to call it at the right time to ensure
;; it works correctly in all scenarios (GUI, daemon mode, multiple frames).
;;
;; RECOMMENDED USAGE PATTERN:
;; Add this to your personal configuration (~/.pathogen.el or ~/.pathogen.d/):
;;
;;   (defun my-setup-fonts ()
;;     "Set up fonts for the current frame."
;;     (when (display-graphic-p)
;;       (pathogen/set-font '(("JetBrains Mono" . 12)
;;                           ("Fira Code" . 12)
;;                           ("Monospace" . 11)))))
;;
;;   ;; For regular Emacs startup
;;   (add-hook 'after-init-hook #'my-setup-fonts)
;;
;;   ;; For daemon mode - ensures new frames get the font
;;   (add-hook 'server-after-make-frame-hook #'my-setup-fonts)
;;
;; WHY USE HOOKS?
;; - Ensures display system is initialized before setting fonts
;; - Works correctly in daemon mode (emacs --daemon)
;; - Applies fonts to all new frames, not just the first one
;; - Guards against errors in terminal-only Emacs
;;
;; IMPORTANT:
;; - Don't call pathogen/set-font directly at top-level in init files
;; - Always check (display-graphic-p) before calling
;; - Include a generic fallback font (e.g., "Monospace") in your list
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pathogen/user-config ()
  "Open user configuration directory in Dired.

Opens the directory specified by `pathogen-config-directory' where
user-specific configuration files are stored. This is useful for
quickly accessing and editing your personal Emacs configuration.

See also `pathogen/devel-config' for accessing the main Pathogen
configuration directory."
  (interactive)
  (dired pathogen-config-directory))

(defun pathogen/devel-config ()
  "Open Pathogen development configuration directory in Dired.

Opens the main Pathogen configuration directory (USER-EMACS-DIRECTORY/pathogen)
containing the core numbered configuration modules. This is where the main
system configuration files are located (00-user-interface.el, 01-editor.el, etc.).

See also `pathogen/user-config' for accessing user-specific configuration files."
  (interactive)
  (dired (concat user-emacs-directory "pathogen")))

(defun pathogen/set-font (font-alist &optional original-alist)
  "Set the first available font from FONT-ALIST as default font.

FONT-ALIST is a list of (FONT-NAME . FONT-SIZE) cons cells.
The function tries each font in order and uses the first available one.

If no fonts are available, displays a warning and falls back to default.

TIMING AND USAGE:
This function should be called from appropriate hooks to ensure fonts
are set correctly in all scenarios (GUI, daemon mode, multiple frames).

Recommended approach:
  (defun my-setup-fonts ()
    \"Set up fonts for the current frame.\"
    (when (display-graphic-p)
      (pathogen/set-font '((\"JetBrains Mono\" . 12)
                          (\"Fira Code\" . 12)
                          (\"Monospace\" . 11)))))

  (add-hook 'after-init-hook #'my-setup-fonts)
  (add-hook 'server-after-make-frame-hook #'my-setup-fonts)

See the \"Font Configuration Example\" section above for detailed
explanation of why hooks are necessary.

IMPORTANT:
- Always wrap calls in (when (display-graphic-p) ...)
- Don't call directly at top-level in init files
- For daemon mode, use server-after-make-frame-hook
- Include a generic fallback font like \"Monospace\"

Returns the font that was set as (FONT-NAME . FONT-SIZE), or nil if
none were available."
  (let ((frame (selected-frame))
        (orig-list (or original-alist font-alist)))
    (cond
     ;; Base case: empty list - no fonts available
     ((null font-alist)
      (when original-alist
        ;; Only warn if we actually tried fonts (recursive call)
        (display-warning 'pathogen
                         (format "None of the requested fonts are available: %s\nUsing system default font."
                                 (mapconcat (lambda (f) (format "\"%s\"" (car f)))
                                           original-alist
                                           ", "))
                         :warning))
      nil)

     ;; Check if current font exists
     ((x-list-fonts (caar font-alist))
      (let ((font-name (caar font-alist))
            (font-size (cdar font-alist)))
        (set-frame-font (format "%s-%d" font-name font-size) t t frame)
        (message "Set font to %s at size %d" font-name font-size)
        ;; Return the font that was set
        (cons font-name font-size)))

     ;; Current font not available, try next
     (t (pathogen/set-font (cdr font-alist) orig-list)))))  ; Recurse with original list

(provide '04-custom-functions)
;;; functions.el ends here
