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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration Management Utilities
;;
;; Functions for managing and maintaining your Pathogen Emacs configuration.
;; These utilities help with common tasks like reloading config, rebuilding
;; packages, clearing cache, and troubleshooting issues.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pathogen/reload-config ()
  "Reload Pathogen Emacs configuration.

This function reloads all core configuration modules without restarting
Emacs. Useful for testing configuration changes quickly.

WARNING: This may not work perfectly for all changes (especially package
configurations). For major changes, consider restarting Emacs.

Note: User configuration files (~/.pathogen.el, ~/.pathogen.d/) are also
reloaded if they exist."
  (interactive)
  (message "Reloading Pathogen configuration...")
  (let ((start-time (current-time)))
    ;; Reload shared variables
    (load (concat user-emacs-directory "pathogen-vars.el"))

    ;; Reload each module
    (dolist (module '(00-user-interface
                      01-editor
                      02-package-manager
                      03-setup-packages
                      04-custom-functions))
      (load (concat user-emacs-directory "pathogen/"
                    (symbol-name module) ".el")))

    ;; Reload user configuration
    (when (file-exists-p pathogen-config-directory)
      (mapc #'load-file (file-expand-wildcards
                         (concat pathogen-config-directory "*.el"))))
    (when (file-exists-p pathogen-config-file)
      (load-file pathogen-config-file))

    (message "Configuration reloaded in %.2fs"
             (float-time (time-subtract (current-time) start-time)))))

(defun pathogen/rebuild-packages ()
  "Rebuild all Elpaca packages.

This forces a complete rebuild of all installed packages, which can fix
issues caused by:
- Emacs version upgrades
- Package compilation errors
- Corrupted package bytecode

WARNING: This will take several minutes to complete."
  (interactive)
  (when (yes-or-no-p "Rebuild all packages? This will take several minutes. ")
    (message "Rebuilding all packages...")
    (elpaca-rebuild)
    (message "Package rebuild initiated. Check *elpaca-log* for progress.")))

(defun pathogen/clear-cache (&optional force)
  "Clear Pathogen cache directory.

This removes saved history files (savehist, recentf) and other cache data.
Useful when cache files become corrupted or too large.

With prefix argument FORCE (\\[universal-argument]), skip confirmation prompt.

WARNING: This will delete your command history, recent files list, and
other cached data. They will be recreated on next Emacs restart."
  (interactive "P")
  (when (or force
            (yes-or-no-p
             (format "Delete all cache files in %s? "
                     pathogen-cache-directory)))
    (if (file-directory-p pathogen-cache-directory)
        (progn
          (delete-directory pathogen-cache-directory t)
          (make-directory pathogen-cache-directory t)
          (message "Cache cleared. Restart Emacs to recreate cache files."))
      (message "Cache directory does not exist: %s"
               pathogen-cache-directory))))

(defun pathogen/reset-to-defaults ()
  "Reset Pathogen configuration to defaults.

This removes:
- User configuration files (~/.pathogen.el, ~/.pathogen.d/)
- Custom.el file (customization settings)
- Cache directory (history, recent files)

WARNING: This is DESTRUCTIVE and cannot be undone! Make backups first.

After reset, Emacs will start with base Pathogen configuration only."
  (interactive)
  (when (yes-or-no-p "DESTRUCTIVE: Reset to defaults? This cannot be undone! ")
    (when (yes-or-no-p "Are you ABSOLUTELY sure? ")
      (let ((deleted-items '()))
        ;; Remove user config file
        (when (file-exists-p pathogen-config-file)
          (delete-file pathogen-config-file)
          (push pathogen-config-file deleted-items))

        ;; Remove user config directory
        (when (file-directory-p pathogen-config-directory)
          (delete-directory pathogen-config-directory t)
          (push pathogen-config-directory deleted-items))

        ;; Remove custom.el
        (let ((custom-file-path (concat user-emacs-directory "custom.el")))
          (when (file-exists-p custom-file-path)
            (delete-file custom-file-path)
            (push custom-file-path deleted-items)))

        ;; Remove cache
        (when (file-directory-p pathogen-cache-directory)
          (delete-directory pathogen-cache-directory t)
          (push pathogen-cache-directory deleted-items))

        (if deleted-items
            (message "Reset complete. Deleted:\n%s\n\nRestart Emacs to complete reset."
                     (mapconcat #'identity deleted-items "\n"))
          (message "Nothing to reset - already at defaults."))))))

(defun pathogen/validate-config ()
  "Check configuration for common issues.

Validates:
- All required directories exist and are writable
- All core modules can be loaded
- Cache files are accessible

Displays a report of any issues found."
  (interactive)
  (message "Validating configuration...")
  (let ((issues '()))

    ;; Check cache directory
    (unless (file-directory-p pathogen-cache-directory)
      (push "Cache directory does not exist" issues))
    (when (and (file-directory-p pathogen-cache-directory)
               (not (file-writable-p pathogen-cache-directory)))
      (push "Cache directory is not writable" issues))

    ;; Check modules exist
    (dolist (module '(00-user-interface 01-editor 02-package-manager
                      03-setup-packages 04-custom-functions))
      (let ((module-file (concat user-emacs-directory "pathogen/"
                                (symbol-name module) ".el")))
        (unless (file-exists-p module-file)
          (push (format "Module missing: %s" module) issues))))

    ;; Check for failed modules
    (when pathogen--failed-modules
      (push (format "%d modules failed to load"
                    (length pathogen--failed-modules)) issues))

    ;; Display results
    (if issues
        (display-warning 'pathogen
                         (format "Configuration issues found:\n%s"
                                 (mapconcat (lambda (i) (concat "  - " i))
                                           issues "\n"))
                         :warning)
      (message "✓ Configuration validation passed - no issues found."))))

(provide '04-custom-functions)
;;; functions.el ends here
