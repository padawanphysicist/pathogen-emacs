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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging and Profiling Helpers
;;
;; Advanced diagnostic functions for troubleshooting performance issues,
;; checking package status, and analyzing configuration behavior.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pathogen/profile-init ()
  "Profile Emacs initialization to identify slow parts.

Uses Emacs built-in profiler to measure CPU usage during init.
This provides detailed information about which functions consume
the most time during startup.

Generates a command to run Emacs with profiling enabled. The command
is copied to the kill ring for easy execution."
  (interactive)
  (let ((profile-command
         (format "emacs -Q --eval '(progn (profiler-start (quote cpu)) (load \"%s\") (load \"%s\") (profiler-stop) (profiler-report))'"
                 (concat user-emacs-directory "early-init.el")
                 (concat user-emacs-directory "init.el"))))
    (kill-new profile-command)
    (with-current-buffer (get-buffer-create "*Profile Init Command*")
      (erase-buffer)
      (insert "=== Profile Initialization Command ===\n\n")
      (insert "To profile Emacs initialization, run this command in your terminal:\n\n")
      (insert profile-command)
      (insert "\n\nThis will:\n")
      (insert "1. Start Emacs with clean config (-Q)\n")
      (insert "2. Enable CPU profiler\n")
      (insert "3. Load your configuration\n")
      (insert "4. Display profiler report showing slowest functions\n\n")
      (insert "The command has been copied to your kill ring.\n")
      (special-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Profile command copied to kill ring - paste in terminal to run")))

(defun pathogen/check-package (package)
  "Check detailed status of PACKAGE.

Shows:
- Whether package is installed
- Load status (loaded, not loaded, failed)
- Package location
- Recent errors (if any)

When called interactively, prompts for package name."
  (interactive
   (list (intern (completing-read "Package: "
                                  obarray
                                  (lambda (sym)
                                    (or (featurep sym)
                                        (locate-library (symbol-name sym))))
                                  t))))
  (let ((info (list))
        (pkg-name (symbol-name package)))
    ;; Check if loaded as feature
    (if (featurep package)
        (push (format "✓ Package loaded as feature: %s" package) info)
      (push (format "✗ Package NOT loaded as feature: %s" package) info))

    ;; Check library location
    (let ((lib-path (locate-library pkg-name)))
      (if lib-path
          (push (format "✓ Library found: %s" lib-path) info)
        (push (format "✗ Library not found in load-path") info)))

    ;; Check if in failed modules
    (let ((failed (assoc package pathogen--failed-modules)))
      (when failed
        (push (format "✗ ERROR: Failed to load\n    %s"
                     (error-message-string (cdr failed))) info)))

    ;; Check if autoloaded
    (when (and (fboundp package) (autoloadp (symbol-function package)))
      (push "⚡ Autoloaded (not yet triggered)" info))

    ;; Display results
    (let ((result (mapconcat #'identity (reverse info) "\n")))
      (with-current-buffer (get-buffer-create "*Package Status*")
        (erase-buffer)
        (insert "=== Package Status ===\n\n")
        (insert result)
        (insert "\n")
        (special-mode)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      result)))

(defun pathogen/show-load-times ()
  "Display module load times in a formatted buffer.

Shows timing information with:
- Load time for each module
- Percentage of total module time
- Visual bar chart
- Sorted by slowest first

Also shows total init time and garbage collection statistics."
  (interactive)
  (with-current-buffer (get-buffer-create "*Load Times*")
    (erase-buffer)
    (insert "=== Pathogen Emacs Load Times ===\n\n")

    ;; Total init time (only available after startup)
    (when (and (boundp 'after-init-time) after-init-time)
      (let ((total-time (float-time (time-subtract after-init-time before-init-time))))
        (insert (format "Total initialization: %.2fs\n" total-time))
        (insert (format "Garbage collections: %d (%.2fs)\n\n" gcs-done gc-elapsed))))

    (if pathogen--module-timings
        (progn
          (insert "Module load times (slowest first):\n")
          (insert (make-string 60 ?-) "\n")
          (let* ((sorted-timings (sort (copy-sequence pathogen--module-timings)
                                      (lambda (a b) (> (cdr a) (cdr b)))))
                 (total-module-time (apply #'+ (mapcar #'cdr sorted-timings)))
                 (max-time (cdar sorted-timings)))
            (dolist (timing sorted-timings)
              (let* ((module (car timing))
                     (time (cdr timing))
                     (percentage (/ (* time 100.0) total-module-time))
                     (bar-width (floor (/ (* time 30.0) max-time))))
                (insert (format "%-25s %6.2fs %5.1f%% %s\n"
                              module time percentage
                              (make-string bar-width ?█)))))
            (insert (make-string 60 ?-) "\n")
            (insert (format "%-25s %6.2fs\n" "Total module time:" total-module-time))))
      (insert "No timing information available.\n"))

    (special-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun pathogen/test-config ()
  "Test configuration in a separate Emacs instance.

Launches a new Emacs instance with your configuration and reports
any errors that occur. Useful for testing changes before committing.

The test runs in a separate process so your current session is unaffected."
  (interactive)
  (message "Testing configuration in new Emacs instance...")
  (let* ((test-buffer (generate-new-buffer "*Config Test*"))
         (test-script (format "emacs -Q --batch --eval '(condition-case err (progn (load \"%s\") (load \"%s\") (message \"✓ Configuration loaded successfully\")) (error (message \"✗ Configuration error: %%s\" err) (kill-emacs 1)))'"
                             (concat user-emacs-directory "early-init.el")
                             (concat user-emacs-directory "init.el"))))
    (with-current-buffer test-buffer
      (insert "=== Configuration Test ===\n\n")
      (insert "Testing configuration in separate Emacs instance...\n\n"))
    (make-process
     :name "config-test"
     :buffer test-buffer
     :command (list shell-file-name "-c" test-script)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           (insert "\n")
           (if (= (process-exit-status proc) 0)
               (progn
                 (insert "✓ Configuration test PASSED\n")
                 (message "✓ Configuration test PASSED"))
             (insert "✗ Configuration test FAILED\n")
             (message "✗ Configuration test FAILED - see *Config Test* buffer")
             (display-buffer (current-buffer)))))))))

(defun pathogen/list-autoloads ()
  "List all autoloaded functions from packages.

Shows which functions will trigger package loading when called.
Useful for understanding deferred loading behavior and optimizing
startup time.

Displays results in a searchable buffer grouped by file."
  (interactive)
  (with-current-buffer (get-buffer-create "*Autoloads*")
    (erase-buffer)
    (insert "=== Package Autoloads ===\n\n")
    (insert "Functions that will trigger package loading when called:\n\n")

    (let ((autoloads (make-hash-table :test 'equal))
          (total-count 0))
      ;; Collect autoloads
      (mapatoms
       (lambda (sym)
         (when (and (fboundp sym)
                   (autoloadp (symbol-function sym)))
           (let* ((def (symbol-function sym))
                  (file (nth 1 def)))
             (when file
               (setq total-count (1+ total-count))
               (push sym (gethash file autoloads)))))))

      ;; Display grouped by file
      (if (> total-count 0)
          (let ((files (sort (hash-table-keys autoloads) #'string<)))
            (insert (format "Total autoloads: %d from %d files\n\n"
                          total-count (length files)))
            (dolist (file files)
              (let ((funcs (sort (gethash file autoloads)
                                (lambda (a b) (string< (symbol-name a)
                                                      (symbol-name b))))))
                (insert (format "[%s] (%d functions)\n"
                              (file-name-nondirectory file)
                              (length funcs)))
                (dolist (func funcs)
                  (insert (format "  %s\n" func)))
                (insert "\n"))))
        (insert "No autoloads found.\n")))

    (special-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide '04-custom-functions)
;;; functions.el ends here
