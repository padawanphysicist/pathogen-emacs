;;; init.el --- Emacs main initialization -*- lexical-binding: t; -*-
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
;;  Code that you want to execute when you start Emacs.
;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ubiquitous packages
;;
;; These packages are bundled with GNU Emacs and should be loaded on startup
;; rather than autoloaded on demand since they are likely to be used in every
;; session.
;;
;; For a detailed explanation of each one, look at the URLs:
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;; https://www.emacswiki.org/emacs/AnsiColor
;; https://www.emacswiki.org/emacs/InstallingPackages
;;
(require 'cl-lib) ;; Common Lisp facilities within Emacs
(require 'ansi-color) ;; Translate ansi color codes to Emacs colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load shared variables
;;
;;
;; Load pathogen-vars.el which defines shared configuration variables.
;; This file is also loaded by early-init.el to ensure variables are
;; available at all stages of initialization.
(require 'pathogen-vars)

(add-to-list 'load-path (concat user-emacs-directory "pathogen/"))
(add-to-list 'load-path pathogen-config-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ensure required directories exist
;;
;;
;; Create cache and config directories if they don't exist. This prevents
;; errors when modules try to write files (savehist, recentf, etc.) to
;; directories that haven't been created yet.
(defun pathogen--ensure-directory (dir description)
  "Ensure DIR exists, creating it if necessary.
DESCRIPTION is used in messages to identify the directory."
  (condition-case err
      (progn
        (unless (file-exists-p dir)
          (make-directory dir t)
          (message "Created %s: %s" description dir))
        ;; Verify it's actually a directory
        (unless (file-directory-p dir)
          (error "%s exists but is not a directory: %s" description dir))
        ;; Check if directory is writable
        (unless (file-writable-p dir)
          (display-warning 'init
                           (format "%s is not writable: %s"
                                   (capitalize description) dir)
                           :error)))
    (error
     (display-warning 'init
                      (format "Failed to create %s '%s': %s"
                              description dir (error-message-string err))
                      :error))))

;; Ensure cache directory exists (required for savehist, recentf, etc.)
(pathogen--ensure-directory pathogen-cache-directory "cache directory")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module loading with error handling
;;
;;
;; Load configuration modules with proper error handling to prevent a single
;; module failure from breaking the entire initialization. This provides:
;;   - Graceful degradation when modules fail
;;   - Clear error messages for debugging
;;   - Load time tracking for performance analysis
;;   - Summary of failed modules at the end
;;
;; Note: pathogen--failed-modules is defined in pathogen-vars.el

(defun pathogen--load-module (module)
  "Load MODULE with error handling, timing, and user feedback.
If MODULE fails to load, record the error and continue initialization
with reduced functionality."
  (let ((start-time (current-time)))
    (condition-case err
        (progn
          (require module)
          (let ((load-time (float-time (time-subtract (current-time) start-time))))
            (push (cons module load-time) pathogen--module-timings)
            (message "Loaded %s in %.2fs" module load-time)))
      (error
       (push (cons module err) pathogen--failed-modules)
       (message "ERROR: Failed to load %s: %s"
                module
                (error-message-string err))
       (display-warning 'init
                        (format "Failed to load module '%s': %s\n\nEmacs will continue with reduced functionality."
                                module (error-message-string err))
                        :error)))))

;; Load core modules
(pathogen--load-module '00-user-interface)
(pathogen--load-module '01-editor)
(pathogen--load-module '02-package-manager)
(pathogen--load-module '03-setup-packages)
(pathogen--load-module '04-custom-functions)

;; Report any failures at the end of initialization
(when pathogen--failed-modules
  (display-warning 'init
                   (format "Failed to load %d module(s):\n%s"
                           (length pathogen--failed-modules)
                           (mapconcat (lambda (x)
                                        (format "  - %s: %s"
                                                (car x)
                                                (error-message-string (cdr x))))
                                      pathogen--failed-modules
                                      "\n"))
                   :error))

;; Load additional settings
(when (file-exists-p pathogen-config-directory)
  (mapc #'load-file (file-expand-wildcards (concat pathogen-config-directory "*.el"))))
(when (file-exists-p pathogen-config-file)
  (load-file pathogen-config-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup performance report
;;
;;
;; Display a comprehensive startup performance report after initialization
;; completes. This helps identify slow modules and optimization opportunities.
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((startup-time (float-time (time-subtract (current-time)
                                                           before-init-time)))
                  (gcs-done gcs-done)
                  (gc-elapsed gc-elapsed))
              (message "\n")
              (message "========================================")
              (message "Pathogen Emacs Startup Report")
              (message "========================================")
              (message "Total startup time: %.2fs" startup-time)
              (message "Garbage collections: %d (%.2fs)" gcs-done gc-elapsed)
              (message "----------------------------------------")
              (when pathogen--module-timings
                (message "Module load times (slowest first):")
                (dolist (timing (sort pathogen--module-timings
                                     (lambda (a b) (> (cdr a) (cdr b)))))
                  (message "  %s: %.2fs" (car timing) (cdr timing)))
                (message "----------------------------------------"))
              (when pathogen--failed-modules
                (message "Failed modules: %d" (length pathogen--failed-modules))
                (message "----------------------------------------"))
              (message "========================================\n"))))

(provide 'init)
;;; init.el ends here
