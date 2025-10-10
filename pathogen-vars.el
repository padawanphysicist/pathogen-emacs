;;; pathogen-vars.el --- Shared configuration variables -*- lexical-binding: t; -*-
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
;; This file defines shared configuration variables used across Pathogen Emacs.
;; It establishes the contract between early-init.el, init.el, and loaded modules.
;;
;; This file is loaded by both early-init.el and init.el to ensure variables
;; are available at all stages of initialization.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API Variables
;;
;; These variables are safe for users to customize in their personal
;; configuration files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pathogen-cache-directory (concat user-emacs-directory "cache/")
  "Directory where Emacs stores cache files.
This includes savehist, recentf, and other persistent data.
Users can customize this to store cache in a different location.")

(defvar pathogen-config-directory (substitute-in-file-name "$HOME/.pathogen.d/")
  "User's personal configuration directory.
This is where user-specific configuration files are stored, separate
from the main Pathogen configuration. Optional - directory does not
need to exist if not used.")

(defvar pathogen-config-file (substitute-in-file-name "$HOME/.pathogen.el")
  "User's personal configuration file.
This file is loaded after all Pathogen modules if it exists. Use this
for personal customizations without modifying Pathogen core files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Garbage Collection Configuration
;;
;; These variables control GC behavior during and after initialization.
;; Defined here but primarily used by early-init.el.
;; Advanced users may customize these for performance tuning.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pathogen/gc-cons-threshold 67108864 ; 64MB
  "The default value to use for `gc-cons-threshold' after initialization.
During startup, GC threshold is set very high to speed up initialization.
After startup completes, this value is restored for normal operation.

If you experience freezing, decrease this value.
If you experience stuttering, increase this value.

Default: 64MB (67108864 bytes)")

(defvar pathogen/gc-cons-percentage 0.5 ; 50%
  "The default value to use for `gc-cons-percentage' after initialization.
This controls how much heap growth triggers garbage collection.

Default: 0.5 (50% growth triggers GC)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private Implementation Variables
;;
;; These variables are internal implementation details and should NOT be
;; modified by users. The double-dash (--) naming convention indicates
;; private/internal use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pathogen--failed-modules nil
  "List of modules that failed to load during initialization.
Internal variable used by the module loading system.
Format: List of (MODULE . ERROR) cons cells.")

(defvar pathogen--module-timings nil
  "List of module load times for performance analysis.
Internal variable used by the module loading system.
Format: List of (MODULE . TIME-IN-SECONDS) cons cells.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable Naming Conventions
;;
;; pathogen-*   : Public API, safe to customize
;; pathogen/*   : Advanced configuration, documented
;; pathogen--*  : Private implementation, do not use
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'pathogen-vars)
;;; pathogen-vars.el ends here
