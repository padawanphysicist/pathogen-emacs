;;; vars.el --- Shared configuration variables -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Victor Santos
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

(defconst pathogen--emacs-dir user-emacs-directory
  "Pathogen main directory.")

(defconst pathogen--core-dir (expand-file-name "core/" pathogen--emacs-dir)
  "Directory containing Pathogen core configuration.")

(defconst pathogen--layers-dirs (list (expand-file-name "layers/" pathogen--emacs-dir))
  "Directory containing Pathogen default layer configuration.")

(defvar pathogen-private-layers-dir "~/.pathogen.d/"
  "Directory containing Pathogen private layer configuration.")

(defvar pathogen--enabled-layers '()
  "List of enabled layers.")

(defvar pathogen--file-name-handler-alist file-name-handler-alist
  "Backup of file-name-handler-alist for restoration after init.")

(defvar pathogen--config-file (substitute-in-file-name "$HOME/.pathogen.el")
  "User's personal configuration file.
This file is loaded after all Pathogen modules if it exists. Use this
for personal customizations without modifying Pathogen core files.")

(defvar pathogen-gc-cons-threshold 67108864 ; 64MB
  "The default value to use for `gc-cons-threshold' after initialization.
During startup, GC threshold is set very high to speed up initialization.
After startup completes, this value is restored for normal operation.

If you experience freezing, decrease this value.
If you experience stuttering, increase this value.

Default: 64MB (67108864 bytes)")

(defvar pathogen-gc-cons-percentage 0.5 ; 50%
  "The default value to use for `gc-cons-percentage' after initialization.
This controls how much heap growth triggers garbage collection.

Default: 0.5 (50% growth triggers GC)")

(provide 'vars)
;;; vars.el ends here
