;;; variables.el --- Shared configuration variables -*- lexical-binding: t; -*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API 
;;
;;
;; These functions and variables are designed to be used in .pathogen.el and
;; user configuration.

(defvar pathogen-layers-dirs '()
  "List of directories to search for layer configurations.")

(defvar pathogen-configuration-layers '()
  "List of layers to load. Set via `.pathogen.el'.")

(defvar pathogen-layer-pre-init-hook nil
  "Hook run before initializing each layer.
Functions receive the layer struct.")

(defvar pathogen-layer-post-init-hook nil
  "Hook run after initializing each layer.
Functions receive the layer struct.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private API 
;;
;; These functions and variables are for internal use only. They may change
;; without notice.

(defvar pathogen--enabled-layers '()
  "Internal list of successfully loaded layers.
Do not modify directly. Use `pathogen-layer-enabled-p' to check.")

(defvar pathogen--layers-table (make-hash-table :test 'equal)
  "Internal hash table storing layer configurations.
Do not access directly. Use `pathogen-layer-get' instead.")

(defvar pathogen--emacs-dir user-emacs-directory
  "Pathogen main directory.")

(defconst pathogen--core-dir (expand-file-name "core/" pathogen--emacs-dir)
  "Directory containing Pathogen core configuration.")

(defconst pathogen-layers-dir (expand-file-name "layers/" pathogen--emacs-dir)
  "Directory containing Pathogen core layers.")
;
;(defconst pathogen--layers-dirs '()
;  "Directories containing Pathogen layers.")

(defvar pathogen-private-layers-dir (substitute-in-file-name "$HOME/.pathogen.d/")
  "Directory containing Pathogen private layer configuration.")

;(defvar pathogen-configuration-layers '()
;  "List of layers to load. Set via .pathogen.el")
;
(defvar pathogen--default-configuration-layers '(+emacs/better-defaults)
  "List of layers to load by default")
;
;(defvar pathogen--enabled-layers '()
;  "List of enabled layers.")


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

(message "Loaded variables module")
(provide 'variables)
;;; variables.el ends here
