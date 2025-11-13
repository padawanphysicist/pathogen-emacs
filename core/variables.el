;;; variables.el --- Shared configuration variables -*- lexical-binding: t; fill-column: 79; -*-
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
;; It establishes the contract between the loaded modules.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API 
;;
;;
;; These functions and variables are designed to be used in .pathogen.el and
;; user configuration.

(defvar pathogen-layers-dirs '()
  "List of directories to search for layer configurations.
Add directories using `pathogen-add-layers-dir'.")

(defvar pathogen-additional-layers-dir
  (cond ((getenv "PATHOGEN_ADDITIONAL_LAYERS_DIR") (getenv "PATHOGEN_ADDITIONAL_LAYERS_DIR"))
	(t (substitute-in-file-name "$HOME/.pathogen.d/")))
  "Directory containing Pathogen private layer configuration. Configurable through PATHOGEN_ADDITIONAL_LAYERS_DIR")

(defvar pathogen-user-config-file
  (cond ((getenv "PATHOGEN_USER_CONFIG_FILE") (getenv "PATHOGEN_USER_CONFIG_FILE"))
	(t (substitute-in-file-name "$HOME/.pathogen.el")))
  "User's personal configuration file.
This file is loaded after all Pathogen modules if it exists. Use this
for personal customizations without modifying Pathogen core files.")

(defvar pathogen-configuration-layers '()
  "List of layers to load. Set via `.pathogen.el'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private API 
;;
;; These functions and variables are for internal use only. They may change
;; without notice.
(defvar pathogen--emacs-dir user-emacs-directory
  "Emacs main directory.")

(defvar pathogen--default-configuration-layers '(+emacs/base)
  "List of layers to load by default")

(provide 'variables)
;;; variables.el ends here
