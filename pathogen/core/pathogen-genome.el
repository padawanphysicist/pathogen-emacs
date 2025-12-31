;;; pathogen-genome.el --- Pathogen configuration variables -*- lexical-binding: t; fill-column: 79; -*-
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

(defvar pathogen-core-germs-directory 
  )

(defvar pathogen-germs-directories
  `("~/.emacs.d/germs/"
    "~/.pathogen.d/"
    ,(getenv "PATHOGEN_GERMS_DIRECTORY"))
  "List of directories containing the germs definitions.")

(defvar pathogen-config-file
  (let ((env-path (getenv "PATHOGEN_CONFIG_FILE")))
    (expand-file-name (or env-path "~/.pathogen.el")))
  "The file containing the pathogen-layers! declaration.")

(defvar pathogen-germ-core-files '("variables.el" "functions.el" "config.el"))

(defvar *pathogen-genome* (make-hash-table :test 'equal)
  "The global registry of all discovered Germs.")

(provide 'pathogen-genome)
;;; pathogen-genome.el ends here
