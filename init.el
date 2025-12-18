;;; init.el --- Main initialization file for Emacs -*- lexical-binding: t; fill-column: 79; -*-
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
;; Code that you want to execute when you start Emacs.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
;;
;;; Code:

(require 'package-manager)
(require 'layers)
(require 'prefixes)

;; Register the internal layer directory
(pathogen-add-layers-dir (expand-file-name "layers/" pathogen--emacs-dir))

;; Register additional layer directory
(pathogen-add-layers-dir pathogen-additional-layers-dir)

;; Extra configuration
(if (file-exists-p pathogen-user-config-file)
  (load-file pathogen-user-config-file)
  (message "[Pathogen] No personal configuration file found."))

;;; Load layers
(pathogen-load-layers pathogen--default-configuration-layers)
;; User layers
(pathogen-load-layers pathogen-configuration-layers)

(message "[Pathogen] Loaded init file.")
;;; init.el ends here

