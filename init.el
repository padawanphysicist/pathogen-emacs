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

(require 'variables)
(require 'package-manager)
(require 'funcs)
(require 'layers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load core layers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register the internal layer directory
(pathogen-add-layers-dir pathogen-layers-dir)
;;(pathogen/load-layers '(better-defaults))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load additional settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional layers
(if (file-exists-p pathogen-private-layers-dir)
  (pathogen-add-layers-dir pathogen-private-layers-dir)
  (message "No private layers found"))
;; Extra configuration
(if (file-exists-p pathogen--config-file)
  (load-file pathogen--config-file)
  (message "No personal configuration file found."))

; Load all configured layers
;; Load all configured layers
(pathogen-load-layers pathogen-configuration-layers)
;; Default layers
;(pathogen--load-layers pathogen--default-configuration-layers)
;;; User layers
;(pathogen--load-layers pathogen-configuration-layers)

(provide 'init.el)
