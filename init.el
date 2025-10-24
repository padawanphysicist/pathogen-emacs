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
(require 'functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load core layers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register the internal layer directory
(add-to-list 'pathogen-layers-dirs (expand-file-name "layers/" pathogen--emacs-dir))
;;(pathogen-load-layers '(+emacs/better-defaults))

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

(dolist (x '(pathogen-layers-dirs pathogen-configuration-layers pathogen--default-configuration-layers))
  (message "%s: %s" (symbol-name x) (eval x)))

;; Load all configured layers
;; Default layers
(pathogen-load-layers pathogen--default-configuration-layers)
;; User layers
(pathogen-load-layers pathogen-configuration-layers)

(message "Loaded init module")

(provide 'init.el)
