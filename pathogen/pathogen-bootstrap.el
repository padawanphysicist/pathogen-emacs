;;; pathogen-bootstrap.el --- Pathogen bootstrap sequence -*- lexical-binding: t; fill-column: 79; -*-
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
;;; Code:

(add-to-load-path (expand-file-name "utils" (file-name-directory load-file-name)))
(add-to-load-path (expand-file-name "core" (file-name-directory load-file-name)))

(require 'pathogen-logging)    ;; Monitoring/tracing
(require 'pathogen-genome)     ;; Configuration variables
(require 'pathogen-germ)       ;; Germ definition
(require 'pathogen-incubator)  ;; Install process
(require 'pathogen-microscope) ;; Diagnostics

;; Trigger External Configuration
(defun pathogen--load-config ()
  "Load the layers defined in the PATHOGEN_CONFIG_FILE."
  (if (file-exists-p pathogen-config-file)
      (progn
        (pathogen/log 'info "Loading configuration from: %s" pathogen-config-file)
        (load pathogen-config-file nil 'nomessage))
    (pathogen/log 'warning "Configuration file not found: %s" pathogen-config-file)))

(provide 'pathogen-bootstrap)
;;; pathogen-bootstrap.el ends here
