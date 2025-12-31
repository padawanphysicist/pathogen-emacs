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

(require 'pathogen-bootstrap)

;; Base configuration (think this as a "metapackage")
(infect! +bootstrap/package-manager
         +distributions/base
         +completion/compleseus
         +ui/appearance)

;; Load user config
(pathogen--load-config)

;;; init.el ends here
