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

(add-to-list 'load-path (concat user-emacs-directory "pathogen/"))

(require 'pathogen-variables)

;; Load custom-file after pathogen-variables has defined it
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(require 'better-defaults)
(require 'package-management)
(require 'builtin-packages)
(require 'better-editing)
(require 'compleseus)
(require 'look-and-feel)

;; Load additional settings
(when (file-exists-p pathogen-config-file)
  (load-file pathogen-config-file))

(provide 'init)
;;; init.el ends here
