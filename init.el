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

(require 'better-defaults)
(require 'package-management)
(require 'builtin-packages)
(require 'better-editing)
(require 'compleseus)
(require 'look-and-feel)

(defvar pathogen-config-file (substitute-in-file-name "$HOME/.pathogen.el")
  "User's personal configuration file.
This file is loaded after all Pathogen modules if it exists. Use this
for personal customizations without modifying Pathogen core files.")

;; Load additional settings
;; (when (file-exists-p pathogen-config-directory)
;;   (mapc #'load-file (file-expand-wildcards (concat pathogen-config-directory "*.el"))))
(when (file-exists-p pathogen-config-file)
  (load-file pathogen-config-file))

(provide 'init)
;;; init.el ends here
