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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ubiquitous packages
;;
;; These packages are bundled with GNU Emacs and should be loaded on startup
;; rather than autoloaded on demand since they are likely to be used in every
;; session.
;;
;; For a detailed explanation of each one, look at the URLs:
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
;; https://www.emacswiki.org/emacs/AnsiColor
;; https://www.emacswiki.org/emacs/InstallingPackages
;;
(require 'cl-lib) ;; Common Lisp facilities within Emacs
(require 'ansi-color) ;; Translate ansi color codes to Emacs colors
(require 'package) ;; Package manager

;; Main variables
(defvar pathogen-cache-directory (concat user-emacs-directory "cache/"))
(defvar pathogen-config-directory (substitute-in-file-name "$HOME/.pathogen.d/"))
(defvar pathogen-custom-config-file (concat pathogen-config-directory "config.el"))

(add-to-list 'load-path (concat user-emacs-directory "pathogen/"))
(add-to-list 'load-path pathogen-config-directory)

(require '00-user-interface)
(require '01-editor)
(require '02-package-manager)
(require '03-setup-packages)
(require '04-setup-keybindings)

;; Load additional settings
(when (file-exists-p pathogen-custom-config-file)
  (load pathogen-custom-config-file))

(provide 'init)
;;; init.el ends here

