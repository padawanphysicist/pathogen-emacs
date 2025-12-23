;;; prefixes.el --- Common prefixes -*- lexical-binding: t; fill-column: 79; -*-
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
;; This file defines common prefixes to be used across Pathogen Emacs.
;;
;;; Code:

(defun pathogen-setup-prefix (symbol key)
  "Create a prefix command for SYMBOL and bind it to KEY."
  (define-prefix-command symbol)
  (global-set-key (kbd key) symbol))
;; Usage:
;; (pathogen-setup-prefix 'ring-map "°")

(pathogen-setup-prefix 'toggles "C-c t")

(provide 'prefixes)
;;; prefixes.el ends here
