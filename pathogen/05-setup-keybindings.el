;;; 05-setup-keybindings.el --- Basic keybindings -*- lexical-binding: t; -*-
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
;;
;;; Code:

(general-define-key "<f6>" 'consult-theme)

(general-define-key "C-x C-r" 'consult-recent-file)

(provide '05-setup-keybindings)
;;; 05-setup-keybindings.el ends here
