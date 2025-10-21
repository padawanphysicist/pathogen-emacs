;;; early-init.el --- Emacs 27+ pre-initialization -*- no-byte-compile: t; lexical-binding: t; -*-
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
;;  Emacs (27+) introduces early-init.el, which is run before init.el, before
;;  package and UI initialization happens.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defer package initialization
;;
;;
;; Package initialization occurs before `user-init-file' is loaded, but after
;; `early-init-file'. This prevent Emacs from doing it early:
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add Pathogen directories to `load-path'
(let ((pathogen-root-dir (file-name-directory (file-truename load-file-name))))
  (dolist (dir '("core"))
    (add-to-list 'load-path (expand-file-name dir pathogen-root-dir))))

(require 'vars)
(require 'early-funcs)

(pathogen/toggle-gui-elements-off)
(pathogen/tune-garbage-collector)
(pathogen/ui-tweaks)
(pathogen/minor-tweaks)

(provide 'early-init)
;;; early-init.el ends here
