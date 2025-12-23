;;; early-init.el --- Early initialization file -*- no-byte-compile: t; lexical-binding: t; fill-column: 79; -*-
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
;; This file is loaded very early in the startup process, before the package
;; system and GUI are initialized.  Use it to customize variables that affect
;; the initial package loading and frame setup.
;;
;; For most customizations, especially those related to GUI features, use the
;; regular init.el file instead.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defer package initialization
;;
;;
;; Prior to Emacs 27, the init file was responsible for initializing the
;; package manager by explicitly calling `package-initialize'. Starting with
;; Emacs 27, the default behavior changed: `package-initialize' is now
;; automatically called before loading the init file. This means package
;; initialization occurs after `early-init-file' is loaded but before
;; `user-init-file' is processed. To prevent Emacs from initializing packages
;; automatically, we set `package-enable-at-startup' to nil:
(setq package-enable-at-startup nil)

;;; early-init.el ends here
