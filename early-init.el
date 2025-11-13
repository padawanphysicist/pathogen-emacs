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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Disable GUI elements early for cleaner interface
;;
;;
;; Toggle off: menu bar, tool bar, scroll bars
;;
;; To prevent the glimpse of un-styled Emacs we disable these UI elements early
;; by directly setting the variable `default-frame-alist', which keeps the
;; default values used when creating a frame (window in the modern parlance):
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; However, doing this only creates a problem: since their respective variables
;; are not set, if the user wants to enable the tool-bar for example, it would
;; be necessary to use the cycle twice the command `tool-bar-mode' to enable.
;;
;; Therefore we need to unset their variables too:
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;; We also inhibit startup elements:
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warning level configuration
;;
;;
;; Set to :error instead of :emergency to avoid suppressing important warnings
;; about configuration issues, deprecated functions, or package loading errors.
;; This still prevents minor warnings from interrupting startup while keeping
;; you informed about actual problems.
;;
;; Warning levels (least to most severe):
;;   :debug < :info < :warning < :error < :emergency
;;
;; - :emergency suppresses almost everything (previous setting)
;; - :error shows errors but hides routine warnings (current setting)
;; - :warning shows all warnings (Emacs default, can be noisy)
(setq warning-minimum-level :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add Pathogen directories to `load-path'
;;
(let ((pathogen-root-dir (file-name-directory (file-truename load-file-name))))
  (dolist (dir '("core"))
    (add-to-list 'load-path (expand-file-name dir pathogen-root-dir))))

(require 'variables)
;;; early-init.el ends here
