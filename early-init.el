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
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

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
(defun add-to-load-path (path)
  "Add PATH to `load-path` if it exists.
If PATH does not exist, emit a warning."
  (let ((expanded-path (expand-file-name path)))
    (if (file-directory-p expanded-path)
        (add-to-list 'load-path expanded-path)
      (display-warning
       'pathogen
       (format "⛔ Directory does not exist; could not add to load-path: %s" expanded-path)
       :warning))))

(add-to-load-path (expand-file-name "pathogen" (file-name-directory load-file-name)))

;;; early-init.el ends here
