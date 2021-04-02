;;; core.el --- Pathogen main initialization -*- lexical-binding: t; -*-
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

(require 'cl-lib)
(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pathogen directory variables
;;
;;

(defconst pathogen/emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst pathogen/core-dir (concat pathogen/emacs-dir "pathogen/")
  "The root directory of Pathogen's core files. Must end with a slash.")

(defvar pathogen/cache-dir
  (let ((local-dir (getenv "PATHOGEN_CACHE_DIR")))
    (if local-dir
        (expand-file-name (file-name-as-directory local-dir))
      (concat pathogen/core-dir "cache/")))
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defvar pathogen/user-config
 (let ((user-config (getenv "PATHOGEN_USER_CONFIG")))
    (if user-config
        (expand-file-name user-config)
      "~/.pathogen.el"))
  "Path for user additional configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Pathogen modules
;;
;;
;; These files provide a high-level interface for configuring Emacs
(mapc
 (lambda (file)
   (load file nil 'nomessage))
 (directory-files-recursively
  (concat pathogen/core-dir "core/")  ".*pathogen.*\\.el$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pathogen load process
;;
;;
;; Pathogen modifies Emacs behaviour using hooks, which are lists of
;; functions called within Emacs on suitable occasions. You can check
;; the list of standard hooks at the following link:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;
;; Pathogen "infects" Emacs in such a way the sequence of actions at
;; startup is as follows:
;;     - Load `~/.emacs.d/early-init.el'
;;     - Run  `before-init-hook'
;;     - Load `~/.emacs.d/init.el'
;;     - Run  `pathogen-init-ui-hook'
;;     - Run  `pathogen-editor-hook'
;;     - Run  `pathogen-package-management-hook'
;;     - Run  `pathogen-user-config-hook'
;;     - Run  `after-init-hook'
;;     - Run  `emacs-startup-hook'
;;     - Run  `window-setup-hook'
;;
;; A full summary of Emacs startup process is given at
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;;
(run-hooks 'pathogen-init-ui-hook)
(run-hooks 'pathogen-editor-hook)
(run-hooks 'pathogen-package-management-hook)
(run-hooks 'pathogen-user-config-hook)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Let the hacking begin! (Load time: %s, with %d garbage collections)."
    (format "%.2f seconds"
            (float-time
             (time-subtract after-init-time
                            before-init-time)))
    gcs-done)))

(provide 'core.el)
;;; core.el ends here
