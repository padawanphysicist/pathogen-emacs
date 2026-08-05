;;; init.el --- Main initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Code that you want to execute when you start Emacs.
;;
;; Reference (emacs info):
;;   (emacs)Top > Customization > Init File

;;; Code:

;;;; Add custom directories to `load-path'
(let ((default-directory (locate-user-emacs-file "pathogen/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(setq warning-minimum-level :emergency)

(defconst pathogen-min-emacs-version "27.2"
  "Minimum Emacs version required to load configuration.")

(catch 'init-done
  ;; Abort config if emacs version < `pathogen-min-emacs-version'
  (when (version< emacs-version pathogen-min-emacs-version)
    (display-warning
     'pathogen
     (format
      "Configuration aborted! Minimum version: %s (Your version: %s)"
      pathogen-min-emacs-version emacs-version)
     :warning)
    (throw 'init-done nil))

  ;; Core features
  (require 'pathogen-package-manager)
  (require 'pathogen-defaults)

  ;; Setup and load custom file
  (customize-set-variable
   'custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (require 'pathogen-ui)

  (when (equal (getenv "PATHOGEN_DEV") "1")
    (message
     "[Pathogen] DEV mode active (stopping additional loading).")
    (throw 'init-done nil))

  ;; Load personal configuration file
  (let ((personal-config (expand-file-name "~/.pathogen.el")))
    (when (file-exists-p personal-config)
      (load personal-config 'noerror 'nomessage))))

(provide 'init)
;;; init.el ends here
