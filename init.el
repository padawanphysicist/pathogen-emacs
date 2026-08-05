;;; init.el --- Main initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.1"))
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

(defconst pathogen-min-emacs-version "27.1"
  "Minimum Emacs version required to load configuration.")

;; Ensure the early init file is always loaded
(when (version< emacs-version pathogen-min-emacs-version)
  (load
   (expand-file-name "early-init.el" user-emacs-directory)))

(when (version< emacs-version pathogen-min-emacs-version)
  (display-warning
   'pathogen
   (format "Configuration aborted! Minimum version: %s (Your version: %s)"
           pathogen-min-emacs-version
           emacs-version)
   :warning)
  (top-level))

;; =============================================================================
;; From here all your customized configuration comes into play.  If
;; the version is < 27.1, Emacs will never read the lines below.
;; =============================================================================

;; Core features
(require 'pathogen-package-manager)
(require 'pathogen-defaults)

(if (file-exists-p custom-file)
    (load custom-file))

;; Optional: you can opt-out be setting the envvar PATHOGEN_DEV to 1
(when (equal (getenv "PATHOGEN_DEV") "1")  
  (message "[Pathogen] DEV mode active (stopping additional loading).")
  (top-level))

(require 'pathogen-ui)

;; Load personal configuration file
(let ((personal-config (expand-file-name "~/.pathogen.el")))
  (when (file-exists-p personal-config)
    (load personal-config 'noerror 'nomessage)))

(provide 'init)
;;; init.el ends here
