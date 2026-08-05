;;; environment.el --- Configure variables -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module serves as the foundational configuration layer for the
;; Pathogen Emacs distribution. It centralizes environmental
;; variables, directory paths, and user-specific identification
;; settings.
;;
;; This file should be loaded early in the initialization process to
;; ensure that subsequent modules have access to the defined path and
;; environment configurations.

;;; Code

(defgroup pathogen nil
  "Customization group for Pathogen Emacs configuration."
  :group 'convenience
  :prefix "pathogen-")

(defconst pathogen-min-emacs-version
  "27.2"
  "Minimum emacs version required for a proper loading of the
configuration.

This is the lowest version tested.")

(defcustom pathogen-days-between-package-manager-cache-updates
  2
  "Days to keep package manager cache."
  :group 'pathogen)

(defcustom pathogen-cache-directory
  (locate-user-emacs-file "cache/")
  "Base directory for Emacs cache files."
  :type 'directory
  :group 'pathogen
  :set (lambda (symbol value)
         (set-default symbol value)
         (unless (file-directory-p value)
           (make-directory value t)))) ; Automatic creation of directory

(defcustom pathogen-custom-file
  "~/.pathogen.el"
  "Additional custom configuration."
  :type 'file
  :group 'pathogen)

(customize-set-variable
 'custom-file
 (locate-user-emacs-file "custom.el"))

(provide 'environment)
;;; environment.el ends here
