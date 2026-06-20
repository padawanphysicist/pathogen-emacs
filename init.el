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

;; Ensure the early init file is always loaded
(when (version< emacs-version "27.1")
  (load
   (expand-file-name "early-init.el" user-emacs-directory)))

;; Check minimum version for proper loading of this config:
(when (version< emacs-version pathogen-min-emacs-version)
  (display-warning
   'pathogen
   (format "
Minimum required version: %s
Current version: %s"
	   pathogen-min-emacs-version
	   emacs-version)
   :warning))

(require 'pathogen-package-manager)
(require 'pathogen-better-defaults)

;; (pathogen--load-config)

(provide 'init)
;;; init.el ends here
