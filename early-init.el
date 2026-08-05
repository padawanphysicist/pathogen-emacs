;;; early-init.el --- Early Init File -*- lexical-binding: t; -*-

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
;; This file is loaded very early in the startup process, before the
;; package system and GUI are initialized.  Use it to customize
;; variables that affect the initial package loading and frame setup.
;;
;; For most customizations, especially those related to GUI features,
;; use the regular init.el file instead.
;;
;; Reference (emacs info):
;;   (emacs)Top > Customization > Init File > Early Init File

;;; Code:

;;;; Defer package initialization

;; Prior to Emacs 27, the init file was responsible for initializing
;; the package manager by explicitly calling
;; `package-initialize'. Starting with Emacs 27, the default behavior
;; changed: `package-initialize' is now automatically called before
;; loading the init file. This means package initialization occurs
;; after `early-init-file' is loaded but before `user-init-file' is
;; processed. To prevent Emacs from initializing packages
;; automatically, we set `package-enable-at-startup' to nil:
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
