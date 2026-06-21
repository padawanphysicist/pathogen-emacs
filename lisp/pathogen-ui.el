;;; pathogen-ui.el --- Visual Aesthetics & Interface Customization  -*- lexical-binding: t; -*-

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
;; This module defines the user interface, typography, and visual ecosystem for
;; the 'pathogen' Emacs configuration. It strips away default GUI clutter to
;; establish a clean, modern, and distraction-free editing environment.
;;
;; Key areas managed within this module:
;; - Themes & Colors: Loading color palettes, configuring faces, and styling the
;;   global modeline.
;; - Typography: Defining default, fixed-pitch, and variable-pitch fonts alongside
;;   line-spacing tweaks.
;; - Frame Geometry: Disabling redundant graphical components (toolbars, scrollbars)
;;   and setting startup window bounds.
;; - Visual Indicators: Customizing fringes, window dividers, and subtle feedback
;;   mechanisms.
;;

;;; Code:

(if (version<= "27.1" emacs-version)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (horizontal-scroll-bar-mode -1)
      (setq inhibit-splash-screen t)

      ;; This ensures we use the version >5
      (use-package modus-themes
        :ensure t
        :demand t)

      ;; Custom flavor palette loaded from a personalized Codeberg fork.
      ;; See: https://codeberg.org/padawanphysicist/modus-catppuccin
      (use-package modus-catppuccin
        :vc (:url "https://codeberg.org/padawanphysicist/modus-catppuccin"
                  :rev :newest)
        :after modus-themes
        :demand t
        :config
        ;; Available flavors: 'frappe', 'latte', 'macchiato', or 'mocha'
        (load-theme 'catppuccin-frappe :no-confirm)))
  (load-theme 'leuven :no-confirm))

(provide 'pathogen-ui)
;;; pathogen-ui.el ends here

