;;; 04-custom-functions.el --- Custom functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; Module: Custom Functions (04)
;; Purpose: User-facing commands and utility functions
;; Dependencies: Can use any previous module
;; Provides: pathogen/* commands available to users
;;
;; Functions to be available to all custom configuration. This module provides
;; interactive commands and helper functions used across the configuration.
;;
;;; Code:

(defun pathogen/user-config ()
  "Open user configuration directory in Dired.

Opens the directory specified by `pathogen-config-directory' where
user-specific configuration files are stored. This is useful for
quickly accessing and editing your personal Emacs configuration.

See also `pathogen/devel-config' for accessing the main Pathogen
configuration directory."
  (interactive)
  (dired pathogen-config-directory))

(defun pathogen/devel-config ()
  "Open Pathogen development configuration directory in Dired.

Opens the main Pathogen configuration directory (USER-EMACS-DIRECTORY/pathogen)
containing the core numbered configuration modules. This is where the main
system configuration files are located (00-user-interface.el, 01-editor.el, etc.).

See also `pathogen/user-config' for accessing user-specific configuration files."
  (interactive)
  (dired (concat user-emacs-directory "pathogen")))

(defun pathogen/set-font (font-alist)
  "Set the first available font from FONT-ALIST (name . size) as default font."
  (let ((frame (selected-frame)))
    (cond ((null font-alist) nil)  ; Base case: empty list
          ((x-list-fonts (caar font-alist))  ; Check if font exists
           (let ((font-name (caar font-alist))
                 (font-size (cdar font-alist)))
             (set-frame-font 
              (format "%s-%d" font-name font-size) t t frame)))
          (t (pathogen/set-font (cdr font-alist))))))  ; Recurse

(provide '04-custom-functions)
;;; functions.el ends here
