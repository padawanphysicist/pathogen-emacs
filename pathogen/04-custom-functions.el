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
;;  Functions to be available to all custom configuration
;;
;;; Code:

(defun pathogen/open-config ()
  (interactive)
  (dired pathogen-config-directory))

(provide '04-custom-functions)
;;; functions.el ends here
