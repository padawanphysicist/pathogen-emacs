;;; pathogen-emacs-lisp.el --- Emacs Lisp settings -*- lexical-binding: t; -*-

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
;; Configuration for Emacs Lisp development.

;;; Code:

;;(defun pathogen-elisp-mode-hook ()
;;  "Custom settings for Emacs Lisp editing."
;;  (eldoc-mode 1)
;;  (setq show-trailing-whitespace t)
;;  (outline-minor-mode 1)
;;  (setq outline-regexp ";;;+")
;;  (display-fill-column-indicator-mode 1)
;;  (setq fill-column 70))
;;
;;(use-package emacs
;;  :ensure nil
;;  :hook
;;  (emacs-lisp-mode . #'pathogen-elisp-mode-hook))

(provide 'pathogen-emacs-lisp)
;;; pathogen-emacs-lisp.el ends here
