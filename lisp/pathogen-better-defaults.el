;;; pathogen-better-defaults.el --- Better defaults for Emacs -*- lexical-binding: t; -*-

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
;; A collection of sensible defaults to improve Emacs ergonomics.
;; This module avoids external dependencies, focusing on core
;; improvements.

;;; Code:

(defgroup better-defaults nil
  "Customizations for improved Emacs defaults."
  :group 'convenience)

;;; Standardizing defaults
(use-package emacs
  :ensure nil
  :config
  ;; UI: Cleanup visual clutter
  ;; (menu-bar-mode -1)
  ;; (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  (column-number-mode t)

  ;; Editing: Modern habits
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (fset 'yes-or-no-p #'y-or-n-p)

  ;; Clipboard & System Integration
  (setq select-enable-clipboard t
        mouse-yank-at-point t)

  ;; Files: Redirect backups and enable auto-revert
  (let ((backup-dir (expand-file-name "backups/" pathogen-cache-directory)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,backup-dir t))))
  (global-auto-revert-mode t))

(provide 'pathogen-better-defaults)
;;; pathogen-better-defaults.el ends here
