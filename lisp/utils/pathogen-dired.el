;;; pathogen-dired.el --- Additional config for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2")) Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later URL:
;; https://codeberg.org/padawanphysicist/pathogen-emacs

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

;;; Code:

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :preface
  ;; Usar &rest permite aceitar os argumentos adicionais que o dired envia
  (defun my/nerd-icons-icon-for-file (file &rest _)
    (nerd-icons-icon-for-file file :height 0.9 :v-adjust 0.45))

  (defun my/nerd-icons-icon-for-dir (dir &rest _)
    (nerd-icons-icon-for-dir dir :height 0.9 :v-adjust 0.45))

  :custom
  (nerd-icons-dired-file-icon-function #'my/nerd-icons-icon-for-file)
  (nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'pathogen-dired)
;;; pathogen-dired.el ends here
