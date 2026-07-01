;;; pathogen-programming.el --- Configuration for programming languages -*- lexical-binding: t; -*-

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

;;; Code:

;;; Adiciona linhas guia nos buffers
;;
;; - https://github.com/jdtsmith/indent-bars
(use-package indent-bars
  :ensure t
  :hook
  ((python-mode yaml-mode) . indent-bars-mode))

;; Programação em Haskell (Basicamente o XMonad no meu caso)
(use-package haskell-mode
  :ensure t)

;; Programação em Lua
(use-package lua-mode
  :ensure t)

(use-package geiser-guile
  :ensure t
  :config
  (with-eval-after-load
    'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/.guix-profile/share/guile/site/3.0"))

(with-eval-after-load
    'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/site/3.0")))

(require 'pathogen-emacs-lisp)

(provide 'pathogen-programming)
;;; pathogen-programming.el ends here
