;;; init-pathogen-bootstrap.el --- Startup core pathogen configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs
;; Package-Requires: ((emacs "27.1"))

;;; License:

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

(require 'configure-environment)

(use-package emacs
  :ensure nil
  :custom
  (pathogen-user-name "Victor Santos")
  (pathogen-user-email "vct.santos@protonmail.com"))

(provide 'init-pathogen-bootstrap)
;;; init-pathogen-bootstrap.el ends here
