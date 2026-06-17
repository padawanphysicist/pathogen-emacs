;;; init.el --- Main initialization file for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Victor Santos

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

;; Code that you want to execute when you start Emacs.

;; References:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

;;; Code:

;; Ensures the early init file is always loaded
(when (version< emacs-version "27.1")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;;+"
;; outline-minor-mode-use-buttons: t
;; outline-minor-mode-cycle: t
;; End:
