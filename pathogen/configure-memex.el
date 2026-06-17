;;; configure-memex.el --- Personal Knowledge Management system configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Victor Santos

;; Author: Victor Santos <vct.santos@protonmail.com>

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




(use-package hyperbole
  :ensure t
  :defer nil
  :demand t
  :config
  (hyperbole-mode 1))

(provide 'configure-memex)
;;; configure-memex.el ends here

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;;+"
;; outline-minor-mode-use-buttons: t
;; outline-minor-mode-cycle: t
;; fill-column: 80
;; End:

