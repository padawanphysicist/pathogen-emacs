;;; pathogen-core.el --- Pathogen core configuration  -*- lexical-binding: t; -*-

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

;;; Code

;;; Built-in package configuration
(use-package savehist  :ensure nil :init (savehist-mode 1))
(use-package recentf   :ensure nil :init (recentf-mode 1))
(use-package saveplace :ensure nil :init (save-place-mode 1))
(use-package icomplete
  :ensure nil
  :config
  (fido-mode 1)
  (when (version<= "28.1" emacs-version)
    (icomplete-vertical-mode 1)))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))


(provide 'pathogen-core)
;;; pathogen-core.el ends here
