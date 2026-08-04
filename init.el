;;; init.el --- Main initialization file -*- lexical-binding: t; -*-

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
;; Code that you want to execute when you start Emacs.
;;
;; Reference (emacs info):
;;   (emacs)Top > Customization > Init File

;;; Code:

(defconst pathogen-min-emacs-version "27.1"
  "Versão mínima do Emacs necessária para carregar as configurações.")

;; Ensure the early init file is always loaded
(when (version< emacs-version "27.1")
  (load
   (expand-file-name "early-init.el" user-emacs-directory)))

(when (version< emacs-version pathogen-min-emacs-version)
  (display-warning
   'pathogen
   (format "Configuração abortada! Versão mínima: %s (Sua versão: %s)"
           pathogen-min-emacs-version
           emacs-version)
   :warning)
  ;; Cancela a avaliação do restante do arquivo init.el
  (top-level))

;; =============================================================================
;; A partir daqui entra toda a sua configuração customizada.
;; Se a versão for < 27.1, o Emacs nunca chegará a ler as linhas abaixo.
;; =============================================================================

;; Core features
(require 'setup-package-manager)
(require 'better-defaults)

;;;; Load Pathogen core packages
(unless (equal (getenv "PATHOGEN_DISABLE") "1")
  (require 'improve-user-experience)
  (require 'setup-programming-and-markup))

;;;; Load local custom configuration
(unless (equal (getenv "PATHOGEN_DISABLE") "1")
  (if (file-exists-p pathogen-custom-file)
       (load pathogen-custom-file)
     ;; (display-warning
     ;;  'pathogen
     ;;  (format "File %s does not exist" pathogen-custom-file)
     ;;  :warning)
     ))

(if (file-exists-p custom-file)
    (load custom-file)
  ;; (display-warning
  ;;  'pathogen
  ;;  (format "File %s does not exist" custom-file)
  ;;  :warning)
  )

(provide 'init)
;;; init.el ends here
