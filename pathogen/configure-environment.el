;;; configure-environment.el --- Configure variables  -*- lexical-binding: t; -*-

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

;; Configure environment variables for Pathogen 

;;; Code:

(defgroup pathogen nil
  "Customization group for Pathogen Emacs configuration."
  :group 'local
  :prefix "pathogen-")

(defcustom pathogen-days-between-package-manager-cache-updates
  2
  "Days to keep package manager cache"
  :group 'pathogen)

(defvaralias 'pathogen-user-name 'user-full-name)
(defvaralias 'pathogen-user-email 'user-mail-address)

(defcustom pathogen-cache-directory
  (expand-file-name "cache/" user-emacs-directory)
  "Base directory for Emacs cache files."
  :type `(choice
          (const     :tag "Inside Emacs config  (cache/ in user-emacs-directory)"
                     ,(expand-file-name "cache/" user-emacs-directory))
          (const     :tag "System temp          (/tmp/emacs-cache/)" "/tmp/emacs-cache/")
          (directory :tag "Custom directory"))
  :group 'pathogen)

(defcustom pathogen-local-config-file
  (expand-file-name ".pathogen.el" (getenv "HOME"))
  "Path for additional configuration. Can be used for testing new features."
  :group 'pathogen)

;;;; Predicate variables

(defconst pathogen-local-config-file-enabled-p
  (not (getenv "PATHOGEN_DISABLE")))

(provide 'configure-environment)
;;; configure-enviroment.el ends here

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;;+"
;; outline-minor-mode-use-buttons: t
;; outline-minor-mode-cycle: t
;; End:
