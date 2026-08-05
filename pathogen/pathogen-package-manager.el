;;; pathogen-package-manager.el --- Setup package manager system -*- lexical-binding: t; -*-

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
;; Configure package manager system.

;;; Code:

;;  Initialize package manager
(require 'package)

(when (version<= "29.1" emacs-version)
  ;; Disable GPG signature verification temporarily to avoid "Bad Signature"
  ;; errors caused by expired or missing keyring signatures.
  (setq package-check-signature nil)
  ;; TLS workaround for older Emacs versions (<= 29.1) to prevent handshake issues
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Add MELPA repositories for bleeding-edge and stable packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Initialize installed packages if not already initialized
(unless package--initialized
  (package-initialize))

;; Install and load `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Automatically download and install missing packages by default
(setq use-package-always-ensure t)

;; Enable verbose logging so package loading errors or timing issues are logged
(setq use-package-verbose t)

(provide 'pathogen-package-manager)
;;; pathogen-package-manager.el ends here
