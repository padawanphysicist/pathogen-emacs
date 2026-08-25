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

 (defvar elpaca-installer-version 0.12)
      (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
      (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
      (defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
      (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				    :ref nil :depth 1 :inherit ignore
				    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
				    :build (:not elpaca-activate)))
      (let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
	     (build (expand-file-name "elpaca/" elpaca-builds-directory))
	     (order (cdr elpaca-order))
	     (default-directory repo))
	(add-to-list 'load-path (if (file-exists-p build) build repo))
	(unless (file-exists-p repo)
	  (make-directory repo t)
	  (when (<= emacs-major-version 28) (require 'subr-x))
	  (condition-case-unless-debug err
              (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
			((zerop (apply #'call-process `("git" nil ,buffer t "clone"
							,@(when-let* ((depth (plist-get order :depth)))
							    (list (format "--depth=%d" depth) "--no-single-branch"))
							,(plist-get order :repo) ,repo))))
			((zerop (call-process "git" nil buffer t "checkout"
                                              (or (plist-get order :ref) "--"))))
			(emacs (concat invocation-directory invocation-name))
			((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                              "--eval" "(byte-recompile-directory \".\" 0 'force)")))
			((require 'elpaca))
			((elpaca-generate-autoloads "elpaca" repo)))
		  (progn (message "%s" (buffer-string)) (kill-buffer buffer))
		(error "%s" (with-current-buffer buffer (buffer-string))))
	    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
	(unless (require 'elpaca-autoloads nil t)
	  (require 'elpaca)
	  (elpaca-generate-autoloads "elpaca" repo)
	  (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
      (add-hook 'after-init-hook #'elpaca-process-queues)
      (elpaca `(,@elpaca-order))
      ;;(elpaca elpaca-order)

      ;; Install use-package support
      (elpaca elpaca-use-package
	;; Enable use-package :ensure support for Elpaca.
	(elpaca-use-package-mode))
      
      ;; Automatically download and install missing packages by default
      (setq elpaca-use-package-by-default t)

      (elpaca-wait)

;; (if (>= emacs-major-version 29)
;;     ;; Configure Elpaca (https://github.com/progfolio/elpaca)
;;     (progn
;;      )
;;   ;; Configure package.el
;;   (progn
;;     ;;  Initialize package manager
;;     (require 'package)

;;     ;; Disable GPG signature verification temporarily to avoid "Bad Signature"
;;     ;; errors caused by expired or missing keyring signatures.
;;     (setq package-check-signature nil)
;;     ;; TLS workaround for older Emacs versions (<= 29.1) to prevent handshake issues
;;     (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;     ;; Add MELPA repositories for bleeding-edge and stable packages
;;     (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;     (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;     ;; Initialize installed packages if not already initialized
;;     (unless package--initialized
;;       (package-initialize))

;;     ;; Install and load `use-package'
;;     (unless (package-installed-p 'use-package)
;;       (package-refresh-contents)
;;       (package-install 'use-package))
;;     (eval-when-compile
;;       (require 'use-package))

;;     ;; Automatically download and install missing packages by default
;;     (setq use-package-always-ensure t)

;;     ;; Enable verbose logging so package loading errors or timing issues are logged
;;     (setq use-package-verbose t)))

(provide 'pathogen-package-manager)
;;; pathogen-package-manager.el ends here
