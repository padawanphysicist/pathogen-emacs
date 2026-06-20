;;; early-init.el --- Early initialization file -*- lexical-binding: t; -*-

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

;; Emacs 27.1 introduced the early init file (called "early-init.el", see
;; `early-init-file'), which is loaded before the regular init file. This file
;; is loaded very early in the startup process, before the package manager and
;; graphical user interface (GUI) are initialized.

;; Core respnsibilities:
;;
;; 1. Control package initialization
;;    - Skipping default package loading: prevents the built-in `package.el' from
;;      scanning and activating extensions automatically. This is required if you
;;      use other package manager than the default.
;; 2. Override default variables:
;;    - Inhibit welcomes: silences splash screens, startup echo messages and
;;      default scratch text before they try to render.
;;    - Configure early paths: sets early environment variables or load paths
;;      before init file is loaded.
;; 3. Prevent unwanted flashing of visual artifacts
;;    - Early GUI stripping: turns off the menu bar, tool bar, and scroll bars
;;      before the initial frame is rendered.
;;    - Frame parameter pre-setting: defines the basic behaviour so the first
;;      frame loads in its correct desired state.
;; 4. Optimizing Startup Performance:
;;    - Garbage Collector tuning: temporarily defer garbage collector to the end
;;      of emacs startup, and tune it to not interferring during regular session.
;;    - File handler deferral: disable file name handlers (like magic file names)
;;      during startup to speed up the reading and loading of scripts.
;;
;; For most customizations, use the regular init.el file instead.

;; References:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;; - https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00237.html
;; - https://www.gnu.org/savannah-checkouts/gnu/emacs/news/NEWS.27.1
;; - https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.27

;;; Code:

;;;; Defer package initialization

;; Prior to Emacs 27, the init file was responsible for initializing the package
;; manager by explicitly calling `package-initialize'. Starting with Emacs 27,
;; the default behavior changed: `package-initialize' is now automatically
;; called before loading the init file. This means package initialization occurs
;; after early init file is loaded but before init file is processed. To prevent
;; Emacs from initializing packages automatically, we set
;; `package-enable-at-startup' to nil:
(when (version<= "27" emacs-version)
  (setq package-enable-at-startup nil))

;;;; Add Pathogen directories to  `load-path'
(let ((default-directory (expand-file-name "pathogen/" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(require 'environment)

(provide 'early-init)
;;; early-init.el ends here
