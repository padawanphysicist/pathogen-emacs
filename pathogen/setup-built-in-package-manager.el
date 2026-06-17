;;; setup-built-in-package-manager.el --- Setup package manager system  -*- lexical-binding: t; -*-

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

;; Initialize the built-in package system and configure the MELPA repository for
;; Emacs 24.1 and above. To optmize startup, we first activate installed
;; packages using the version-appropriate method (`package-activate-all' for
;; Emacs 27+ or `package-initialize' for older versions). We then load the local
;; cache from disk; if no local archive metadata is found (e.g., on a fresh
;; installation), a network refresh is automatically triggered.

;;; Code:

(when (version<= "26.3" emacs-version)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Refresh package contents periodically (e.g., every 2 days)
(let* ((archive-dir (expand-file-name "elpa/archives/melpa/archive-contents" user-emacs-directory))
       (days-between-updates 2) ; TODO add as a defcustom variable
       (seconds-between-updates (* days-between-updates 24 60 60)))

  (if (or (not package-archive-contents) ; if no cache
          (not (file-exists-p archive-dir)) ; if there is no file
          (> (float-time (time-since (file-attribute-modification-time (file-attributes archive-dir))))
             seconds-between-updates)) ; if file is older than interval
      (progn
        (message "MELPA cache is deprecated. Updating index...")
        (package-refresh-contents))
    (message "MELPA cache is up-to-date (< %d days)." days-between-updates)))

;; Activate packages according the installed emacs version
(if (version<= "27.1" emacs-version)
    (package-activate-all))

;; Declarative Package Management (use-package)

;; Configure `use-package' to enable clean, declarative package isolation.
;; Since `use-package' is built-in starting with Emacs 29.1, we conditionally
;; install it from downstream repositories only when running on older Emacs
;; versions. Additionally, `use-package-always-ensure' is enabled globally to
;; automatically fetch and install missing packages during startup without
;; requiring explicit `:ensure t' keywords in every declaration.

;; Install use-package if it's not already there
(when (version< emacs-version "29.1")
  (unless (package-installed-p 'use-package)
    (condition-case nil
        (package-install 'use-package)
      (error
       (message "Failed upon installing use-package. Updating MELPA index...")
       (package-refresh-contents)
       (package-install 'use-package)))))

;; Activates use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; If any package declared with `use-package' fails to install, this hook forces
;; an update of MELPA and attempts to install again automatically.
(add-hook 'use-package-ensure-failed-hook
          (lambda (package error)
            (message "Failed installing %s due to: %s. Trying to update MELPA..." package error)
            (package-refresh-contents)
            (package-install package)))

(provide 'setup-built-in-package-manager)
;;; setup-built-in-package-manager.el ends here

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; outline-regexp: ";;;+"
;; outline-minor-mode-use-buttons: t
;; outline-minor-mode-cycle: t
;; fill-column: 80
;; End:
