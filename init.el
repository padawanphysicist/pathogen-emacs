;;; init.el --- Main initialization file for Emacs -*- lexical-binding: t; fill-column: 79; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Code that you want to execute when you start Emacs.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Manager Initialization
;;
;; Initialize the built-in package system and configure the MELPA repository
;; for Emacs 24.1 and above. To optimize startup, we first activate installed
;; packages using the version-appropriate method (`package-activate-all' for
;; Emacs 27+ or `package-initialize' for older versions). We then load the
;; local cache from disk; if no local archive metadata is found (e.g., on a
;; fresh installation), a network refresh is automatically triggered.
;;
(when (version<= "24.1" emacs-version)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; 1. Activate packages according the installed emacs version
  (if (version<= "27.1" emacs-version)
      (package-activate-all)
    (package-initialize))

  ;; 2. Forces Emacs to read local cache first
  (package-read-all-archive-contents)

  ;; 3. Refresh package contents if they haven't been downloaded yet
  (unless package-archive-contents
    (package-refresh-contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Declarative Package Management (use-package)
;;
;; Configure `use-package' to enable clean, declarative package isolation.
;; Since `use-package' is built-in starting with Emacs 29.1, we conditionally
;; install it from downstream repositories only when running on older Emacs
;; versions. Additionally, `use-package-always-ensure' is enabled globally
;; to automatically fetch and install missing packages during startup without
;; requiring explicit `:ensure t' keywords in every declaration.
;;

;; Install use-package if it's not already there
(when (version<= "29.0" emacs-version)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

;;; init.el ends here
